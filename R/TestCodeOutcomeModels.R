TestCodeOutcomeModels <- function(){
  library(CohortMethod)
  setwd("c:/temp")
  
  #Settings for running SQL against a local Postgres DB:
  pw <- "F1r3starter"
  dbms <- "postgresql"
  user <- "postgres"
  server <- "localhost/ohdsi"
  cdmSchema <- "cdm4_sim"
  resultsSchema <- "scratch"
  port <- "5432"
  
  #Settings for running SQL in the IMEDS lab:
  pw <- "" #see e-mail
  dbms <- "redshift"
  user <- "demosymp"
  server <- "omop-datasets.cqlmv7nlakap.us-east-1.redshift.amazonaws.com/truven"
  cdmSchema <- "mslr_cdm4"
  port <- "5439"
  
  #Settings for running SQL against JnJ Sql Server:
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07"
  cdmSchema <- "cdm4_sim"
  resultsSchema <- "scratch"
  port <- NULL
  
    
  #Part one: loading the data:
  connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema,port=port)
  
  cohortData <- dbGetCohortData(connectionDetails,cdmSchema=cdmSchema,resultsSchema=resultsSchema)
  
  save.cohortData(cohortData,file.path(getwd(),"simCohortData"))
  
  cohortData <- load.cohortData(file.path(getwd(),"simCohortData"))
  
  #Part two: Creating propensity scores, and match people on propensity score:
  ps <- psCreate(cohortData, prior=prior("laplace",0.1))

  strata <- psMatch(ps, caliper = 0.25, caliperScale = "standardized",maxRatio=0)
  strata <- psMatch(ps, caliper = 0.25, caliperScale = "standardized",maxRatio=0, stratificationColumns=c("AGE"))
    
  #Part three: fit an outcome model (This should all be part of the estimateEffect function):
  riskWindowStart = 0
  riskWindowEnd = 9999
  addExposureDaysToEnd = FALSE
  useCovariates = TRUE
  modelType = "cpr"
  
  useStrata = (modelType == "clr" | modelType == "cpr")
  
  outcomes <- cohortData$outcomes
  colnames(outcomes) <- toupper(colnames(outcomes))
  
  cohorts <- as.ram(cohortData$cohorts)
  colnames(cohorts) <- toupper(colnames(cohorts))
  
  if (useStrata) {
    colnames(strata) <- toupper(colnames(strata))
    cohorts <- merge(strata,cohorts) #keeping only persons that have been matched
  }
  
  if (useCovariates){
    covariates <- cohortData$covariates
    colnames(covariates) <- toupper(colnames(covariates))  
    
    #Add treatment status as a set of covariates:
    treatment <- data.frame(ROW_ID = cohorts$ROW_ID[cohorts$TREATMENT == 1],COVARIATE_ID = 100, COVARIATE_VALUE = 1)
    covariates = ffdfappend(covariates,treatment)
  }
  
  ### Conditional Poisson regression ###
  outcomes <- aggregate(TIME_TO_OUTCOME ~ ROW_ID,data=outcomes,length) #count outcomes per person
  data <- merge(cohorts,outcomes, all.x=TRUE)
  data$Y <- data$TIME_TO_OUTCOME
  data$Y[is.na(data$Y)] <- 0
  data$TIME <- data$TIME_TO_CENSOR
  data <- data[data$TIME > 0,] #Remove zero-length time intervals
  
  if (useStrata) { #data needs to be sorted by stratum_id then row_id
    data <- data[order(data$STRATUM_ID,data$ROW_ID),]
    covariates <- merge(covariates,as.ffdf(data[c("ROW_ID","STRATUM_ID")]))    
    covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","ROW_ID")]),]
  } else {#data needs to be sorted by row_id
    data <- data[order(data$ROW_ID),]
    covariates <- merge(covariates,as.ffdf(data[c("ROW_ID")]))    
    covariates <- covariates[ffdforder(covariates[c("ROW_ID")]),]
  }

  #Fit outcome model without covariates:
  cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT + strata(STRATUM_ID) + offset(TIME),data=data, modelType = "cpr")
  fit <- fitCyclopsModel(cyclopsData, prior=prior("none"))
  logRr <- coef(fit)[1]
  ci <-confint(fit,parm=1)
  effectSize <-  data.frame(LOGRR = logRr, LOGLB95 = ci[2], LOGUB95 = ci[3])
  print(effectSize)
  
  #Fit outcome model With covariates:
  cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cpr",addIntercept=FALSE)
  #a <- summary(cyclopsData)
  #a[1:10,]
  fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1, exclude=100)) 
  logRr <- coef(fit)[names(coef(fit)) == "100"]
  ci <- confint(fit,parm=100)
  effectSize <-  data.frame(LOGRR = logRr, LOGLB95 = ci[2], LOGUB95 = ci[3])
  print(effectSize)
  
  
  ### Conditional logistic regression ###
  if (useCovariates){
    covariates <- cohortData$covariates
    colnames(covariates) <- toupper(colnames(covariates))  
    
    #Add treatment status as a set of covariates:
    treatment <- data.frame(ROW_ID = cohorts$ROW_ID[cohorts$TREATMENT == 1],COVARIATE_ID = 100, COVARIATE_VALUE = 1)
    covariates = ffdfappend(covariates,treatment)
  }
  
  data <- cohorts
  data$Y <- 0
  data$Y[data$ROW_ID %in% as.ram(outcomes$ROW_ID)] <- 1

  if (useStrata) { #data needs to be sorted by stratum_id then row_id
    data <- data[order(data$STRATUM_ID,data$ROW_ID),]
    covariates <- merge(covariates,as.ffdf(data[c("ROW_ID","STRATUM_ID")]))    
    covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","ROW_ID")]),]
  } else {#data needs to be sorted by row_id
    data <- data[order(data$ROW_ID),]
    covariates <- merge(covariates,as.ffdf(data[c("ROW_ID")]))    
    covariates <- covariates[ffdforder(covariates[c("ROW_ID")]),]
  }
  
  #Fit outcome model without covariates:
  cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT + strata(STRATUM_ID),data=data, modelType = "clr")
  fit <- fitCyclopsModel(cyclopsData, prior=prior("none"))
  logRr <- coef(fit)[1]
  ci <-confint(fit,parm=1)
  effectSize <-  data.frame(LOGRR = logRr, LOGLB95 = ci[2], LOGUB95 = ci[3])
  print(effectSize)
  #fit2 <- clogit(Y ~ TREATMENT + strata(STRATUM_ID),data=data)
  #coef(fit2)
  #confint(fit2)
  
  #Fit outcome model With covariates:
  cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="clr",addIntercept=FALSE)
  #a <- summary(cyclopsData)
  #a[1:10,]
  fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1, exclude=100)) 
  logRr <- coef(fit)[names(coef(fit)) == "100"]
  ci <- confint(fit,parm=100)
  effectSize <-  data.frame(LOGRR = logRr, LOGLB95 = ci[2], LOGUB95 = ci[3])
  print(effectSize)
}
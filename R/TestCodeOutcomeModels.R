TestCodeOutcomeModels <- function(){
  library(CohortMethod)
  setwd("c:/temp")
  
  #Settings for running SQL against a local Postgres DB:
  pw <- ""
  dbms <- "postgresql"
  user <- "postgres"
  server <- "localhost/ohdsi"
  cdmSchema <- "cdm4_sim"
  port <- "5432"
  
  #Settings for running SQL in the IMEDS lab:
  pw <- "" #see e-mail
  dbms <- "redshift"
  user <- "demosymp"
  server <- "omop-datasets.cqlmv7nlakap.us-east-1.redshift.amazonaws.com/truven"
  cdmSchema <- "mslr_cdm4"
  port <- "5439"
    
  #Part one: loading the data:
  connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema,port=port)
  
  cohortData <- dbGetCohortData(connectionDetails,cdmSchema=cdmSchema)
  
  save.cohortData(cohortData,file.path(getwd(),"simCohortData"))
  
  cohortData <- load.cohortData(file.path(getwd(),"simCohortData"))
  
  #Part two: Creating propensity scores, and match people on propensity score:
  ps <- psCreate(cohortData, prior=prior("laplace",0.1))
  
  psAuc(ps)
  
  psPlot(ps) #Plot unmatched PS distributions
  
  strata <- psMatch(ps, caliper = 0.25, caliperScale = "standardized",maxRatio=0)
  
  psPlot(strata) #Plot matched PS distributions
  
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
    
    if (useStrata) { #covariates need to be sorted by stratum_id then row_id
      covariates <- merge(covariates,as.ffdf(cohorts[c("ROW_ID","STRATUM_ID")]))    
      covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","ROW_ID")]),]
    } else {#covariates need to be sorted by row_id
      covariates <- covariates[ffdforder(covariates[c("ROW_ID")]),]
    }
  }
  
  #Poisson regression specific code:
  outcomes <- aggregate(TIME_TO_OUTCOME ~ ROW_ID,data=outcomes,length) #count outcomes per person
  data <- merge(cohorts,outcomes, all.x=TRUE)
  data$Y <- data$TIME_TO_OUTCOME
  data$Y[is.na(data$Y)] <- 0
  data$TIME <- data$TIME_TO_CENSOR
  data <- data[order(data$STRATUM_ID,data$ROW_ID),]
  
  #Fit outcome model without covariates:
  cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT + strata(STRATUM_ID) + offset(TIME),data=data, modelType = "cpr")
  fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1))
  #works just fine
  
  #Fit outcome model With covariates:
  cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cpr",addIntercept=FALSE)
  fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1)) 
  #currently says: Warning! problem is ill-conditioned for this choice of hyperparameter. Enforcing convergence!

}
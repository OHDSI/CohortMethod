
testCode <- function(){
  
  # Note: on machines with lots of RAM, use    Rgui --max-mem-size=32000M     to use ffbase functions
  # See https://github.com/edwindj/ffbase/issues/37
  #
  # If ff is complaining it can't find the temp folder, use   options("fftempdir" = "c:/temp")
  
  pw <- "F1r3starter"
  user <- "postgres"
  server <- "localhost/ohdsi"
  cdmSchema = "cdm4_sim"
  useFf = TRUE
  
  setwd("c:/temp")
  connectionDetails <- createConnectionDetails(dbms="postgresql", server=server, user=user, password=pw, schema=cdmSchema)

  cohortData <- dbGetCohortData(connectionDetails,cdmSchema=cdmSchema, useFf = useFf)
  save.cohortData(cohortData,file.path(getwd(),"simCohortDataFf"))
  
  cohortData <- load.cohortData(file.path(getwd(),"simCohortDataFf2"))
  
  ps <- psCreate(cohortData, prior=prior("laplace",0.1))
  
  psAuc(ps)
  
  model <- psShowModel(ps,connectionDetails,cdmSchema=cdmSchema)
  
  psPlot(ps) #Plot unmatched PS distributions
  
  strata <- psMatch(ps, caliper = 0.25, caliperScale = "standardized",maxRatio=0)
 
  psPlot(strata) #Plot matched PS distributions
  
  fit <- estimateEffect(cohortData, strata, useCovariates = FALSE, modelType = "cox")
  
  
  
  ### Old stuff : ###

  #cdmSchema = "CDM_Truven_MDCR"
  #connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07",schema=cdmSchema)
  
  
  conn <- connect(connectionDetails)
  dbSendUpdate(conn,"CREATE TEMP TABLE ccd_outcome_input_for_ps AS SELECT * FROM temp_outcomes")
  dbSendUpdate(conn,"CREATE TEMP TABLE ccd_covariate_input_for_ps AS SELECT * FROM temp_covariates")
  outcomeSql <-"SELECT * FROM #ccd_outcome_input_for_ps ORDER BY stratum_id,row_id"
  outcomeSql <- translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql
  covariateSql <-"SELECT * FROM #ccd_covariate_input_for_ps ORDER BY stratum_id,row_id,covariate_id"
  covariateSql <- translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql

  outcomes1 <- dbGetQuery.ffdf(conn,outcomeSql)
  covariates1 <- dbGetQuery.ffdf(conn,covariateSql)
  ccdData1 <- createCcdData.ffdf(outcomes1,covariates1,modelType="lr")
  
  outcomes2 <- dbGetQuery(conn,outcomeSql)
  covariates2 <- dbGetQuery(conn,covariateSql)
  ccdData2 <- createCcdData(outcomes2,covariates2,modelType="lr")
  
  ccdData3 <- dbCreateCcdData(conn,outcomeSql,covariateSql,modelType = "lr")
  
  ccdFit1 <- fitCcdModel(ccdData1, prior = prior("laplace",0.1))
  ccdFit2 <- fitCcdModel(ccdData2, prior = prior("laplace",0.1))
  ccdFit3 <- fitCcdModel(ccdData3, prior = prior("laplace",0.1))
  min(coef(ccdFit1) == coef(ccdFit2))
  min(coef(ccdFit2) == coef(ccdFit3))
  min(coef(ccdFit1) == coef(ccdFit3))
  
  
  ### Populate temp tables for propensity scores ###
  # Option 1: Run full query against DB:
  renderedSql <- loadRenderTranslateSql("CohortMethod.sql",packageName = "CohortMethod",dbms = connectionDetails$dbms,CDM_schema = cdmSchema)
  executeSql(conn,connectionDetails$dbms,renderedSql,TRUE)
  dbSendUpdate(conn,"DROP TABLE IF EXISTS temp_outcomes");
  dbSendUpdate(conn,"DROP TABLE IF EXISTS temp_covariates");
  dbSendUpdate(conn,"CREATE TABLE temp_outcomes AS SELECT * FROM ccd_outcome_input_for_ps")
  dbSendUpdate(conn,"CREATE TABLE temp_covariates AS SELECT * FROM ccd_covariate_input_for_ps")
  
  # Option 2: Retrieve stored tables:
  dbSendUpdate(conn,"CREATE TEMP TABLE ccd_outcome_input_for_ps AS SELECT * FROM temp_outcomes")
  dbSendUpdate(conn,"CREATE TEMP TABLE ccd_covariate_input_for_ps AS SELECT * FROM temp_covariates")
  
  
  ### Get PS data from temp tables ###
  outcomeSql <-"SELECT * FROM #ccd_outcome_input_for_ps ORDER BY stratum_id,row_id"
  outcomeSql <- translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql
  covariateSql <-"SELECT * FROM #ccd_covariate_input_for_ps ORDER BY stratum_id,row_id,covariate_id"
  covariateSql <- translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql
 
  #Option 1: Load data for propensity score directly to CcdData:
  startQuery <- Sys.time()
  ccdData <- dbCreateCcdData(conn,outcomeSql,covariateSql,modelType = "lr")
  ccdDelta <- Sys.time() - startQuery
  writeLines(paste("Load took", signif(ccdDelta,3), attr(ccdDelta,"units")))
  
  #Option 2: load data for propensity score via ffdf:
  startQuery <- Sys.time()
  outcomes <- dbGetQuery.ffdf(conn,outcomeSql)
  covariates <- dbGetQuery.ffdf(conn,covariateSql)
  ccdData <- createCcdData.ffdf(outcomes,covariates,modelType="lr")
  ccdDelta <- Sys.time() - startQuery
  writeLines(paste("Load took", signif(ccdDelta,3), attr(ccdDelta,"units")))
  save.ffdf(outcomes,covariates,dir=file.path(getwd(),"ffdb"))
  outcomes <- as.ram(outcomes)
  covariates <- as.ram(covariates)
  save(outcomes,covariates,file=file.path(getwd(),"df.rda"))
  
  #Option 3: load data from saved ffdf objects:
  load.ffdf(file.path(getwd(),"ffdb"))
  ccdData <- createCcdData.ffdf(outcomes,covariates,modelType="lr")
 
  #Option 4: load data from saved data frame objects:
  load(file.path(getwd(),"df.rda"))
  ccdData <- createCcdData(outcomes,covariates,modelType="lr")
  
  
  ### Fit propensity score mode, and match ###
  ccdFit <- fitCcdModel(ccdData, prior = prior("laplace",0.1))
  p <- predict(ccdFit)
  if (is.null(outcomes)){
    ySql <-"SELECT row_id,y FROM #ccd_outcome_input_for_ps"
    ySql <- translateSql(ySql,"sql server",connectionDetails$dbms)$sql
    y <- dbGetQuery(conn,ySql)
  } else 
    y <- as.ram(outcomes[,c("row_id","y")])
  colnames(y) <- toupper(colnames(y))
  colnames(y)[colnames(y) == "ROW_ID"] <- "rowId"
  colnames(y)[colnames(y) == "Y"] <- "treatment"
  data <- data.frame(propensityScore = p, rowId = as.numeric(attr(p,"names")))
  data <- merge(data,y,by="rowId")

  psAuc(data)
  #psPlot(data) #Plot unmatched PS distributions
  matchedData <- psMatch(data, caliper = 0.25, caliperScale = "standardized",maxRatio=10)
  #psPlot(matchedData) #Plot matched PS distributions
  save(matchedData,file="matchedData")
  
  ### Populate outcome data temp tables### 

  # Option 1: Run full query against DB:
  renderedSql <- loadRenderTranslateSql("CohortMethod.sql",packageName = "CohortMethod",dbms = connectionDetails$dbms,CDM_schema = cdmSchema)
  executeSql(conn,connectionDetails$dbms,renderedSql,TRUE)
  dbSendUpdate(conn,"DROP TABLE IF EXISTS temp_outcomes_o");
  dbSendUpdate(conn,"DROP TABLE IF EXISTS temp_covariates_o");
  dbSendUpdate(conn,"CREATE TABLE temp_outcomes_o AS SELECT * FROM ccd_outcome_input_for_outcome")
  dbSendUpdate(conn,"CREATE TABLE temp_covariates_o AS SELECT * FROM ccd_covariate_input_for_outcome")
  
  # Option 2: Retrieve stored tables:
  dbSendUpdate(conn,"CREATE TEMP TABLE ccd_outcome_input_for_outcome AS SELECT * FROM temp_outcomes_o")
  dbSendUpdate(conn,"CREATE TEMP TABLE ccd_covariate_input_for_outcome AS SELECT * FROM temp_covariates_o")
  
  ### Load outcome data, and merge with stratification from matching ***
  outcomeSql <-"SELECT * FROM #ccd_outcome_input_for_outcome ORDER BY row_id"
  outcomeSql <- translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql
  covariateSql <-"SELECT * FROM #ccd_covariate_input_for_outcome ORDER BY row_id, covariate_id"
  covariateSql <- translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql
  outcomesForO <- dbGetQuery.ffdf(conn,outcomeSql)
  covariatesForO <- dbGetQuery.ffdf(conn,covariateSql)
  #outcomesForO <- dbGetQuery(conn,outcomeSql)
  #covariatesForO <- dbGetQuery(conn,covariateSql)
  
  load(file="matchedData")
  colnames(outcomesForO) <- toupper(colnames(outcomesForO))
  colnames(covariatesForO) <- toupper(colnames(covariatesForO))
  colnames(matchedData) <- toupper(colnames(matchedData))
  colnames(matchedData)[colnames(matchedData) == "ROWID"]  <- "ROW_ID"
  colnames(matchedData)[colnames(matchedData) == "STRATUMID"]  <- "STRATUM_ID"
  y <- merge(outcomesForO[,c("ROW_ID","Y","TIME")],matchedData[,c("ROW_ID","STRATUM_ID","TREATMENT")],by="ROW_ID")
  #y <- as.ffdf(y)
  #rowIds <- as.ram(y$ROW_ID)
  rowIds <- y$ROW_ID
  x <-subset(covariatesForO,ROW_ID %in% rowIds)
  save(y,file="y.rda")
  
  
  ### Build outcome model
  load("y.rda")
  
  # Option 1: only use treatment as covariate, use clogit
  library(survival)
  fit1 <- clogit(Y ~ TREATMENT + strata(STRATUM_ID),data=y)
  exp(coef(fit1))
  exp(confint(fit1, c(1)))
  
  # Option 2: only use treatment as covariate, use CCD CLR
  #Doesn't work yet:
  #outc <- y
  #cova <- data.frame(ROW_ID = y$ROW_ID, COVARIATE_ID = rep(1,nrow(y)), COVARIATE_VALUE = y$TREATMENT)
  #ccdData <- createCcdData(outc,cova,modelType="clr", addIntercept=FALSE)
  #fit2 <- fitCcdModel(ccdData, prior = prior("none"))
  dp <- createCcdDataFrame(Y ~ TREATMENT + strata(STRATUM_ID),data=y, modelType = "clr")
  fit2 <- fitCcdModel(dp, prior = prior("none"))
  exp(coef(fit2))
  exp(confint(fit2, c(1)))
  
  # Option 3: use full covariate matrix
  
  # Option 4: only use treatment as covariate, use coxph
  y$START <- 0
  fit <- coxph( Surv(START, TIME, Y) ~ TREATMENT + strata(STRATUM_ID),data=y )   
  
  
  
  dbDisconnect(conn)
  return(NULL)
  
   
  #cross-validation example:
  ccdFit <- fitCcdModel(ccdData, 
                        prior = prior("laplace", useCrossValidation = TRUE),
                        control = control(cvType = "auto", cvRepetitions = 2, noiseLevel = "quiet"))
  
}

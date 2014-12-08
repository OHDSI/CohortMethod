TestCodePatrick <- function(){
  ### Test code ###
  library(CohortMethod)
  setwd("c:/temp")
  
  # If ff is complaining it can't find the temp folder, use   options("fftempdir" = "c:/temp")
  
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07"
  #cdmSchema <- "cdm4_sim"
  cdmSchema <- "CDM_Truven_MDCR"
  resultsSchema <- "scratch"
  port <- NULL
  
  connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema,port=port)
  
  
  cohortData <- getDbCohortData(connectionDetails,cdmSchema=cdmSchema,resultsSchema=resultsSchema,
                                      targetDrugConceptId = 1,
                                      comparatorDrugConceptId = 2, 
                                      indicationConceptIds = "",
                                      washoutWindow = 183, 
                                      indicationLookbackWindow = 183,
                                      studyStartDate = "", 
                                      studyEndDate = "", 
                                      exclusionConceptIds = "",
                                      outcomeConceptIds = c(1000028060,1000134438,1035406331,1035406349,1035406361,1035406391,1035406402,1035506612,1035506621,1035606949,1035607026,1035607337,1035607461,1036110386,1036110587,1036110933,1036110951,1036211101,1036313741,1036314156,1036315380,1036315910,1036315934,1036416637,1036416695,1036516895,1036516905,1036516909,1036617158,1036617163,1036617187,1036617553,1036718555,1036918850,1036918858,1036919212,1037019460,1037119529,1037119539,1037119607,1037320079,1037320098,1037320318,1037420426,1037420593,1037521024,1037522022), 
                                      outcomeConditionTypeConceptIds = "", 
                                      maxOutcomeCount = 1,
                                      exposureSchema = resultsSchema,
                                      exposureTable = "MDCR_RivaWarfCER_cohort",
                                      outcomeSchema = resultsSchema,
                                      outcomeTable = "MDCR_RivaWarfCER_cohort",
                                      useCovariateDemographics = TRUE, 
                                      useCovariateConditionOccurrence = TRUE,
                                      useCovariateConditionEra = FALSE, 
                                      useCovariateConditionGroup = FALSE,
                                      useCovariateDrugExposure = FALSE, 
                                      useCovariateDrugEra = FALSE,
                                      useCovariateDrugGroup = FALSE, 
                                      useCovariateProcedureOccurrence = FALSE,
                                      useCovariateProcedureGroup = FALSE, 
                                      useCovariateObservation = FALSE,
                                      useCovariateConceptCounts = FALSE, 
                                      useCovariateRiskScores = FALSE,
                                      useCovariateInteractionYear = FALSE, 
                                      useCovariateInteractionMonth = FALSE,
                                      excludedCovariateConceptIds = "", 
                                      deleteCovariatesSmallCount = 100)
  
  saveCohortData(cohortData,"mdcrCohortDataNegControls")
  
  cohortData <- loadCohortData("mdcrCohortDataNegControls") 
 
  summary(cohortData)
  
  #Part two: Creating propensity scores, and match people on propensity score:
  ps <- createPs(cohortData, outcomeConceptId = 1000134438, prior=createPrior("laplace",0.1))
  
  computePsAuc(ps)
  
  propensityModel <- getPsModel(ps,cohortData)
  
  plotPs(ps)
  
  psTrimmed <- trimByPsToEquipoise(ps)
  
  plotPs(psTrimmed,ps) #Plot trimmed PS distributions
  
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=1)
  
  plotPs(strata,ps) #Plot matched PS distributions
  
  balance <- computeCovariateBalance(strata, cohortData, outcomeConceptId = 1000134438)
  
  plotCovariateBalanceScatterPlot(balance,fileName = "balanceScatterplot.png")
  
  plotCovariateBalanceOfTopVariables(balance,fileName = "balanceTopVarPlot.png")
  
  
  #Part three: Fit the outcome model:
  outcomeModel <- fitOutcomeModel(1000134438,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 180, addExposureDaysToEnd = TRUE,useCovariates = TRUE, modelType = "cox", prior=createPrior("laplace",0.1))
  
  plotKaplanMeier(outcomeModel)
  
  fullOutcomeModel <- getOutcomeModel(outcomeModel,cohortData)
  
  summary(outcomeModel)
  
  coef(outcomeModel)
  
  confint(outcomeModel)
  
  #This code retrieves the list of cohort_ids:
  sql <- "SELECT cohort_definition_id,count(subject_id) as num_persons FROM [Scratch].[dbo].[MDCR_RivaWarfCER_cohort] group by cohort_definition_id order by cohort_definition_id"
  x <-querySql(connect(connectionDetails),sql)
  paste(x$COHORT_DEFINITION_ID[x$COHORT_DEFINITION_ID > 1000000000 & x$COHORT_DEFINITION_ID < 2000000000],collapse=",")
}
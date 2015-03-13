#' @keywords internal
testCode <- function(){
  ### Test code ###
  library(CohortMethod)
  setwd("s:/temp")
  options("fftempdir" = "s:/temp")

  #Settings for running SQL against JnJ Sql Server:
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07"
  cdmSchema <- "cdm4_sim"
  #cdmSchema <- "CDM_Truven_MDCR"
  resultsSchema <- "scratch"
  port <- NULL
  
  pw <- NULL
  dbms <- "pdw"
  user <- NULL
  server <- "JRDUSHITAPS01"
  cdmSchema <- "CDM_Truven_MDCR"
  resultsSchema <- "CDM_Truven_MDCR"
  port <- 17001

  pw <- pw
  dbms <- "postgresql"
  user <- "postgres"
  server <- "localhost/ohdsi"
  cdmSchema <- "cdm_truven_ccae_6k"
  port <- NULL
  
  
  #Part one: loading the data:
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema,port=port)

  
  
  cohortData <- getDbCohortData(connectionDetails,
                                cdmDatabaseSchema=cdmSchema,
                                targetDrugConceptId = 755695,
                                comparatorDrugConceptId = 739138,
                                indicationConceptIds = 439926,
                                exclusionConceptIds = c(4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019),
                                outcomeConceptIds = 194133,
                                excludedCovariateConceptIds = c(4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019),
                                deleteCovariatesSmallCount = 0
                                )

  saveCohortData(cohortData, "s:/temp/cohortData")
  cohortData <- loadCohortData("s:/temp/cohortData")
  
  
  
  summary(cohortData)
  
  #Part two: Creating propensity scores, and match people on propensity score:
  ps <- createPs(cohortData, outcomeConceptId = 194133, prior=createPrior("laplace",1,exclude=c(0)))
  ps <- createPs(cohortData,outcomeConceptId = 194133)
   
  computePsAuc(ps)
  #computePsAuc(ps2)
  
  propensityModel <- getPsModel(ps,cohortData)
  
  plotPs(ps)
  
  psTrimmed <- trimByPsToEquipoise(ps)
  
  plotPs(psTrimmed,ps) #Plot trimmed PS distributions
  
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=1)

  plotPs(strata,ps) #Plot matched PS distributions
  
  balance <- computeCovariateBalance(strata, cohortData, outcomeConceptId = 194133)
  
  plotCovariateBalanceScatterPlot(balance,fileName = "balanceScatterplot.png")
  
  plotCovariateBalanceOfTopVariables(balance,fileName = "balanceTopVarPlot.png")
  
  
  ####
  cohortData <- loadCohortData("cohortData") 
  ps <- createPs(cohortData, outcomeConceptId = 194133, prior=createPrior("laplace",0.1,exclude=c(0)))
  psTrimmed <- trimByPsToEquipoise(ps)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=1)
  
  
  #Part three: Fit the outcome model:
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "cox", prior=createPrior("laplace",0.1))
  
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "clr", prior=createPrior("laplace",0.1))
  
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "pr", prior=createPrior("laplace",0.1))
 
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "lr", prior=createPrior("laplace",0.1))
  #
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, stratifiedCox =  FALSE, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = FALSE, modelType = "cox", prior=createPrior("laplace",0.1))
  #
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = FALSE, modelType = "clr", prior=createPrior("laplace",0.1))
  
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = FALSE, modelType = "pr", prior=createPrior("laplace",0.1))
  
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = FALSE, modelType = "lr", prior=createPrior("laplace",0.1))
  
  
  plotKaplanMeier(outcomeModel)
  
  fullOutcomeModel <- getOutcomeModel(outcomeModel,cohortData)

  summary(outcomeModel)
  
  coef(outcomeModel)
  
  confint(outcomeModel)
  
  drawAttritionDiagram(outcomeModel)
}

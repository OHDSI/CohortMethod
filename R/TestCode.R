
testCode <- function(){
  ### Test code ###
  library(CohortMethod)
  setwd("c:/temp")
  
  # If ff is complaining it can't find the temp folder, use   options("fftempdir" = "c:/temp")

  #Settings for running SQL against JnJ Sql Server:
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07"
  #cdmSchema <- "cdm4_sim"
  cdmSchema <- "CDM_Truven_MDCR"
  resultsSchema <- "scratch"
  port <- NULL
  
  #Part one: loading the data:
  connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema,port=port)
  
  cohortData <- dbGetCohortData(connectionDetails,cdmSchema=cdmSchema,resultsSchema=resultsSchema)
  
  saveCohortData(cohortData,"mdcrCohortData")
    
  cohortData <- loadCohortData("mdcrCohortData") 
  
  summary(cohortData)
  
  #Part two: Creating propensity scores, and match people on propensity score:
  ps <- psCreate(cohortData, outcomeConceptId = 194133, regressionPrior=prior("laplace",0.1))
  #ps <- psCreate(cohortData,outcomeConceptId = 194133)
   
  psAuc(ps)
  
  propensityModel <- psGetModel(ps,cohortData)
  
  psPlot(ps)
  
  psTrimmed <- psTrimToEquipoise(ps)
  
  psPlot(psTrimmed,ps) #Plot trimmed PS distributions
  
  strata <- psMatch(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=1)

  psPlot(strata,ps) #Plot matched PS distributions
  
  balance <- psComputeCovariateBalance(strata, cohortData, outcomeConceptId = 194133)
  
  psPlotCovariateBalanceScatterPlot(balance,fileName = "balanceScatterplot.png")
  
  psPlotCovariateBalanceTopVariables(balance,fileName = "balanceTopVarPlot.png")
  
  
  cohortData <- loadCohortData("mdcrCohortData") 
  
  #Part three: Fit the outcome model:
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "cox", regressionPrior=prior("laplace",0.1))
  
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "clr", regressionPrior=prior("laplace",0.1))
  
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "pr", regressionPrior=prior("laplace",0.1))
 
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "lr", regressionPrior=prior("laplace",0.1))
  
  plotKaplanMeier(outcomeModel)
  
  fullOutcomeModel <- getFullOutcomeModel(outcomeModel,cohortData)

  summary(outcomeModel)
  
  coef(outcomeModel)
  
  confint(outcomeModel)
  
  #Generate PDF (set working directory back to package path):
  shell("R CMD Rd2pdf ./")
}

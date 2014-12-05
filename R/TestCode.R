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
  
  cohortData <- getDbCohortDataObject(connectionDetails,cdmSchema=cdmSchema,resultsSchema=resultsSchema)
  
  saveCohortDataObject(cohortData,"mdcrCohortData")
    
  cohortData <- loadCohortDataObject("mdcrCohortData") 
  
  summary(cohortData)
  
  #Part two: Creating propensity scores, and match people on propensity score:
  ps <- createPs(cohortData, outcomeConceptId = 194133, prior=createPrior("laplace",0.1))
  #ps <- psCreate(cohortData,outcomeConceptId = 194133)
   
  computePsAuc(ps)
  
  propensityModel <- getPsModel(ps,cohortData)
  
  plotPs(ps)
  
  psTrimmed <- trimByPsToEquipoise(ps)
  
  plotPs(psTrimmed,ps) #Plot trimmed PS distributions
  
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=1)

  plotPs(strata,ps) #Plot matched PS distributions
  
  balance <- computeCovariateBalance(strata, cohortData, outcomeConceptId = 194133)
  
  plotCovariateBalanceScatterPlot(balance,fileName = "balanceScatterplot.png")
  
  plotCovariateBalanceOfTopVariables(balance,fileName = "balanceTopVarPlot.png")
  
  
  #Part three: Fit the outcome model:
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "cox", prior=createPrior("laplace",0.1))
  
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "clr", prior=createPrior("laplace",0.1))
  
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "pr", prior=createPrior("laplace",0.1))
 
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "lr", prior=createPrior("laplace",0.1))
  
  plotKaplanMeier(outcomeModel)
  
  fullOutcomeModel <- getOutcomeModel(outcomeModel,cohortData)

  summary(outcomeModel)
  
  coef(outcomeModel)
  
  confint(outcomeModel)
  
  #Generate PDF (set working directory back to package path):
  shell("R CMD Rd2pdf ./")
}

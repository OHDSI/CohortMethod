
testCode <- function(){
  ### Test code for Patrick ###
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
  
  save.cohortData(cohortData,file.path(getwd(),"mdcrCohortData"))
  
  cohortData <- load.cohortData(file.path(getwd(),"mdcrCohortData"))
  
  #Part two: Creating propensity scores, and match people on propensity score:
  ps <- psCreate(cohortData, outcomeConceptId = 194133, prior=prior("laplace",0.1))
   
  psAuc(ps)
  
  model <- psGetModel(ps,cohortData)
  
  psPlot(ps) #Plot unmatched PS distributions
  
  psTrimmed <- psTrimToEquipoise(ps)
  
  psPlot(psTrimmed) #Plot trimmed PS distributions
  
  strata <- psMatch(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=0)

  psPlot(strata) #Plot matched PS distributions
  
  balance <- psComputeCovariateBalance(strata, cohortData, outcomeConceptId = 194133)
  
  psPlotCovariateBalance(balance)
  
  #Part three: Fit the outcome model:
  effect <- estimateEffect(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 9999,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "cox")
  effect
  
}

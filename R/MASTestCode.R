
testCode <- function(){
  ### Test code for Patrick ###
  library(CohortMethod)
  #setwd("c:/temp")
  
  # If ff is complaining it can't find the temp folder, use   options("fftempdir" = "c:/temp")

  #Settings for running SQL against ledley:
  pw <- "msuchard"
  dbms <- "postgresql"
  user <- "ohdsi"
  server <- "ledley.idre.ucla.edu/ohdsi"
  cdmSchema <- "cdm4_sim" 
  resultsSchema <- "cdm4_sim"
  port <- NULL
  
  #Part one: loading the data:
  connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema,port=port)
  
  cohortData <- dbGetCohortData(connectionDetails,cdmSchema=cdmSchema,resultsSchema=resultsSchema)
  
  save.cohortData(cohortData,file.path(getwd(),"simCohortData"))
  
  cohortData <- load.cohortData(file.path(getwd(),"mdcrCohortData"))
  
  #Part two: Creating propensity scores, and match people on propensity score:
  ps <- psCreate(cohortData, outcomeConceptId = 194133, prior=prior("laplace",0.1))
   
  psAuc(ps)
  
  model <- psGetModel(ps,cohortData)
  
  psPlot(ps) #Plot unmatched PS distributions
  
  psTrimmed <- psTrimToEquipoise(ps)
  
  psPlot(psTrimmed) #Plot trimmed PS distributions
  
  strata <- psMatch(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio = 1)
  
  psPlot(strata) #Plot matched PS distributions
  
  balance <- psComputeCovariateBalance(strata, cohortData, outcomeConceptId = 194133)
  
  #Part three: Fit the outcome model:
  effect <- estimateEffect(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 9999,addExposureDaysToEnd = FALSE,useCovariates = TRUE, 
                           returnOutcomeData = TRUE, modelType = "cox")
  effect$estimates
  
  getOutcomeSummaryStatistics(effect$data, plot = TRUE)  
  
}

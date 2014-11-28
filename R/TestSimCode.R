testSimCode <- function(){
  library(CohortMethod)
  setwd("c:/temp")
  
  # If ff is complaining it can't find the temp folder, use   options("fftempdir" = "c:/temp")
  
  cohortData <- loadCohortData("mdcrCohortData") 
  cohortDataSimulationProfile <- createCohortDataSimulationProfile(cohortData)
  save(cohortDataSimulationProfile,file="sim.Rdata")
  
  load("sim.Rdata")
  cohortData <- simulateCohortData(cohortDataSimulationProfile, n=10000)
  
  summary(cohortData)
  
  ps <- psCreate(cohortData, outcomeConceptId = 194133, regressionPrior=prior("laplace",0.1))
  
  coefs <- attr(ps,"coefficients")
  coefs <- coefs[order(names(coefs))]
  cohortDataSimulationProfile$propensityModel <- cohortDataSimulationProfile$propensityModel[order(names(cohortDataSimulationProfile$propensityModel))]
  cor(coefs,cohortDataSimulationProfile$propensityModel )
  
  coefs <- coefs[order(-abs(coefs))]
  cohortDataSimulationProfile$propensityModel <- cohortDataSimulationProfile$propensityModel[order(-abs(cohortDataSimulationProfile$propensityModel))]
  head(coefs)
  head(cohortDataSimulationProfile$propensityModel)
  
  psAuc(ps)
  
  propensityModel <- psGetModel(ps,cohortData)
  
  head(propensityModel)
  
  psPlot(ps)
  
  psTrimmed <- psTrimToEquipoise(ps)
  
  psPlot(psTrimmed,ps) #Plot trimmed PS distributions
  
  strata <- psMatch(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=1)
  
  psPlot(strata,ps) #Plot matched PS distributions
  
  balance <- psComputeCovariateBalance(strata, cohortData, outcomeConceptId = 194133)
  
  psPlotCovariateBalanceScatterPlot(balance,fileName = "balanceScatterplot.png")
  
  psPlotCovariateBalanceTopVariables(balance,fileName = "balanceTopVarPlot.png")
  
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 9999,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "cox", regressionPrior=prior("laplace",0.1))
  
  plotKaplanMeier(outcomeModel)
  
  fullOutcomeModel <- getFullOutcomeModel(outcomeModel,cohortData)
  
  summary(outcomeModel)
  
  coef(outcomeModel)
  
  confint(outcomeModel)
  
}

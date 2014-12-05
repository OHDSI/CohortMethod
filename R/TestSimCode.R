testSimCode <- function(){
  library(CohortMethod)
  setwd("c:/temp")
  
  # If ff is complaining it can't find the temp folder, use   options("fftempdir" = "c:/temp")
  
  cohortData <- loadCohortDataObject("mdcrCohortData") 
  cohortDataSimulationProfile <- createCohortDataSimulationProfile(cohortData)
  save(cohortDataSimulationProfile,file="sim.Rdata")
  
  load("sim.Rdata")
  cohortData <- simulateCohortData(cohortDataSimulationProfile, n=10000)
  
  summary(cohortData)
  
  ps <- createPs(cohortData, outcomeConceptId = 194133, prior=createPrior("laplace",0.1))
  
  coefs <- attr(ps,"coefficients")
  coefs <- coefs[order(names(coefs))]
  cohortDataSimulationProfile$propensityModel <- cohortDataSimulationProfile$propensityModel[order(names(cohortDataSimulationProfile$propensityModel))]
  cor(coefs,cohortDataSimulationProfile$propensityModel )
  
  coefs <- coefs[order(-abs(coefs))]
  cohortDataSimulationProfile$propensityModel <- cohortDataSimulationProfile$propensityModel[order(-abs(cohortDataSimulationProfile$propensityModel))]
  head(coefs)
  head(cohortDataSimulationProfile$propensityModel)
  
  computePsAuc(ps)
  
  propensityModel <- getPsModel(ps,cohortData)
  
  head(propensityModel)
  
  plotPs(ps)
  
  psTrimmed <- trimByPsToEquipoise(ps)
  
  plotPs(psTrimmed,ps) #Plot trimmed PS distributions
  
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=1)
  
  plotPs(strata,ps) #Plot matched PS distributions
  
  balance <- computeCovariateBalance(strata, cohortData, outcomeConceptId = 194133)
  
  plotCovariateBalanceScatterPlot(balance,fileName = "balanceScatterplot.png")
  
  plotCovariateBalanceOfTopVariables(balance,fileName = "balanceTopVarPlot.png")
  
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 9999,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "cox", prior=createPrior("laplace",0.1))
  
  plotKaplanMeier(outcomeModel)
  
  fullOutcomeModel <- getOutcomeModel(outcomeModel,cohortData)
  
  summary(outcomeModel)
  
  coef(outcomeModel)
  
  confint(outcomeModel)
  
}

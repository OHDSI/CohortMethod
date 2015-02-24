# @file test-parameterSweep.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library("testthat")

# This is a broad, shallow sweep of all functionality. It checks whether the code produces 
# an output (and does not throw an error) under a wide range of parameter settings 
set.seed(123)
print(getOption("fftempdir"))
data(cohortDataSimulationProfile)
sampleSize <- 1000
cohortData <- simulateCohortData(cohortDataSimulationProfile, n=sampleSize)

test_that("CohortData functions", {
  s <- summary(cohortData)
  expect_is(s,"summary.cohortData")
  expect_equal(s$treatedPersons + s$comparatorPersons,sampleSize)
})

test_that("Propensity score functions", {
  #Cross-validation:
  ps <- createPs(cohortData,outcomeConceptId = 194133)
  
  ps <- createPs(cohortData, outcomeConceptId = 194133, prior=createPrior("laplace", 0.1, exclude = 0))
  expect_less_than(0.7,computePsAuc(ps)[1])
  
  propensityModel <- getPsModel(ps,cohortData)
  expect_is(propensityModel,"data.frame")
  
  for (scale in c("preference","propensity")){
    for (type in c("density","histogram")){
      p <- plotPs(ps, scale = scale, type = type)
      expect_is(p,"ggplot")
    }
  }
  
  psTrimmed <- trimByPsToEquipoise(ps)
  expect_is(psTrimmed,"data.frame")
  
  for (scale in c("preference","propensity")){
    for (type in c("density","histogram")){
      p <- plotPs(psTrimmed,ps, scale = scale, type = type)
      expect_is(p,"ggplot")
    }
  }
  
  for (numberOfStrata in c(2,5,10,20)){
    strata <- stratifyByPs(psTrimmed, numberOfStrata = numberOfStrata)
    expect_is(strata,"data.frame")
  }
  
  for (numberOfStrata in c(2,5,10,20)){
    strata <- stratifyByPsAndCovariates(psTrimmed, 
                                        numberOfStrata = numberOfStrata, 
                                        cohortData = cohortData,
                                        covariateIds = c(11:27,8507)) #age + sex
    expect_is(strata,"data.frame")
  }
  
  for (caliper in c(0,0.25)){
    for (caliperScale in c("propensity score","standardized")){
      for (maxRatio in c(0,1,3)){
        strata <- matchOnPs(psTrimmed, caliper = caliper, caliperScale = caliperScale,maxRatio=maxRatio)      
        expect_is(strata,"data.frame")
      }
    }
  }
  
  for (caliper in c(0,0.25)){
    for (caliperScale in c("propensity score","standardized")){
      for (maxRatio in c(0,1,3)){
        strata <- matchOnPsAndCovariates(psTrimmed,
                                         caliper = caliper, 
                                         caliperScale = caliperScale,
                                         maxRatio=maxRatio,
                                         cohortData = cohortData,
                                         covariateIds = c(11:27,8507)) #age + sex      
        expect_is(strata,"data.frame")
      }
    }
  }
  
})

test_that("Balance functions", {    
  ps <- createPs(cohortData, outcomeConceptId = 194133, prior=createPrior("laplace",0.1, exclude=0))
  psTrimmed <- trimByPsToEquipoise(ps)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=1)
  
  balance <- computeCovariateBalance(strata, cohortData, outcomeConceptId = 194133)
  expect_is(balance,"data.frame")
  
  p <- plotCovariateBalanceScatterPlot(balance)
  expect_is(p,"ggplot")
  
  p <- plotCovariateBalanceOfTopVariables(balance)
  expect_is(p,"ggplot")  
})

test_that("Outcome functions", {    
  ps <- createPs(cohortData, outcomeConceptId = 194133, prior=createPrior("laplace",0.1,exclude=0))
  psTrimmed <- trimByPsToEquipoise(ps)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=1)
  
  logRrs <- c()
  #params <- c()
  for (type in c("logistic","poisson","survival")){
    for (stratified in c(TRUE,FALSE)){
      for (useCovariates in c(TRUE,FALSE)){
        for (addExposureDaysToEnd in c(TRUE,FALSE)){
          writeLines(paste("type:",type,",stratified:",stratified,",useCovariates:",useCovariates,",addExposureDaysToEnd:",addExposureDaysToEnd))
          stratifiedCox <- NULL
          if (type == "logistic")
            if (stratified){
              modelType = "clr"
            } else { 
              modelType = "lr"
            }
          if (type == "poisson")
            if (stratified){
              modelType = "cpr"
            } else { 
              modelType = "pr"
            }
          if (type == "survival")
            if (stratified){
              modelType = "cox"
              stratifiedCox = TRUE
            } else { 
              modelType = "cox"
              stratifiedCox = FALSE
            }
          outcomeModel <- fitOutcomeModel(194133,
                                          cohortData,
                                          strata,
                                          stratifiedCox = stratifiedCox,
                                          riskWindowStart = 0, 
                                          riskWindowEnd = 365,
                                          addExposureDaysToEnd = addExposureDaysToEnd,
                                          useCovariates = useCovariates, 
                                          modelType = modelType,
                                          prior=createPrior("laplace", 0.1))
          expect_is(outcomeModel,"outcomeModel")        
          logRrs <- c(logRrs,coef(outcomeModel))
          #params <- c(params,paste("type:",type,",stratified:",stratified,",useCovariates:",useCovariates,",addExposureDaysToEnd:",addExposureDaysToEnd))
        }
      }
    }
  }
  #results <- data.frame(logRr = logRrs, param = params)
  #results <- results[order(results$logRr),]
  #results
  
  # All analyses are fundamentally different, so should have no duplicate values at full precision:
  expect_equal(length(unique(logRrs)), length(logRrs))
})


test_that("Functions on outcome model", {   
  ps <- createPs(cohortData, outcomeConceptId = 194133, prior=createPrior("laplace",0.1, exclude=0))
  strata <- matchOnPs(ps, caliper = 0.25, caliperScale = "standardized",maxRatio=1)
  outcomeModel <- fitOutcomeModel(194133,cohortData,strata,riskWindowStart = 0, riskWindowEnd = 365,addExposureDaysToEnd = FALSE,useCovariates = TRUE, modelType = "cox", prior=createPrior("laplace",0.1))
  
  s <- summary(outcomeModel)
  expect_is(s,"summary.outcomeModel")
  
  p <- plotKaplanMeier(outcomeModel)
  expect_is(p,"ggplot")  
  
  p <- drawAttritionDiagram(outcomeModel)
  expect_is(p,"ggplot")  
  
  cf <- coef(outcomeModel)
  ci <- confint(outcomeModel)  
  expect_more_than(cf,ci[1])
  expect_less_than(cf,ci[2])
  
  fullOutcomeModel <- getOutcomeModel(outcomeModel, cohortData)
  expect_is(fullOutcomeModel,"data.frame")  
})
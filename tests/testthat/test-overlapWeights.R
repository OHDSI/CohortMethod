# @file test-parameterSweep.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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
# library(CohortMethod)
library("testthat")

# this is a shallow test script for the overlap weights functionality
# passing these test blocks does not guarantee smooth code execution
# over the full spectrum of use cases
# 0. generate synthetic chort data
set.seed(1234)
data(cohortMethodDataSimulationProfile)
sampleSize <- 1000
cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = sampleSize)

theEstimand = 'ato' # ATO indicated by overlap weights

# 1. test output sanity out of `createPs`
test_that("Propensity score functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 1
  )
  # Cross-validation:
  ps <- createPs(cohortMethodData, studyPop,
                 prior = createPrior("laplace", 0.1, exclude = 0),
                 estimator = theEstimand)
  expect_true(all(!is.na(ps$iptw)))
  expect_true(all(ps$iptw <= 1) && all(ps$iptw >= 0))

  expect_lt(0.65, computePsAuc(ps)[1])

  propensityModel <- getPsModel(ps, cohortMethodData)
  expect_s3_class(propensityModel, "data.frame")

  for (scale in c("preference", "propensity")) {
    for (type in c("density", "histogram")) {
      p <- plotPs(ps, scale = scale, type = type)
      expect_s3_class(p, "ggplot")
    }
  }

  psTrimmed <- trimByPsToEquipoise(ps)
  expect_s3_class(psTrimmed, "data.frame")

  for (scale in c("preference", "propensity")) {
    for (type in c("density", "histogram")) {
      p <- plotPs(psTrimmed, ps, scale = scale, type = type)
      expect_s3_class(p, "ggplot")
    }
  }

  equipoise <- computeEquipoise(ps)
  expect_gt(equipoise, 0.5)

  for (numberOfStrata in c(2, 5, 10, 20)) {
    strata <- stratifyByPs(psTrimmed, numberOfStrata = numberOfStrata)
    expect_s3_class(strata, "data.frame")
  }

  for (numberOfStrata in c(2, 5, 10, 20)) {
    strata <- stratifyByPsAndCovariates(psTrimmed,
                                        numberOfStrata = numberOfStrata,
                                        cohortMethodData = cohortMethodData,
                                        covariateIds = c(0:27 * 1000 + 3, 8532001)
    ) # age + sex
    expect_s3_class(strata, "data.frame")
  }

  for (caliper in c(0, 0.25)) {
    for (caliperScale in c("propensity score", "standardized", "standardized logit")) {
      for (maxRatio in c(0, 1, 3)) {
        strata <- matchOnPs(psTrimmed,
                            caliper = caliper,
                            caliperScale = caliperScale,
                            maxRatio = maxRatio
        )
        expect_s3_class(strata, "data.frame")
      }
    }
  }

  for (caliper in c(0, 0.25)) {
    for (caliperScale in c("propensity score", "standardized", "standardized logit")) {
      for (maxRatio in c(0, 1, 3)) {
        strata <- matchOnPsAndCovariates(psTrimmed,
                                         caliper = caliper,
                                         caliperScale = caliperScale,
                                         maxRatio = maxRatio,
                                         cohortMethodData = cohortMethodData,
                                         covariateIds = c(11:27, 8507)
        ) # age + sex
        expect_s3_class(strata, "data.frame")
      }
    }
  }
})

# 2. test sanity of outcome modeling fitting using the weights
# -- ensure that estimates (and CIs) are somewhat different when using the weights
test_that("Outcome functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 1,
                                    riskWindowStart = 0,
                                    riskWindowEnd = 365
  )
  ps <- createPs(cohortMethodData, studyPop,
                 prior = createPrior("laplace", 0.1, exclude = 0),
                 estimator = theEstimand)

  psTrimmed <- trimByPsToEquipoise(ps)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)

  #lbs <- c()
  # params <- c()
  testModels = c('poisson') #c("logistic", "poisson", "cox")
  stratifyChoices = c(TRUE) #c(TRUE, FALSE)
  covariateChoices = c(TRUE) #c(TRUE, FALSE)

  for (modelType in testModels) {
    for (stratified in stratifyChoices) {
      for (useCovariates in covariateChoices) {
        writeLines(paste(
          "modelType:",
          modelType,
          ",stratified:",
          stratified,
          ",useCovariates:",
          useCovariates
        ))

        writeLines("Regular regression...")
        outcomeModel1 <- fitOutcomeModel(
          population = strata,
          cohortMethodData = cohortMethodData,
          modelType = modelType,
          stratified = stratified,
          useCovariates = useCovariates,
          inversePtWeighting = FALSE,
          prior = createPrior("laplace", 0.1)
        )
        expect_s3_class(outcomeModel1, "OutcomeModel")
        coef1 = outcomeModel1$outcomeModelCoefficients
        lb1 = outcomeModel1$outcomeModelTreatmentEstimate$logLb95

        writeLines("Using weights for outcome regression...")
        outcomeModel2 <- fitOutcomeModel(
          population = strata,
          cohortMethodData = cohortMethodData,
          modelType = modelType,
          stratified = stratified,
          useCovariates = useCovariates,
          inversePtWeighting = TRUE,
          prior = createPrior("laplace", 0.1)
        )
        expect_s3_class(outcomeModel2, "OutcomeModel")
        coef2 = outcomeModel2$outcomeModelCoefficients
        lb2 = outcomeModel2$outcomeModelTreatmentEstimate$logLb95


        expect_false(all(coef1 == coef2))
        expect_false(all(lb1 == lb2))


        #lbs <- c(lbs, confint(outcomeModel)[1])
        # params <-
        # c(params,paste('type:',type,',stratified:',stratified,',useCovariates:',useCovariates,',addExposureDaysToEnd:',addExposureDaysToEnd))
      }
    }
  }


  # results <- data.frame(logRr = logRrs, param = params) results <- results[order(results$logRr),]
  # results
})

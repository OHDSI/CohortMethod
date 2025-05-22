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

set.seed(1234)
data(cohortMethodDataSimulationProfile)
sampleSize <- 1
cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = sampleSize)
cohorts <- cohortMethodData$cohorts |> collect()
cohortMethodData$cohorts <- cohorts[-1, ]

test_that("Create study population functions with zero rows", {
  studyPop <- createStudyPopulation(cohortMethodData,
    outcomeId = 194133,
    removeSubjectsWithPriorOutcome = TRUE,
    minDaysAtRisk = 1
  )
  expect_true(nrow(studyPop) == 0)
})

test_that("Propensity score functions with zero rows", {
  studyPop <- createStudyPopulation(cohortMethodData,
    outcomeId = 194133,
    removeSubjectsWithPriorOutcome = TRUE,
    minDaysAtRisk = 1
  )
  # Cross-validation:
  ps <- createPs(cohortMethodData, studyPop)
  expect_true(nrow(ps) == 0)

  propensityModel <- getPsModel(ps, cohortMethodData)
  expect_s3_class(propensityModel, "data.frame")

  psTrimmed <- trimByPsToEquipoise(ps)
  expect_s3_class(psTrimmed, "data.frame")

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

test_that("Balance functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
    outcomeId = 194133,
    removeSubjectsWithPriorOutcome = TRUE,
    minDaysAtRisk = 1
  )
  ps <- createPs(cohortMethodData, studyPop, prior = createPrior("laplace", 0.1, exclude = 0))
  psTrimmed <- trimByPsToEquipoise(ps)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)

  balance <- computeCovariateBalance(strata, cohortMethodData)
  expect_s3_class(balance, "data.frame")
})

test_that("Outcome functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
    outcomeId = 194133,
    removeSubjectsWithPriorOutcome = TRUE,
    minDaysAtRisk = 1,
    riskWindowStart = 0,
    riskWindowEnd = 365
  )
  ps <- createPs(cohortMethodData, studyPop, prior = createPrior("laplace", 0.1, exclude = 0))

  psTrimmed <- trimByPsToEquipoise(ps)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)

  for (modelType in c("logistic", "poisson", "cox")) {
    for (stratified in c(TRUE, FALSE)) {
      for (useCovariates in c(TRUE, FALSE)) {
        writeLines(paste(
          "modelType:",
          modelType,
          ",stratified:",
          stratified,
          ",useCovariates:",
          useCovariates
        ))

        outcomeModel <- fitOutcomeModel(
          population = strata,
          cohortMethodData = cohortMethodData,
          modelType = modelType,
          stratified = stratified,
          useCovariates = useCovariates,
          prior = createPrior("laplace", 0.1)
        )
        expect_s3_class(outcomeModel, "OutcomeModel")
      }
    }
  }
  writeLines("IPTW")
  outcomeModel <- fitOutcomeModel(
    population = strata,
    cohortMethodData = cohortMethodData,
    modelType = modelType,
    stratified = FALSE,
    useCovariates = FALSE,
    inversePtWeighting = TRUE
  )
  expect_s3_class(outcomeModel, "OutcomeModel")
})

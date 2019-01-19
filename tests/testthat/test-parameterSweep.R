# @file test-parameterSweep.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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

# This is a broad, shallow sweep of all functionality. It checks whether the code produces an output
# (and does not throw an error) under a wide range of parameter settings
set.seed(1234)
print(getOption("fftempdir"))
data(cohortMethodDataSimulationProfile)
sampleSize <- 1000
cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = sampleSize)

test_that("cohortMethodData functions", {
  expect_output(print(cohortMethodData), "CohortMethodData object.*")
  s <- summary(cohortMethodData)
  expect_is(s, "summary.cohortMethodData")
  expect_equal(s$targetPersons + s$comparatorPersons, sampleSize)
  expect_output(print(s), "CohortMethodData object summary.*")

  folder <- tempdir()
  saveCohortMethodData(cohortMethodData, folder)
  cmd2 <- loadCohortMethodData(folder)
  expect_identical(cohortMethodData$cohorts, cmd2$cohorts)
  expect_identical(cohortMethodData$outcomes, cmd2$outcomes)
  expect_equal(cohortMethodData$covariates, cmd2$covariates)
  expect_equal(cohortMethodData$covariateRef, cmd2$covariateRef)
  expect_equal(cohortMethodData$analysisRef, cmd2$analysisRef)
  expect_identical(cohortMethodData$metaData, cmd2$metaData)
  ff::close.ffdf(cmd2$covariates)
  ff::close.ffdf(cmd2$covariateRef)
  ff::close.ffdf(cmd2$analysisRef)
  unlink(folder, recursive = TRUE, force = TRUE)

  cn <- grepCovariateNames("^age group.*", cohortMethodData)
  expect_gt(nrow(cn), 1)
})

test_that("Create study population functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 1)
  expect_true(all(studyPop$timeAtRisk > 0))
  peopleWithPriorOutcomes <- cohortMethodData$outcomes$rowId[cohortMethodData$outcomes$daysToEvent <
    0]
  expect_false(any(peopleWithPriorOutcomes %in% studyPop$rowId))

  aTable <- getAttritionTable(studyPop)
  expect_is(aTable, "data.frame")
})

test_that("Propensity score functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 1)
  # Cross-validation:
  ps <- createPs(cohortMethodData, studyPop)

  ps <- createPs(cohortMethodData, studyPop, prior = createPrior("laplace", 0.1, exclude = 0))
  expect_lt(0.65, computePsAuc(ps)[1])

  propensityModel <- getPsModel(ps, cohortMethodData)
  expect_is(propensityModel, "data.frame")

  for (scale in c("preference", "propensity")) {
    for (type in c("density", "histogram")) {
      p <- plotPs(ps, scale = scale, type = type)
      expect_is(p, "ggplot")
    }
  }

  psTrimmed <- trimByPsToEquipoise(ps)
  expect_is(psTrimmed, "data.frame")

  for (scale in c("preference", "propensity")) {
    for (type in c("density", "histogram")) {
      p <- plotPs(psTrimmed, ps, scale = scale, type = type)
      expect_is(p, "ggplot")
    }
  }

  for (numberOfStrata in c(2, 5, 10, 20)) {
    strata <- stratifyByPs(psTrimmed, numberOfStrata = numberOfStrata)
    expect_is(strata, "data.frame")
  }

  for (numberOfStrata in c(2, 5, 10, 20)) {
    strata <- stratifyByPsAndCovariates(psTrimmed,
                                        numberOfStrata = numberOfStrata,
                                        cohortMethodData = cohortMethodData,
                                        covariateIds = c(0:27 * 1000 + 3, 8532001))  #age + sex
    expect_is(strata, "data.frame")
  }

  for (caliper in c(0, 0.25)) {
    for (caliperScale in c("propensity score", "standardized", "standardized logit")) {
      for (maxRatio in c(0, 1, 3)) {
        strata <- matchOnPs(psTrimmed,
                            caliper = caliper,
                            caliperScale = caliperScale,
                            maxRatio = maxRatio)
        expect_is(strata, "data.frame")
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
                                         covariateIds = c(11:27, 8507))  #age + sex
        expect_is(strata, "data.frame")
      }
    }
  }

})

test_that("Balance functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 1)
  ps <- createPs(cohortMethodData, studyPop, prior = createPrior("laplace", 0.1, exclude = 0))
  psTrimmed <- trimByPsToEquipoise(ps)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)

  balance <- computeCovariateBalance(strata, cohortMethodData)
  expect_is(balance, "data.frame")

  p <- plotCovariateBalanceScatterPlot(balance)
  expect_is(p, "ggplot")

  p <- plotCovariateBalanceOfTopVariables(balance)
  expect_is(p, "ggplot")
})

test_that("Outcome functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 1,
                                    riskWindowStart = 0,
                                    riskWindowEnd = 365)
  ps <- createPs(cohortMethodData, studyPop, prior = createPrior("laplace", 0.1, exclude = 0))

  psTrimmed <- trimByPsToEquipoise(ps)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)

  logRrs <- c()
  # params <- c()
  for (modelType in c("logistic", "poisson", "cox")) {
    for (stratified in c(TRUE, FALSE)) {
      for (useCovariates in c(TRUE, FALSE)) {
        writeLines(paste("modelType:",
                         modelType,
                         ",stratified:",
                         stratified,
                         ",useCovariates:",
                         useCovariates))

        outcomeModel <- fitOutcomeModel(population = strata,
                                        cohortMethodData = cohortMethodData,
                                        modelType = modelType,
                                        stratified = stratified,
                                        useCovariates = useCovariates,
                                        prior = createPrior("laplace", 0.1))
        expect_is(outcomeModel, "outcomeModel")
        logRrs <- c(logRrs, coef(outcomeModel))
        # params <-
        # c(params,paste('type:',type,',stratified:',stratified,',useCovariates:',useCovariates,',addExposureDaysToEnd:',addExposureDaysToEnd))
      }
    }
  }
  # results <- data.frame(logRr = logRrs, param = params) results <- results[order(results$logRr),]
  # results

  # All analyses are fundamentally different, so should have no duplicate values at full precision:
  expect_equal(length(unique(logRrs)), length(logRrs))
})


test_that("Functions on outcome model", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 1,
                                    riskWindowStart = 0,
                                    riskWindowEnd = 365)
  ps <- createPs(cohortMethodData, studyPop, prior = createPrior("laplace", 0.1, exclude = 0))

  strata <- matchOnPs(ps, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)
  outcomeModel <- fitOutcomeModel(population = strata,
                                  cohortMethodData = cohortMethodData,
                                  modelType = "cox",
                                  stratified = TRUE,
                                  useCovariates = TRUE,
                                  prior = createPrior("laplace", 0.1))

  expect_output(print(outcomeModel), "Model type: cox.*")

  s <- summary(outcomeModel)
  expect_is(s, "summary.outcomeModel")

  expect_output(print(s), "Model type: cox.*")

  p <- plotKaplanMeier(strata)
  expect_is(p, "grob")

  p <- drawAttritionDiagram(outcomeModel)
  expect_is(p, "ggplot")

  cf <- coef(outcomeModel)
  ci <- confint(outcomeModel)
  expect_gt(cf, ci[1])
  expect_lt(cf, ci[2])

  fullOutcomeModel <- getOutcomeModel(outcomeModel, cohortMethodData)
  expect_is(fullOutcomeModel, "data.frame")
})

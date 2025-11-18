library(CohortMethod)
library(testthat)

set.seed(1234)
data(cohortMethodDataSimulationProfile)
sampleSize <- 1
cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = sampleSize)
cohorts <- cohortMethodData$cohorts |> collect()
cohortMethodData$cohorts <- cohorts[-1, ]

test_that("Create study population functions with zero rows", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    createStudyPopulationArgs = createCreateStudyPopulationArgs(
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1
                                    ))
  expect_true(nrow(studyPop) == 0)
})

test_that("Propensity score functions with zero rows", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    createStudyPopulationArgs = createCreateStudyPopulationArgs(
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1
                                    ))
  # Cross-validation:
  ps <- createPs(cohortMethodData, studyPop, createPsArgs = createCreatePsArgs())
  expect_true(nrow(ps) == 0)

  propensityModel <- getPsModel(ps, cohortMethodData)
  expect_s3_class(propensityModel, "data.frame")

  psTrimmed <- trimByPs(ps, trimByPsArgs = createTrimByPsArgs(trimFraction = 0.05))
  expect_s3_class(psTrimmed, "data.frame")

  strata <- stratifyByPs(psTrimmed, stratifyByPsArgs = createStratifyByPsArgs()
  )
  expect_s3_class(strata, "data.frame")

  strata <- stratifyByPs(psTrimmed,
                         cohortMethodData = cohortMethodData,
                         stratifyByPsArgs = createStratifyByPsArgs(
                           stratificationCovariateIds = c(0, 1, 3)
                         ))
  expect_s3_class(strata, "data.frame")

  strata <- matchOnPs(psTrimmed, matchOnPsArgs = createMatchOnPsArgs())
  expect_s3_class(strata, "data.frame")

  strata <- matchOnPs(psTrimmed,
                      cohortMethodData = cohortMethodData,
                      matchOnPsArgs = createMatchOnPsArgs(
                        matchCovariateIds = c(0, 1, 3)
                      ))
  expect_s3_class(strata, "data.frame")
})

test_that("Balance functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    createStudyPopulationArgs = createCreateStudyPopulationArgs(
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1
                                    ))
  ps <- createPs(cohortMethodData = cohortMethodData,
                 population = studyPop,
                 createPsArgs = createCreatePsArgs(
                   prior = createPrior("laplace", 0.1, exclude = 0)
                 ))
  strata <- matchOnPs(ps, matchOnPsArgs = createMatchOnPsArgs())

  balance <- computeCovariateBalance(strata, cohortMethodData, computeCovariateBalanceArgs = createComputeCovariateBalanceArgs())
  expect_s3_class(balance, "data.frame")
})

test_that("Outcome functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    createStudyPopulationArgs = createCreateStudyPopulationArgs(
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1
                                    ))
  ps <- createPs(cohortMethodData = cohortMethodData,
                 population = studyPop,
                 createPsArgs = createCreatePsArgs(
                   prior = createPrior("laplace", 0.1, exclude = 0)
                 ))
  strata <- matchOnPs(ps, matchOnPsArgs = createMatchOnPsArgs())
  outcomeModel <- fitOutcomeModel(
    population = strata,
    cohortMethodData = cohortMethodData,
    fitOutcomeModelArgs = createFitOutcomeModelArgs()
  )
  expect_s3_class(outcomeModel, "OutcomeModel")
})

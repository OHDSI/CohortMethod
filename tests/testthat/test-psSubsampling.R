library(testthat)
library(Eunomia)

set.seed(1234)
data(cohortMethodDataSimulationProfile)
sampleSize <- 1000
cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = sampleSize)

test_that("Subsampling cohort throws no error", {
  population <- CohortMethod::createStudyPopulation(
    cohortMethodData = cohortMethodData,
    outcomeId = 3,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(
      riskWindowEnd = 99999
    )
  )
  # Set Cyclops prior to guarantee all coefficients to be zero.
  prior_var <- 1 / 2^1023
  cyclops_control <- Cyclops::createControl(
    lowerLimit = prior_var,
    upperLimit = prior_var,
    minCVData = 0,
    fold = 1
  )

  # Ensure subsampling took place
  targetPopSize <- length(population$rowId[population$treatment == 1])
  comparatorPopSize <- length(population$rowId[population$treatment == 0])
  expect_true(min(targetPopSize, comparatorPopSize) > 10)

  expect_error(
    createPs(
      cohortMethodData = cohortMethodData,
      population = population,
      createPsArgs = createCreatePsArgs(
        errorOnHighCorrelation = FALSE,
        maxCohortSizeForFitting = 10,
        control = cyclops_control
      )
    ),
    "ILLCONDITIONED"
  )
})

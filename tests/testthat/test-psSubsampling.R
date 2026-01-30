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
  prior <- createPrior("laplace", 0.1, exclude = 0)

  # Ensure subsampling takes place
  targetPopSize <- length(population$rowId[population$treatment == 1])
  comparatorPopSize <- length(population$rowId[population$treatment == 0])
  expect_true(min(targetPopSize, comparatorPopSize) > 100)

  ps <- createPs(
      cohortMethodData = cohortMethodData,
      population = population,
      createPsArgs = createCreatePsArgs(
        errorOnHighCorrelation = FALSE,
        maxCohortSizeForFitting = 100,
        prior = prior
      )
    )
  expect_s3_class(ps, "data.frame")
})

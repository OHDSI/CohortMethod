library(testthat)
library(Eunomia)

test_that("Subsampling cohort throws no error", {
  # Extract arbitrary study population
  connectionDetails <- getEunomiaConnectionDetails()
  Eunomia::createCohorts(connectionDetails)
  cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = "main",
                                            targetId = 1,
                                            comparatorId = 2,
                                            outcomeIds = 3,
                                            exposureDatabaseSchema = "main",
                                            outcomeDatabaseSchema = "main",
                                            exposureTable = "cohort",
                                            outcomeTable = "cohort",
                                            covariateSettings = createDefaultCovariateSettings())
  population <- CohortMethod::createStudyPopulation(cohortMethodData = cohortMethodData,
                                                    outcomeId = 3,
                                                    riskWindowEnd = 99999)
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
      cohortMethodData, population,
      errorOnHighCorrelation = FALSE,
      maxCohortSizeForFitting = 10,
      control = cyclops_control
    ),
    NA
  )
})


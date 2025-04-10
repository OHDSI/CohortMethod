test_that("createCohortMethodDataSimulationProfile", {
  covarSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAge = TRUE,
    useDemographicsAgeGroup = TRUE,
    useDemographicsIndexMonth = TRUE,
    useDemographicsPriorObservationTime = TRUE,
    useDemographicsPostObservationTime = TRUE,
    useDemographicsTimeInCohort = TRUE,
    useDemographicsIndexYearMonth = TRUE,
    useMeasurementValueLongTerm = TRUE,
    useMeasurementValueMediumTerm = TRUE,
    useMeasurementValueShortTerm = TRUE,
    useMeasurementRangeGroupAnyTimePrior = TRUE,
    useMeasurementRangeGroupLongTerm = TRUE,
    useMeasurementRangeGroupMediumTerm = TRUE,
    useMeasurementRangeGroupShortTerm = TRUE,
    useObservationAnyTimePrior = TRUE,
    useObservationLongTerm = TRUE,
    useObservationMediumTerm = TRUE,
    useObservationShortTerm = TRUE,
    endDays = 180
  )

  cohortMethodData <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    cdmVersion = "5",
    washoutPeriod = 183,
    firstExposureOnly = TRUE,
    removeDuplicateSubjects = "keep all",
    restrictToCommonPeriod = TRUE,
    maxCohortSize = 100000,
    covariateSettings = covarSettings
  )


  cohorts <- data.frame(cohortMethodData$cohorts)
  cohorts$daysToCohortEnd <- rexp(nrow(cohorts), rate = 10)
  cohortMethodData$cohorts <- cohorts

  cohortDataSimulationProfile <- createCohortMethodDataSimulationProfile(cohortMethodData)

  # Basic checks to see if output simulated data are meaninful
  expect_s3_class(cohortDataSimulationProfile, "CohortDataSimulationProfile")
  expect_true(cohortDataSimulationProfile$cohortEndRate > 0)
  expect_true(cohortDataSimulationProfile$obsStartRate > 0)
  expect_true(cohortDataSimulationProfile$obsEndRate > 0)

  truncatedProfile <- .truncateSimulationProfile(cohortDataSimulationProfile, 100)

  minPrevalenceAfterTruncation <- truncatedProfile$covariatePrevalence |>
    filter(prevalence > 0) |>
    summarize(min(prevalence)) |>
    pull()

  minPrevalenceBeforeTruncation <- cohortDataSimulationProfile$covariatePrevalence |>
    filter(prevalence > 0) |>
    summarize(min(prevalence)) |>
    pull()

  # test truncation of covariate prevalence
  expect_true(minPrevalenceAfterTruncation > minPrevalenceBeforeTruncation)
})

test_that("Test bad covariate data", {
  covarSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE)

  cohortMethodData <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    cdmVersion = "5",
    washoutPeriod = 183,
    firstExposureOnly = TRUE,
    removeDuplicateSubjects = "keep all",
    restrictToCommonPeriod = TRUE,
    maxCohortSize = 100000,
    covariateSettings = covarSettings
  )

  expect_warning(
    {
      cohortDataSimulationProfile <- createCohortMethodDataSimulationProfile(cohortMethodData)
    },
    "Cohort data appears to be limited, check daysToCohortEnd which appears to be all zeros"
  )


  warnings <- capture_warnings({
    cohortMethodData <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      targetId = 99,
      comparatorId = 99,
      outcomeIds = c(99, 99),
      cdmVersion = "5",
      washoutPeriod = 183,
      firstExposureOnly = TRUE,
      removeDuplicateSubjects = "keep all",
      restrictToCommonPeriod = TRUE,
      maxCohortSize = 100000,
      covariateSettings = covarSettings
    )
  })
  expect_match(warnings, "Target and comparator cohorts are empty", all = FALSE)
  expect_match(warnings, "Population is empty. No covariates were constructed", all = FALSE)
  expect_error(
    {
      cohortDataSimulationProfile <- createCohortMethodDataSimulationProfile(cohortMethodData)
    },
    "Cohorts are empty"
  )
})

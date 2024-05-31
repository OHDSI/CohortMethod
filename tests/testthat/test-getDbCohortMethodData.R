library(CohortMethod)
library(testthat)

nsaids <- c(1118084, 1124300)

covSettings <- createDefaultCovariateSettings(
  excludedCovariateConceptIds = nsaids,
  addDescendantsToExclude = TRUE
)

test_that("CohortMethodData table dimension check", {
  cmd <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings
  )

  # Dims of CohortMethodData tables
  expect_equal(nrow(collect(cmd$cohorts)), 2630)
  expect_equal(ncol(collect(cmd$cohorts)), 8)

  expect_gte(nrow(collect(cmd$analysisRef)), 24)
  expect_equal(ncol(collect(cmd$analysisRef)), 7)

  expect_gte(nrow(collect(cmd$covariateRef)), 389)
  expect_gte(ncol(collect(cmd$covariateRef)), 4)

  expect_gte(nrow(collect(cmd$covariates)), 26923)
  expect_equal(ncol(collect(cmd$covariates)), 3)

  expect_equal(nrow(collect(cmd$outcomes)), 3109)
  expect_equal(ncol(collect(cmd$outcomes)), 3)

})

test_that("studyStartDate", {
  # pattern: ^[12][0-9]{3}[01][0-9][0-3][0-9]
  # min: 1000-00-00
  # max: 2999-19-39

  ## 20230822 (2023-08-22) ----
  expect_no_error(
    expect_warning(
      getDbCohortMethodData(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = "main",
        targetId = 1,
        comparatorId = 2,
        outcomeIds = c(3, 4),
        exposureDatabaseSchema = "main",
        outcomeDatabaseSchema = "main",
        exposureTable = "cohort",
        outcomeTable = "cohort",
        covariateSettings = covSettings,
        studyStartDate = "20230822"
      )
    )
  )

  ## "" ("") ----
  expect_no_error(
    suppressWarnings(
      getDbCohortMethodData(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = "main",
        targetId = 1,
        comparatorId = 2,
        outcomeIds = c(3, 4),
        exposureDatabaseSchema = "main",
        outcomeDatabaseSchema = "main",
        exposureTable = "cohort",
        outcomeTable = "cohort",
        covariateSettings = covSettings,
        studyStartDate = ""
      )
    )
  )

  ## "02032022" (0203-20-22) ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyStartDate = "02032022"
    ),
    "Date: .+ is not valid"
  )

  ## "10000000" (1000-00-00) ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyStartDate = "10000000"
    ),
    "Date: .+ is not valid"
  )

  ## "09991231" (0999-12-31) ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyStartDate = "09991231"
    ), "Date must be >= 1000-01-01"
  )

  ## "29991939" (2999-19-39) ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyStartDate = "29991939"
    ),
    "Date: .+ is not valid"
  )

  ## "29991940" (2999-19-40) ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyStartDate = "29991940"
    ),
    "Date: .+ is not valid"
  )

  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyStartDate = "30000101"
    ),
    "Date must be <= 2999-12-31"
  )
})

test_that("studyEndDate", {
  # pattern: ^[12][0-9]{3}[01][0-9][0-3][0-9]
  # min: 1000-00-00
  # max: 2999-19-39

  ## 20230822 (2023-08-22) ----
  expect_no_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyEndDate = "20230822"
    )
  )

  ## "" ("") ----
  expect_no_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyEndDate = ""
    )
  )

  ## "02032022" (0203-20-22) ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyEndDate = "02032022"
    ),
    "Date: .+ is not valid"
  )

  ## "10000000" (1000-00-00) ----
  expect_error(
    expect_warning(
      getDbCohortMethodData(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = "main",
        targetId = 1,
        comparatorId = 2,
        outcomeIds = c(3, 4),
        exposureDatabaseSchema = "main",
        outcomeDatabaseSchema = "main",
        exposureTable = "cohort",
        outcomeTable = "cohort",
        covariateSettings = covSettings,
        studyEndDate = "10000000"
      ),
      "Date: .+ is not valid"
    )
  )

  ## "09991231" (0999-12-31) ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyEndDate = "09991231"
    ),
    "Date must be >= 1000-01-01"
  )

  ## "29991939" (2999-19-39) ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyEndDate = "29991939"
    ),
    "Date: .+ is not valid"
  )

  ## "29991940" (2999-19-40) ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyEndDate = "29991940"
    ),
    "Date: .+ is not valid"
  )

  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      studyEndDate = "30000101"
    ),
    "Date must be <= 2999-12-31"
  )
})

test_that("tempEmulationSchema", {
  ## default ----
  expect_no_error(
    suppressWarnings(
      getDbCohortMethodData(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = "main",
        targetId = 1,
        comparatorId = 2,
        outcomeIds = c(3, 4),
        exposureDatabaseSchema = "main",
        outcomeDatabaseSchema = "main",
        exposureTable = "cohort",
        outcomeTable = "cohort",
        covariateSettings = covSettings,
        # In Eunomia
        tempEmulationSchema = "main"
      )
    )
  )
  ## comments ----
  # Throws:
  # Warning message:
  #   The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.
  # on first run
})


test_that("cdmVersion", {
  ## default ----
  expect_no_error(
    expect_no_warning(
      getDbCohortMethodData(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = "main",
        targetId = 1,
        comparatorId = 2,
        outcomeIds = c(3, 4),
        exposureDatabaseSchema = "main",
        outcomeDatabaseSchema = "main",
        exposureTable = "cohort",
        outcomeTable = "cohort",
        covariateSettings = covSettings,
        cdmVersion = "5"
      )
    )
  )

  ## "9" ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      cdmVersion = "9"
    ) # invalid cdmVersion
  )

  ## "madeUpVersion" ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      cdmVersion = "madeUpVersion"
    ) # invalid cdmVersion
  )
  ## comments ----
  # Expected error; i.e. "cdmVersion not supported"
  # Current checks: len = 1, is.character
  # Regex: ^[5]$
  # Regex multiple version: ^[456]$
})


test_that("firstExposureOnly", {
  ## default ----
  res1 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    firstExposureOnly = FALSE
  )

  meta1 <- attr(res1, "metaData")
  expect_identical(nrow(meta1$attrition), 1L)

  ## TRUE ----
  res2 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    firstExposureOnly = TRUE
  )

  meta2 <- attr(res2, "metaData")
  expect_true("First exp. only" %in% meta2$attrition$description)
  expect_identical(nrow(meta2$attrition), 2L)

  ## "TRUE" ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      firstExposureOnly = "TRUE"
    )
  )
})

test_that("removeDuplicateSubjects", {
  ## default: "keep all" ----
  res1 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    removeDuplicateSubjects = "keep all"
  )

  meta1 <- attr(res1, "metaData")
  expect_identical(nrow(meta1$attrition), 1L)

  ## "keep first" ----
  res2 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    removeDuplicateSubjects = "keep first"
  )

  meta2 <- attr(res2, "metaData")
  expect_identical(nrow(meta2$attrition), 2L)
  expect_true("First cohort only" %in% meta2$attrition$description)

  ## "remove all" ----
  res3 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    removeDuplicateSubjects = "remove all"
  )

  meta3 <- attr(res3, "metaData")
  expect_identical(nrow(meta3$attrition), 2L)
  expect_true("Removed subs in both cohorts" %in% meta3$attrition$description)

  ## "do nothing" ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      removeDuplicateSubjects = "do nothing"
    )
  )
})

test_that("restrictToCommonPeriod", {
  ## default: FALSE ----
  res1 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    restrictToCommonPeriod = FALSE
  )

  meta1 <- attr(res1, "metaData")
  expect_equal(meta1$populationSize, 2630)
  expect_equal(nrow(collect(res1$outcomes)), 3109)
  expect_gte(nrow(collect(res1$covariates)), 26923)

  ## TRUE ----
  res2 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    restrictToCommonPeriod = TRUE
  )

  meta2 <- attr(res2, "metaData")
  expect_equal(meta2$populationSize, 2627)
  expect_equal(nrow(collect(res2$outcomes)), 3106)
  expect_gte(nrow(collect(res2$covariates)), 26893)

  ## "TRUE" ----
  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      restrictToCommonPeriod = "TRUE"
    )
  )
})

test_that("washoutPeriod", {
  ## default: 0 ----
  res1 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    washoutPeriod = 0
  )

  meta1 <- attr(res1, "metaData")
  expect_equal(meta1$populationSize, 2630)

  ## 150000 ----
  res2 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    washoutPeriod = 15000
  )

  meta2 <- attr(res2, "metaData")
  expect_equal(meta2$populationSize, 681)

  ## 17000 ----
  res3 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    washoutPeriod = 17000
  )

  meta3 <- attr(res3, "metaData")
  expect_equal(meta3$populationSize, 3)

  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      washoutPeriod = -1
    ),
    ">= 0"
  )

  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      washoutPeriod = "1"
    ),
    "not 'character'"
  )

  expect_no_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      washoutPeriod = 1L
    )
  )

  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      washoutPeriod = 1.3
    ), "not 'double'"
  )
})

test_that("maxCohortSize", {
  ## default: 0 ----
  res1 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    maxCohortSize = 0
  )

  meta1 <- attr(res1, "metaData")
  expect_equal(meta1$populationSize, 2630)

  ## 100 ----
  res2 <- getDbCohortMethodData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    targetId = 1,
    comparatorId = 2,
    outcomeIds = c(3, 4),
    exposureDatabaseSchema = "main",
    outcomeDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    covariateSettings = covSettings,
    maxCohortSize = 100
  )

  meta2 <- attr(res2, "metaData")
  expect_equal(meta2$populationSize, 200)

  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      maxCohortSize = -1
    ),
    ">= 0"
  )

  expect_error(
    getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      covariateSettings = covSettings,
      maxCohortSize = "100"
    ),
    "not 'character'"
  )
})

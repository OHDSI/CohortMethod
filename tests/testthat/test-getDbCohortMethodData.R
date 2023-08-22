library(CohortMethod)
library(testthat)

dim_tbl_dbi <- function(x) {
  return(c(x %>%
    summarise(n = n()) %>%
    pull(),
    ncol(x)
  ))
}

nsaids <- c(1118084, 1124300)

covSettings <- createDefaultCovariateSettings(
  excludedCovariateConceptIds = nsaids,
  addDescendantsToExclude = TRUE
)

test_that("CohortMethodData table dimension check", {
  cmd <<- getDbCohortMethodData(
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
  expect_identical(dim_tbl_dbi(cmd$analysisRef), c(24L, 7L))
  expect_identical(dim_tbl_dbi(cmd$cohorts), c(2630L, 8L))
  expect_identical(dim_tbl_dbi(cmd$covariateRef), c(389L, 4L))
  expect_identical(dim_tbl_dbi(cmd$covariates), c(26923L, 3L))
  expect_identical(dim_tbl_dbi(cmd$outcomes), c(3109L, 3L))
})

test_that("studyStartDate", {
  # pattern: ^[12][0-9]{3}[01][0-9][0-3][0-9]
  # min: 1000-00-00
  # max: 2999-19-39

  ## "02032022" (0203-20-22) ----
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
        studyStartDate = "02032022"
      ),
      "Study start date must have format YYYYMMDD"
    )
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
        studyStartDate = "10000000"
      ) # invalid date
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
      studyStartDate = "09991231"
    ) # "date out of bounds ('10000101' - '29991231')"
  )

  ## "29991939" (2999-19-39) ----
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
        studyStartDate = "29991939"
      ) # invalid date
    )
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
    ) # invalid date
  )
})

test_that("studyEndDate", {
  # pattern: ^[12][0-9]{3}[01][0-9][0-3][0-9]
  # min: 1000-00-00
  # max: 2999-19-39

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
    )
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
      )# "invalid date"
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
    ) # "date out of bounds ('10000101' - '29991231')"
  )

  ## "29991939" (2999-19-39) ----
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
        studyEndDate = "29991939"
      )
    ) # "invalid date"
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
    ) # "date out of bounds ('10000101' - '29991231')"
  )
})

test_that("tempEmulationSchema", {
  ## default ----
  expect_no_error(
    # expect_no_warning(
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
    # )
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
  expect_equal(dim_tbl_dbi(res1$outcomes)[1], 3109)
  expect_equal(dim_tbl_dbi(res1$covariates)[1], 26923)

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
  expect_equal(dim_tbl_dbi(res2$outcomes)[1], 3106)
  expect_equal(dim_tbl_dbi(res2$covariates)[1], 26893)

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

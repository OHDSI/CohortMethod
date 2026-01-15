library(CohortMethod)
library(testthat)

if (!isFALSE(tryCatch(find.package("Eunomia"), error = function(e) FALSE))) {

  # Eunomia connection details set in setup.R

  set.seed(1234)

  nsaids <- c(1118084, 1124300)

  fullCovSettings <- FeatureExtraction::createDefaultCovariateSettings(
    excludedCovariateConceptIds = nsaids,
    addDescendantsToExclude = TRUE
  )
  covSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE)


  # Create a new cohort table where everyone has many exposures to the two drugs. To make sure
  # everyone is observed, we construct the cohort table from the observation_period table:
  connection <- DatabaseConnector::connect(connectionDetails)
  observationPeriod <- DatabaseConnector::querySql(connection = connection,
                                           sql = "SELECT * FROM main.observation_period;",
                                           snakeCaseToCamelCase = TRUE) |>
    as_tibble()
  person <- DatabaseConnector::querySql(connection = connection,
                                        sql = "SELECT * FROM main.person;",
                                        snakeCaseToCamelCase = TRUE) |>
    as_tibble()
  # Not all persons in observation period table are in the person table:
  observationPeriod <- observationPeriod |>
    filter(personId %in% c(person$personId))

  groups <- observationPeriod |>
    filter(observationPeriodEndDate - observationPeriodStartDate > 600) |>
    slice_sample(n = 1000) |>
    group_by(personId) |>
    group_split()
  # group = groups[[1]]
  generateCohorts <- function(group) {
    nCohorts <- 4
    personCohorts <- tibble(
      cohortDefinitionId = sample(c(1, 2), nCohorts, replace = TRUE),
      subjectId = group$personId,
      cohortStartDate = group$observationPeriodStartDate + sample.int(600, nCohorts),
      cohortEndDate = cohortStartDate + sample.int(100, nCohorts, replace = TRUE),
    )
  }
  newCohort <- lapply(groups, generateCohorts) |>
    bind_rows()

  # Construct nesting cohort as a subset of the exposure cohorts:
  nestingCohort <- newCohort |>
    group_by(subjectId) |>
    summarise(minDate = min(cohortStartDate, na.rm = TRUE),
              maxDate = max(cohortStartDate, na.rm = TRUE)) |>
    mutate(cohortDefinitionId = 9,
           cohortStartDate = minDate + round(rnorm(n(), 0, 50)),
           cohortEndDate = maxDate + round(rnorm(n(), 0, 50))) |>
    select(cohortDefinitionId, subjectId, cohortStartDate, cohortEndDate)

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "newCohort",
    data = newCohort,
    dropTableIfExists = TRUE,
    camelCaseToSnakeCase = TRUE
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "nesting",
    data = nestingCohort,
    dropTableIfExists = TRUE,
    camelCaseToSnakeCase = TRUE
  )
  DatabaseConnector::disconnect(connection)

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
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = fullCovSettings
      )
    )

    # Dims of CohortMethodData tables
    expect_equal(nrow(collect(cmd$cohorts)), 2627)
    expect_equal(ncol(collect(cmd$cohorts)), 8)

    expect_gte(nrow(collect(cmd$analysisRef)), 24)
    expect_equal(ncol(collect(cmd$analysisRef)), 7)

    expect_gte(nrow(collect(cmd$covariateRef)), 388)
    expect_gte(ncol(collect(cmd$covariateRef)), 4)

    expect_gte(nrow(collect(cmd$covariates)), 20000)
    expect_equal(ncol(collect(cmd$covariates)), 3)

    expect_equal(nrow(collect(cmd$outcomes)), 3106)
    expect_equal(ncol(collect(cmd$outcomes)), 3)
  })

  test_that("studyStartDate", {
    # pattern: ^[12][0-9]{3}[01][0-9][0-3][0-9]
    # min: 1000-00-00
    # max: 2999-19-39

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
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        studyStartDate = "20000601",
      )
    )

    meta <- attr(cmd, "metaData")
    expect_identical(nrow(meta$attrition), 6L)
    expect_true("Restrict to study period" %in% meta$attrition$description)

    minStartDate <- cmd$cohorts |>
      summarise(minStartDate = min(cohortStartDate, na.rm = TRUE)) |>
      pull()
    expect_gte(minStartDate, as.Date("2000-06-01"))

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
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = covSettings,
          studyStartDate = "02032022"
        )
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
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = covSettings,
          studyStartDate = "09991231"
        )
      ), "Date must be >= 1000-01-01"
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
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = covSettings,
          studyStartDate = "30001231"
        )
      ), "Date must be <= 2999-12-31"
    )
  })

  test_that("studyEndDate", {
    # pattern: ^[12][0-9]{3}[01][0-9][0-3][0-9]
    # min: 1000-00-00
    # max: 2999-19-39

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
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        studyEndDate = "20230822"
      )
    )
    meta <- attr(cmd, "metaData")
    expect_identical(nrow(meta$attrition), 6L)
    expect_true("Restrict to study period" %in% meta$attrition$description)

    maxEndDate <- cmd$cohorts |>
      collect() |>
      summarise(maxEndDate = max(cohortStartDate + daysToCohortEnd, na.rm = TRUE)) |>
      pull()
    expect_lte(maxEndDate, as.Date("2023-08-22"))

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
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = covSettings,
          studyEndDate = "02032022"
        )
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
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = covSettings,
          studyEndDate = "09991231"
        )
      ),
      "Date must be >= 1000-01-01"
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
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = covSettings,
          studyEndDate = "30000101"
        )
      ),
      "Date must be <= 2999-12-31"
    )
  })

  test_that("firstExposureOnly", {

    ## firstExposureOnly = FALSE ----
    cmd1 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        firstExposureOnly = FALSE,
        removeDuplicateSubjects = "keep all",
        washoutPeriod = 0,
        restrictToCommonPeriod = FALSE
      )
    )

    meta1 <- attr(cmd1, "metaData")
    expect_identical(nrow(meta1$attrition), 1L)

    cohorts1 <- cmd1$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    expect_equal(select(arrange(cohorts1, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohort, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)


    ## firstExposureOnly = TRUE ----
    cmd2 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        firstExposureOnly = TRUE,
        removeDuplicateSubjects = "keep all",
        washoutPeriod = 0,
        restrictToCommonPeriod = FALSE
      )
    )

    meta2 <- attr(cmd2, "metaData")
    expect_true("First exposure only" %in% meta2$attrition$description)
    expect_identical(nrow(meta2$attrition), 2L)

    cohorts2 <- cmd2$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    newCohortGs <- newCohort |>
      group_by(cohortDefinitionId, subjectId) |>
      arrange(cohortStartDate) |>
      filter(row_number() == 1) |>
      ungroup()
    expect_equal(select(arrange(cohorts2, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohortGs, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)

  })

  test_that("removeDuplicateSubjects", {
    ## "keep all" ----
    cmd1 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        restrictToCommonPeriod = FALSE
      )
    )

    meta1 <- attr(cmd1, "metaData")
    expect_identical(nrow(meta1$attrition), 1L)

    cohorts1 <- cmd1$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    expect_equal(select(arrange(cohorts1, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohort, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)

    ## "keep first, truncate to second" ----
    cmd2 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        removeDuplicateSubjects = "keep first, truncate to second",
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        restrictToCommonPeriod = FALSE
      )
    )

    meta2 <- attr(cmd2, "metaData")
    expect_identical(nrow(meta2$attrition), 2L)
    expect_true("Keep first, truncate when entering second cohort" %in% meta2$attrition$description)

    cohorts2 <- cmd2$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    firstPerCohort <- newCohort |>
      group_by(subjectId, cohortDefinitionId) |>
      arrange(cohortStartDate) |>
      filter(row_number() == 1) |>
      ungroup()
    firstOverall <- firstPerCohort |>
      group_by(subjectId) |>
      arrange(cohortStartDate) |>
      filter(row_number() == 1) |>
      ungroup()
    newCohortGs <- firstOverall |>
      left_join(firstPerCohort |>
                  transmute(subjectId,
                            cohortDefinitionId = if_else(cohortDefinitionId == 1, 2, 1),
                            cohortStartDate2 = cohortStartDate),
                by = join_by(subjectId, cohortDefinitionId)) |>
      filter(is.na(cohortStartDate2) | cohortStartDate2 != cohortStartDate) |>
      mutate(daysToCohortEnd = as.numeric(difftime(cohortEndDate, cohortStartDate, units = "days"))) |>
      mutate(daysToCohortEnd = if_else(!is.na(cohortStartDate2) & cohortStartDate2 < cohortEndDate,
                                       as.numeric(difftime(cohortStartDate2 - 1, cohortStartDate, units = "days")),
                                       daysToCohortEnd))

    expect_equal(select(arrange(cohorts2, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate, daysToCohortEnd),
                 select(arrange(newCohortGs, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate, daysToCohortEnd),
                 check.attribues = FALSE)

    ## "keep first" ----
    cmd3 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        removeDuplicateSubjects = "keep first",
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        restrictToCommonPeriod = FALSE
      )
    )

    meta3 <- attr(cmd3, "metaData")
    expect_identical(nrow(meta3$attrition), 2L)
    expect_true("Keep first when in both cohorts" %in% meta3$attrition$description)

    cohorts3 <- cmd3$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    newCohortGs <- newCohort |>
      group_by(subjectId) |>
      arrange(cohortStartDate) |>
      filter(row_number() == 1) |>
      ungroup()
    expect_equal(select(arrange(cohorts3, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohortGs, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)


    ## "remove all" ----
    cmd4 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        removeDuplicateSubjects = "remove all",
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        restrictToCommonPeriod = FALSE
      )
    )

    meta4 <- attr(cmd4, "metaData")
    expect_identical(nrow(meta4$attrition), 2L)
    expect_true("Remove subjects in both cohorts" %in% meta4$attrition$description)

    cohorts4 <- cmd4$cohorts |>
      collect() |>
      mutate(subjectId = as.numeric(personId))

    subjectIdsOneTreatment <- newCohort |>
      group_by(subjectId) |>
      summarise(treatments = n_distinct(cohortDefinitionId)) |>
      filter(treatments == 1) |>
      pull(subjectId)
    newCohortGs <- newCohort |>
      filter(subjectId %in% subjectIdsOneTreatment)
    expect_equal(select(arrange(cohorts4, subjectId, cohortStartDate), subjectId, cohortStartDate),
                 select(arrange(newCohortGs, subjectId, cohortStartDate), subjectId, cohortStartDate),
                 check.attribues = FALSE)

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
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = covSettings,
          removeDuplicateSubjects = "illegal option"
        )
      )
    )
  })

  test_that("restrictToCommonPeriod", {
    ## default: FALSE ----
    cmd1 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        restrictToCommonPeriod = FALSE,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        washoutPeriod = 0
      )
    )

    meta1 <- attr(cmd1, "metaData")
    expect_identical(nrow(meta1$attrition), 1L)

    cohorts1 <- cmd1$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    expect_equal(select(arrange(cohorts1, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohort, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)

    ## TRUE ----
    cmd2 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        restrictToCommonPeriod = TRUE,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        washoutPeriod = 0
      )
    )

    meta2 <- attr(cmd2, "metaData")
    expect_identical(nrow(meta2$attrition), 2L)
    expect_true("Restrict to common period" %in% meta2$attrition$description)

    cohorts2 <- cmd2$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    commonPeriod <- newCohort |>
      group_by(cohortDefinitionId) |>
      summarize(minDate = min(cohortStartDate, na.rm = TRUE),
                maxDate = max(cohortStartDate, na.rm = TRUE)) |>
      summarise(startDate = max(minDate, na.rm = TRUE),
                endDate = min(maxDate, na.rm = TRUE))

    newCohortGs <- newCohort |>
      filter(cohortStartDate >= commonPeriod$startDate,
             cohortStartDate <= commonPeriod$endDate)
    expect_equal(select(arrange(cohorts2, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohortGs, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)
  })

  test_that("restrictByAge", {
    ## default: NULL ----
    cmd1 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        restrictToCommonPeriod = FALSE,
        minAge = NULL,
        maxAge = NULL,
        washoutPeriod = 0
      )
    )

    meta1 <- attr(cmd1, "metaData")
    expect_identical(nrow(meta1$attrition), 1L)

    cohorts1 <- cmd1$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    expect_equal(select(arrange(cohorts1, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohort, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)

    ## 0-0 ----
    # Everyone has age 0 or 1, so set max to 0 to have some attrition:
    minAge <- 0
    maxAge <- 0
    cmd2 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        restrictToCommonPeriod = FALSE,
        minAge = minAge,
        maxAge = maxAge,
        washoutPeriod = 0
      )
    )

    meta2 <- attr(cmd2, "metaData")
    expect_identical(nrow(meta2$attrition), 2L)
    expect_true("Restrict by age" %in% meta2$attrition$description)

    cohorts2 <- cmd2$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    newCohortGs <- newCohort |>
      inner_join(person, by = join_by(subjectId == personId)) |>
      mutate(dateOfBirth = ISOdate(yearOfBirth, monthOfBirth, dayOfBirth)) |>
      mutate(age = floor(as.numeric(difftime(cohortStartDate, dateOfBirth, units = "days")) / 365.25)) |>
      filter(age >= minAge, age <= maxAge)

    expect_equal(select(arrange(cohorts2, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohortGs, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)
  })

  test_that("restrictByGender", {
    ## default: NULL ----
    cmd1 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        restrictToCommonPeriod = FALSE,
        genderConceptIds = NULL,
        washoutPeriod = 0
      )
    )

    meta1 <- attr(cmd1, "metaData")
    expect_identical(nrow(meta1$attrition), 1L)

    cohorts1 <- cmd1$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    expect_equal(select(arrange(cohorts1, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohort, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)

    ## 8532 ----
    conceptId <- 8532
    cmd2 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        restrictToCommonPeriod = FALSE,
        genderConceptIds = conceptId,
        washoutPeriod = 0
      )
    )

    meta2 <- attr(cmd2, "metaData")
    expect_identical(nrow(meta2$attrition), 2L)
    expect_true("Restrict by gender" %in% meta2$attrition$description)

    cohorts2 <- cmd2$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    newCohortGs <- newCohort |>
      inner_join(person, by = join_by(subjectId == personId)) |>
      filter(genderConceptId == conceptId)

    expect_equal(select(arrange(cohorts2, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohortGs, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)
  })

  test_that("washoutPeriod", {
    ## default: 0 ----
    cmd1 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        restrictToCommonPeriod = FALSE
      )
    )

    meta1 <- attr(cmd1, "metaData")
    expect_identical(nrow(meta1$attrition), 1L)

    cohorts1 <- cmd1$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    expect_equal(select(arrange(cohorts1, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohort, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)

    ## 365 ----
    cmd2 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        restrictToCommonPeriod = FALSE
      )
    )

    meta2 <- attr(cmd2, "metaData")
    expect_identical(nrow(meta2$attrition), 2L)
    expect_true("365 days of prior observation" %in% meta2$attrition$description)

    cohorts2 <- cmd2$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    newCohortGs <- newCohort |>
      inner_join(observationPeriod, by = join_by(subjectId == personId)) |>
      filter(difftime(cohortStartDate, observationPeriodStartDate, units = "days") >= 365)
    expect_equal(select(arrange(cohorts2, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohortGs, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)

    expect_error(
      getDbCohortMethodData(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = "main",
        targetId = 1,
        comparatorId = 2,
        outcomeIds = c(3, 4),
        exposureDatabaseSchema = "main",
        outcomeDatabaseSchema = "main",
        exposureTable = "newCohort",
        outcomeTable = "cohort",
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = covSettings,
          washoutPeriod = -1
        )
      ),
      ">= 0"
    )
  })

  test_that("maxCohortSize", {
    ## default: 0 ----
    cmd1 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        maxCohortSize = 0,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        restrictToCommonPeriod = FALSE
      )
    )

    meta1 <- attr(cmd1, "metaData")
    expect_identical(nrow(meta1$attrition), 1L)

    cohorts1 <- cmd1$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    expect_equal(select(arrange(cohorts1, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohort, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)

    ## 100 ----
    cmd2 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        maxCohortSize = 100,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        restrictToCommonPeriod = FALSE
      )
    )

    meta2 <- attr(cmd2, "metaData")
    expect_identical(nrow(meta2$attrition), 2L)
    expect_true("Random sample" %in% meta2$attrition$description)

    cohorts2 <- cmd2$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    expect_equal(nrow(cohorts2), 2 * 100)

    expect_error(
      getDbCohortMethodData(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = "main",
        targetId = 1,
        comparatorId = 2,
        outcomeIds = c(3, 4),
        exposureDatabaseSchema = "main",
        outcomeDatabaseSchema = "main",
        exposureTable = "newCohort",
        outcomeTable = "cohort",
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = covSettings,
          maxCohortSize = -1
        )
      ),
      ">= 0"
    )
  })

  test_that("nesting cohort", {
    ## default: none ----
    cmd1 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        nestingCohortId = NULL,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        restrictToCommonPeriod = FALSE
      )
    )

    meta1 <- attr(cmd1, "metaData")
    expect_identical(nrow(meta1$attrition), 1L)

    cohorts1 <- cmd1$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    expect_equal(select(arrange(cohorts1, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohort, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)



    ## Nesting ----
    cmd2 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      nestingCohortDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      nestingCohortTable = "nesting",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        nestingCohortId = 9,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        firstExposureOnly = FALSE,
        restrictToCommonPeriod = FALSE
      )
    )

    meta2 <- attr(cmd2, "metaData")
    expect_identical(nrow(meta2$attrition), 2L)
    expect_true("Restrict to nesting cohort" %in% meta2$attrition$description)

    cohorts2 <- cmd2$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    newCohortGs <- newCohort |>
      inner_join(nestingCohort |>
                   transmute(subjectId,
                             startDate = cohortStartDate,
                             endDate = cohortEndDate),
                 by = join_by(subjectId)) |>
      filter(cohortStartDate >= startDate,
             cohortStartDate <= endDate)

    expect_equal(select(arrange(cohorts2, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohortGs, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)
  })


  test_that("all at once (except random sample)", {

    cmd2 <- getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      targetId = 1,
      comparatorId = 2,
      outcomeIds = c(3, 4),
      exposureDatabaseSchema = "main",
      outcomeDatabaseSchema = "main",
      nestingCohortDatabaseSchema = "main",
      exposureTable = "newCohort",
      outcomeTable = "cohort",
      nestingCohortTable = "nesting",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = covSettings,
        firstExposureOnly = TRUE,
        washoutPeriod = 30,
        removeDuplicateSubjects = "keep first",
        restrictToCommonPeriod = TRUE,
        minAge = 0,
        maxAge = 0,
        genderConceptIds = 8532,
        studyStartDate = "19200101",
        studyEndDate = "19751231",
        nestingCohortId = 9
      )
    )

    meta2 <- attr(cmd2, "metaData")
    expect_identical(nrow(meta2$attrition), 9L)

    cohorts2 <- cmd2$cohorts |>
      collect() |>
      mutate(cohortDefinitionId = if_else(treatment == 1, 1, 2),
             subjectId = as.numeric(personId))

    # Keep first
    newCohortGs <- newCohort |>
      group_by(subjectId) |>
      arrange(cohortStartDate) |>
      filter(row_number() == 1) |>
      ungroup()

    # First exposure only
    newCohortGs <- newCohortGs |>
      group_by(cohortDefinitionId, subjectId) |>
      arrange(cohortStartDate) |>
      filter(row_number() == 1) |>
      ungroup()

    # Washout
    newCohortGs <- newCohortGs |>
      inner_join(observationPeriod, by = join_by(subjectId == personId)) |>
      filter(difftime(cohortStartDate, observationPeriodStartDate, units = "days") >= 30)

    # Nesting
    newCohortGs <- newCohortGs |>
      inner_join(nestingCohort |>
                   transmute(subjectId,
                             startDate = cohortStartDate,
                             endDate = cohortEndDate),
                 by = join_by(subjectId)) |>
      filter(cohortStartDate >= startDate,
             cohortStartDate <= endDate)

    # Restrict to common period
    commonPeriod <- newCohort |>
      group_by(cohortDefinitionId) |>
      summarize(minDate = min(cohortStartDate, na.rm = TRUE),
                maxDate = max(cohortStartDate, na.rm = TRUE)) |>
      summarise(startDate = max(minDate, na.rm = TRUE),
                endDate = min(maxDate, na.rm = TRUE))

    newCohortGs <- newCohortGs |>
      filter(cohortStartDate >= commonPeriod$startDate,
             cohortStartDate <= commonPeriod$endDate)

    # Study period
    newCohortGs <- newCohortGs |>
      filter(cohortStartDate >= as.Date("1920-01-01"),
             cohortStartDate <= as.Date("1975-12-31"))

    # Age and gender
    newCohortGs <- newCohortGs |>
      inner_join(person, by = join_by(subjectId == personId)) |>
      mutate(dateOfBirth = ISOdate(yearOfBirth, monthOfBirth, dayOfBirth)) |>
      mutate(age = floor(as.numeric(difftime(cohortStartDate, dateOfBirth, units = "days")) / 365.25)) |>
      filter(age >= minAge, age <= maxAge, genderConceptId == 8532)


    expect_equal(select(arrange(cohorts2, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 select(arrange(newCohortGs, subjectId, cohortStartDate), cohortDefinitionId, subjectId, cohortStartDate),
                 check.attribues = FALSE)


  })
}

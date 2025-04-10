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

#' Get the cohort data from the server
#'
#' @description
#' This function executes a large set of SQL statements against the database in OMOP CDM format to
#' extract the data needed to perform the analysis.
#'
#' @details
#' Based on the arguments, the treatment and comparator cohorts are retrieved, as well as outcomes
#' occurring in exposed subjects. The treatment and comparator cohorts can be identified using the
#' DRUG_ERA table, or through user-defined cohorts in a cohort table either inside the CDM schema or
#' in a separate schema. Similarly, outcomes are identified using the CONDITION_ERA table or through
#' user-defined cohorts in a cohort table either inside the CDM schema or in a separate schema.
#' Covariates are automatically extracted from the appropriate tables within the CDM.
#'
#' **Important**: The target and comparator drug must not be included in the covariates, including any descendant
#' concepts. You will need to manually add the drugs and descendants to the `excludedCovariateConceptIds`
#' of the `covariateSettings` argument.
#'
#' The `removeduplicateSubjects` argument can have one of the following values:
#'
#' - `"keep all"`: Do not remove subjects that appear in both target and comparator cohort
#' - `"keep first"`: When a subjects appear in both target and comparator cohort, only keep whichever cohort is first in time.
#' - `"remove all"`: Remove subjects that appear in both target and comparator cohort completely from the analysis."
#'
#' If the `covariateSettings` include cohort-based covariates, and the `covariateCohortTable` is `NULL`, the
#' `covariateCohortDatabaseSchema` and `covariateCohortTable` will be set to the `exposureDatabaseSchema` and
#' `exposureTable`, respectively .
#'
#' @param connectionDetails            An R object of type `connectionDetails` created using the
#'                                     [DatabaseConnector::createConnectionDetails()] function.
#' @param cdmDatabaseSchema            The name of the database schema that contains the OMOP CDM
#'                                     instance. Requires read permissions to this database. On SQL
#'                                     Server, this should specify both the database and the schema,
#'                                     so for example 'cdm_instance.dbo'.
#' @param tempEmulationSchema Some database platforms like Oracle and Impala do not truly support temp tables. To
#'                            emulate temp tables, provide a schema with write privileges where temp tables
#'                            can be created.
#' @param targetId                     A unique identifier to define the target cohort.  If
#'                                     exposureTable = DRUG_ERA, targetId is a concept ID and all
#'                                     descendant concepts within that concept ID will be used to
#'                                     define the cohort.  If exposureTable <> DRUG_ERA, targetId is
#'                                     used to select the COHORT_DEFINITION_ID in the cohort-like table.
#' @param comparatorId                 A unique identifier to define the comparator cohort.  If
#'                                     exposureTable = DRUG_ERA, comparatorId is a concept ID and all
#'                                     descendant concepts within that concept ID will be used to
#'                                     define the cohort.  If exposureTable <> DRUG_ERA, comparatorId
#'                                     is used to select the COHORT_DEFINITION_ID in the cohort-like
#'                                     table.
#' @param outcomeIds                   A list of cohort IDs used to define outcomes.
#' @param studyStartDate               A calendar date specifying the minimum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate                 A calendar date specifying the maximum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'. Important: the study
#'                                     end data is also used to truncate risk windows, meaning no
#'                                     outcomes beyond the study end date will be considered.
#' @param exposureDatabaseSchema       The name of the database schema that is the location where the
#'                                     exposure data used to define the exposure cohorts is available.
#' @param exposureTable                The tablename that contains the exposure cohorts. If
#'                                     exposureTable <> DRUG_ERA, then expectation is `exposureTable` has
#'                                     format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                     COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema        The name of the database schema that is the location where the
#'                                     data used to define the outcome cohorts is available.
#' @param outcomeTable                 The tablename that contains the outcome cohorts.  If
#'                                     outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                     outcomeTable has format of COHORT table: COHORT_DEFINITION_ID,
#'                                     SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.
#' @param cdmVersion                   Define the OMOP CDM version used: currently supports "5".
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note
#'                                     that this is typically done in the [createStudyPopulation()]
#'                                     function, but can already be done here for efficiency reasons.
#' @param removeDuplicateSubjects      Remove subjects that are in both the target and comparator
#'                                     cohort? See details for allowed values.Note that this is typically done in the
#'                                     `createStudyPopulation` function, but can already be done
#'                                     here for efficiency reasons.
#' @param restrictToCommonPeriod       Restrict the analysis to the period when both treatments are observed?
#' @param washoutPeriod                The minimum required continuous observation time prior to index
#'                                     date for a person to be included in the cohort. Note that this
#'                                     is typically done in the `createStudyPopulation` function,
#'                                     but can already be done here for efficiency reasons.
#' @param maxCohortSize                If either the target or the comparator cohort is larger than
#'                                     this number it will be sampled to this size. `maxCohortSize = 0`
#'                                     indicates no maximum size.
#' @param covariateSettings            An object of type `covariateSettings` as created using the
#'                                     [FeatureExtraction::createCovariateSettings()] function.
#'
#' @return
#' A [CohortMethodData] object.
#'
#' @export
getDbCohortMethodData <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                  targetId,
                                  comparatorId,
                                  outcomeIds,
                                  studyStartDate = "",
                                  studyEndDate = "",
                                  exposureDatabaseSchema = cdmDatabaseSchema,
                                  exposureTable = "drug_era",
                                  outcomeDatabaseSchema = cdmDatabaseSchema,
                                  outcomeTable = "condition_occurrence",
                                  cdmVersion = "5",
                                  firstExposureOnly = FALSE,
                                  removeDuplicateSubjects = "keep all",
                                  restrictToCommonPeriod = FALSE,
                                  washoutPeriod = 0,
                                  maxCohortSize = 0,
                                  covariateSettings) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(targetId, add = errorMessages)
  checkmate::assertNumeric(comparatorId, add = errorMessages)
  checkmate::assertNumeric(outcomeIds, add = errorMessages)
  checkmate::assertTRUE(all(c(targetId, comparatorId, outcomeIds) %% 1 == 0), add = errorMessages)
  checkmate::assertCharacter(studyStartDate, len = 1, add = errorMessages)
  checkmate::assertCharacter(studyEndDate, len = 1, add = errorMessages)
  checkmate::assertCharacter(exposureDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(exposureTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(outcomeDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(outcomeTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(cdmVersion, len = 1, n.chars = 1, pattern = "^[5]$", add = errorMessages)
  checkmate::assertLogical(firstExposureOnly, len = 1, add = errorMessages)
  checkmate::assertChoice(removeDuplicateSubjects, c("keep all", "keep first", "remove all"), add = errorMessages)
  checkmate::assertLogical(restrictToCommonPeriod, len = 1, add = errorMessages)
  checkmate::assertInt(washoutPeriod, lower = 0, add = errorMessages)
  checkmate::assertInt(maxCohortSize, lower = 0, add = errorMessages)
  checkmate::assertList(covariateSettings, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  dateCheck(studyStartDate)
  dateCheck(studyEndDate)

  if (studyStartDate != "" &&
    regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyStartDate) == -1) {
    stop("Study start date must have format YYYYMMDD")
  }
  if (studyEndDate != "" &&
    regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyEndDate) == -1) {
    stop("Study end date must have format YYYYMMDD")
  }

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  message("Constructing target and comparator cohorts")
  renderedSql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "CreateCohorts.sql",
    packageName = "CohortMethod",
    dbms = connectionDetails$dbms,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema,
    exposure_database_schema = exposureDatabaseSchema,
    exposure_table = exposureTable,
    target_id = targetId,
    comparator_id = comparatorId,
    study_start_date = studyStartDate,
    study_end_date = studyEndDate,
    first_only = firstExposureOnly,
    remove_duplicate_subjects = removeDuplicateSubjects,
    washout_period = washoutPeriod,
    restrict_to_common_period = restrictToCommonPeriod
  )
  DatabaseConnector::executeSql(connection, renderedSql)

  if (maxCohortSize != 0) {
    outList <- downSample(
      connection = connection,
      tempEmulationSchema = tempEmulationSchema,
      targetId = targetId,
      maxCohortSize = maxCohortSize
    )
    sampled <- outList$sampled
    preSampleCounts <- outList$preSampleCounts
  } else {
    sampled <- FALSE
  }

  message("Fetching cohorts from server")
  start <- Sys.time()
  cohortSql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "GetCohorts.sql",
    packageName = "CohortMethod",
    dbms = connectionDetails$dbms,
    tempEmulationSchema = tempEmulationSchema,
    target_id = targetId,
    sampled = sampled
  )
  cohorts <- DatabaseConnector::querySql(
    connection = connection,
    sql = cohortSql,
    snakeCaseToCamelCase = TRUE
  )
  cohorts$rowId <- as.numeric(cohorts$rowId)
  ParallelLogger::logDebug(
    "Fetched cohort total rows in target is ",
    sum(cohorts$treatment),
    ", total rows in comparator is ",
    sum(!cohorts$treatment)
  )
  if (nrow(cohorts) == 0) {
    warning("Target and comparator cohorts are empty")
  } else if (sum(cohorts$treatment == 1) == 0) {
    warning("Target cohort is empty")
  } else if (sum(cohorts$treatment == 0) == 0) {
    warning("Comparator cohort is empty")
  }
  metaData <- list(
    targetId = targetId,
    comparatorId = comparatorId,
    studyStartDate = studyStartDate,
    studyEndDate = studyEndDate
  )
  if (firstExposureOnly || removeDuplicateSubjects != "keep all" || washoutPeriod != 0) {
    rawCountSql <- SqlRender::loadRenderTranslateSql(
      sqlFilename = "CountOverallExposedPopulation.sql",
      packageName = "CohortMethod",
      dbms = connectionDetails$dbms,
      tempEmulationSchema = tempEmulationSchema,
      cdm_database_schema = cdmDatabaseSchema,
      exposure_database_schema = exposureDatabaseSchema,
      exposure_table = tolower(exposureTable),
      target_id = targetId,
      comparator_id = comparatorId,
      study_start_date = studyStartDate,
      study_end_date = studyEndDate
    )
    rawCount <- DatabaseConnector::querySql(
      connection = connection,
      sql = rawCountSql,
      snakeCaseToCamelCase = TRUE
    )
    if (nrow(rawCount) == 0) {
      counts <- tibble(
        description = "Original cohorts",
        targetPersons = 0,
        comparatorPersons = 0,
        targetExposures = 0,
        comparatorExposures = 0
      )
    } else {
      counts <- tibble(
        description = "Original cohorts",
        targetPersons = rawCount$exposedCount[rawCount$treatment == 1],
        comparatorPersons = rawCount$exposedCount[rawCount$treatment == 0],
        targetExposures = rawCount$exposureCount[rawCount$treatment == 1],
        comparatorExposures = rawCount$exposureCount[rawCount$treatment == 0]
      )
    }
    metaData$attrition <- counts
    label <- c()
    if (firstExposureOnly) {
      label <- c(label, "first exp. only")
    }
    if (removeDuplicateSubjects == "remove all") {
      label <- c(label, "removed subs in both cohorts")
    } else if (removeDuplicateSubjects == "keep first") {
      label <- c(label, "first cohort only")
    }

    if (restrictToCommonPeriod) {
      label <- c(label, "restrict to common period")
    }
    if (washoutPeriod) {
      label <- c(label, paste(washoutPeriod, "days of obs. prior"))
    }
    label <- paste(label, collapse = " & ")
    substring(label, 1) <- toupper(substring(label, 1, 1))
    if (sampled) {
      preSampleCounts$description <- label
      metaData$attrition <-
        rbind(metaData$attrition, preSampleCounts)
      metaData$attrition <-
        rbind(metaData$attrition, getCounts(cohorts, "Random sample"))
    } else {
      metaData$attrition <-
        rbind(metaData$attrition, getCounts(cohorts, label))
    }
  } else {
    if (sampled) {
      preSampleCounts$description <- "Original cohorts"
      metaData$attrition <- preSampleCounts
      metaData$attrition <- rbind(
        metaData$attrition,
        getCounts(cohorts, "Random sample")
      )
    } else {
      metaData$attrition <- getCounts(cohorts, "Original cohorts")
    }
  }
  delta <- Sys.time() - start
  message("Fetching cohorts took ", signif(delta, 3), " ", attr(delta, "units"))
  if (sampled) {
    cohortTable <- "#cohort_sample"
  } else {
    cohortTable <- "#cohort_person"
  }
  covariateSettings <- handleCohortCovariateBuilders(
    covariateSettings = covariateSettings,
    exposureDatabaseSchema = exposureDatabaseSchema,
    exposureTable = exposureTable
  )
  covariateData <- FeatureExtraction::getDbCovariateData(
    connection = connection,
    oracleTempSchema = tempEmulationSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cdmVersion = cdmVersion,
    cohortTable = cohortTable,
    cohortTableIsTemp = TRUE,
    rowIdField = "row_id",
    covariateSettings = covariateSettings
  )
  ParallelLogger::logDebug(
    "Fetched covariates total count is ",
    nrow_temp(covariateData$covariates)
  )
  message("Fetching outcomes from server")
  start <- Sys.time()
  outcomeSql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "GetOutcomes.sql",
    packageName = "CohortMethod",
    dbms = connectionDetails$dbms,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema,
    outcome_database_schema = outcomeDatabaseSchema,
    outcome_table = outcomeTable,
    outcome_ids = outcomeIds,
    sampled = sampled
  )
  outcomes <- DatabaseConnector::querySql(
    connection = connection,
    sql = outcomeSql,
    snakeCaseToCamelCase = TRUE
  )
  outcomes$rowId <- as.numeric(outcomes$rowId)
  metaData$outcomeIds <- outcomeIds
  delta <- Sys.time() - start
  message("Fetching outcomes took ", signif(delta, 3), " ", attr(delta, "units"))
  ParallelLogger::logDebug("Fetched outcomes total count is ", nrow(outcomes))

  # Remove temp tables:
  renderedSql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "RemoveCohortTempTables.sql",
    packageName = "CohortMethod",
    dbms = connectionDetails$dbms,
    tempEmulationSchema = tempEmulationSchema,
    sampled = sampled
  )
  DatabaseConnector::executeSql(
    connection = connection,
    sql = renderedSql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  covariateData$cohorts <- cohorts
  covariateData$outcomes <- outcomes
  attr(covariateData, "metaData") <- append(attr(covariateData, "metaData"), metaData)
  class(covariateData) <- "CohortMethodData"
  attr(class(covariateData), "package") <- "CohortMethod"
  return(covariateData)
}

downSample <- function(connection,
                       tempEmulationSchema,
                       targetId,
                       maxCohortSize) {
  sampled <- FALSE
  renderedSql <- SqlRender::loadRenderTranslateSql(
    "CountCohorts.sql",
    packageName = "CohortMethod",
    dbms = connection@dbms,
    tempEmulationSchema = tempEmulationSchema,
    target_id = targetId
  )
  counts <- DatabaseConnector::querySql(connection, renderedSql, snakeCaseToCamelCase = TRUE)
  ParallelLogger::logDebug("Pre-sample total row count is ", sum(counts$rowCount))
  preSampleCounts <- bind_cols(
    countPreSample(id = 1, counts = counts),
    countPreSample(id = 0, counts = counts)
  )
  if (preSampleCounts$targetExposures > maxCohortSize) {
    message(
      "Downsampling target cohort from ", preSampleCounts$targetExposures,
      " to ", maxCohortSize
    )
    sampled <- TRUE
  }
  if (preSampleCounts$comparatorExposures > maxCohortSize) {
    message(
      "Downsampling comparator cohort from ", preSampleCounts$comparatorExposures,
      " to ", maxCohortSize
    )
    sampled <- TRUE
  }
  if (sampled) {
    renderedSql <- SqlRender::loadRenderTranslateSql(
      "SampleCohorts.sql",
      packageName = "CohortMethod",
      dbms = connection@dbms,
      tempEmulationSchema = tempEmulationSchema,
      max_cohort_size = maxCohortSize
    )
    DatabaseConnector::executeSql(connection, renderedSql)
  }
  return(list(sampled = sampled, preSampleCounts = preSampleCounts))
}

countPreSample <- function(id, counts) {
  preSampleCounts <- tibble(dummy = 0)
  idx <- which(counts$treatment == id)

  switch(id + 1,
    {
      personsCol <- "comparatorPersons"
      exposuresCol <- "comparatorExposures"
    },
    {
      personsCol <- "targetPersons"
      exposuresCol <- "targetExposures"
    }
  )

  preSampleCounts[personsCol] <- 0
  preSampleCounts[exposuresCol] <- 0

  if (length(idx) != 0) {
    preSampleCounts[personsCol] <- counts$personCount[idx]
    preSampleCounts[exposuresCol] <- counts$rowCount[idx]
  }
  preSampleCounts$dummy <- NULL
  return(preSampleCounts)
}

handleCohortCovariateBuilders <- function(covariateSettings,
                                          exposureDatabaseSchema,
                                          exposureTable) {
  if (is(covariateSettings, "covariateSettings")) {
    covariateSettings <- list(covariateSettings)
  }
  for (i in 1:length(covariateSettings)) {
    object <- covariateSettings[[i]]
    if ("covariateCohorts" %in% names(object) &&
      is.null(object$covariateCohortTable)) {
      object$covariateCohortDatabaseSchema <- exposureDatabaseSchema
      object$covariateCohortTable <- exposureTable
      covariateSettings[[i]] <- object
    }
  }
  return(covariateSettings)
}

dateCheck <- function(date) {
  if (date != "") {
    tryCatch({
      dateFormatted <- paste0(
        substr(x = date, start = 1, stop = 4), "-",
        substr(x = date, start = 5, stop = 6), "-",
        substr(x = date, start = 7, stop = 8)
      )

      date <- as.Date(dateFormatted)
    }, error = function(e) {
      stop(sprintf("Date: %s (%s) is not valid", date, dateFormatted))
    })
    checkmate::assertDate(date, lower = "1000-01-01", upper = "2999-12-31")
  }
}

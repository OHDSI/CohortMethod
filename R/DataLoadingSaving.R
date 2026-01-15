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
#' Optionally, the target and comparator cohorts can be restricted to be within a nesting cohort,
#' which can reside in a  different database schema and table.
#'
#' @param connectionDetails            An R object of type `connectionDetails` created using the
#'                                     [DatabaseConnector::createConnectionDetails()] function.
#' @param cdmDatabaseSchema            The name of the database schema that contains the OMOP CDM
#'                                     instance. Requires read permissions to this database. On SQL
#'                                     Server, this should specify both the database and the schema,
#'                                     so for example 'cdm_instance.dbo'.
#' @param tempEmulationSchema          Some database platforms like Oracle and Impala do not truly
#'                                     support temp tables. To emulate temp tables, provide a schema
#'                                     with write privileges where temp tables can be created.
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
#' @param exposureDatabaseSchema       The name of the database schema that is the location where the
#'                                     exposure data used to define the exposure cohorts is available.
#' @param exposureTable                The tablename that contains the exposure cohorts. If
#'                                     exposureTable <> DRUG_ERA, then expectation is `exposureTable`
#'                                     has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                     COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema        The name of the database schema that is the location where the
#'                                     data used to define the outcome cohorts is available.
#' @param outcomeTable                 The tablename that contains the outcome cohorts.  If
#'                                     outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                     outcomeTable has format of COHORT table: COHORT_DEFINITION_ID,
#'                                     SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.
#' @param nestingCohortDatabaseSchema  The name of the database schema that is the location where the
#'                                     data used to define the nesting cohorts is available.
#' @param nestingCohortTable           The tablename that contains the nesting cohorts. Must have
#'                                     the format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                     COHORT_START_DATE, COHORT_END_DATE.
#' @param getDbCohortMethodDataArgs    An object of type `GetDbCohortMethodDataArgs` as created by
#'                                     the [createGetDbCohortMethodDataArgs()] function.
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
                                  exposureDatabaseSchema = cdmDatabaseSchema,
                                  exposureTable = "drug_era",
                                  outcomeDatabaseSchema = cdmDatabaseSchema,
                                  outcomeTable = "condition_occurrence",
                                  nestingCohortDatabaseSchema = cdmDatabaseSchema,
                                  nestingCohortTable = "cohort",
                                  getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs()) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(targetId, add = errorMessages)
  checkmate::assertNumeric(comparatorId, add = errorMessages)
  checkmate::assertNumeric(outcomeIds, add = errorMessages)
  checkmate::assertTRUE(all(c(targetId, comparatorId, outcomeIds) %% 1 == 0), add = errorMessages)
  checkmate::assertCharacter(exposureDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(exposureTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(outcomeDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(outcomeTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(nestingCohortDatabaseSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(nestingCohortTable, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertR6(getDbCohortMethodDataArgs, "GetDbCohortMethodDataArgs", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  useNestingCohort <- !is.null(getDbCohortMethodDataArgs$nestingCohortId)

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  message("Constructing target and comparator cohorts")
  renderedSql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "CreateOrCountCohorts.sql",
    packageName = "CohortMethod",
    dbms = connectionDetails$dbms,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema,
    exposure_database_schema = exposureDatabaseSchema,
    exposure_table = exposureTable,
    target_id = targetId,
    comparator_id = comparatorId,
    study_start_date = getDbCohortMethodDataArgs$studyStartDate,
    study_end_date = getDbCohortMethodDataArgs$studyEndDate,
    first_only = getDbCohortMethodDataArgs$firstExposureOnly,
    remove_duplicate_subjects = getDbCohortMethodDataArgs$removeDuplicateSubjects,
    washout_period = getDbCohortMethodDataArgs$washoutPeriod,
    restrict_to_common_period = getDbCohortMethodDataArgs$restrictToCommonPeriod,
    min_age = getDbCohortMethodDataArgs$minAge,
    max_age = getDbCohortMethodDataArgs$maxAge,
    gender_concept_ids = getDbCohortMethodDataArgs$genderConceptIds,
    use_nesting_cohort = useNestingCohort,
    nesting_cohort_database_schema = nestingCohortDatabaseSchema,
    nesting_cohort_table = nestingCohortTable,
    nesting_cohort_id = getDbCohortMethodDataArgs$nestingCohortId,
    action = "CREATE"
  )
  DatabaseConnector::executeSql(connection, renderedSql)

  renderedSql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "CreateOrCountCohorts.sql",
    packageName = "CohortMethod",
    dbms = connectionDetails$dbms,
    tempEmulationSchema = tempEmulationSchema,
    cdm_database_schema = cdmDatabaseSchema,
    exposure_database_schema = exposureDatabaseSchema,
    exposure_table = exposureTable,
    target_id = targetId,
    comparator_id = comparatorId,
    study_start_date = getDbCohortMethodDataArgs$studyStartDate,
    study_end_date = getDbCohortMethodDataArgs$studyEndDate,
    first_only = getDbCohortMethodDataArgs$firstExposureOnly,
    remove_duplicate_subjects = getDbCohortMethodDataArgs$removeDuplicateSubjects,
    washout_period = getDbCohortMethodDataArgs$washoutPeriod,
    restrict_to_common_period = getDbCohortMethodDataArgs$restrictToCommonPeriod,
    min_age = getDbCohortMethodDataArgs$minAge,
    max_age = getDbCohortMethodDataArgs$maxAge,
    gender_concept_ids = getDbCohortMethodDataArgs$genderConceptIds,
    use_nesting_cohort = useNestingCohort,
    nesting_cohort_database_schema = nestingCohortDatabaseSchema,
    nesting_cohort_table = nestingCohortTable,
    nesting_cohort_id = getDbCohortMethodDataArgs$nestingCohortId,
    action = "COUNT"
  )
  attrition <- DatabaseConnector::querySql(connection, renderedSql, snakeCaseToCamelCase = TRUE)
  attrition <- pivotAttrition(attrition, targetId)

  sampled <- downSample(
    attrition,
    connection = connection,
    tempEmulationSchema = tempEmulationSchema,
    targetId = targetId,
    maxCohortSize = getDbCohortMethodDataArgs$maxCohortSize
  )
  if (sampled) {
    cohortTable <- "#cohort_sample"
  } else {
    cohortTable <- "#cohort_person"
  }

  message("Fetching cohorts from server")
  start <- Sys.time()
  cohortSql <- SqlRender::loadRenderTranslateSql(
    sqlFilename = "GetCohorts.sql",
    packageName = "CohortMethod",
    dbms = connectionDetails$dbms,
    tempEmulationSchema = tempEmulationSchema,
    target_id = targetId,
    cohortTable = cohortTable
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
  if (sampled) {
    attrition <- attrition |>
      bind_rows(getCounts(cohorts, "Random sample"))
  }
  metaData <- list(
    targetId = targetId,
    comparatorId = comparatorId,
    nestingCohortId = getDbCohortMethodDataArgs$nestingCohortId,
    studyStartDate = getDbCohortMethodDataArgs$studyStartDate,
    studyEndDate = getDbCohortMethodDataArgs$studyEndDate,
    attrition = attrition
  )
  delta <- Sys.time() - start
  message("Fetching cohorts took ", signif(delta, 3), " ", attr(delta, "units"))

  covariateSettings <- handleCohortCovariateBuilders(
    covariateSettings = getDbCohortMethodDataArgs$covariateSettings,
    exposureDatabaseSchema = exposureDatabaseSchema,
    exposureTable = exposureTable
  )
  covariateData <- FeatureExtraction::getDbCovariateData(
    connection = connection,
    tempEmulationSchema = tempEmulationSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortTableIsTemp = TRUE,
    rowIdField = "row_id",
    covariateSettings = getDbCohortMethodDataArgs$covariateSettings
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

downSample <- function(attrition,
                       connection,
                       tempEmulationSchema,
                       targetId,
                       maxCohortSize) {
  if (maxCohortSize == 0) {
    return(FALSE)
  } else {
    sampled <- FALSE
    preSampleCounts <- attrition |>
      tail(1)
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
    return(sampled)
  }
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

pivotAttrition <- function(attrition, targetId) {
  attrition <- inner_join(
    attrition |>
      filter(.data$cohortDefinitionId == targetId) |>
      select(
        description = "description",
        targetPersons = "persons",
        targetExposures = "exposures",
        "seqId"
      ),
    attrition |>
      filter(.data$cohortDefinitionId != targetId) |>
      select(
        comparatorPersons = "persons",
        comparatorExposures = "exposures",
        "seqId"
      ),
    by = join_by("seqId")
  ) |>
    select("description", "targetPersons", "comparatorPersons", "targetExposures", "comparatorExposures")
  return(attrition)
}

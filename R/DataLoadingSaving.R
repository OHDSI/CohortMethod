# @file DataLoadingSaving.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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
#' @description
#' This function executes a large set of SQL statements against the database in OMOP CDM format to
#' extract the data needed to perform the analysis.
#'
#' @details
#' Based on the arguments, the treatment and comparator cohorts are retrieved, as well as outcomes
#' occurring in exposed subjects. The treatment and comparator cohorts can be identified using the
#' drug_era table, or through user-defined cohorts in a cohort table either inside the CDM instance or
#' in a separate schema. Similarly, outcomes are identified using the condition_era table or through
#' user-defined cohorts in a cohort table either inside the CDM instance or in a separate schema.
#' Covariates are automatically extracted from the appropriate tables within the CDM. Important: The
#' target and comparator drug must not be included in the covariates, including any descendant
#' concepts. If the \code{targetId} and \code{comparatorId} arguments represent real concept IDs, you
#' can set the \code{excludeDrugsFromCovariates} argument to TRUE and automatically the drugs and
#' their descendants will be excluded from the covariates. However, if the \code{targetId} and
#' \code{comparatorId} arguments do not represent concept IDs, you will need to manually add the drugs
#' and descendants to the \code{excludedCovariateConceptIds} of the \code{covariateSettings} argument.
#'
#' The \code{removeduplicateSubjects} argument can have one of the following values:
#' \describe{
#'   \item{"keep all"}{Do not remove subjects that appear in both target and comparator cohort}
#'   \item{"keep first"}{When a subjects appear in both target and comparator cohort, only keep whichever cohort is first in time.}
#'   \item{"remove all"}{Remove subjects that appear in both target and comparator cohort completely from the analysis."}
#' }
#'
#' @param connectionDetails            An R object of type\cr\code{connectionDetails} created using the
#'                                     function \code{createConnectionDetails} in the
#'                                     \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema            The name of the database schema that contains the OMOP CDM
#'                                     instance.  Requires read permissions to this database. On SQL
#'                                     Server, this should specifiy both the database and the schema,
#'                                     so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema             For Oracle only: the name of the database schema where you want
#'                                     all temporary tables to be managed. Requires create/insert
#'                                     permissions to this database.
#' @param targetId                     A unique identifier to define the target cohort.  If
#'                                     exposureTable = DRUG_ERA, targetId is a CONCEPT_ID and all
#'                                     descendant concepts within that CONCEPT_ID will be used to
#'                                     define the cohort.  If exposureTable <> DRUG_ERA, targetId is
#'                                     used to select the cohort_concept_id in the cohort-like table.
#' @param comparatorId                 A unique identifier to define the comparator cohort.  If
#'                                     exposureTable = DRUG_ERA, comparatorId is a CONCEPT_ID and all
#'                                     descendant concepts within that CONCEPT_ID will be used to
#'                                     define the cohort.  If exposureTable <> DRUG_ERA, comparatorId
#'                                     is used to select the cohort_concept_id in the cohort-like
#'                                     table.
#' @param outcomeIds                   A list of cohort_definition_ids used to define outcomes.
#' @param studyStartDate               A calendar date specifying the minimum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate                 A calendar date specifying the maximum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'. Important: the study
#'                                     end data is also used to truncate risk windows, meaning no
#'                                     outcomes beyond the study end date will be considered.
#' @param exposureDatabaseSchema       The name of the database schema that is the location where the
#'                                     exposure data used to define the exposure cohorts is available.
#'                                     If exposureTable = DRUG_ERA, exposureDatabaseSchema is not used
#'                                     by assumed to be cdmSchema.  Requires read permissions to this
#'                                     database.
#' @param exposureTable                The tablename that contains the exposure cohorts.  If
#'                                     exposureTable <> DRUG_ERA, then expectation is exposureTable has
#'                                     format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                                     COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema        The name of the database schema that is the location where the
#'                                     data used to define the outcome cohorts is available. If
#'                                     exposureTable = CONDITION_ERA, exposureDatabaseSchema is not
#'                                     used by assumed to be cdmSchema.  Requires read permissions to
#'                                     this database.
#' @param outcomeTable                 The tablename that contains the outcome cohorts.  If
#'                                     outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                     outcomeTable has format of COHORT table: COHORT_DEFINITION_ID,
#'                                     SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.
#' @param cdmVersion                   Define the OMOP CDM version used: currently support "4" and "5".
#' @param excludeDrugsFromCovariates   Should the target and comparator drugs (and their descendant
#'                                     concepts) be excluded from the covariates? Note that this will
#'                                     work if the drugs are actualy drug concept IDs (and not cohort
#'                                     IDs).
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note
#'                                     that this is typically done in the \code{createStudyPopulation}
#'                                     function, but can already be done here for efficiency reasons.
#' @param removeDuplicateSubjects      Remove subjects that are in both the target and comparator
#'                                     cohort? See details for allowed values.N ote that this is typically done in the
#'                                     \code{createStudyPopulation} function, but can already be done
#'                                     here for efficiency reasons.
#' @param restrictToCommonPeriod       Restrict the analysis to the period when both treatments are observed?
#' @param washoutPeriod                The mininum required continuous observation time prior to index
#'                                     date for a person to be included in the cohort. Note that this
#'                                     is typically done in the \code{createStudyPopulation} function,
#'                                     but can already be done here for efficiency reasons.
#' @param maxCohortSize                If either the target or the comparator cohort is larger than
#'                                     this number it will be sampled to this size. \code{maxCohortSize = 0}
#'                                     indicates no maximum size.
#' @param covariateSettings            An object of type \code{covariateSettings} as created using the
#'                                     \code{createCovariateSettings} function in the
#'                                     \code{FeatureExtraction} package.
#'
#' @return
#' Returns an object of type \code{cohortMethodData}, containing information on the cohorts, their
#' outcomes, and baseline covariates. Information about multiple outcomes can be captured at once for
#' efficiency reasons. This object is a list with the following components: \describe{
#' \item{outcomes}{A data frame listing the outcomes per person, including the time to event, and the
#' outcome id. Outcomes are not yet filtered based on risk window, since this is done at a later
#' stage.} \item{cohorts}{A data frame listing the persons in each cohort, listing their exposure
#' status as well as the time to the end of the observation period and time to the end of the cohort
#' (usually the end of the exposure era).} \item{covariates}{An ffdf object listing the baseline
#' covariates per person in the two cohorts. This is done using a sparse representation: covariates
#' with a value of 0 are omitted to save space.} \item{covariateRef}{An ffdf object describing the
#' covariates that have been extracted.} \item{metaData}{A list of objects with information on how the
#' cohortMethodData object was constructed.} } The generic \code{print()} and \code{summary()}
#' functions have been implemented for this object.
#'
#' @export
getDbCohortMethodData <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  oracleTempSchema = cdmDatabaseSchema,
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
                                  excludeDrugsFromCovariates = TRUE,
                                  firstExposureOnly = FALSE,
                                  removeDuplicateSubjects = FALSE,
                                  restrictToCommonPeriod = FALSE,
                                  washoutPeriod = 0,
                                  maxCohortSize = 0,
                                  covariateSettings) {
  if (is.null(studyStartDate)) {
    studyStartDate <- ""
  }
  if (is.null(studyEndDate)) {
    studyEndDate <- ""
  }
  if (studyStartDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyStartDate) == -1)
    stop("Study start date must have format YYYYMMDD")
  if (studyEndDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyEndDate) == -1)
    stop("Study end date must have format YYYYMMDD")
  if (is.logical(removeDuplicateSubjects)) {
    if (removeDuplicateSubjects)
      removeDuplicateSubjects <- "remove all"
    else
      removeDuplicateSubjects <- "keep all"
  }
  if (!(removeDuplicateSubjects %in% c("keep all", "keep first", "remove all")))
    stop("removeDuplicateSubjects should have value \"keep all\", \"keep first\", or \"remove all\".")
  ParallelLogger::logTrace("Getting cohort method data for target ID ", targetId, " and comparator ID ", comparatorId)

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  if (excludeDrugsFromCovariates) {
    if (exposureTable != "drug_era")
      warning("Removing drugs from covariates, but not sure if exposure IDs are valid drug concepts")
    sql <- "SELECT descendant_concept_id FROM @cdm_database_schema.concept_ancestor WHERE ancestor_concept_id IN (@target_id, @comparator_id)"
    sql <- SqlRender::renderSql(sql,
                                cdm_database_schema = cdmDatabaseSchema,
                                target_id = targetId,
                                comparator_id = comparatorId)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    conceptIds <- DatabaseConnector::querySql(connection, sql)
    names(conceptIds) <- SqlRender::snakeCaseToCamelCase(names(conceptIds))
    conceptIds <- conceptIds$descendantConceptId
    ParallelLogger::logDebug("Excluding concept Ids from covariates: ", paste(conceptIds, collapse = ", "))
    if (is(covariateSettings, "covariateSettings")) {
      covariateSettings$excludedCovariateConceptIds <- c(covariateSettings$excludedCovariateConceptIds,
                                                         conceptIds)
    } else if (is.list(covariateSettings)) {
      for (i in 1:length(covariateSettings)) {
        covariateSettings[[i]]$excludedCovariateConceptIds <- c(covariateSettings[[i]]$excludedCovariateConceptIds,
                                                                conceptIds)
      }
    }
  }

  ParallelLogger::logInfo("\nConstructing target and comparator cohorts")
  renderedSql <- SqlRender::loadRenderTranslateSql("CreateCohorts.sql",
                                                   packageName = "CohortMethod",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database_schema = cdmDatabaseSchema,
                                                   exposure_database_schema = exposureDatabaseSchema,
                                                   exposure_table = exposureTable,
                                                   cdm_version = cdmVersion,
                                                   target_id = targetId,
                                                   comparator_id = comparatorId,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   first_only = firstExposureOnly,
                                                   remove_duplicate_subjects = removeDuplicateSubjects,
                                                   washout_period = washoutPeriod,
                                                   restrict_to_common_period = restrictToCommonPeriod)
  DatabaseConnector::executeSql(connection, renderedSql)

  sampled <- FALSE
  if (maxCohortSize != 0) {
    renderedSql <- SqlRender::loadRenderTranslateSql("CountCohorts.sql",
                                                     packageName = "CohortMethod",
                                                     dbms = connectionDetails$dbms,
                                                     oracleTempSchema = oracleTempSchema,
                                                     cdm_version = cdmVersion,
                                                     target_id = targetId)
    counts <- DatabaseConnector::querySql(connection, renderedSql)
    colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))
    ParallelLogger::logDebug("Pre-sample total row count is ", sum(counts$rowCount))
    preSampleCounts <- data.frame(dummy = 0)
    idx <- which(counts$treatment == 1)
    if (length(idx) == 0) {
      preSampleCounts$targetPersons = 0
      preSampleCounts$targetExposures = 0
    } else {
      preSampleCounts$targetPersons = counts$personCount[idx]
      preSampleCounts$targetExposures = counts$rowCount[idx]
    }
    idx <- which(counts$treatment == 0)
    if (length(idx) == 0) {
      preSampleCounts$comparatorPersons = 0
      preSampleCounts$comparatorExposures = 0
    } else {
      preSampleCounts$comparatorPersons = counts$personCount[idx]
      preSampleCounts$comparatorExposures = counts$rowCount[idx]
    }
    preSampleCounts$dummy <- NULL
    if (preSampleCounts$targetExposures > maxCohortSize) {
      ParallelLogger::logInfo(paste0("Downsampling target cohort from ", preSampleCounts$targetExposures, " to ", maxCohortSize))
      sampled <- TRUE
    }
    if (preSampleCounts$comparatorExposures > maxCohortSize) {
      ParallelLogger::logInfo(paste0("Downsampling comparator cohort from ", preSampleCounts$comparatorExposures, " to ", maxCohortSize))
      sampled <- TRUE
    }
    if (sampled) {
      renderedSql <- SqlRender::loadRenderTranslateSql("SampleCohorts.sql",
                                                       packageName = "CohortMethod",
                                                       dbms = connectionDetails$dbms,
                                                       oracleTempSchema = oracleTempSchema,
                                                       cdm_version = cdmVersion,
                                                       max_cohort_size = maxCohortSize)
      DatabaseConnector::executeSql(connection, renderedSql)
    }
  }

  ParallelLogger::logInfo("Fetching cohorts from server")
  start <- Sys.time()
  cohortSql <- SqlRender::loadRenderTranslateSql("GetCohorts.sql",
                                                 packageName = "CohortMethod",
                                                 dbms = connectionDetails$dbms,
                                                 oracleTempSchema = oracleTempSchema,
                                                 cdm_version = cdmVersion,
                                                 target_id = targetId,
                                                 sampled = sampled)
  cohorts <- DatabaseConnector::querySql(connection, cohortSql)
  colnames(cohorts) <- SqlRender::snakeCaseToCamelCase(colnames(cohorts))
  ParallelLogger::logDebug("Fetched cohort total rows in target is ", sum(cohorts$treatment), ", total rows in comparator is ", sum(!cohorts$treatment))
  if (nrow(cohorts) == 0) {
    warning("Target and comparator cohorts are empty")
  } else if (sum(cohorts$treatment == 1) == 0) {
    warning("Target cohort is empty")
  } else if (sum(cohorts$treatment == 0) == 0) {
    warning("Comparator cohort is empty")
  }
  metaData <- list(targetId = targetId,
                   comparatorId = comparatorId,
                   studyStartDate = studyStartDate,
                   studyEndDate = studyEndDate)
  if (firstExposureOnly || removeDuplicateSubjects != "keep all" || washoutPeriod != 0) {
    rawCountSql <- SqlRender::loadRenderTranslateSql("CountOverallExposedPopulation.sql",
                                                     packageName = "CohortMethod",
                                                     dbms = connectionDetails$dbms,
                                                     oracleTempSchema = oracleTempSchema,
                                                     cdm_database_schema = cdmDatabaseSchema,
                                                     exposure_database_schema = exposureDatabaseSchema,
                                                     exposure_table = tolower(exposureTable),
                                                     cdm_version = cdmVersion,
                                                     target_id = targetId,
                                                     comparator_id = comparatorId,
                                                     study_start_date = studyStartDate,
                                                     study_end_date = studyEndDate)
    rawCount <- DatabaseConnector::querySql(connection, rawCountSql)
    colnames(rawCount) <- SqlRender::snakeCaseToCamelCase(colnames(rawCount))
    if (nrow(rawCount) == 0) {
      counts <- data.frame(description = "Original cohorts",
                           targetPersons = 0,
                           comparatorPersons = 0,
                           targetExposures = 0,
                           comparatorExposures = 0)
    } else {
      counts <- data.frame(description = "Original cohorts",
                           targetPersons = rawCount$exposedCount[rawCount$treatment ==  1],
                           comparatorPersons = rawCount$exposedCount[rawCount$treatment == 0],
                           targetExposures = rawCount$exposureCount[rawCount$treatment == 1],
                           comparatorExposures = rawCount$exposureCount[rawCount$treatment == 0])
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
      metaData$attrition <- rbind(metaData$attrition, preSampleCounts)
      metaData$attrition <- rbind(metaData$attrition, getCounts(cohorts, "Random sample"))
    } else {
      metaData$attrition <- rbind(metaData$attrition, getCounts(cohorts, label))
    }
  } else {
    if (sampled) {
      preSampleCounts$description <- "Original cohorts"
      metaData$attrition <- preSampleCounts
      metaData$attrition <- rbind(metaData$attrition, getCounts(cohorts, "Random sample"))
    } else {
      metaData$attrition <- getCounts(cohorts, "Original cohorts")
    }
  }
  attr(cohorts, "metaData") <- metaData

  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Fetching cohorts took", signif(delta, 3), attr(delta, "units")))
  if (sampled) {
    cohortTable <- "#cohort_sample"
  } else {
    cohortTable <- "#cohort_person"
  }
  covariateData <- FeatureExtraction::getDbCovariateData(connection = connection,
                                                         oracleTempSchema = oracleTempSchema,
                                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                                         cdmVersion = cdmVersion,
                                                         cohortTable = cohortTable,
                                                         cohortTableIsTemp = TRUE,
                                                         rowIdField = "row_id",
                                                         covariateSettings = covariateSettings)
  ParallelLogger::logDebug("Fetched covariates total count is ", nrow(covariateData$covariates))
  ParallelLogger::logInfo("Fetching outcomes from server")
  start <- Sys.time()
  outcomeSql <- SqlRender::loadRenderTranslateSql("GetOutcomes.sql",
                                                  packageName = "CohortMethod",
                                                  dbms = connectionDetails$dbms,
                                                  oracleTempSchema = oracleTempSchema,
                                                  cdm_database_schema = cdmDatabaseSchema,
                                                  outcome_database_schema = outcomeDatabaseSchema,
                                                  outcome_table = outcomeTable,
                                                  outcome_ids = outcomeIds,
                                                  cdm_version = cdmVersion,
                                                  sampled = sampled)
  outcomes <- DatabaseConnector::querySql(connection, outcomeSql)
  colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
  metaData <- data.frame(outcomeIds = outcomeIds)
  attr(outcomes, "metaData") <- metaData
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Fetching outcomes took", signif(delta, 3), attr(delta, "units")))
  ParallelLogger::logDebug("Fetched outcomes total count is ", nrow(outcomes))

  # Remove temp tables:
  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveCohortTempTables.sql",
                                                   packageName = "CohortMethod",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   sampled = sampled)
  DatabaseConnector::executeSql(connection,
                                renderedSql,
                                progressBar = FALSE,
                                reportOverallTime = FALSE)

  metaData <- covariateData$metaData
  metaData$call <- match.call()

  result <- list(cohorts = cohorts,
                 outcomes = outcomes,
                 covariates = covariateData$covariates,
                 covariateRef = covariateData$covariateRef,
                 analysisRef = covariateData$analysisRef,
                 metaData = metaData)

  class(result) <- "cohortMethodData"
  return(result)
}

#' Save the cohort data to folder
#'
#' @description
#' \code{saveCohortMethodData} saves an object of type cohortMethodData to folder.
#'
#' @param cohortMethodData   An object of type \code{cohortMethodData} as generated using
#'                           \code{getDbCohortMethodData}.
#' @param file               The name of the folder where the data will be written. The folder should
#'                           not yet exist.
#' @param compress           Should compression be used when saving?
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @export
saveCohortMethodData <- function(cohortMethodData, file, compress = FALSE) {
  if (missing(cohortMethodData))
    stop("Must specify cohortMethodData")
  if (missing(file))
    stop("Must specify file")
  if (class(cohortMethodData) != "cohortMethodData")
    stop("Data not of class cohortMethodData")
  ParallelLogger::logTrace("Saving CohortMethodData to ", file)

  covariates <- cohortMethodData$covariates
  covariateRef <- cohortMethodData$covariateRef
  analysisRef <- cohortMethodData$analysisRef
  if (is.data.frame(covariates)) {
    dir.create(file, recursive = TRUE)
    saveRDS(covariates, file = file.path(file, "covariates.rds"))
    saveRDS(covariateRef, file = file.path(file, "covariateRef.rds"))
    saveRDS(analysisRef, file = file.path(file, "analysisRef.rds"))
  } else {
    if (compress) {
      saveCompressedFfdf(covariates, file.path(file, "covariates"))
      saveCompressedFfdf(covariateRef, file.path(file, "covariateRef"))
      saveCompressedFfdf(covariateRef, file.path(file, "analysisRef"))
    } else {
      ffbase::save.ffdf(covariates, covariateRef, analysisRef, dir = file, clone = TRUE)
    }
  }
  saveRDS(cohortMethodData$cohorts, file = file.path(file, "cohorts.rds"))
  saveRDS(cohortMethodData$outcomes, file = file.path(file, "outcomes.rds"))
  saveRDS(cohortMethodData$metaData, file = file.path(file, "metaData.rds"))
}

#' Load the cohort data from a folder
#'
#' @description
#' \code{loadCohortMethodData} loads an object of type cohortMethodData from a folder in the file
#' system.
#'
#' @param file           The name of the folder containing the data.
#' @param readOnly       If true, the data is opened read only.
#' @param skipCovariates Do not load the covariates. Can save a lot of time.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class cohortMethodData.
#'
#' @examples
#' # todo
#'
#' @export
loadCohortMethodData <- function(file, readOnly = TRUE, skipCovariates = FALSE) {
  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))
  ParallelLogger::logTrace("Loading CohortMethodData from ", file, if (skipCovariates) ", skipping covariates" else "")
  if (skipCovariates) {
    covariates <- NULL
    covariateRef <- NULL
    analysisRef <- NULL
  } else if (file.exists(file.path(file, "covariates.rds")) && !file.exists(file.path(file, "covariates.zip"))) {
    ParallelLogger::logDebug("Covariates are empty data frame")
    covariates <- readRDS(file.path(file, "covariates.rds"))
    covariateRef <- readRDS(file.path(file, "covariateRef.rds"))
    if (file.exists(file.path(file, "analysisRef.rds"))) {
      analysisRef <- readRDS(file.path(file, "analysisRef.rds"))
    } else {
      analysisRef <- NULL
    }
  } else {
    temp <- setwd(file)
    absolutePath <- setwd(temp)
    if (file.exists(file.path(absolutePath, "covariates.zip"))) {
      covariates <- loadCompressedFfdf(file.path(absolutePath, "covariates"))
      covariateRef <- loadCompressedFfdf(file.path(absolutePath, "covariateRef"))
      analysisRef <- loadCompressedFfdf(file.path(absolutePath, "analysisRef"))
    } else {
      e <- new.env()
      ffbase::load.ffdf(absolutePath, e)
      covariates = get("covariates", envir = e)
      covariateRef = get("covariateRef", envir = e)
      open(covariates, readonly = readOnly)
      open(covariateRef, readonly = readOnly)
      if (exists("analysisRef", envir = e)) {
        analysisRef <- get("analysisRef", envir = e)
        open(analysisRef, readonly = readOnly)
      } else {
        analysisRef <- NULL
      }
      rm(e)
    }
  }
  result <- list(covariates = covariates,
                 covariateRef = covariateRef,
                 analysisRef = analysisRef,
                 cohorts = readRDS(file.path(file, "cohorts.rds")),
                 outcomes = readRDS(file.path(file, "outcomes.rds")),
                 metaData = readRDS(file.path(file, "metaData.rds")))

  # For backwards compatability with version < 3.0.0:
  metaData <- attr(result$cohorts, "metaData")
  if (!is.null(metaData$attrition$treatedPersons)) {
    colnames(metaData$attrition)[colnames(metaData$attrition) == "treatedPersons"] <- "targetPersons"
    colnames(metaData$attrition)[colnames(metaData$attrition) == "treatedExposures"] <- "targetExposures"
    attr(result$cohorts, "metaData") <- metaData
  }
  class(result) <- "cohortMethodData"
  return(result)
}

#' @export
print.cohortMethodData <- function(x, ...) {
  writeLines("CohortMethodData object")
  writeLines("")
  writeLines(paste("Treatment concept ID:", attr(x$cohorts, "metaData")$targetId))
  writeLines(paste("Comparator concept ID:", attr(x$cohorts, "metaData")$comparatorId))
  writeLines(paste("Outcome concept ID(s):",
                   paste(attr(x$outcomes, "metaData")$outcomeIds, collapse = ",")))
}

#' @export
summary.cohortMethodData <- function(object, ...) {
  targetPersons <- length(unique(object$cohorts$subjectId[object$cohorts$treatment == 1]))
  comparatorPersons <- length(unique(object$cohorts$subjectId[object$cohorts$treatment == 0]))
  outcomeCounts <- data.frame(outcomeId = attr(object$outcomes, "metaData")$outcomeIds,
                              eventCount = 0,
                              personCount = 0)
  for (i in 1:nrow(outcomeCounts)) {
    outcomeCounts$eventCount[i] <- sum(object$outcomes$outcomeId == attr(object$outcomes,
                                                                         "metaData")$outcomeIds[i])
    outcomeCounts$personCount[i] <- length(unique(object$outcomes$rowId[object$outcomes$outcomeId ==
                                                                          attr(object$outcomes, "metaData")$outcomeIds[i]]))
  }
  result <- list(metaData = append(append(object$metaData, attr(object$cohorts, "metaData")),
                                   attr(object$outcomes, "metaData")),
                 targetPersons = targetPersons,
                 comparatorPersons = comparatorPersons,
                 outcomeCounts = outcomeCounts,
                 covariateCount = nrow(object$covariateRef),
                 covariateValueCount = nrow(object$covariates))
  class(result) <- "summary.cohortMethodData"
  return(result)
}

#' @export
print.summary.cohortMethodData <- function(x, ...) {
  writeLines("CohortMethodData object summary")
  writeLines("")
  writeLines(paste("Treatment concept ID:", x$metaData$targetId))
  writeLines(paste("Comparator concept ID:", x$metaData$comparatorId))
  writeLines(paste("Outcome concept ID(s):", x$metaData$outcomeIds, collapse = ","))
  writeLines("")
  writeLines(paste("Treated persons:", paste(x$targetPersons)))
  writeLines(paste("Comparator persons:", paste(x$comparatorPersons)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- x$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeId
  outcomeCounts$outcomeId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Person count")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Covariates:")
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:", x$covariateValueCount))
}

#' Extract covariate names
#'
#' @description
#' Extracts covariate names using a regular-expression.
#'
#' @details
#' This function extracts covariate names that match a regular-expression for a
#' \code{cohortMethodData} or \code{covariateData} object.
#'
#' @param object    An R object of type \code{cohortMethodData} or \code{covariateData}.
#' @param pattern   A regular expression with which to name covariate names
#'
#' @return
#' Returns a \code{data.frame} containing information about covariates that match a regular
#' expression.  This \code{data.frame} has the following columns: \describe{
#' \item{covariateId}{Numerical identifier for use in model fitting using these covariates}
#' \item{covariateName}{Text identifier} \item{analysisId}{Analysis identifier} \item{conceptId}{OMOP
#' common data model concept identifier, or 0} }
#'
#' @export
grepCovariateNames <- function(pattern, object) {
  if (is.null(object$covariateRef)) {
    stop("object does not contain a covariateRef")
  }
  select <- ffbase::ffwhich(object$covariateRef, grepl(pattern, covariateName))
  if (is.null(select)) {
    data.frame(covariateId = numeric(0),
               covariateName = character(0),
               analysisID = numeric(0),
               conceptId = numeric(0))
  } else {
    ff::as.ram(object$covariateRef[select, ])
  }
}

#' Insert a population into a database
#'
#' @details
#' Inserts a population table into a database. The table in the database will have the same structure
#' as the 'cohort' table in the Common Data Model.
#'
#' @param population             Either an object of type \code{cohortMethodData} or a population
#'                               object generated by functions like \code{createStudyPopulation}.
#' @param cohortIds              The IDs to be used for the target and comparator cohort,
#'                               respectively.
#' @param connectionDetails      An R object of type\cr\code{connectionDetails} created using the
#'                               function \code{createConnectionDetails} in the
#'                               \code{DatabaseConnector} package.
#' @param cohortDatabaseSchema   The name of the database schema where the data will be written.
#'                               Requires write permissions to this database. On SQL Server, this
#'                               should specifiy both the database and the schema, so for example
#'                               'cdm_instance.dbo'.
#' @param cohortTable            The name of the table in the database schema where the data will be
#'                               written.
#' @param createTable            Should a new table be created? If not, the data will be inserted into
#'                               an existing table.
#' @param dropTableIfExists      If \code{createTable = TRUE} and the table already exists it will be
#'                               overwritten.
#' @param cdmVersion             Define the OMOP CDM version used: currently support "4" and "5".
#'
#' @export
insertDbPopulation <- function(population,
                               cohortIds = c(1, 0),
                               connectionDetails,
                               cohortDatabaseSchema,
                               cohortTable = "cohort",
                               createTable = FALSE,
                               dropTableIfExists = TRUE,
                               cdmVersion = "5") {
  if (is(population, "cohortMethodData")) {
    population <- population$cohorts
  }
  newCohortIds <- plyr::mapvalues(population$treatment, c(1, 0), cohortIds)
  population <- population[, c("subjectId", "cohortStartDate")]
  if (cdmVersion == "4") {
    population$cohortConceptId <- newCohortIds
  } else {
    population$cohortDefinitionId <- newCohortIds
  }
  population$cohortEndDate <- NA
  colnames(population) <- SqlRender::camelCaseToSnakeCase(colnames(population))
  connection <- DatabaseConnector::connect(connectionDetails)
  ParallelLogger::logInfo(paste("Writing",
                             nrow(population),
                             "rows to",
                             paste(cohortDatabaseSchema, cohortTable, sep = ".")))
  start <- Sys.time()
  if (!createTable) {
    if (cdmVersion == "4") {
      sql <- "DELETE FROM @table WHERE cohort_concept_id IN (@cohort_ids);"
    } else {
      sql <- "DELETE FROM @table WHERE cohort_definition_id IN (@cohort_ids);"
    }
    sql <- SqlRender::renderSql(sql,
                                table = paste(cohortDatabaseSchema, cohortTable, sep = "."),
                                cohort_ids = cohortIds)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(connection = connection,
                                  sql = sql,
                                  progressBar = FALSE,
                                  reportOverallTime = FALSE)
  }
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = paste(cohortDatabaseSchema, cohortTable, sep = "."),
                                 data = population,
                                 dropTableIfExists = dropTableIfExists,
                                 createTable = createTable,
                                 tempTable = FALSE,
                                 oracleTempSchema = NULL)
  DatabaseConnector::disconnect(connection)
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Inserting rows took", signif(delta, 3), attr(delta, "units")))
  invisible(TRUE)
}


saveCompressedFfdf <- function(ffdf, fileName) {
  dir.create(dirname(fileName), showWarnings = FALSE, recursive = TRUE)
  saveRDS(ffdf, paste0(fileName, ".rds"))
  fileNames <- sapply(bit::physical(ffdf), function(x) bit::physical(x)$filename)
  sourceDir <- dirname(fileNames[1])
  oldWd <- setwd(sourceDir)
  on.exit(setwd(oldWd))
  sourceNames <- basename(fileNames)
  ff::close.ffdf(ffdf)
  DatabaseConnector::createZipFile(zipFile = paste0(fileName, ".zip"), files = sourceNames)
  ff::open.ffdf(ffdf)
}

loadCompressedFfdf <- function(fileName) {
  ffdf <- readRDS(paste0(fileName, ".rds"))
  tempRoot <- ff::fftempfile("temp")
  utils::unzip(zipfile = paste0(fileName, ".zip"), exdir = tempRoot)
  for (ff in bit::physical(ffdf)) {
    newFileName <- ff::fftempfile("")
    file.rename(file.path(tempRoot, basename(bit::physical(ff)$filename)), newFileName)
    bit::physical(ff)$filename <- newFileName
    bit::physical(ff)$finalizer <- "delete"
    ff::open.ff(ff)
    reg.finalizer(attr(ff,"physical"), ff::finalize.ff_pointer, onexit = bit::physical(ff)$finonexit)
  }
  unlink(tempRoot, recursive = TRUE)
  return(ffdf)
}

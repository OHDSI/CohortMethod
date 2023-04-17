# Copyright 2023 Observational Health Data Sciences and Informatics
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
getDbCohortMethodData <- function(
    connectionDetails,
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
  # Initiate CohortDbInterface
  cohortDbInterface <- CohortDbInterface$new(
    connectionDetails = connectionDetails,
    tempEmulationSchema = tempEmulationSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    exposureDatabaseSchema = exposureDatabaseSchema,
    exposureTable = exposureTable,
    cdmVersion = cdmVersion,
    outcomeTable = outcomeTable,
    outcomeDatabaseSchema = outcomeDatabaseSchema
  )

  on.exit(cohortDbInterface$disconnect())

  metaData <- list(
    targetId = targetId,
    comparatorId = comparatorId,
    studyStartDate = studyStartDate,
    studyEndDate = studyEndDate
  )

  # Initiate CohortMethodData instance
  cohortMethodDataR6 <- CohortMethodDataR6$new(
    metaData = metaData,
    outcomeIds = outcomeIds,
    firstExposureOnly = firstExposureOnly,
    removeDuplicateSubjects = removeDuplicateSubjects,
    restrictToCommonPeriod = restrictToCommonPeriod,
    washoutPeriod = washoutPeriod,
    maxCohortSize = maxCohortSize,
    covariateSettings = covariateSettings,
    cohortDbInterface = cohortDbInterface
  )

  cohortMethodDataR6$createCohorts()

  if (maxCohortSize != 0) {
    cohortMethodDataR6$downSample()
  }

  start <- Sys.time()
  cohortMethodDataR6$getCohorts()
  delta <- Sys.time() - start
  message(
    "Fetching cohorts took ", signif(delta, 3), " ", attr(delta, "units")
  )

  return(cohortMethodDataR6$buildCovariateData())
}

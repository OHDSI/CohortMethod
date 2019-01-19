# @file HelperFunctions.R
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


#' Build eras
#'
#' @description
#' Constructs eras (continuous periods of exposure or disease).
#'
#' @details
#' This function creates eras from source data. For example, one could use this function to create
#' drug eras based on drug exposures. The function allows drugs to be rolled up to ingredients, and
#' prescriptions to the same ingredient that overlap in time are merged into a single ingredient. Note
#' that stockpiling is not assumed to take place (ie. overlap is discarded), but a grace period can be
#' specified allowing for a small gap between prescriptions when merging. The user can specify the
#' source and target table. These tables are assumed to have the same structure as the cohort table in
#' the Common Data Model (CDM), except when the table names are 'drug_exposure' or
#' 'condition_occurrence' for the source table, or 'drug_era' or 'condition_era' for the target table,
#' in which case the tables are assumed to have the structure defined for those tables in the CDM. If
#' both the source and target table specify a field for type_concept_id, the era construction will
#' partition by the type_concept_id, in other words periods with different type_concept_ids will be
#' treated independently.
#'
#' @param connectionDetails      An R object of type \code{connectionDetails} created using the
#'                               function \code{createConnectionDetails} in the
#'                               \code{DatabaseConnector} package.
#' @param sourceDatabaseSchema   The name of the database schema that contains the source table.
#'                               Requires read permissions to this database. On SQL Server, this should
#'                               specifiy both the database and the schema, so for example
#'                               'cdm_instance.dbo'.
#' @param sourceTable            The name of the source table.
#' @param targetDatabaseSchema   The name of the database schema that contains the target table.
#'                               Requires write permissions to this database. On SQL Server, this
#'                               should specifiy both the database and the schema, so for example
#'                               'cdm_instance.dbo'.
#' @param targetTable            The name of the target table.
#' @param createTargetTable      Should the target table be created? If not, the data is inserted in an
#'                               existing table.
#' @param cdmDatabaseSchema      Only needed when rolling up concepts to ancestors: The name of the
#'                               database schema that contains the vocabulary files.  Requires read
#'                               permissions to this database. On SQL Server, this should specifiy both
#'                               the database and the schema, so for example 'cdm_instance.dbo'.
#' @param gracePeriod            The number of days allowed between periods for them to still be
#'                               considered part of the same era.
#' @param rollUp                 Should concepts be rolled up to their ancestors?
#' @param rollUpConceptClassId   The identifier of the concept class to which concepts should be rolled
#'                               up.
#' @param rollUpVocabularyId     The identifier of the vocabulary to which concepts should be rolled
#'                               up.
#' @param cdmVersion             The verion of the CDM that is being used.
#'
#' @examples
#' \dontrun{
#' # Constructing drug eras in CDM v4:
#' constructEras(connectionDetails,
#'               sourceDatabaseSchema = cdmDatabaseSchema,
#'               sourceTable = "drug_exposure",
#'               targetTable = "drug_era",
#'               createTargetTable = FALSE,
#'               gracePeriod = 30,
#'               rollUpVocabularyId = 8,
#'               rollUpConceptClassId = "Ingredient",
#'               cdmVersion = "4")
#'
#' # Constructing drug eras in CDM v5:
#' constructEras(connectionDetails,
#'               sourceDatabaseSchema = cdmDatabaseSchema,
#'               sourceTable = "drug_exposure",
#'               targetTable = "drug_era",
#'               createTargetTable = FALSE,
#'               gracePeriod = 30,
#'               rollUpVocabularyId = "RxNorm",
#'               rollUpConceptClassId = "Ingredient",
#'               cdmVersion = "5")
#'
#' }
#' @export
constructEras <- function(connectionDetails,
                          sourceDatabaseSchema,
                          sourceTable = "drug_exposure",
                          targetDatabaseSchema = sourceDatabaseSchema,
                          targetTable = "drug_era",
                          createTargetTable = FALSE,
                          cdmDatabaseSchema = sourceDatabaseSchema,
                          gracePeriod = 30,
                          rollUp = TRUE,
                          rollUpConceptClassId = "Ingredient",
                          rollUpVocabularyId = "RxNorm",
                          cdmVersion = "5") {
  if (connectionDetails$dbms == "pdw")
    stop("Currently not supporting Microsoft PDW")
  if (!rollUp)
    rollUpConceptClassId <- ""
  if (sourceTable == "drug_exposure") {
    sourcePersonId <- "person_id"
    sourceStartDate <- "drug_exposure_start_date"
    sourceEndDate <- "drug_exposure_end_date"
    sourceConceptId <- "drug_concept_id"
    sourceTypeConceptId <- "drug_type_concept_id"
  } else if (sourceTable == "condition_occurrence") {
    sourcePersonId <- "person_id"
    sourceStartDate <- "condition_start_date"
    sourceEndDate <- "condition_end_date"
    sourceConceptId <- "condition_concept_id"
    sourceTypeConceptId <- "condition_type_concept_id"
  } else {
    sourcePersonId <- "subject_id"
    sourceStartDate <- "cohort_start_date"
    sourceEndDate <- "cohort_end_date"
    if (cdmVersion == "4")
      sourceConceptId <- "cohort_concept_id" else sourceConceptId <- "cohort_definition_id"
    sourceTypeConceptId <- ""
  }
  if (targetTable == "drug_era") {
    targetId <- "drug_era_id"
    targetPersonId <- "person_id"
    targetStartDate <- "drug_era_start_date"
    targetEndDate <- "drug_era_end_date"
    targetConceptId <- "drug_concept_id"
    if (cdmVersion == "4")
      targetTypeConceptId <- "drug_type_concept_id" else targetTypeConceptId <- ""
    targetCount <- "drug_exposure_count"
  } else if (targetTable == "condition_era") {
    targetId <- "condition_era_id"
    targetPersonId <- "person_id"
    targetStartDate <- "condition_era_start_date"
    targetEndDate <- "condition_era_end_date"
    targetConceptId <- "condition_concept_id"
    if (cdmVersion == "4")
      targetTypeConceptId <- "condition_type_concept_id" else targetTypeConceptId <- ""
    targetCount <- "condition_occurrence_count"
  } else {
    targetId <- ""
    targetPersonId <- "subject_id"
    targetStartDate <- "cohort_start_date"
    targetEndDate <- "cohort_end_date"
    if (cdmVersion == "4")
      targetConceptId <- "cohort_concept_id" else targetConceptId <- "cohort_definition_id"
    targetTypeConceptId <- ""
    targetCount <- ""
  }
  renderedSql <- SqlRender::loadRenderTranslateSql("ConstructEras.sql",
                                                   packageName = "CohortMethod",
                                                   dbms = connectionDetails$dbms,
                                                   grace_period = gracePeriod,
                                                   source_database_schema = sourceDatabaseSchema,
                                                   source_table = sourceTable,
                                                   source_person_id = sourcePersonId,
                                                   source_start_date = sourceStartDate,
                                                   source_end_date = sourceEndDate,
                                                   source_concept_id = sourceConceptId,
                                                   source_type_concept_id = sourceTypeConceptId,
                                                   target_database_schema = targetDatabaseSchema,
                                                   target_table = targetTable,
                                                   target_id = targetId,
                                                   target_person_id = targetPersonId,
                                                   target_start_date = targetStartDate,
                                                   target_end_date = targetEndDate,
                                                   target_concept_id = targetConceptId,
                                                   target_type_concept_id = targetTypeConceptId,
                                                   target_count = targetCount,
                                                   create_target_table = createTargetTable,
                                                   cdm_database_schema = cdmDatabaseSchema,
                                                   roll_up_concept_class_id = rollUpConceptClassId,
                                                   roll_up_vocabulary_id = rollUpVocabularyId,
                                                   cdm_version = cdmVersion)
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))

  ParallelLogger::logInfo("Executing one large query. This could take a while")
  DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE)
  ParallelLogger::logInfo("Done")
  return()
}

#' Check is CohortMethod and its dependencies are correctly installed
#'
#' @details
#' This function checks whether CohortMethod and its dependencies are correctly installed. This will
#' check the database connectivity, large scale regresion engine (Cyclops), and large data object
#' handling (ff).
#'
#' @param connectionDetails   An R object of type\cr\code{connectionDetails} created using the function
#'                            \code{createConnectionDetails} in the \code{DatabaseConnector} package.
#'
#' @export
checkCmInstallation <- function(connectionDetails) {
  ParallelLogger::logInfo("Checking database connectivity")
  conn <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::disconnect(conn)
  ParallelLogger::logInfo("- Ok")

  ParallelLogger::logInfo("\nChecking large scale regression engine")
  counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
  outcome <- gl(3, 1, 9)
  treatment <- gl(3, 3)
  cyclopsData <- Cyclops::createCyclopsData(counts ~ outcome + treatment, modelType = "pr")
  cyclopsFit <- fitCyclopsModel(cyclopsData)
  if (length(coef(cyclopsFit)) != 5)
    stop("Error fitting regression model")
  ParallelLogger::logInfo("- Ok")

  ParallelLogger::logInfo("\nChecking support for large data objects")
  x <- ff::as.ffdf(data.frame(a = 1:100, b = "test"))
  if (nrow(x) != 100)
    stop("Error creating large data object")
  ParallelLogger::logInfo("- Ok")

  ParallelLogger::logInfo("\nCohortMethod is correctly installed")
  ParallelLogger::logInfo(paste0("\nResponse code: ", round(pi * 123456)))
}

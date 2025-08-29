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

# exportFolder = file.path(folder, "export")
# library(dplyr)


#' Insert exported results into a SQLite database
#'
#' @param sqliteFileName The name of the SQLite file to store the results in. If the
#'                       file does not exist it will be created.
#' @param exportFolder   The folder containing the CSV files to upload, as generated
#'                       using the [exportToCsv()] function.
#' @param cohorts        A data frame describing the cohorts used in the study. Should
#'                       include the target, comparator, and outcome of interest cohorts.
#'                       The data frame should at least have a `cohortDefinitionId` and
#'                       `cohortName` columns.
#'
#' @return
#' Does not return anything. Called for the side effect of inserting data into the
#' SQLite database.
#'
#' @export
insertExportedResultsInSqlite <- function(sqliteFileName, exportFolder, cohorts) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sqliteFileName, len = 1, add = errorMessages)
  checkmate::assertCharacter(exportFolder, len = 1, add = errorMessages)
  checkmate::assertDataFrame(cohorts, add = errorMessages)
  checkmate::assertNames(colnames(cohorts), must.include = c("cohortDefinitionId", "cohortName"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  ensureInstalled("RSQLite")
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteFileName
  )
  append <- file.exists(sqliteFileName)
  if (append) {
    message("File `", sqliteFileName, "` already exists, so appending results instead of creating new tables")
  } else {
    createResultsDataModel(
      connectionDetails = connectionDetails,
      databaseSchema = "main",
      tablePrefix = ""
    )
  }
  zipFiles <- list.files(exportFolder, "Results.*.zip")
  databaseIds <- gsub("^.*_", "", gsub(".zip", "", zipFiles))

  # Upload cohorts and database meta_data
  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "cg_cohort_definition",
    data = cohorts,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    camelCaseToSnakeCase = TRUE
  )
  databases <- tibble(
    database_id = databaseIds,
    cdm_source_name = databaseIds,
    cdm_source_abbreviation = databaseIds
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "database_meta_data",
    data = databases,
    dropTableIfExists = TRUE,
    createTable = TRUE
  )
  DatabaseConnector::disconnect(connection)

  # Upload results
  for (zipFile in zipFiles) {
    uploadResults(
      connectionDetails = connectionDetails,
      schema = "main",
      zipFileName = file.path(exportFolder, zipFile),
      purgeSiteDataBeforeUploading = FALSE
    )
  }
}

#' Launch Shiny app using a SQLite database
#'
#' @param sqliteFileName The name of the SQLite file where the results were stored
#'                       using the [insertExportedResultsInSqlite()] function.
#'
#' @return
#' Does not return anything. Is called for the side-effect of launching the Shiny
#' app.
#'
#' @export
launchResultsViewerUsingSqlite <- function(sqliteFileName) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sqliteFileName, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  ensureInstalled("RSQLite")
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteFileName
  )
  launchResultsViewer(
    connectionDetails = connectionDetails,
    databaseSchema = "main"
  )
}

#' Launch Shiny app using
#'
#' @param connectionDetails An R object of type `connectionDetails` created using the
#'                          [DatabaseConnector::createConnectionDetails()] function.
#' @param databaseSchema    The name of the database schema where the results were
#'                          written using [uploadResults()].
#'
#' @return
#' Does not return anything. Is called for the side-effect of launching the Shiny
#' app.
#'
#' @export
launchResultsViewer <- function(connectionDetails, databaseSchema) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(databaseSchema, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  ensureInstalled("OhdsiShinyAppBuilder")
  ensureInstalled("markdown")
  aboutModule <- OhdsiShinyAppBuilder::createDefaultAboutConfig()
  resultDatabaseDetails <- list(
    dbms = connectionDetails$dbms,
    tablePrefix = 'cm_',
    cohortTablePrefix = 'cg_',
    databaseTablePrefix = '',
    schema =  "main",
    databaseTable = 'DATABASE_META_DATA'
  )
  estimationModule <- OhdsiShinyAppBuilder::createDefaultEstimationConfig()
  shinyAppConfig <- OhdsiShinyAppBuilder::initializeModuleConfig() |>
    OhdsiShinyAppBuilder::addModuleConfig(aboutModule) |>
    OhdsiShinyAppBuilder::addModuleConfig(estimationModule)
  connectionHandler <- ResultModelManager::ConnectionHandler$new(connectionDetails)
  on.exit(connectionHandler$closeConnection())
  OhdsiShinyAppBuilder::viewShiny(shinyAppConfig, connectionHandler)
}

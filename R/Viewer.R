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
# exportFolder = file.path(folder, "export")
# library(dplyr)


#' Insert exported results into a SQLite database
#'
#' @param sqliteFileName The name of the SQLite file to store the results in. If the
#'                       file does not exist it will be created.
#' @param exportFolder   The folder containing the CSV files to upload, as generated
#'                       using the [exportToCsv()] function.
#' @template Cohorts
#'
#' @return
#' Does not return anything. Called for the side effect of inserting data into the
#' SQLite database.
#'
#' @export
insertExportedResultsInSqlite <- function(sqliteFileName, exportFolder, cohorts) {
  ensureInstalled("RSQLite")
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteFileName
  )
  uploadExportedResults(
    connectionDetails = connectionDetails,
    databaseSchema = "main",
    exportFolder = exportFolder,
    cohorts = cohorts
  )
}

#' Upload exported results to a database
#'
#' @param connectionDetails An R object of type `connectionDetails` created using the
#'                          [DatabaseConnector::createConnectionDetails()] function.
#' @param databaseSchema    The name of the database schema where the results will be
#'                          written.
#' @param exportFolder      The folder containing the CSV files to upload, as generated
#'                          using the [exportToCsv()] function.
#' @template Cohorts
#'
#'
#' @return
#' Does not return anything. Is called for the side-effect of having the results uploaded
#' to the server.
#'
#' @export
uploadExportedResults <- function(connectionDetails,
                                  databaseSchema,
                                  exportFolder,
                                  cohorts) {
  # ensureInstalled("CohortGenerator")
  ensureInstalled("ResultModelManager")
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Create tables
  rdmsFile <- system.file("csv", "resultsDataModelSpecification.csv", package = "CohortMethod")
  # specification <- CohortGenerator::readCsv(file = rdmsFile)
  specification <- readr::read_csv(file = rdmsFile, show_col_types = FALSE) %>%
    SqlRender::snakeCaseToCamelCaseNames()
  sql <- ResultModelManager::generateSqlSchema(csvFilepath = rdmsFile)
  sql <- SqlRender::render(
    sql = sql,
    database_schema = databaseSchema
  )
  DatabaseConnector::executeSql(connection = connection, sql = sql)

  # Extract database ID and create a temp file and table
  resultsFile <- file.path(exportFolder, "cm_result.csv")
  databaseIdentifier <- readr::read_csv(resultsFile, show_col_types = FALSE) %>%
    head(1) %>%
    transmute(
      .data$database_id,
      cdm_source_name = .data$database_id,
      cdm_source_abbreviation = .data$database_id
    )
  databaseIdentifierFile <- tempfile(fileext = ".csv")
  readr::write_csv(databaseIdentifier, databaseIdentifierFile)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema =databaseSchema,
    tableName = "database_meta_data",
    data = databaseIdentifier,
    dropTableIfExists = FALSE,
    createTable = TRUE
  )
  on.exit(unlink(databaseIdentifierFile), add = TRUE)

  # Upload cohorts
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema =databaseSchema,
    tableName = "cg_cohort_definition",
    data = cohorts %>%
      rename(cohortDefinitionId = "cohortId"),
    dropTableIfExists = TRUE,
    createTable = TRUE,
    camelCaseToSnakeCase = TRUE
  )

  # Upload results
  ResultModelManager::uploadResults(
    connection = connection,
    schema = databaseSchema,
    resultsFolder = exportFolder,
    purgeSiteDataBeforeUploading = TRUE,
    databaseIdentifierFile = databaseIdentifierFile,
    specifications = specification
  )
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
#'                          written using [uploadExportedResults(].
#'
#' @return
#' Does not return anything. Is called for the side-effect of launching the Shiny
#' app.
#'
#' @export
launchResultsViewer <- function(connectionDetails, databaseSchema) {
  ensureInstalled("ShinyAppBuilder")
  ensureInstalled("markdown")
  aboutModule <- ShinyAppBuilder::createDefaultAboutConfig(
    resultDatabaseDetails = NULL,
    useKeyring = TRUE
  )
  resultDatabaseDetails <- list(
    dbms = connectionDetails$dbms,
    tablePrefix = 'cm_',
    cohortTablePrefix = 'cg_',
    databaseTablePrefix = '',
    schema = databaseSchema,
    databaseTable = 'DATABASE_META_DATA'
  )
  cohortMethodModule <- ShinyAppBuilder::createDefaultEstimationConfig(
    resultDatabaseDetails = resultDatabaseDetails,
    useKeyring = TRUE
  )
  shinyAppConfig <- ShinyAppBuilder::initializeModuleConfig() %>%
    ShinyAppBuilder::addModuleConfig(aboutModule) %>%
    # addModuleConfig(cohortGeneratorModule) %>%
    ShinyAppBuilder::addModuleConfig(cohortMethodModule)

  # Launch shiny app locally -----------------------------------------------------
  connectionHandler <- ResultModelManager::ConnectionHandler$new(connectionDetails)
  ShinyAppBuilder::viewShiny(shinyAppConfig, connectionHandler)
  connectionHandler$closeConnection()
}

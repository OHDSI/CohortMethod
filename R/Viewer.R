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
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sqliteFileName, len = 1, add = errorMessages)
  checkmate::assertCharacter(exportFolder, len = 1, add = errorMessages)
  checkmate::assertDataFrame(cohorts, add = errorMessages)
  checkmate::assertNames(colnames(cohorts), must.include = c("cohortId", "cohortName"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  ensureInstalled("RSQLite")
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteFileName
  )
  append <- file.exists(sqliteFileName)
  if (append) {
    message("File `", sqliteFileName, "` already exists, so appending results instead of creating new tables")
  }
  uploadExportedResults(
    connectionDetails = connectionDetails,
    databaseSchema = "main",
    append = append,
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
#' @param append            Append the results to existing tables? Can be used for
#'                          uploading results from multiple databases into a single
#'                          results schema.
#' @param exportFolder      The folder containing the CSV files to upload, as generated
#'                          using the [exportToCsv()] function.
#' @template Cohorts
#'
#' @return
#' Does not return anything. Is called for the side-effect of having the results uploaded
#' to the server.
#'
#' @export
uploadExportedResults <- function(connectionDetails,
                                  databaseSchema,
                                  append = FALSE,
                                  exportFolder,
                                  cohorts) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(databaseSchema, len = 1, add = errorMessages)
  checkmate::assertLogical(append, len = 1, add = errorMessages)
  checkmate::assertCharacter(exportFolder, len = 1, add = errorMessages)
  checkmate::assertDataFrame(cohorts, add = errorMessages)
  checkmate::assertNames(colnames(cohorts), must.include = c("cohortId", "cohortName"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  ensureInstalled("ResultModelManager")
  connection <- DatabaseConnector::connect(connectionDetails)

  withr::defer({
    tryCatch({
      DatabaseConnector::disconnect(connection)
    }, error = function(e) {
      message(e)
    })
  })
  rdmsFile <- system.file("csv", "resultsDataModelSpecification.csv", package = "CohortMethod")
  specification <- readr::read_csv(file = rdmsFile, show_col_types = FALSE) %>%
    SqlRender::snakeCaseToCamelCaseNames()

  if (!append) {
    # Create tables
    sql <- ResultModelManager::generateSqlSchema(csvFilepath = rdmsFile)
    sql <- SqlRender::render(
      sql = sql,
      database_schema = databaseSchema
    )
    DatabaseConnector::executeSql(connection = connection, sql = sql)

    # Upload cohorts
    DatabaseConnector::insertTable(
      connection = connection,
      databaseSchema = databaseSchema,
      tableName = "cg_cohort_definition",
      data = cohorts %>%
        rename(cohortDefinitionId = "cohortId"),
      dropTableIfExists = TRUE,
      createTable = TRUE,
      camelCaseToSnakeCase = TRUE
    )
  }

  # Extract database ID and create a temp file and table
  resultsFile <- file.path(exportFolder, "cm_result.csv")
  databaseIdentifier <- readr::read_csv(resultsFile, show_col_types = FALSE) %>%
    head(1) %>%
    transmute(
      database_id = as.character(.data$database_id),
      cdm_source_name = as.character(.data$database_id),
      cdm_source_abbreviation = as.character(.data$database_id)
    )
  databaseIdentifierFile <- tempfile(fileext = ".csv")
  readr::write_csv(databaseIdentifier, databaseIdentifierFile)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema =databaseSchema,
    tableName = "database_meta_data",
    data = databaseIdentifier,
    dropTableIfExists = FALSE,
    createTable = !append
  )
  withr::defer({
    tryCatch({
      unlink(databaseIdentifierFile)
    }, error = function(e) {
      message(e)
    })
  })
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
#'                          written using [uploadExportedResults()].
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
  cohortMethodModule <- ShinyAppBuilder::createDefaultCohortMethodConfig(
    resultDatabaseDetails = resultDatabaseDetails,
    useKeyring = TRUE
  )
  shinyAppConfig <- ShinyAppBuilder::initializeModuleConfig() %>%
    ShinyAppBuilder::addModuleConfig(aboutModule) %>%
    # addModuleConfig(cohortGeneratorModule) %>%
    ShinyAppBuilder::addModuleConfig(cohortMethodModule)
  connectionHandler <- ResultModelManager::ConnectionHandler$new(connectionDetails)
  ShinyAppBuilder::viewShiny(shinyAppConfig, connectionHandler)
  connectionHandler$closeConnection()
}

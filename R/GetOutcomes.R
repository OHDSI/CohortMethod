# @file GetCovariates.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' Get outcomes for persons in the cohorts
#'
#' @description
#' Gets the outcomes for the cohorts in the \code{cohortData} object.
#'
#' @details
#' If the \code{connection} parameter is specified, the cohorts are already assumed to be on the server in the appropriate temp table. Else, the temp table will be created by loading
#' the cohorts from the \code{cohortData} object to the server.
#'
#' This function can be used to add additional outcomes to an existing cohortData object.
#'
#' @param connectionDetails  	An R object of type \code{ConnectionDetails} created using the function \code{createConnectionDetails} in the \code{DatabaseConnector} package.
#' @param connection          A connection to the server containing the schema as created using the \code{connect} function in the \code{DatabaseConnector} package.
#' @param oracleTempSchema		A schema where temp tables can be created in Oracle.
#' @param cdmDatabaseSchema    The name of the database schema that contains the OMOP CDM instance.  Requires read permissions to this database. On SQL Server, this should specifiy both the database and the schema, so for example 'cdm_instance.dbo'.
#' @param cohortData          An object of type \code{cohortData} as generated using \code{getDbCohortData}.
#' @template GetOutcomesParams
#'
#' @return
#' The original \code{cohortData} object with the new outcome data added.
#'
#' @export
getDbOutcomes <- function(connectionDetails = NULL,
                          connection = NULL,
                          cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
                          cohortData,
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "condition_occurrence",
                          outcomeConceptIds = "",
                          outcomeConditionTypeConceptIds = "") {
  cdmDatabase <- strsplit(cdmDatabaseSchema ,"\\.")[[1]][1]
  if (is.null(connectionDetails) && is.null(connection))
    stop("Either connectionDetails or connection has to be specified")
  if (!is.null(connectionDetails) && !is.null(connection))
    stop("Cannot specify both connectionDetails and connection")

  if (is.null(connection)){
    conn <- DatabaseConnector::connect(connectionDetails)
    cohort <- data.frame(subject_id = ff::as.ram.ff(cohortData$cohorts$rowId),
                         cohort_definition_id = ff::as.ram.ff(cohortData$cohorts$treatment),
                         cohort_start_date = ff::as.ram.ff(cohortData$cohorts$cohortStartDate))
    DatabaseConnector::dbInsertTable(conn, "#cohort_person", cohort, TRUE, TRUE, TRUE, oracleTempSchema)
  } else {
    conn <- connection
  }

  renderedSql <- SqlRender::loadRenderTranslateSql("GetOutcomes.sql",
                                                   packageName = "CohortMethod",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database = cdmDatabase,
                                                   outcome_database_schema = outcomeDatabaseSchema,
                                                   outcome_table = outcomeTable,
                                                   outcome_concept_ids = outcomeConceptIds,
                                                   outcome_condition_type_concept_ids = outcomeConditionTypeConceptIds)

  writeLines("Executing multiple queries. This could take a while")
  DatabaseConnector::executeSql(conn,renderedSql)
  writeLines("Done")

  writeLines("Fetching data from server")
  start <- Sys.time()
  outcomeSql <-"SELECT person_id AS row_id, outcome_id, time_to_event FROM #cohort_outcome ORDER BY outcome_id, person_id"
  outcomeSql <- SqlRender::translateSql(outcomeSql, "sql server", attr(conn, "dbms"), oracleTempSchema)$sql
  outcomes <- DatabaseConnector::dbGetQuery.ffdf(conn, outcomeSql)
  excludeSql <-"SELECT person_id AS row_id, outcome_id FROM #cohort_excluded_person ORDER BY outcome_id, person_id"
  excludeSql <- SqlRender::translateSql(excludeSql, "sql server", attr(conn, "dbms"), oracleTempSchema)$sql
  exclude <- DatabaseConnector::dbGetQuery.ffdf(conn, excludeSql)
  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta,3), attr(delta,"units")))

  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveOutcomeTempTables.sql",
                                                   packageName = "CohortMethod",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema)
  DatabaseConnector::executeSql(conn,renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  if (is.null(connection)){
    RJDBC::dbDisconnect(conn)
  }

  colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
  colnames(exclude) <- SqlRender::snakeCaseToCamelCase(colnames(exclude))
  if (nrow(outcomes) == 0){
    warning("No data found")
  } else {
    open(outcomes)
    open(exclude)
  }
  if (is.null(cohortData$outcomes)){
    cohortData$outcomes <- outcomes
    cohortData$exclude <- exclude
    cohortData$metaData$outcomeConceptIds <- outcomeConceptIds
    cohortData$metaData$sql <- c(cohortData$metaData$sql, renderedSql)
  } else {
    ffbase::ffdfappend(cohortData$outcomes, outcomes)
    ffbase::ffdfappend(cohortData$exclude, exclude)
    cohortData$metaData$outcomeConceptIds = rbind(cohortData$metaData$outcomeConceptIds, outcomeConceptIds)
    cohortData$metaData$sql <- c(cohortData$metaData$sql, renderedSql)
  }
  return(cohortData)
}

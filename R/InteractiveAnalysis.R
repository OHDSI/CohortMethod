library(DatabaseConnector)

log <- function(text) {
  f = file("/tmp/log")
  write(text, f)
  close(f)
}

getSqlPath <- function(filename) {
  sqlBaseDir = "sql/redshift_fake/"
  system.file(paste(sqlBaseDir, filename,sep=""), package="CohortMethod")
}

#' @export
setSearchPath <- function(conn, cdmSchema="MSLR_CDM4") {
  sqlPath = getSqlPath("SetSearchPath.sql")
  f = file(sqlPath)
  sqlStatement = paste(readLines(f), collapse="\n")
  close(f)

  sqlStatement = sprintf(sqlStatement, cdmSchema)
  executeSql(conn, sqlStatement)
}


#' @export
getCohortSize <- function(conn) {
  query = "
  SELECT
      cohort_id, COUNT(cohort_id)
  FROM cohort_person
  GROUP BY cohort_id
  ;
  "
  result = dbSendQuery(conn, query)
  dbFetch(result)
}


#' @export
balanceCohorts <- function(conn, dbms="sql server") {
  size = getCohortSize(conn)
  limit = min(size$count)
  
  renderedSql <- loadRenderTranslateSql(
                    "BalanceCohorts.sql",
                    "CohortMethod",
                    dbms=dbms,
                    limit=limit)
  executeSql(conn, renderedSql)
}


#' @export
buildCovariates <- function(conn) {
  cleanupStatement = "
  TRUNCATE TABLE covariates;
  DROP TABLE covariates;
  TRUNCATE TABLE concept_counts;
  DROP TABLE concept_counts;
  TRUNCATE TABLE snomed_to_all_meddra;
  DROP TABLE snomed_to_all_meddra;
  TRUNCATE TABLE rxnorm_to_atc;
  DROP TABLE rxnorm_to_atc;
  "
  tryCatch(dbSendUpdate(conn, cleanupStatement), error=function(e) NULL)

  sqlPath = getSqlPath("BuildCovariates.sql")
  f = file(sqlPath)
  sqlStatement = paste(readLines(f), collapse="\n")
  close(f)

  executeSql(conn, sqlStatement)
}


#' @export
buildOutcomes <- function(conn) {
  cleanupStatement = "
  TRUNCATE TABLE outcomes;
  DROP TABLE outcomes;
  "
  tryCatch(dbSendUpdate(conn, cleanupStatement), error=function(e) NULL)

  sqlPath = getSqlPath("BuildOutcomes.sql")
  f = file(sqlPath)
  sqlStatement = paste(readLines(f), collapse="\n")
  close(f)

  executeSql(conn, sqlStatement)
}

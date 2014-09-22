library(DatabaseConnector)

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
buildCohorts <- function(conn, drugA, drugB, indicator, dbms="redshift",
                         cdmSchema="mslr_cdm4", resultsSchema="mslr_cdm4",
                         washoutWindow=183, indicationLookbackWindow=183,
                         studyStartDate="", studyEndDate="",
                         exclusionConceptIds = c(4027133, 4032243, 4146536,
                         2002282, 2213572, 2005890, 43534760, 21601019),
                         exposureTable="DRUG_ERA") {
  renderedSql <- loadRenderTranslateSql(
                    "BuildCohorts.sql",
                    "CohortMethod",
                    dbms=dbms,
                    cdm_schema=cdmSchema,
                    results_schema=resultsSchema,
                    target_drug_concept_id=drugA,
                    comparator_drug_concept_id=drugB,
                    indication_concept_ids=indicator,
                    washout_window=washoutWindow,
                    indication_lookback_window=indicationLookbackWindow,
                    study_start_date=studyStartDate,
                    study_end_date=studyEndDate,
                    exclusion_concept_ids=exclusionConceptIds,
                    exposure_table=exposureTable)
  executeSql(conn, renderedSql)
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
balanceCohorts <- function(conn) {
  sqlPath = getSqlPath("BalanceCohortsp1.sql")
  f = file(sqlPath)
  sqlStatement = paste(readLines(f), collapse="\n")
  close(f)

  result = dbSendQuery(conn, sqlStatement)
  limit = min(dbFetch(result))
  
  sqlPath = getSqlPath("BalanceCohortsp2.sql")
  f = file(sqlPath)
  sqlStatement = paste(readLines(f), collapse="\n")
  close(f)
  
  sqlStatement = sprintf(sqlStatement, limit, limit)
  executeSql(conn, sqlStatement)
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

library(DatabaseConnector)


mylogger <- function(text, filename="/tmp/log") {
  f = file(filename)
  write(text, f)
  close(f)
}


oldconnect = connect

connect <- function() {
  connectionDetails <- createConnectionDetails(
                          dbms=dbms,
                          server=server,
                          user=user,
                          password=pw,
                          schema=cdmSchema,
                          port=port)
  conn <<- oldconnect(connectionDetails)
  connected <<- TRUE
}

disconnect <- function() {
  dbDisconnect(conn)
  connected <<- FALSE
}

buildCohorts <- function(drugA, drugB, indicator,
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
                    results_schema=cdmSchema,
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


# TODO: Change query to use loadRenderTranslateSql.
getCohortSize <- function() {
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


balanceCohorts <- function() {
  size = getCohortSize()
  limit = min(size$count)
  
  renderedSql <- loadRenderTranslateSql(
                    "BalanceCohorts.sql",
                    "CohortMethod",
                    dbms=dbms,
                    limit=limit)
  executeSql(conn, renderedSql)
}


buildCovariates <- function(drugA, exclusionConceptIds = c(4027133, 4032243,
                            4146536, 2002282, 2213572, 2005890, 43534760,
                            21601019), excludedCovariateConceptIds =
                            c(4027133, 4032243, 4146536, 2002282, 2213572,
                            2005890, 43534760, 21601019),
                            deleteCovariatesSmallCount = 100) {
  renderedSql <- loadRenderTranslateSql(
                    "BuildCovariates.sql",
                    "CohortMethod",
                    dbms=dbms,
                    cdm_schema=cdmSchema,
                    results_schema=cdmSchema,
                    target_drug_concept_id=drugA,
                    exclusion_concept_ids=exclusionConceptIds,
                    excluded_covariate_concept_ids=excludedCovariateConceptIds,
                    delete_covariates_small_count=deleteCovariatesSmallCount)
  executeSql(conn, renderedSql)
}


# TODO: Change query to use loadRenderTranslateSql.
getCovariateSize <- function() {
  query = "
  SELECT
      COUNT(DISTINCT(person_id)) AS num_persons,
      COUNT(DISTINCT(covariate_id)) AS num_covariates
  FROM cohort_covariate
  ;
  "
  result = dbSendQuery(conn, query)
  dbFetch(result)
}


#' @export
CohortConn <- setRefClass(
                  "CohortConn",
                  fields=list(
                      pw = "character",
                      dbms = "character",
                      user = "character",
                      server = "character",
                      cdmSchema = "character",
                      port = "character",
                      connected = "logical",
                      conn = "ANY"
                  ),
                  prototype=list(
                      connected = FALSE
                  ),
                  methods=list(
                      initialize = function(..., connected=FALSE) {
                          callSuper(..., connected=FALSE)
                      },
                      connect = connect,
                      disconnect = disconnect,
                      buildCohorts = buildCohorts,
                      getCohortSize = getCohortSize,
                      balanceCohorts = balanceCohorts,
                      buildCovariates = buildCovariates,
                      getCovariateSize = getCovariateSize
                  )
              )

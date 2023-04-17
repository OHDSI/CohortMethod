# R6 CohortDbInterface ----
#' @title
#'   CohortDbInterface
#'
#' @description
#'   Class to interface with database.
CohortDbInterface <- R6::R6Class(
  classname = "CohortDbInterface",
  ## Public ----
  public = list(
    ### Methods ----
    #' @description
    #'   Initializer method
    #'
    #' @param connectionDetails
    #'   <ConnectionDetails> created by
    #'   \link[DatabaseConnector]{createConnectionDetails} >= 6.0.0
    #' @param tempEmulationSchema
    #'   <character>
    #' @param cdmDatabaseSchema
    #'   <character>
    #' @param exposureDatabaseSchema
    #'   <character>
    #' @param exposureTable
    #'   <character>
    #' @param cdmVersion
    #'   <character>
    #' @param outcomeTable
    #'   <character>
    #' @param outcomeDatabaseSchema
    #'   <character>
    initialize = function(connectionDetails,
                          tempEmulationSchema,
                          cdmDatabaseSchema,
                          exposureDatabaseSchema,
                          exposureTable,
                          cdmVersion,
                          outcomeTable,
                          outcomeDatabaseSchema) {
      private$connectionDetails <- connectionDetails
      private$connection <- DatabaseConnector::connect(connectionDetails)
      private$tempEmulationSchema <- tempEmulationSchema
      private$cdmDatabaseSchema <- cdmDatabaseSchema
      private$exposureDatabaseSchema <- exposureDatabaseSchema
      private$exposureTable <- exposureTable
      private$cdmVersion <- cdmVersion
      private$outcomeTable <- outcomeTable
      private$outcomeDatabaseSchema <- outcomeDatabaseSchema
    },
    #' @description
    #'   Method to disconnect from database
    disconnect = function() {
      DatabaseConnector::disconnect(private$connection)
      return(invisible(self))
    },
    #' @description
    #'   Method to create cohorts in the database
    #'
    #' @param targetId
    #'   <numeric>
    #' @param comparatorId
    #'   <numeric>
    #' @param studyStartDate
    #'   <character>
    #' @param studyEndDate
    #'   <character>
    #' @param firstExposureOnly
    #'   <logical>
    #' @param removeDuplicateSubjects
    #'   <character> One of: "keep all", "keep first", "remove all"
    #' @param washoutPeriod
    #'   <numeric>
    #' @param restrictToCommonPeriod
    #'   <logical>
    createCohorts = function(targetId,
                             comparatorId,
                             studyStartDate,
                             studyEndDate,
                             firstExposureOnly,
                             removeDuplicateSubjects,
                             washoutPeriod,
                             restrictToCommonPeriod) {
      message("Constructing target and comparator cohorts")

      private$connection <- DatabaseConnector::connect(
        private$connectionDetails
      )

      renderedSql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "CreateCohorts.sql",
        packageName = "CohortMethod",
        dbms = private$connectionDetails$dbms,
        tempEmulationSchema = private$tempEmulationSchema,
        cdm_database_schema = private$cdmDatabaseSchema,
        exposure_database_schema = private$exposureDatabaseSchema,
        exposure_table = private$exposureTable,
        target_id = targetId,
        comparator_id = comparatorId,
        study_start_date = studyStartDate,
        study_end_date = studyEndDate,
        first_only = firstExposureOnly,
        remove_duplicate_subjects = removeDuplicateSubjects,
        washout_period = washoutPeriod,
        restrict_to_common_period = restrictToCommonPeriod
      )

      DatabaseConnector::executeSql(private$connection, renderedSql)
      return(invisible(self))
    },
    #' @description
    #'   Retrieve cohorts from the database
    #'
    #' @param sampled
    #'   <logical>
    #' @param targetId
    #'   <numeric>
    #'
    #' @return
    #'   <data.frame>
    getCohorts = function(sampled, targetId) {
      message("Fetching cohorts from server")

      cohortSql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "GetCohorts.sql",
        packageName = "CohortMethod",
        dbms = private$connectionDetails$dbms,
        tempEmulationSchema = private$tempEmulationSchema,
        target_id = targetId,
        sampled = sampled
      )
      cohorts <- DatabaseConnector::querySql(
        connection = private$connection,
        sql = cohortSql,
        snakeCaseToCamelCase = TRUE
      )

      cohorts$rowId <- as.numeric(cohorts$rowId)

      ParallelLogger::logDebug(
        "Fetched cohort total rows in target is ",
        sum(cohorts$treatment),
        ", total rows in comparator is ",
        sum(!cohorts$treatment)
      )
      return(cohorts)
    },

    #' @description
    #'   Counts cohorts
    #'
    #' @param targetId
    #'   <numeric>
    #'
    #' @return
    #'   <data.frame>
    countCohorts = function(targetId) {
      message("Counting cohorts")

      renderedSql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "CountCohorts.sql",
        packageName = "CohortMethod",
        dbms = private$connectionDetails$dbms,
        tempEmulationSchema = private$tempEmulationSchema,
        target_id = targetId
      )
      return(DatabaseConnector::querySql(
        connection = private$connection,
        sql = renderedSql,
        snakeCaseToCamelCase = TRUE
      ))
    },
    #' @description
    #'   Method to sample cohorts in the database to the specified maxCohortSize
    #'
    #' @param maxCohortSize
    #'   <numeric>
    sampleCohorts = function(maxCohortSize) {
      renderedSql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "SampleCohorts.sql",
        packageName = "CohortMethod",
        dbms = private$connectionDetails$dbms,
        tempEmulationSchema = private$tempEmulationSchema,
        max_cohort_size = maxCohortSize
      )
      DatabaseConnector::executeSql(
        private$connection,
        renderedSql
      )
      return(invisible(self))
    },
    #' @description
    #'   Count overall exposed population
    #'
    #' @param targetId
    #'   <numeric>
    #' @param comparatorId
    #'   <numeric>
    #' @param studyStartDate
    #'   <character>
    #' @param studyEndDate
    #'   <character>
    #'
    #' @return
    #'   <data.frame>
    countRaw = function(targetId, comparatorId, studyStartDate, studyEndDate) {
      rawCountSql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "CountOverallExposedPopulation.sql",
        packageName = "CohortMethod",
        dbms = private$connectionDetails$dbms,
        tempEmulationSchema = private$tempEmulationSchema,
        cdm_database_schema = private$cdmDatabaseSchema,
        exposure_database_schema = private$exposureDatabaseSchema,
        exposure_table = tolower(private$exposureTable),
        target_id = targetId,
        comparator_id = comparatorId,
        study_start_date = studyStartDate,
        study_end_date = studyEndDate
      )
      return(DatabaseConnector::querySql(
        connection = private$connection,
        sql = rawCountSql,
        snakeCaseToCamelCase = TRUE
      ))
    },
    #' @description
    #'   Method to retrieve the outcomes
    #'
    #' @param metaData
    #'   <list>
    #' @param outcomeIds
    #'   <numeric>
    #' @param sampled
    #'   <logical>
    #'
    #' @return metaData
    #'   <list>
    getOutcomes = function(metaData, outcomeIds, sampled) {
      start <- Sys.time()
      outcomeSql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "GetOutcomes.sql",
        packageName = "CohortMethod",
        dbms = private$connectionDetails$dbms,
        tempEmulationSchema = private$tempEmulationSchema,
        cdm_database_schema = private$cdmDatabaseSchema,
        outcome_database_schema = private$outcomeDatabaseSchema,
        outcome_table = private$outcomeTable,
        outcome_ids = outcomeIds,
        sampled = sampled
      )

      outcomes <- DatabaseConnector::querySql(
        connection = private$connection,
        sql = outcomeSql,
        snakeCaseToCamelCase = TRUE
      )

      outcomes$rowId <- as.numeric(outcomes$rowId)

      metaData$outcomeIds <- outcomeIds
      delta <- Sys.time() - start
      message("Fetching outcomes took ", signif(delta, 3), " ", attr(delta, "units"))
      return(list(metadata = metaData, outcomes = outcomes))
    },
    #' @description
    #'   Remove temp tables
    #'
    #' @param sampled
    #'   <logical>
    rmTempTables = function(sampled) {
      # Remove temp tables:
      renderedSql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "RemoveCohortTempTables.sql",
        packageName = "CohortMethod",
        dbms = private$connectionDetails$dbms,
        tempEmulationSchema = private$tempEmulationSchema,
        sampled = sampled
      )

      DatabaseConnector::executeSql(
        connection = private$connection,
        sql = renderedSql,
        progressBar = FALSE,
        reportOverallTime = FALSE
      )
      return(invisible(self))
    },
    #' @description
    #'   Wrapper to extract covariate data. See
    #'   \link[FeatureExtraction]{getDbCovariateData}
    #'
    #' @param cohortTable
    #'   <character>
    #' @param covariateSettings
    #'   <CovariateSettings> class. See
    #'   \link[FeatureExtraction]{createCovariateSettings}
    extractCovarDat = function(cohortTable, covariateSettings) {
      covariateData <- FeatureExtraction::getDbCovariateData(
        connection = private$connection,
        oracleTempSchema = private$tempEmulationSchema,
        cdmDatabaseSchema = private$cdmDatabaseSchema,
        cdmVersion = private$cdmVersion,
        cohortTable = cohortTable,
        cohortTableIsTemp = TRUE,
        rowIdField = "row_id",
        covariateSettings = covariateSettings
      )

      ParallelLogger::logDebug(
        "Fetched covariates total count is ",
        nrow_temp(covariateData$covariates)
      )

      return(covariateData)
    }
  ),
  ## Private ----
  private = list(
    ### Variables ----
    connectionDetails = NULL,
    connection = NULL,
    tempEmulationSchema = "",
    cdmDatabaseSchema = "",
    exposureDatabaseSchema = "",
    exposureTable = "",
    cdmVersion = "",
    outcomeTable = "",
    outcomeDatabaseSchema = "",
    ### Methods ----
    # @description
    #   Validation method
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(private$connectionDetails, "ConnectionDetails", add = errorMessages)
      checkmate::assertCharacter(private$cdmDatabaseSchema, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertCharacter(private$exposureDatabaseSchema, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$exposureTable, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$outcomeDatabaseSchema, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$outcomeTable, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$cdmVersion, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    }
  )
)

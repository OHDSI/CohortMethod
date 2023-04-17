# R6 CohortMethodData ----
#' @title
#'   CohortMethodData
#'
#' @description
#'   Class to build COhortMethodData.
CohortMethodData <- R6::R6Class(
  classname = "CohortMethodData",
  ## Public ----
  public = list(
    ### Methods ----
    #' @description
    #'   Initializer method
    #' @param metaData
    #'   <list> containing: targetId, comparatorId, studyStartDate, studyEndDate
    #' @param outcomeIds
    #'   <numeric>
    #' @param firstExposureOnly
    #'   <logical>
    #' @param removeDuplicateSubjects
    #'   <character> One of: "keep all", "keep first", "remove all"
    #' @param restrictToCommonPeriod
    #'   <logical>
    #' @param washoutPeriod
    #'   <numeric>
    #' @param maxCohortSize
    #'   <numeric>
    #' @param covariateSettings
    #'   <CovariateSettings> class
    #' @param cohortDbInterface
    #'   <CohortDbInterface> R6 Class
    initialize = function(metaData,
                          outcomeIds,
                          firstExposureOnly,
                          removeDuplicateSubjects,
                          restrictToCommonPeriod,
                          washoutPeriod,
                          maxCohortSize,
                          covariateSettings,
                          cohortDbInterface) {
      private$metaData <- metaData
      private$outcomeIds <- outcomeIds
      private$firstExposureOnly <- firstExposureOnly
      private$removeDuplicateSubjects <- removeDuplicateSubjects
      private$restrictToCommonPeriod <- restrictToCommonPeriod
      private$washoutPeriod <- washoutPeriod
      private$maxCohortSize <- maxCohortSize
      private$covariateSettings <- covariateSettings
      private$cohortDbInterface <- cohortDbInterface

      if (is.null(private$metaData$studyStartDate)) {
        private$metaData$studyStartDate <- ""
      }

      if (is.null(private$metaData$studyEndDate)) {
        private$metaData$studyEndDate <- ""
      }

      private$validate()

      return(invisible(self))
    },


    #' @description
    #'   Wrapper for `CohortDbInterface$createCohorts`
    createCohorts = function() {
      private$cohortDbInterface$createCohorts(
        targetId = private$metaData$targetId,
        comparatorId = private$metaData$comparatorId,
        studyStartDate = private$metaData$studyStartDate,
        studyEndDate = private$metaData$studyEndDate,
        firstExposureOnly = private$firstExposureOnly,
        removeDuplicateSubjects = private$removeDuplicateSubjects,
        washoutPeriod = private$washoutPeriod,
        restrictToCommonPeriod = private$restrictToCommonPeriod
      )
      return(invisible(self))
    },


    #' @description
    #'   Downsample cohorts
    downSample = function() {
      private$counts <- private$cohortDbInterface$countCohorts(private$metaData$targetId)

      ParallelLogger::logDebug(
        "Pre-sample total row count is ", sum(private$counts$rowCount)
      )
      private$countPreSample(id = 1, counts = private$counts)
      private$countPreSample(id = 0, counts = private$counts)
      private$preSampleCounts$dummy <- NULL

      if (private$preSampleCounts$targetExposures > private$maxCohortSize) {
        message(
          "Downsampling target cohort from ",
          private$preSampleCounts$targetExposures, " to ", maxCohortSize
        )
        private$sampled <- TRUE
      }

      if (private$preSampleCounts$comparatorExposures > private$maxCohortSize) {
        message(
          "Downsampling comparator cohort from ",
          private$preSampleCounts$comparatorExposures,
          " to ", private$maxCohortSize
        )
        private$sampled <- TRUE
      }

      if (private$sampled) {
        private$sampleCohorts()
      }
      return(invisible(self))
    },


    #' @description
    #'   Wrapper for `cohortDbInterface$getCohorts`
    getCohorts = function() {
      private$cohorts <- private$cohortDbInterface$getCohorts(
        sampled = private$sampled,
        targetId = private$metaData$targetId
      )
      return(invisible(self))
    },


    #' @description
    #'   Build covariate data
    buildCovariateData = function() {
      if (private$sampled) {
        cohortTable <- "#cohort_sample"
      } else {
        cohortTable <- "#cohort_person"
      }

      private$handleCohortCovariateBuilders()
      private$covariateData <- private$cohortDbInterface$extractCovarDat(
        cohortTable = cohortTable,
        covariateSettings = private$covariateSettings
      )

      message("Fetching outcomes from server")
      private$metaData <- private$cohortDbInterface$getOutcomes(
        metaData = private$metaData,
        outcomeIds = private$outcomeIds,
        sampled = private$sampled
      )
      private$cohortDbInterface$rmTempTables(sampled = private$sampled)

      private$covariateData$cohorts <- private$cohorts
      attr(private$covariateData, "metaData") <- append(attr(private$covariateData, "metaData"), private$metaData)
      class(private$covariateData) <- "CohortMethodData"
      attr(class(private$covariateData), "package") <- "CohortMethod"
      return(private$covariateData)
    }
  ),
  ## Private ----
  private = list(
    ### Variables ----
    metaData = list(),
    outcomeIds = NULL,
    cdmVersion = "",
    firstExposureOnly = FALSE,
    removeDuplicateSubjects = "",
    restrictToCommonPeriod = FALSE,
    washoutPeriod = 0,
    maxCohortSize = 0,
    covariateSettings = NULL,
    sampled = FALSE,
    counts = 0,
    preSampleCounts = dplyr::tibble(dummy = 0),
    cohorts = NULL,
    covariateData = NULL,
    cohortDbInterface = NULL,

    ### Methods ----


    # @description
    #   Validation method
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertInt(private$metaData$targetId, add = errorMessages)
      checkmate::assertInt(private$metaData$comparatorId, add = errorMessages)
      checkmate::assertIntegerish(private$outcomeIds, add = errorMessages)
      checkmate::assertCharacter(private$metaData$studyStartDate, len = 1, add = errorMessages)
      checkmate::assertCharacter(private$metaData$studyEndDate, len = 1, add = errorMessages)
      checkmate::assertLogical(private$firstExposureOnly, len = 1, add = errorMessages)
      checkmate::assertChoice(private$removeDuplicateSubjects, c("keep all", "keep first", "remove all"), add = errorMessages)
      checkmate::assertLogical(private$restrictToCommonPeriod, len = 1, add = errorMessages)
      checkmate::assertInt(private$washoutPeriod, lower = 0, add = errorMessages)
      checkmate::assertInt(private$maxCohortSize, lower = 0, add = errorMessages)
      checkmate::assertList(private$covariateSettings, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      if (private$metaData$studyStartDate != "" &&
          regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", private$studyStartDate) == -1) {
        stop("Study start date must have format YYYYMMDD")
      }

      if (private$metaData$studyEndDate != "" &&
          regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", private$studyEndDate) == -1) {
        stop("Study end date must have format YYYYMMDD")
      }

      return(invisible(self))
    },

    # @description
    #   Count before sampling
    #
    # @param id
    #   <numeric> Treatment ID
    # @param counts
    #   <data.frame> Cohort counts
    countPreSample = function(id, counts) {
      idx <- which(private$counts$treatment == id)
      switch(id + 1,
             {
               personsCol <- "comparatorPersons"
               exposuresCol <- "comparatorExposures"
             },
             {
               personsCol <- "targetPersons"
               exposuresCol <- "targetExposures"
             }
      )

      if (length(idx) != 0) {
        private$preSampleCounts[personsCol] <- private$counts$personCount[idx]
        private$preSampleCounts[exposuresCol] <- private$counts$rowCount[idx]
      } else {
        private$preSampleCounts[personsCol] <- 0
        private$preSampleCounts[exposuresCol] <- 0
      }
      return(invisible(self))
    },


    # @description
    #   Check if cohorts are empty
    checkCohorts = function() {
      if (nrow(private$cohorts) == 0) {
        warning("Target and comparator cohorts are empty")
      } else if (sum(private$cohorts$treatment == 1) == 0) {
        warning("Target cohort is empty")
      } else if (sum(private$cohorts$treatment == 0) == 0) {
        warning("Comparator cohort is empty")
      }
      return(invisible(self))
    },

    # @description
    #   Construct labels for firstExposureOnly, removeDuplicateSubjects,
    #   restrictToCommonPeriod, and washoutPeriod
    constructLabel = function() {
      label <- c()
      if (private$firstExposureOnly) {
        label <- c(label, "first exp. only")
      }
      if (private$removeDuplicateSubjects == "remove all") {
        label <- c(label, "removed subs in both cohorts")
      } else if (private$removeDuplicateSubjects == "keep first") {
        label <- c(label, "first cohort only")
      }

      if (private$restrictToCommonPeriod) {
        label <- c(label, "restrict to common period")
      }
      if (private$washoutPeriod) {
        label <- c(label, paste(private$washoutPeriod, "days of obs. prior"))
      }
      return(paste(label, collapse = " & "))
    },
    # @description
    #   Filter cohorts based on input parameters
    filterCohorts = function() {
      if (private$firstExposureOnly || private$removeDuplicateSubjects != "keep all" || private$washoutPeriod != 0) {
        rawCount <- private$cohortDbInterface$countRaw()
        if (nrow(rawCount) == 0) {
          counts <- dplyr::tibble(
            description = "Original cohorts",
            targetPersons = 0,
            comparatorPersons = 0,
            targetExposures = 0,
            comparatorExposures = 0
          )
        } else {
          counts <- dplyr::tibble(
            description = "Original cohorts",
            targetPersons = rawCount$exposedCount[rawCount$treatment == 1],
            comparatorPersons = rawCount$exposedCount[rawCount$treatment == 0],
            targetExposures = rawCount$exposureCount[rawCount$treatment == 1],
            comparatorExposures = rawCount$exposureCount[rawCount$treatment == 0]
          )
        }
        private$metaData$attrition <- counts
        label <- private$constructLabel()
        substring(label, 1) <- toupper(substring(label, 1, 1))
        if (private$sampled) {
          private$preSampleCounts$description <- label
          private$metaData$attrition <-
            rbind(private$metaData$attrition, private$preSampleCounts)
          private$metaData$attrition <-
            rbind(private$metaData$attrition, getCounts(private$cohorts, "Random sample"))
        } else {
          private$metaData$attrition <-
            rbind(private$metaData$attrition, getCounts(private$cohorts, label))
        }
      } else if (private$sampled) {
        private$preSampleCounts$description <- "Original cohorts"
        private$metaData$attrition <- preSampleCounts
        private$metaData$attrition <- rbind(
          private$metaData$attrition,
          getCounts(cohorts, "Random sample")
        )
      } else {
        private$metaData$attrition <- getCounts(
          private$cohorts, "Original cohorts"
        )
      }
      return(invisible(self))
    },
    # @description
    #   Handle cohort covariate builders
    handleCohortCovariateBuilders = function() {
      if (is(private$covariateSettings, "covariateSettings")) {
        private$covariateSettings <- list(private$covariateSettings)
      }

      for (i in seq_len(length(private$covariateSettings))) {
        object <- private$covariateSettings[[i]]
        if ("covariateCohorts" %in% names(object) &&
            is.null(object$covariateCohortTable)) {
          object$covariateCohortDatabaseSchema <- private$exposureDatabaseSchema
          object$covariateCohortTable <- private$exposureTable
          private$covariateSettings[[i]] <- object
        }
      }
      return(invisible(self))
    }
  )
)

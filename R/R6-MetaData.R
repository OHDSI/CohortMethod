#' @title
#'   MetaData
#'
#' @description
#'   R6 MetaData object.
MetaData <- R6::R6Class(
  classname = "MetaData",
  public = list(
    # Public ----
    ## Fields ----
    #' @field targetId
    #'  A unique identifier to define the target cohort, as \link[base]{numeric}.
    targetId = 0,

    #' @field comparatorId
    #'   A unique identifier to define the comparator cohort, as \link[base]{numeric}.
    comparatorId = 0,

    #' @field studyStartDate
    #'   A calendar date specifying the minimum date that a cohort index date
    #'   can appear. Date format is 'yyyymmdd', as \link[base]{character}.
    studyStartDate = "",

    #' @field studyEndDate
    #'   A calendar date specifying the maximum date that a cohort index date
    #'   can appear. Date format is 'yyyymmdd'. Important: the study end data
    #'   is also used to truncate risk windows, meaning no outcomes beyond the
    #'   study end date will be considered, as \link[base]{character}.
    studyEndDate = "",

    #' @field attrition
    #'   Attrition table as \link[base]{data.frame}.
    attrition = NULL,

    #' @field outcomeIds
    #'   Vector (\link[base]{c}) of outcome ID's (\link[base]{numeric}) of length `n`.
    outcomeIds = NULL,

    #' @field populationSize
    #'   Population size as \link[base]{numeric} of length `1`.
    populationSize = 0,

    #' @field deletedRedundantCovariateIds
    #'   Vector (\link[base]{c}) of deleted redundant covariate ID's
    #'   (\link[base]{numeric}) of length `n`.
    deletedRedundantCovariateIds = NULL,

    #' @field deletedInfrequentCovariateIds
    #'   Vector (\link[base]{c}) of deleted infrequent covariate ID's
    #'   (\link[base]{numeric}) of length `n`.
    deletedInfrequentCovariateIds = NULL,

    #' @field deletedRedundantCovariateIdsForOutcomeModel
    #'   Vector (\link[base]{c}) of deleted redundant covariate ID's for the
    #'   outcome model (\link[base]{numeric}) of length `n`.
    deletedRedundantCovariateIdsForOutcomeModel = NULL,

    #' @field deletedInfrequentCovariateIdsForOutcomeModel
    #'   Vector (\link[base]{c}) of deleted infrequent covariate ID's for the
    #'   outcome model (\link[base]{numeric}) of length `n`.
    deletedInfrequentCovariateIdsForOutcomeModel = NULL,

    #' @field psModelCoef
    #'   Coefficient of the Propensity Score Model, returned by
    #'   \link[CohortMethod]{coef.OutcomeModel}. See \link[stats]{coef}
    psModelCoef = NULL,

    #' @field psModelPriorVariance
    #'   Variance returned by \link[stats]{predict} using a fitted Cyclops
    #'   model. See \link[Cyclops]{fitCyclopsModel}
    psModelPriorVariance = NULL,

    #' @field psError
    #'   Error message as \link[base]{character}.
    psError = "",

    #' @field psHighCorrelation
    #'   if `errorOnHighCorrelation == TRUE`: `CohortMethodData$covariateRef`
    #'   else: `NULL`.
    psHighCorrelation = NULL,

    #' @field estimator
    #'   Estimator as (\link[base]{character}) passed to createPs.
    #'   See \link[CohortMethod]{createPs}.
    estimator = "att",

    ## Methods ----
    #' @description
    #'   Initializer for the MetaData object.
    #'
    #' @param targetId
    #'    A unique identifier to define the target cohort, as \link[base]{numeric}.
    #' @param comparatorId
    #'   A unique identifier to define the comparator cohort, as \link[base]{numeric}.
    #' @param studyStartDate
    #'   A calendar date specifying the minimum date that a cohort index date
    #'   can appear. Date format is 'yyyymmdd', as \link[base]{character}.
    #' @param studyEndDate
    #'   A calendar date specifying the maximum date that a cohort index date
    #'   can appear. Date format is 'yyyymmdd'. Important: the study end data
    #'   is also used to truncate risk windows, meaning no outcomes beyond the
    #'   study end date will be considered, as \link[base]{character}.
    #'
    #' @return
    #'   `invisible(self)`
    initialize = function(targetId, comparatorId, studyStartDate, studyEndDate) {
      self$targetId <- targetId
      self$comparatorId <- comparatorId
      self$studyStartDate <- studyStartDate
      self$studyEndDate <- studyEndDate
      private$formatStudyDates()

      self$validate()
      return(invisible(self))
    },

    #' @description
    #'   Validation method for MetaData object.
    #'
    #' @return
    #'   `invisible(self)`
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()

      checkmate::assertInt(self$targetId, add = errorMessages)
      checkmate::assertInt(self$comparatorId, add = errorMessages)
      checkmate::assertCharacter(self$studyStartDate, len = 1, add = errorMessages)
      checkmate::assertCharacter(self$studyEndDate, len = 1, add = errorMessages)
      checkmate::assertDataFrame(self$attrition, null.ok = TRUE)
      checkmate::assertInt(self$outcomeIds, na.ok = FALSE, null.ok = TRUE)
      checkmate::assertInt(self$populationSize, lower = 0)
      checkmate::assertInt(self$deletedRedundantCovariateIds, self$outcomeIds, na.ok = FALSE, null.ok = TRUE)
      checkmate::assertInt(self$deletedInfrequentCovariateIds, self$outcomeIds, na.ok = FALSE, null.ok = TRUE)
      checkmate::assertInt(self$deletedRedundantCovariateIdsForOutcomeModel, self$outcomeIds, na.ok = FALSE, null.ok = TRUE)
      checkmate::assertInt(self$deletedInfrequentCovariateIdsForOutcomeModel, self$outcomeIds, na.ok = FALSE, null.ok = TRUE)
      checkmate::assertNumeric(self$psModelCoef, null.ok = TRUE)
      checkmate::assertNumeric(self$psModelPriorVariance, null.ok = TRUE)
      checkmate::assertCharacter(self$psError)
      checkmate::assertDataFrame(self$psHighCorrelation, null.ok = TRUE)
      checkmate::assertChoice(self$estimator, c("ate", "att", "ato"), add = errorMessages)

      checkmate::reportAssertions(collection = errorMessages)
      return(invisible(self))
    },

    #' @description
    #'   Returns a \link[base]{list} containing all fields of the MetaData
    #'   object.
    #'
    #' @return
    #'   \link[base]{list}
    getMetaData = function() {
      return(list(
        targetId = self$targetId,
        comparatorId = self$comparatorId,
        studyStartDate = self$studyStartDate,
        studyEndDate = self$studyEndDate,
        attrition = self$attrition,
        outcomeIds = self$outcomeIds,
        populationSize = self$populationSize,
        deletedRedundantCovariateIds = self$deletedRedundantCovariateIds,
        deletedInfrequentCovariateIds = self$deletedInfrequentCovariateIds,
        deletedRedundantCovariateIdsForOutcomeModel = self$deletedRedundantCovariateIdsForOutcomeModel,
        deletedInfrequentCovariateIdsForOutcomeModel = self$deletedInfrequentCovariateIdsForOutcomeModel,
        psModelCoef = self$psModelCoef,
        psModelPriorVariance = self$psModelPriorVariance,
        psError = self$psError,
        psHighCorrelation = self$psHighCorrelation,
        estimator = self$estimator
      ))
    },

    ### Overloads ----
    #' Title
    #'
    #' @param x
    #'   \link[CohortMethod]{MetaData} object.
    #' @param ...
    #'   Further arguments passed to or from other methods.
    #'
    #' @return
    #'   `invisible(self)`
    print = function(x, ...) {
      writeLines(paste("Class:", paste0(class(o), collapse = " ")))
      writeLines(paste("Target ID: ", self$targetId))
      writeLines(paste("Comparator ID: ", self$comparatorId))
      writeLines(paste("Study Start Date: ", self$studyStartDate))
      writeLines(paste("Study End Date: ", self$studyEndDate))
      writeLines(paste("Attrition: ", dim(self$attrition)))
      writeLines(paste("Number of Outcome IDs: ", length(self$outcomeIds)))
      writeLines(paste("Population size: ", self$populationSize))
      writeLines(paste("Number of redunded covariate IDs deleted: ", self$deletedRedundantCovariateIds))
      writeLines(paste("Number of infrequent covariate IDs deleted: ", self$deletedInfrequentCovariateIds))
      writeLines(paste("Number of redunded outcome model covariate IDs deleted: ", self$deletedRedundantCovariateIdsForOutcomeModel))
      writeLines(paste("Number of infrequent outcome model covariate IDs deleted: ", self$deletedInfrequentCovariateIdsForOutcomeModel))
      writeLines(paste("Propensity Score Model Coefficient: ", self$psModelCoef))
      writeLines(paste("Propensity Score Model Variance: ", self$psModelPriorVariance))
      writeLines(paste("Propensity Score Error", self$psError))
      writeLines(paste("High Correlation Propensity Scores: ", dim(self$psHighCorrelation)))
      writeLines(paste("Estimator: ", self$estimator))
      return(invisible(self))
    }
  ),
  private = list(
    # Private ----
    ## Methods ----
    formatStudyDates = function() {
      if (is.null(self$studyStartDate)) {
        self$studyStartDate <- ""
      }
      if (is.null(self$studyEndDate)) {
        self$studyEndDate <- ""
      }
      if (self$studyStartDate != "" &&
          regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", self$studyStartDate) == -1) {
        stop("Study start date must have format YYYYMMDD")
      }
      if (self$studyEndDate != "" &&
          regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", self$studyEndDate) == -1) {
        stop("Study end date must have format YYYYMMDD")
      }
      return(invisible(self))
    }
  )
)

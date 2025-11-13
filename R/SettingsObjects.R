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

library(R6)
library(Cyclops)

.recurseToList <- function(x) {
  if ("toList" %in% names(x)) {
    return(x$toList())
  } else if (is.list(x)) {
    for (i in seq_along(x)) {
      # If member is NULL then assigning NULL will remove it from the list altogether.
      if (!is.null(x[[i]])) {
        x[[i]] <- .recurseToList(x[[i]])
      }
    }
    if (is.object(x)) {
      # jsonlite::toJSON throws error on S3 objects
      class(x) <- NULL
    }
    return(x)
  } else {
    return(x)
  }
}

AbstractSerializableSettings <- R6Class(
  "AbstractSerializableSettings",
  public = list(
    initialize = function(typedList, untypedList, json) {
      if (!missing(typedList)) {
        # Initialize with list where objects already have correct type
        args <- typedList
        requireTyping <- FALSE
      } else if (!missing(untypedList)) {
        # Initialize with list where objects are still lists, amd need to be converted to right type.
        args <- untypedList
        requireTyping <- TRUE
      } else if (!missing(json)) {
        # Initialize with a JSON string
        args <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
        requireTyping <- TRUE
      } else {
        stop("Must provide either typedList, untypedList, or json argument to constructor")
      }
      self$fromList(args, requireTyping)
      self$validate()
    },
    toList = function() {
      # Using is(x, "environment") instead of is.environment(x) because former is FALSE for R6 objects.
      asList <- Filter(function(x) !is.function(x) & !is(x, "environment"), as.list(self))
      asList <- .recurseToList(asList)
      return(asList)
    },
    toJson  = function() {
      jsonlite::toJSON(
        self$toList(),
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null"
      )
    },
    fromList = function(list, requireTyping) {
      for (name in names(list)) {
        if (name %in% names(self)) {
          self[[name]] <- list[[name]]
        }
      }
    }
  )
)

.dateCheck <- function(date) {
  if (date != "") {
    tryCatch({
      dateFormatted <- paste0(
        substr(x = date, start = 1, stop = 4), "-",
        substr(x = date, start = 5, stop = 6), "-",
        substr(x = date, start = 7, stop = 8)
      )

      date <- as.Date(dateFormatted)
    }, error = function(e) {
      stop(sprintf("Date: %s (%s) is not valid", date, dateFormatted))
    })
    checkmate::assertDate(date, lower = "1000-01-01", upper = "2999-12-31")
  }
}

.convertAttrToMember <- function(object) {
  if (is.function(object)) {
    return(list(serialized_code = as.character(serialize(object, NULL))))
  } else if (is.list(object)) {
    if (length(object) > 0) {
      for (i in 1:length(object)) {
        if (!is.null(object[[i]])) {
          object[[i]] <- .convertAttrToMember(object[[i]])
        }
      }
    }
    a <- names(attributes(object))
    a <- a[!a %in% c("names", "class")]
    class <- class(object)
    if (length(class) > 1 || class != "list") {
      class(object) <- "list"
      object$attr_class <- class
    }
    if (length(a) > 0) {
      object[paste("attr", a, sep = "_")] <- attributes(object)[a]
    }
  }
  return(object)
}

.convertMemberToAttr <- function(object) {
  if (is.list(object)) {
    if (length(object) > 0) {
      if (length(object) == 1 && !is.null(names(object)) && names(object) == "serialized_code") {
        return(unserialize(as.raw(sapply(object$serialized_code, strtoi, base = 16L))))
      }
      for (i in 1:length(object)) {
        if (!is.null(object[[i]])) {
          object[[i]] <- .convertMemberToAttr(object[[i]])
        }
      }
      attrNames <- names(object)[grep("^attr_", names(object))]
      cleanNames <- gsub("^attr_", "", attrNames)
      if (any(cleanNames == "class")) {
        class(object) <- object$attr_class
        object$attr_class <- NULL
        attrNames <- attrNames[attrNames != "attr_class"]
        cleanNames <- cleanNames[cleanNames != "class"]
      }
      if (any(cleanNames == "row.names") &&  length(object$attr_row.names) ==  0) {
        object$attr_row.names <- as.character(c())
      }
      attributes(object)[cleanNames] <- object[attrNames]
      object[attrNames] <- NULL
    }
  }
  return(object)
}

.convertEmptyListToLogical <- function(object) {
  # To mimic FeatureExtraction covariate settings behavior: empty vectors are logical
  if (is.list(object)) {
    if (length(object) > 0) {
      for (i in 1:length(object)) {
        if (!is.null(object[[i]])) {
          object[[i]] <- .convertEmptyListToLogical(object[[i]])
        }
      }

      idx <-sapply(object, function(x) is.list(x) && length(x) == 0)
      object[idx] <- sapply(object[idx], as.logical)
    }
  }
  return(object)
}

#' Create a parameter object for the function [getDbCohortMethodData()]
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param studyStartDate  A calendar date specifying the minimum date that a cohort index date can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate  A calendar date specifying the maximum date that a cohort index date can appear. Date format is 'yyyymmdd'. Important: the study end data is also used to truncate risk windows, meaning no outcomes beyond the study end date will be considered.
#' @param firstExposureOnly  Should only the first exposure per subject be included? Note that this is typically done in the createStudyPopulation() function, but can already be done here for efficiency reasons.
#' @param removeDuplicateSubjects  Remove subjects that are in both the target and comparator cohort? See details for allowed values.Note that this is typically done in the createStudyPopulation function, but can already be done here for efficiency reasons.
#' @param restrictToCommonPeriod  Restrict the analysis to the period when both treatments are observed?
#' @param washoutPeriod  The minimum required continuous observation time prior to index date for a person to be included in the cohort. Note that this is typically done in the createStudyPopulation function, but can already be done here for efficiency reasons.
#' @param maxCohortSize  If either the target or the comparator cohort is larger than this number it will be sampled to this size. maxCohortSize = 0 indicates no maximum size.
#' @param covariateSettings  An object of type covariateSettings as created using the FeatureExtraction::createCovariateSettings() function, or a list of covariate settings objects.
#'
#' @return
#' An object of type `GetDbCohortMethodDataArgs`.
#'
#' @export
createGetDbCohortMethodDataArgs <- function(studyStartDate = "",
                                            studyEndDate = "",
                                            firstExposureOnly = FALSE,
                                            removeDuplicateSubjects = "keep all",
                                            restrictToCommonPeriod = FALSE,
                                            washoutPeriod = 0,
                                            maxCohortSize = 0,
                                            covariateSettings) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(GetDbCohortMethodDataArgs$new(typedList = args))
}

GetDbCohortMethodDataArgs <- R6Class(
  "GetDbCohortMethodDataArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    studyStartDate = NULL,
    studyEndDate = NULL,
    firstExposureOnly = NULL,
    removeDuplicateSubjects = NULL,
    restrictToCommonPeriod = NULL,
    washoutPeriod = NULL,
    maxCohortSize = NULL,
    covariateSettings = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(self$studyStartDate, len = 1, add = errorMessages)
      checkmate::assertCharacter(self$studyEndDate, len = 1, add = errorMessages)
      checkmate::assertLogical(self$firstExposureOnly, len = 1, add = errorMessages)
      checkmate::assertChoice(self$removeDuplicateSubjects, c("keep all", "keep first", "remove all"), add = errorMessages)
      checkmate::assertLogical(self$restrictToCommonPeriod, len = 1, add = errorMessages)
      checkmate::assertInt(self$washoutPeriod, lower = 0, add = errorMessages)
      checkmate::assertInt(self$maxCohortSize, lower = 0, add = errorMessages)
      checkmate::assertList(self$covariateSettings, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      .dateCheck(self$studyStartDate)
      .dateCheck(self$studyEndDate)
    },
    toList = function() {
      asList <- super$toList()
      asList$covariateSettings <- .convertAttrToMember(self$covariateSettings)
      return(asList)
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        self$covariateSettings <- .convertEmptyListToLogical(.convertMemberToAttr(list$covariateSettings))
      }
    }
  )
)

#' Create a parameter object for the function [createStudyPopulation()]
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param firstExposureOnly  Should only the first exposure per subject be included?
#' @param restrictToCommonPeriod  Restrict the analysis to the period when both exposures are observed?
#' @param washoutPeriod  The minimum required continuous observation time prior to index date for a person to be included in the cohort.
#' @param removeDuplicateSubjects  Remove subjects that are in both the target and comparator cohort? See details for allowed values.
#' @param removeSubjectsWithPriorOutcome  Remove subjects that have the outcome prior to the risk window start?
#' @param priorOutcomeLookback  How many days should we look back when identifying prior outcomes?
#' @param minDaysAtRisk  The minimum required number of days at risk. Risk windows with fewer days than this number are removed from the analysis.
#' @param maxDaysAtRisk  The maximum allowed number of days at risk. Risk windows that are longer will be truncated to this number of days.
#' @param riskWindowStart  The start of the risk window (in days) relative to the startAnchor.
#' @param startAnchor  The anchor point for the start of the risk window. Can be "cohort start" or "cohort end".
#' @param riskWindowEnd  The end of the risk window (in days) relative to the endAnchor.
#' @param endAnchor  The anchor point for the end of the risk window. Can be "cohort start" or "cohort end".
#' @param censorAtNewRiskWindow  If a subject is in multiple cohorts, should time-at-risk be censored when the new time-at-risk starts to prevent overlap?
#'
#' @return
#' An object of type `CreateStudyPopulationArgs`.
#'
#' @export
createCreateStudyPopulationArgs <- function(firstExposureOnly = FALSE,
                                            restrictToCommonPeriod = FALSE,
                                            washoutPeriod = 0,
                                            removeDuplicateSubjects = "keep all",
                                            removeSubjectsWithPriorOutcome = TRUE,
                                            priorOutcomeLookback = 99999,
                                            minDaysAtRisk = 1,
                                            maxDaysAtRisk = 99999,
                                            riskWindowStart = 0,
                                            startAnchor = "cohort start",
                                            riskWindowEnd = 0,
                                            endAnchor = "cohort end",
                                            censorAtNewRiskWindow = FALSE) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(CreateStudyPopulationArgs$new(typedList = args))
}

CreateStudyPopulationArgs <- R6Class(
  "CreateStudyPopulationArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    firstExposureOnly = NULL,
    restrictToCommonPeriod = NULL,
    washoutPeriod = NULL,
    removeDuplicateSubjects = NULL,
    removeSubjectsWithPriorOutcome = NULL,
    priorOutcomeLookback = NULL,
    minDaysAtRisk = NULL,
    maxDaysAtRisk = NULL,
    riskWindowStart = NULL,
    startAnchor = NULL,
    riskWindowEnd = NULL,
    endAnchor = NULL,
    censorAtNewRiskWindow = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertLogical(self$firstExposureOnly, len = 1, add = errorMessages)
      checkmate::assertLogical(self$restrictToCommonPeriod, len = 1, add = errorMessages)
      checkmate::assertInt(self$washoutPeriod, lower = 0, add = errorMessages)
      checkmate::assertChoice(self$removeDuplicateSubjects, c("keep all", "keep first", "remove all"), add = errorMessages)
      checkmate::assertLogical(self$removeSubjectsWithPriorOutcome, len = 1, add = errorMessages)
      checkmate::assertInt(self$priorOutcomeLookback, lower = 0, add = errorMessages)
      checkmate::assertInt(self$minDaysAtRisk, lower = 0, add = errorMessages)
      checkmate::assertInt(self$maxDaysAtRisk, lower = 0, add = errorMessages)
      checkmate::assertInt(self$riskWindowStart, add = errorMessages)
      checkmate::assertInt(self$riskWindowEnd, add = errorMessages)
      checkmate::assertLogical(self$censorAtNewRiskWindow, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (!grepl("start$|end$", self$startAnchor, ignore.case = TRUE)) {
        stop("startAnchor should have value 'cohort start' or 'cohort end'")
      }
      if (!grepl("start$|end$", self$endAnchor, ignore.case = TRUE)) {
        stop("endAnchor should have value 'cohort start' or 'cohort end'")
      }
    }
  )
)

#' Create a parameter object for the function [createPs()]
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param excludeCovariateIds  Exclude these covariates from the propensity model.
#' @param includeCovariateIds  Include only these covariates in the propensity model.
#' @param maxCohortSizeForFitting  If the target or comparator cohort are larger than this number, they will be downsampled before fitting the propensity model. The model will be used to compute propensity scores for all subjects. The purpose of the sampling is to gain speed. Setting this number to 0 means no downsampling will be applied.
#' @param errorOnHighCorrelation  If true, the function will test each covariate for correlation with the treatment assignment. If any covariate has an unusually high correlation (either positive or negative), this will throw and error.
#' @param stopOnError  If an error occur, should the function stop? Else, the two cohorts will be assumed to be perfectly separable.
#' @param prior  The prior used to fit the model. See Cyclops::createPrior() for details.
#' @param control  The control object used to control the cross-validation used to determine the hyperparameters of the prior (if applicable). See Cyclops::createControl() for details.
#' @param estimator  The type of estimator for the IPTW. Options are estimator = "ate" for the average treatment effect, estimator = "att" for the average treatment effect in the treated, and estimator = "ato" for the average treatment effect in the overlap population.
#'
#' @return
#' An object of type `CreatePsArgs`.
#'
#' @export
createCreatePsArgs <- function(excludeCovariateIds = c(),
                               includeCovariateIds = c(),
                               maxCohortSizeForFitting = 250000,
                               errorOnHighCorrelation = TRUE,
                               stopOnError = TRUE,
                               prior = createPrior(priorType = "laplace",
                                                   exclude = c(0),
                                                   useCrossValidation = TRUE),
                               control = createControl(noiseLevel = "silent",
                                                       cvType = "auto",
                                                       seed = 1,
                                                       resetCoefficients = TRUE,
                                                       tolerance = 2e-07,
                                                       cvRepetitions = 10,
                                                       startingVariance = 0.01),
                               estimator = "att") {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(CreatePsArgs$new(typedList = args))
}

CreatePsArgs <- R6Class(
  "CreatePsArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    excludeCovariateIds = NULL,
    includeCovariateIds = NULL,
    maxCohortSizeForFitting = NULL,
    errorOnHighCorrelation = NULL,
    stopOnError = NULL,
    prior = NULL,
    control = NULL,
    estimator = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      .assertCovariateId(self$excludeCovariateIds, null.ok = TRUE, add = errorMessages)
      .assertCovariateId(self$includeCovariateIds, null.ok = TRUE, add = errorMessages)
      checkmate::assertInt(self$maxCohortSizeForFitting, lower = 0, add = errorMessages)
      checkmate::assertLogical(self$errorOnHighCorrelation, len = 1, add = errorMessages)
      checkmate::assertLogical(self$stopOnError, len = 1, add = errorMessages)
      checkmate::assertClass(self$prior, "cyclopsPrior", add = errorMessages)
      checkmate::assertClass(self$control, "cyclopsControl", add = errorMessages)
      checkmate::assertChoice(self$estimator, c("ate", "att", "ato"), add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        class(self$control) <- "cyclopsControl"
        class(self$prior) <- "cyclopsPrior"
      }
    }
  )
)

#' Create a parameter object for the function [trimByPs()]
#'
#' @details
#' Create an object defining the parameter values. Set any argument to `NULL` to not use it for
#' trimming.
#'
#' @param trimFraction  This fraction will be removed from each treatment group. In the target
#'                      group, persons with the highest propensity scores will be removed, in the
#'                      comparator group person with the lowest scores will be removed.
#' @param equipoiseBounds A 2-dimensional numeric vector containing the upper and lower bound on the
#'                        preference score (Walker, 201) for keeping persons.
#' @param maxWeight     The maximum allowed IPTW.
#'
#' @references
#' Walker AM, Patrick AR, Lauer MS, Hornbrook MC, Marin MG, Platt R, Roger VL, Stang P, and
#' Schneeweiss S. (2013) A tool for assessing the feasibility of comparative effectiveness research,
#' Comparative Effective Research, 3, 11-20
#'
#' @return
#' An object of type `TrimByPsArgs`.
#'
#' @export
createTrimByPsArgs <- function(trimFraction = NULL,
                               equipoiseBounds = NULL,
                               maxWeight = NULL) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(TrimByPsArgs$new(typedList = args))
}

TrimByPsArgs <- R6Class(
  "TrimByPsArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    trimFraction = NULL,
    equipoiseBounds = NULL,
    maxWeight = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertNumber(self$trimFraction, null.ok = TRUE, lower = 0, upper = 1, add = errorMessages)
      checkmate::assertNumeric(self$equipoiseBounds, null.ok = TRUE, len = 2, lower = 0, upper = 1, add = errorMessages)
      checkmate::assertNumber(self$maxWeight, null.ok = TRUE, lower = 0, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (is.null(self$trimFraction) && is.null(self$equipoiseBounds) && is.null(self$maxWeight)) {
        stop("Must specify at least one of trimFraction, equipoiseBounds, or maxWeight")
      }
    }
  )
)

#' Create a parameter object for the function [truncateIptw()]
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param maxWeight  The maximum allowed IPTW.
#'
#' @return
#' An object of type `TruncateIptwArgs`.
#'
#' @export
createTruncateIptwArgs <- function(maxWeight = 10) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(TruncateIptwArgs$new(typedList = args))
}

TruncateIptwArgs <- R6Class(
  "TruncateIptwArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    maxWeight = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertNumber(self$maxWeight, lower = 0, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    }
  )
)

#' Create a parameter object for the function [matchOnPs()]
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param caliper                 The caliper for matching. A caliper is the distance which is
#'                                acceptable for any match. Observations which are outside of the
#'                                caliper are dropped. A caliper of 0 means no caliper is used.
#' @param caliperScale            The scale on which the caliper is defined. Three scales are supported:
#'                                `caliperScale = 'propensity score'`,
#'                                `caliperScale = 'standardized'`, or
#'                                `caliperScale = 'standardized logit'`.
#'                                On the standardized scale, the caliper is interpreted in standard
#'                                deviations of the propensity score distribution. 'standardized logit'
#'                                is similar, except that the propensity score is transformed to the logit
#'                                scale because the PS is more likely to be normally distributed on that scale
#'                                (Austin, 2011).
#' @param maxRatio                The maximum number of persons in the comparator arm to be matched to
#'                                each person in the treatment arm. A `maxRatio` of 0 means no maximum:
#'                                all comparators will be assigned to a target person.
#' @param allowReverseMatch       Allows n-to-1 matching if target arm is larger
#' @param stratificationColumns   Names or numbers of one or more columns in the `data` data.frame
#'                                on which subjects should be stratified prior to matching. No persons
#'                                will be matched with persons outside of the strata identified by the
#'                                values in these columns.
#' @param stratificationCovariateIds One or more covariate IDs in the `cohortMethodData` object on which
#'                                   subjects should be also matched.
#'
#' @references
#' Austin, PC. (2011) Optimal caliper widths for propensity-score matching when estimating differences in
#' means and differences in proportions in observational studies, Pharmaceutical statistics, March, 10(2):150-161.
#'
#' @return
#' An object of type `MatchOnPsArgs`.
#'
#' @export
createMatchOnPsArgs <- function(caliper = 0.2,
                                caliperScale = "standardized logit",
                                maxRatio = 1,
                                allowReverseMatch = FALSE,
                                stratificationColumns = c(),
                                stratificationCovariateIds = c()) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(MatchOnPsArgs$new(typedList = args))
}

MatchOnPsArgs <- R6Class(
  "MatchOnPsArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    caliper = NULL,
    caliperScale = NULL,
    maxRatio = NULL,
    allowReverseMatch = NULL,
    stratificationColumns = NULL,
    stratificationCovariateIds = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertNumber(self$caliper, lower = 0, add = errorMessages)
      checkmate::assertChoice(self$caliperScale, c("standardized", "propensity score", "standardized logit"), add = errorMessages)
      checkmate::assertInt(self$maxRatio, lower = 0, add = errorMessages)
      checkmate::assertLogical(self$allowReverseMatch, len = 1, add = errorMessages)
      checkmate::assertCharacter(self$stratificationColumns, null.ok = TRUE, add = errorMessages)
      .assertCovariateId(self$stratificationCovariateIds, null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (length(self$stratificationColumns) != 0 && length(self$stratificationCovariateIds)) {
        stop("Cannot specify both stratificationColumns and stratificationCovariateIds")
      }
    }
  )
)

#' Create a parameter object for the function [stratifyByPs()]
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param numberOfStrata          How many strata? The boundaries of the strata are automatically
#'                                defined to contain equal numbers of target persons.
#' @param baseSelection           What is the base selection of subjects where the strata bounds are
#'                                to be determined? Strata are defined as equally-sized strata inside
#'                                this selection. Possible values are "all", "target", and "comparator".
#' @param stratificationColumns   Names or numbers of one or more columns in the `data` data.frame
#'                                on which subjects should be stratified prior to matching. No persons
#'                                will be matched with persons outside of the strata identified by the
#'                                values in these columns.
#' @param stratificationCovariateIds One or more covariate IDs in the `cohortMethodData` object on which
#'                                   subjects should also be stratified.
#'
#'
#'
#' @return
#' An object of type `StratifyByPsArgs`.
#'
#' @export
createStratifyByPsArgs <- function(numberOfStrata = 5,
                                   baseSelection = "all",
                                   stratificationColumns = c(),
                                   stratificationCovariateIds = c()) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(StratifyByPsArgs$new(typedList = args))
}

StratifyByPsArgs <- R6Class(
  "StratifyByPsArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    numberOfStrata = NULL,
    baseSelection = NULL,
    stratificationColumns = NULL,
    stratificationCovariateIds = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertInt(self$numberOfStrata, lower = 1, add = errorMessages)
      checkmate::assertCharacter(self$stratificationColumns, null.ok = TRUE, add = errorMessages)
      checkmate::assertChoice(self$baseSelection, c("all", "target", "comparator"), add = errorMessages)
      checkmate::assertCharacter(self$stratificationColumns, null.ok = TRUE, add = errorMessages)
      .assertCovariateId(self$stratificationCovariateIds, null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    }
  )
)

#' Create a parameter object for the function [computeCovariateBalance()]
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param subgroupCovariateId  Optional: a covariate ID of a binary covariate that indicates a subgroup of
#'                             interest. Both the before and after populations will be restricted to this
#'                             subgroup before computing covariate balance.
#' @param maxCohortSize  If the target or comparator cohort are larger than this number, they
#'                                 will be downsampled before computing covariate balance to save time.
#'                                 Setting this number to 0 means no downsampling will be applied.
#' @param covariateFilter   Determines the covariates for which to compute covariate balance. Either a vector
#'                          of covariate IDs, or a table 1 specifications object as generated for example using
#'                          [FeatureExtraction::getDefaultTable1Specifications()]. If `covariateFilter = NULL`,
#'                          balance will be computed for all variables found in the data.
#' @param threshold   Threshold value for the absolute value of the standardized difference of means (ASDM).
#'                    If the ASDM exceeds this threshold it will be marked as unbalanced. (Hripcsak et al. 2025)
#' @param alpha       The family-wise alpha for testing whether the absolute value of the standardized
#'                    difference of means is greater than the threshold. If not provided, any value greater
#'                    than the threshold will be marked as unbalanced.
#'
#' @references
#' Hripcsak G, Zhang L, Chen Y, Li K, Suchard MA, Ryan PB, Schuemie MJ (2025)
#' Assessing Covariate Balance with Small Sample Sizes. medRxiv. Feb 21:2024.04.23.24306230.
#'
#' @return
#' An object of type `ComputeCovariateBalanceArgs`.
#'
#' @export
createComputeCovariateBalanceArgs <- function(subgroupCovariateId = NULL,
                                              maxCohortSize = 250000,
                                              covariateFilter = NULL,
                                              threshold = 0.1,
                                              alpha = 0.05) {
  # FeatureExtraction::createTable1CovariateSettings() uses vroom which creates a weird table object
  # with many attributes. Best to cast to tibble here so all unit tests work.
  if (!is.null(covariateFilter)) {
    covariateFilter <- as_tibble(covariateFilter)
  }
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(ComputeCovariateBalanceArgs$new(typedList = args))
}

ComputeCovariateBalanceArgs <- R6Class(
  "ComputeCovariateBalanceArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    subgroupCovariateId = NULL,
    maxCohortSize = NULL,
    covariateFilter = NULL,
    threshold = NULL,
    alpha = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      .assertCovariateId(self$subgroupCovariateId, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertInt(self$maxCohortSize, lower = 0, add = errorMessages)
      if (is.numeric(self$covariateFilter)) {
        checkmate::assertIntegerish(self$covariateFilter, add = errorMessages)
      } else if (!is.null(self$covariateFilter)) {
        checkmate::assertDataFrame(self$covariateFilter, add = errorMessages)
        checkmate::assertNames(colnames(self$covariateFilter), must.include = c("analysisId", "covariateIds"), add = errorMessages)
      }
      checkmate::assertNumber(self$threshold, lower = 0, add = errorMessages)
      checkmate::assertNumber(self$alpha, lower = 0, null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        if (!is.null(self$covariateFilter)) {
          self$covariateFilter <- as_tibble(list$covariateFilter)
        }
      }
    }
  )
)

#' Create a parameter object for the function [fitOutcomeModel()]
#'
#' @details
#' Create an object defining the parameter values.
#'
#' For likelihood profiling, either specify the `profileGrid` for a completely user- defined grid, or
#' `profileBounds` for an adaptive grid. Both should be defined on the log effect size scale. When both
#' `profileGrid` and `profileGrid` are `NULL` likelihood profiling is disabled.
#'
#' @param modelType             The type of outcome model that will be used. Possible values are
#'                              "logistic", "poisson", or "cox".
#' @param stratified            Should the regression be conditioned on the strata defined in the
#'                              population object (e.g. by matching or stratifying on propensity
#'                              scores)?
#' @param useCovariates         Whether to use the covariates in the `cohortMethodData`
#'                              object in the outcome model.
#' @param inversePtWeighting    Use inverse probability of treatment weighting (IPTW)
#' @param bootstrapCi           Compute confidence interval using bootstrapping instead of likelihood profiling?
#' @param bootstrapReplicates   When using bootstrapping to compute confidence intervals, how many replicates
#'                              should be sampled?
#' @param interactionCovariateIds  An optional vector of covariate IDs to use to estimate interactions
#'                                 with the main treatment effect.
#' @param excludeCovariateIds   Exclude these covariates from the outcome model.
#' @param includeCovariateIds   Include only these covariates in the outcome model.
#' @param profileGrid           A one-dimensional grid of points on the log(relative risk) scale where
#'                              the likelihood for coefficient of variables is sampled. See details.
#' @param profileBounds         The bounds (on the log relative risk scale) for the adaptive sampling
#'                              of the likelihood function. See details.
#' @param prior                 The prior used to fit the model. See [Cyclops::createPrior()]
#'                              for details. The prior is only applied to non-treatment variables,
#'                              so is not used when `useCovariates = FALSE`.
#' @param control               The control object used to control the cross-validation used to
#'                              determine the hyperparameters of the prior (if applicable). See
#'                              [Cyclops::createControl()] for details.
#'
#' @return
#' An object of type `ComputeCovariateBalanceArgs`.
#'
#' @export
createFitOutcomeModelArgs <- function(modelType = "cox",
                                      stratified = FALSE,
                                      useCovariates = FALSE,
                                      inversePtWeighting = FALSE,
                                      bootstrapCi = FALSE,
                                      bootstrapReplicates = 1000,
                                      interactionCovariateIds = c(),
                                      excludeCovariateIds = c(),
                                      includeCovariateIds = c(),
                                      profileGrid = NULL,
                                      profileBounds = c(log(0.1), log(10)),
                                      prior = createPrior(priorType = "laplace",
                                                          useCrossValidation = TRUE),
                                      control = createControl(cvType = "auto",
                                                              seed = 1,
                                                              resetCoefficients = TRUE,
                                                              startingVariance = 0.01,
                                                              tolerance = 2e-07,
                                                              cvRepetitions = 10,
                                                              noiseLevel = "quiet")) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(FitOutcomeModelArgs$new(typedList = args))
}

FitOutcomeModelArgs <- R6Class(
  "FitOutcomeModelArgs",
  inherit = AbstractSerializableSettings,
  public = list(
    modelType = NULL,
    stratified = NULL,
    useCovariates = NULL,
    inversePtWeighting = NULL,
    bootstrapCi = NULL,
    bootstrapReplicates = NULL,
    interactionCovariateIds = NULL,
    excludeCovariateIds = NULL,
    includeCovariateIds = NULL,
    profileGrid = NULL,
    profileBounds = NULL,
    prior = NULL,
    control = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertChoice(self$modelType, c("logistic", "poisson", "cox"), add = errorMessages)
      checkmate::assertLogical(self$stratified, len = 1, add = errorMessages)
      checkmate::assertLogical(self$useCovariates, len = 1, add = errorMessages)
      checkmate::assertLogical(self$inversePtWeighting, len = 1, add = errorMessages)
      checkmate::assertLogical(self$bootstrapCi, len = 1, add = errorMessages)
      checkmate::assert_int(self$bootstrapReplicates, lower = 1, add = errorMessages)
      .assertCovariateId(self$interactionCovariateIds, null.ok = TRUE, add = errorMessages)
      .assertCovariateId(self$excludeCovariateIds, null.ok = TRUE, add = errorMessages)
      .assertCovariateId(self$includeCovariateIds, null.ok = TRUE, add = errorMessages)
      checkmate::assertNumeric(self$profileGrid, null.ok = TRUE, add = errorMessages)
      checkmate::assertNumeric(self$profileBounds, null.ok = TRUE, len = 2, add = errorMessages)
      checkmate::assertClass(self$prior, "cyclopsPrior", add = errorMessages)
      checkmate::assertClass(self$control, "cyclopsControl", add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (any(self$excludeCovariateIds %in% self$interactionCovariateIds)) {
        stop("Can't exclude covariates that are to be used for interaction terms")
      }
      if (any(self$includeCovariateIds %in% self$excludeCovariateIds)) {
        stop("Can't exclude covariates that are to be included")
      }
      if (!is.null(self$profileGrid) && !is.null(self$profileBounds)) {
        stop("Can't specify both a grid and bounds for likelihood profiling")
      }
      if (self$bootstrapCi && !is.null(self$interactionCovariateIds)) {
        stop("Bootstrap confidence intervals has not been implemented for interactions")
      }
      if (self$bootstrapCi && self$useCovariates) {
        stop("Bootstrap confidence intervals has not been implemented for models including other covariates")
      }
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        class(self$control) <- "cyclopsControl"
        class(self$prior) <- "cyclopsPrior"
      }
    }
  )
)

#' Create a CohortMethod analysis specification
#'
#' @details
#' Create a set of analysis choices, to be used with the [runCmAnalyses()] function.
#'
#' Providing a NULL value for any of the argument applies the corresponding step will not be executed.
#' For example, if `createPsArgs = NULL`, no propensity scores will be computed.
#'
#' @param analysisId                      An integer that will be used later to refer to this specific
#'                                        set of analysis choices.
#' @param description                     A short description of the analysis.
#' @param getDbCohortMethodDataArgs       An object representing the arguments to be used when calling
#'                                        the [getDbCohortMethodData()] function.
#' @param createStudyPopArgs              An object representing the arguments to be used when calling
#'                                        the [createStudyPopulation()] function.
#' @param createPsArgs                    An object representing the arguments to be used when calling
#'                                        the [createPs()] function.
#' @param trimByPsArgs                    An object representing the arguments to be used when calling
#'                                        the [trimByPs()] function.
#' @param truncateIptwArgs                An object representing the arguments to be used when calling
#'                                        the [truncateIptw()] function.
#' @param matchOnPsArgs                   An object representing the arguments to be used when calling
#'                                        the [matchOnPs()] function.
#' @param stratifyByPsArgs                An object representing the arguments to be used when calling
#'                                        the [stratifyByPs()] function.
#' @param computeSharedCovariateBalanceArgs  An object representing the arguments to be used when calling
#'                                          the [computeCovariateBalance()] function per target-comparator-analysis.
#' @param computeCovariateBalanceArgs     An object representing the arguments to be used when calling
#'                                        the [computeCovariateBalance()] function per target-comparator-outcome-analysis.
#' @param fitOutcomeModelArgs             An object representing the arguments to be used when calling
#'                                        the [fitOutcomeModel()] function.
#'
#' @return
#' An object of type `CmAnalysis`, to be used with the [runCmAnalyses] function.
#'
#' @export
createCmAnalysis <- function(analysisId = 1,
                             description = "",
                             getDbCohortMethodDataArgs,
                             createStudyPopulationArgs,
                             createPsArgs = NULL,
                             trimByPsArgs = NULL,
                             truncateIptwArgs = NULL,
                             matchOnPsArgs = NULL,
                             stratifyByPsArgs = NULL,
                             computeSharedCovariateBalanceArgs = NULL,
                             computeCovariateBalanceArgs = NULL,
                             fitOutcomeModelArgs = NULL) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(CmAnalysis$new(typedList = args))
}

CmAnalysis <- R6Class(
  "CmAnalysis",
  inherit = AbstractSerializableSettings,
  public = list(
    analysisId = NULL,
    description = NULL,
    getDbCohortMethodDataArgs = NULL,
    createStudyPopulationArgs = NULL,
    createPsArgs = NULL,
    trimByPsArgs = NULL,
    truncateIptwArgs = NULL,
    matchOnPsArgs = NULL,
    stratifyByPsArgs = NULL,
    computeSharedCovariateBalanceArgs = NULL,
    computeCovariateBalanceArgs = NULL,
    fitOutcomeModelArgs = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertInt(self$analysisId, add = errorMessages)
      checkmate::assertCharacter(self$description, len = 1, add = errorMessages)
      checkmate::assertR6(self$getDbCohortMethodDataArgs, "GetDbCohortMethodDataArgs", add = errorMessages)
      checkmate::assertR6(self$createStudyPopulationArgs, "CreateStudyPopulationArgs", add = errorMessages)
      checkmate::assertR6(self$createPsArgs, "CreatePsArgs", null.ok = TRUE, add = errorMessages)
      checkmate::assertR6(self$trimByPsArgs, "TrimByPsArgs", null.ok = TRUE, add = errorMessages)
      checkmate::assertR6(self$truncateIptwArgs, "TruncateIptwArgs", null.ok = TRUE, add = errorMessages)
      checkmate::assertR6(self$matchOnPsArgs, "MatchOnPsArgs", null.ok = TRUE, add = errorMessages)
      checkmate::assertR6(self$stratifyByPsArgs, "StratifyByPsArgs", null.ok = TRUE, add = errorMessages)
      checkmate::assertR6(self$computeSharedCovariateBalanceArgs, "ComputeCovariateBalanceArgs", null.ok = TRUE, add = errorMessages)
      checkmate::assertR6(self$computeCovariateBalanceArgs, "ComputeCovariateBalanceArgs", null.ok = TRUE, add = errorMessages)
      checkmate::assertR6(self$fitOutcomeModelArgs, "FitOutcomeModelArgs", null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if ((!is.null(self$matchOnPsArgs)) +
          (!is.null(self$matchOnPsAndCovariatesArgs)) +
          (!is.null(self$stratifyByPsArgs)) +
          (!is.null(self$stratifyByPsAndCovariatesArgs)) > 1) {
        stop("Need to pick one matching or stratification function")
      }
      if (is.null(self$createPsArgs) && (!is.null(self$trimByPsArgs) |
                                         !is.null(self$truncateIptwArgs) |
                                         !is.null(self$matchOnPsArgs) |
                                         !is.null(self$stratifyByPsArgs) )) {
        stop("Must create propensity score model to use it for trimming, matching, or stratification")
      }
      if (!is.null(self$fitOutcomeModelArgs) &&
          self$fitOutcomeModelArgs$stratified && (is.null(self$matchOnPsArgs) &
                                                  is.null(self$matchOnPsAndCovariatesArgs) &
                                                  is.null(self$stratifyByPsArgs) &
                                                  is.null(self$stratifyByPsAndCovariatesArgs))) {
        stop("Must create strata by using matching or stratification to fit a stratified outcome model")
      }
      if (!is.null(self$createPsArgs) &&
          (is.null(self$computeSharedCovariateBalanceArgs) &&
           is.null(self$computeCovariateBalanceArgs))) {
        message("Note: Using propensity scores but not computing covariate balance")
      }
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        self$getDbCohortMethodDataArgs <- GetDbCohortMethodDataArgs$new(untypedList = self$getDbCohortMethodDataArgs)
        self$createStudyPopulationArgs <- CreateStudyPopulationArgs$new(untypedList = self$createStudyPopulationArgs)
        if (!is.null(self$createPsArgs)) {
          self$createPsArgs <- CreatePsArgs$new(untypedList = self$createPsArgs)
        }
        if (!is.null(self$trimByPsArgs)) {
          self$trimByPsArgs <- TrimByPsArgs$new(untypedList = self$trimByPsArgs)
        }
        if (!is.null(self$truncateIptwArgs)) {
          self$truncateIptwArgs <- TruncateIptwArgs$new(untypedList = self$truncateIptwArgs)
        }
        if (!is.null(self$matchOnPsArgs)) {
          self$matchOnPsArgs <- MatchOnPsArgs$new(untypedList = self$matchOnPsArgs)
        }
        if (!is.null(self$stratifyByPsArgs)) {
          self$stratifyByPsArgs <- StratifyByPsArgs$new(untypedList = self$stratifyByPsArgs)
        }
        if (!is.null(self$computeSharedCovariateBalanceArgs)) {
          self$computeSharedCovariateBalanceArgs <- ComputeCovariateBalanceArgs$new(untypedList = self$computeSharedCovariateBalanceArgs)
        }
        if (!is.null(self$computeCovariateBalanceArgs)) {
          self$computeCovariateBalanceArgs <- ComputeCovariateBalanceArgs$new(untypedList = self$computeCovariateBalanceArgs)
        }
        if (!is.null(self$fitOutcomeModelArgs)) {
          self$fitOutcomeModelArgs <- FitOutcomeModelArgs$new(untypedList = self$fitOutcomeModelArgs)
        }
      }
    }
  )
)

#' Create outcome definition
#'
#' @param outcomeId                        An integer used to identify the outcome in the outcome cohort table.
#' @param outcomeOfInterest                Is this an outcome of interest? If not, creation of non-essential
#'                                         files will be skipped, including outcome=specific covariate balance
#'                                         files. This could be helpful to speed up analyses with many controls,
#'                                         for which we're only interested in the effect size estimate.
#' @param trueEffectSize                   For negative and positive controls: the known true effect size. To be used
#'                                         for empirical calibration. Negative controls have `trueEffectSize = 1`. If
#'                                         the true effect size is unknown, use `trueEffectSize = NA`
#' @param priorOutcomeLookback             How many days should we look back when identifying prior.
#'                                         outcomes?
#' @param riskWindowStart                  The start of the risk window (in days) relative to the `startAnchor`.
#' @param startAnchor                      The anchor point for the start of the risk window. Can be `"cohort start"`
#'                                         or `"cohort end"`.
#' @param riskWindowEnd                    The end of the risk window (in days) relative to the `endAnchor`.
#' @param endAnchor                        The anchor point for the end of the risk window. Can be `"cohort start"`
#'                                         or `"cohort end"`.
#'
#' @details
#' Any settings here that are not `NULL` will override any values set in [createCreateStudyPopulationArgs()].
#'
#'
#' @return
#' An object of type `Outcome`, to be used in [createTargetComparatorOutcomes()].
#'
#' @export
createOutcome <- function(outcomeId,
                          outcomeOfInterest = TRUE,
                          trueEffectSize = NA,
                          priorOutcomeLookback = NULL,
                          riskWindowStart = NULL,
                          startAnchor = NULL,
                          riskWindowEnd = NULL,
                          endAnchor = NULL) {

  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(Outcome$new(typedList = args))
}

Outcome <- R6Class(
  "Outcome",
  inherit = AbstractSerializableSettings,
  public = list(
    outcomeId = NULL,
    outcomeOfInterest = NULL,
    trueEffectSize = NULL,
    priorOutcomeLookback = NULL,
    riskWindowStart = NULL,
    startAnchor = NULL,
    riskWindowEnd = NULL,
    endAnchor = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertNumeric(self$outcomeId, add = errorMessages)
      checkmate::assertTRUE(all(self$outcomeId %% 1 == 0), add = errorMessages)
      checkmate::assertLogical(self$outcomeOfInterest, add = errorMessages)
      checkmate::assertNumeric(self$trueEffectSize, len = 1, null.ok = TRUE, add = errorMessages)
      checkmate::assertInt(self$riskWindowStart, null.ok = TRUE, add = errorMessages)
      checkmate::assertInt(self$riskWindowEnd, null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (!is.null(self$startAnchor) && !grepl("start$|end$", self$startAnchor, ignore.case = TRUE)) {
        stop("startAnchor should have value 'cohort start' or 'cohort end'")
      }
      if (!is.null(self$riskWindowEnd) && !grepl("start$|end$", self$endAnchor, ignore.case = TRUE)) {
        stop("endAnchor should have value 'cohort start' or 'cohort end'")
      }
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        self$trueEffectSize <- if (is.null(self$trueEffectSize)) NA else self$trueEffectSize
      }
    }
  )
)

#' Create target-comparator-outcomes combinations.
#'
#' @details
#' Create a set of hypotheses of interest, to be used with the [runCmAnalyses()] function.
#'
#' @param targetId                      A cohort ID identifying the target exposure in the exposure
#'                                      table.
#' @param comparatorId                  A cohort ID identifying the comparator exposure in the exposure
#'                                      table.
#' @param outcomes                      A list of object of type `Outcome` as created by
#'                                      [createOutcome()].
#' @param excludedCovariateConceptIds   A list of concept IDs that cannot be used to construct
#'                                      covariates. This argument is to be used only for exclusion
#'                                      concepts that are specific to the target-comparator combination.
#' @param includedCovariateConceptIds   A list of concept IDs that must be used to construct
#'                                      covariates. This argument is to be used only for inclusion
#'                                      concepts that are specific to the target-comparator combination.
#'
#' @return
#' An object of type `TargetComparatorOutcomes`.
#'
#' @export
createTargetComparatorOutcomes <- function(targetId,
                                           comparatorId,
                                           outcomes,
                                           excludedCovariateConceptIds = c(),
                                           includedCovariateConceptIds = c()) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(TargetComparatorOutcomes$new(typedList = args))
}

TargetComparatorOutcomes <- R6Class(
  "TargetComparatorOutcomes",
  inherit = AbstractSerializableSettings,
  public = list(
    targetId = NULL,
    comparatorId = NULL,
    outcomes = NULL,
    excludedCovariateConceptIds = NULL,
    includedCovariateConceptIds = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertNumeric(self$targetId, add = errorMessages)
      checkmate::assertNumeric(self$comparatorId, add = errorMessages)
      checkmate::assertTRUE(all(c(self$targetId, self$comparatorId) %% 1 == 0), add = errorMessages)
      checkmate::assertList(self$outcomes, min.len = 1, add = errorMessages)
      for (i in seq_along(self$outcomes)) {
        checkmate::assertR6(self$outcomes[[i]], "Outcome", add = errorMessages)
      }
      checkmate::assertIntegerish(self$excludedCovariateConceptIds, null.ok = TRUE, add = errorMessages)
      checkmate::assertIntegerish(self$includedCovariateConceptIds, null.ok = TRUE, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      outcomeIds <- rep(0, length(self$outcomes))
      for (i in seq_along(self$outcomes)) {
        outcomeIds[i] <- self$outcomes[[i]]$outcomeId
      }
      duplicatedIds <- outcomeIds[duplicated(outcomeIds)]
      if (length(duplicatedIds) > 0) {
        stop(sprintf("Found duplicate outcome IDs: %s", paste(duplicatedIds, paste = ", ")))
      }
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        for (i in seq_along(self$outcomes)) {
          self$outcomes[[i]] <- Outcome$new(untypedList = self$outcomes[[i]])
        }
      }
    }
  )
)

#' Create CohortMethod diagnostics thresholds
#'
#' @description
#' Threshold used when calling [exportToCsv()] to determine if we pass or fail diagnostics.
#'
#' @param mdrrThreshold         What is the maximum allowed minimum detectable relative risk
#'                              (MDRR)?
#' @param easeThreshold         What is the maximum allowed expected absolute systematic error
#'                              (EASE).
#' @param sdmThreshold          What is the maximum allowed standardized difference of mean (SDM)? If
#'                              any covariate has an SDM exceeding this threshold, the diagnostic will
#'                              fail.
#' @param equipoiseThreshold    What is the minimum required equipoise?
#' @param generalizabilitySdmThreshold What is the maximum allowed standardized difference of mean
#'                                     (SDM)when comparing the population before and after PS
#'                                     adjustments? If the SDM is greater than this value, the diagnostic
#'                                     will fail.
#'
#' @return
#' An object of type `CmDiagnosticThresholds`.
#'
#' @export
createCmDiagnosticThresholds <- function(mdrrThreshold = 10,
                                         easeThreshold = 0.25,
                                         sdmThreshold = 0.1,
                                         equipoiseThreshold = 0.2,
                                         generalizabilitySdmThreshold = 1) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(CmDiagnosticThresholds$new(typedList = args))
}

CmDiagnosticThresholds <- R6Class(
  "CmDiagnosticThresholds",
  inherit = AbstractSerializableSettings,
  public = list(
    mdrrThreshold = NULL,
    easeThreshold = NULL,
    sdmThreshold = NULL,
    equipoiseThreshold = NULL,
    generalizabilitySdmThreshold = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertNumeric(self$mdrrThreshold, len = 1, lower = 0, add = errorMessages)
      checkmate::assertNumeric(self$easeThreshold, len = 1, lower = 0, add = errorMessages)
      checkmate::assertNumeric(self$sdmThreshold, len = 1, lower = 0, add = errorMessages)
      checkmate::assertNumeric(self$equipoiseThreshold, len = 1, lower = 0, add = errorMessages)
      checkmate::assertNumeric(self$generalizabilitySdmThreshold, len = 1, lower = 0, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
    }
  )
)

#' Create full CM analysis specifications
#'
#' @param cmAnalysisList                 A list of objects of type `cmAnalysis` as created using
#'                                       the `[createCmAnalysis] function.
#' @param targetComparatorOutcomesList   A list of objects of type `targetComparatorOutcomes` as
#'                                       created using the [createTargetComparatorOutcomes]
#'                                       function.
#' @param analysesToExclude              Analyses to exclude. See the Analyses to Exclude section for details.
#' @param refitPsForEveryOutcome         Should the propensity model be fitted for every outcome (i.e.
#'                                       after people who already had the outcome are removed)? If
#'                                       false, a single propensity model will be fitted, and people
#'                                       who had the outcome previously will be removed afterwards.
#' @param refitPsForEveryStudyPopulation Should the propensity model be fitted for every study population
#'                                       definition? If false, a single propensity model will be fitted,
#'                                       and the study population criteria will be applied afterwards.
#' @param cmDiagnosticThresholds         An object of type `CmDiagnosticThresholds` as created using
#'                                       [createCmDiagnosticThresholds()].
#'
#' @details
#' ## Analyses to Exclude
#'
#' Normally, `runCmAnalyses` will run all combinations of target-comparator-outcome-analyses settings.
#' However, sometimes we may not need all those combinations. Using the `analysesToExclude` argument,
#' we can remove certain items from the full matrix. This argument should be a data frame with at least
#' one of the following columns:
#'
#' - targetId
#' - comparatorId
#' - outcomeId
#' - analysisId
#'
#' This data frame will be joined to the outcome model reference table before executing, and matching rows
#' will be removed. For example, if one specifies only one target ID and analysis ID, then any analyses with
#' that target and that analysis ID will be skipped.
#'
#' @returns
#' An object of type `CmAnalysesSpecifications`.
#'
#' @export
createCmAnalysesSpecifications <- function(cmAnalysisList,
                                           targetComparatorOutcomesList,
                                           analysesToExclude = NULL,
                                           refitPsForEveryOutcome = FALSE,
                                           refitPsForEveryStudyPopulation = TRUE,
                                           cmDiagnosticThresholds = createCmDiagnosticThresholds()) {
  args <- list()
  for (name in names(formals())) {
    args[[name]] <- get(name)
  }
  return(CmAnalysesSpecifications$new(typedList = args))
}

CmAnalysesSpecifications <- R6Class(
  "CmAnalysesSpecifications",
  inherit = AbstractSerializableSettings,
  public = list(
    cmAnalysisList = NULL,
    targetComparatorOutcomesList = NULL,
    analysesToExclude = NULL,
    refitPsForEveryOutcome = NULL,
    refitPsForEveryStudyPopulation = TRUE,
    cmDiagnosticThresholds = NULL,
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertList(self$cmAnalysisList, min.len = 1, add = errorMessages)
      for (i in seq_along(self$cmAnalysisList)) {
        checkmate::assertR6(self$cmAnalysisList[[i]], "CmAnalysis", add = errorMessages)
      }
      checkmate::assertList(self$targetComparatorOutcomesList, min.len = 1, add = errorMessages)
      for (i in 1:length(self$targetComparatorOutcomesList)) {
        checkmate::assertR6(self$targetComparatorOutcomesList[[i]], "TargetComparatorOutcomes", add = errorMessages)
      }
      checkmate::assertDataFrame(self$analysesToExclude, null.ok = TRUE, add = errorMessages)
      checkmate::assertLogical(self$refitPsForEveryOutcome, len = 1, add = errorMessages)
      checkmate::assertLogical(self$refitPsForEveryStudyPopulation, len = 1, add = errorMessages)
      checkmate::assertR6(self$cmDiagnosticThresholds, "CmDiagnosticThresholds", add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      if (!is.null(self$analysesToExclude)) {
        if (nrow(self$analysesToExclude) == 0) {
          warning("Passed `data.frame` with 0 rows to parameter: `analysesToExclude`, no analyses excluded.")
        }
        if (!any(c("targetId", "outcomeId", "analysisId") %in% colnames(self$analysesToExclude))) {
          stop("AnalysesToExclude should have at least one of these columns: 'targetId', 'outcomeId', or 'analysisId'")
        }
      }
      uniquetargetComparatorOutcomesList <- unique(ParallelLogger::selectFromList(
        self$targetComparatorOutcomesList,
        c(
          "targetId",
          "comparatorId",
          "outcomes"
        )
      ))
      if (length(uniquetargetComparatorOutcomesList) != length(self$targetComparatorOutcomesList)) {
        stop("Duplicate target-comparator-outcomes combinations are not allowed")
      }
      analysisIds <- unlist(ParallelLogger::selectFromList(self$cmAnalysisList, "analysisId"))
      uniqueAnalysisIds <- unique(analysisIds)
      if (length(uniqueAnalysisIds) != length(analysisIds)) {
        stop("Duplicate analysis IDs are not allowed")
      }
      if (!self$refitPsForEveryStudyPopulation && self$refitPsForEveryOutcome) {
        stop("Cannot have refitPsForEveryStudyPopulation = FALSE and refitPsForEveryOutcome = TRUE")
      }
    },
    fromList = function(list, requireTyping) {
      super$fromList(list)
      if (requireTyping) {
        for (i in seq_along(self$cmAnalysisList)) {
          self$cmAnalysisList[[i]] <- CmAnalysis$new(untypedList = self$cmAnalysisList[[i]])
        }
        for (i in seq_along(self$targetComparatorOutcomesList)) {
          self$targetComparatorOutcomesList[[i]] <- TargetComparatorOutcomes$new(untypedList = self$targetComparatorOutcomesList[[i]])
        }
        if (!is.null(self$analysesToExclude)) {
          self$analysesToExclude <- as.data.frame(self$analysesToExclude)
        }
        self$cmDiagnosticThresholds <- CmDiagnosticThresholds$new(untypedList = self$cmDiagnosticThresholds)
      }
    }
  )
)

# Loading, saving, conversion ----------------------------------------------------------------------

#' Save a list of CmAnalysis to file
#'
#' @description
#' Write a list of objects of type `CmAnalysis` to file. The file is in JSON format.
#'
#' @param CmAnalysisList   A list of objects of type `CmAnalysis` as created using the [createCmAnalysis()] function.
#' @param file               The name of the file where the results will be written
#'
#' @export
saveCmAnalysisList <- function(CmAnalysisList, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(CmAnalysisList, min.len = 1, add = errorMessages)
  for (i in seq_along(CmAnalysisList)) {
    checkmate::assertR6(CmAnalysisList[[i]], "CmAnalysis", add = errorMessages)
  }
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  for (i in seq_along(CmAnalysisList)) {
    CmAnalysisList[[i]] <- CmAnalysisList[[i]]$toList()
  }
  json <- jsonlite::toJSON(
    CmAnalysisList,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  file <- normalizePath(file, mustWork = FALSE)
  write(json, file)
}

#' Load a list of CmAnalysis from file
#'
#' @description
#' Load a list of objects of type `CmAnalysis` from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type `CmAnalysis`.
#'
#' @export
loadCmAnalysisList <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  file <- normalizePath(file)
  json <- readChar(file, file.info(file)$size)
  CmAnalysisList <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  for (i in seq_along(CmAnalysisList)) {
    CmAnalysisList[[i]] <- CmAnalysis$new(untypedList = CmAnalysisList[[i]])
  }
  return(CmAnalysisList)
}

#' Save a list of `TargetComparatorOutcomes` to file
#'
#' @description
#' Write a list of objects of type `TargetComparatorOutcomes` to file. The file is in JSON format.
#'
#' @param exposuresOutcomeList  A list of objects of type `TargetComparatorOutcomes` as created
#'                              using the [createTargetComparatorOutcomes()] function.
#' @param file                  The name of the file where the results will be written
#'
#' @export
saveTargetComparatorOutcomesList <- function(targetComparatorOutcomesList, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(targetComparatorOutcomesList, min.len = 1, add = errorMessages)
  for (i in 1:length(targetComparatorOutcomesList)) {
    checkmate::assertR6(targetComparatorOutcomesList[[i]], "TargetComparatorOutcomes", add = errorMessages)
  }
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  for (i in seq_along(targetComparatorOutcomesList)) {
    targetComparatorOutcomesList[[i]] <- targetComparatorOutcomesList[[i]]$toList()
  }
  json <- jsonlite::toJSON(
    targetComparatorOutcomesList,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  file <- normalizePath(file, mustWork = FALSE)
  write(json, file)
}

#' Load a list of `TargetComparatorOutcomes` from file
#'
#' @description
#' Load a list of objects of type `TargetComparatorOutcomes` from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type `TargetComparatorOutcomes`.
#'
#' @export
loadTargetComparatorOutcomesList <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  file <- normalizePath(file)
  json <- readChar(file, file.info(file)$size)
  targetComparatorOutcomesList <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  for (i in seq_along(targetComparatorOutcomesList)) {
    targetComparatorOutcomesList[[i]] <- TargetComparatorOutcomes$new(untypedList = targetComparatorOutcomesList[[i]])
  }
  return(targetComparatorOutcomesList)
}

#' Convert untyped list to SccsAnalysesSpecifications
#'
#' @param untypedList A list of untyped objects. For example, these could be objects from a call
#'                    to `jsonlite::fromJSON()`. Importantly, `simplifyDataFrame` must be set to
#'                    `FALSE` when doing so.
#'
#' @returns
#' An object of type `SccsAnalysesSpecifications`.
#'
#'
#' @export
convertUntypedListToCmAnalysesSpecifications <- function(untypedList) {
  return(CmAnalysesSpecifications$new(untypedList = untypedList))
}

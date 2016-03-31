# @file StudyPopulation.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

#' Create a study population
#'
#' @details
#' Create a study population by enforcing certain inclusion and exclusion criteria, defining
#' a risk window, and determining which outcomes fall inside the risk window.
#'
#' @param cohortMethodData      An object of type \code{cohortMethodData} as generated using
#'                              \code{getDbCohortMethodData}.
#' @param outcomeId             The  ID of the outcome. If not specified, no outcome-specific transformations will
#'                              be performed.
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note that
#'                                     this is typically done in the \code{createStudyPopulation} function,
#' @param washoutPeriod                The mininum required continuous observation time prior to index
#'                                     date for a person to be included in the cohort.
#' @param removeDuplicateSubjects      Remove subjects that are in both the treated and comparator cohort?
#' @param removeSubjectsWithPriorOutcome  Remove subjects that have the outcome prior to the risk window start?
#' @param priorOutcomeLookback            How many days should we look back when identifying prior outcomes?
#' @param riskWindowStart        The start of the risk window (in days) relative to the index date (+
#'                               days of exposure if the \code{addExposureDaysToStart} parameter is
#'                               specified).
#' @param addExposureDaysToStart   Add the length of exposure the start of the risk window?
#' @param riskWindowEnd          The end of the risk window (in days) relative to the index data (+
#'                               days of exposure if the \code{addExposureDaysToEnd} parameter is
#'                               specified).
#' @param addExposureDaysToEnd   Add the length of exposure the risk window?
#' @param modelType              The type of outcome model that will be used. Possible values are
#'                               "logistic", "poisson", or "cox".
#'
#' @return
#' A data frame specifying the study population. This data frame will have the following columns:
#' \describe{
#' \item{rowId}{A unique identifier for an exposure}
#' \item{subjectId}{The person ID of the subject}
#' \item{cohortStartdate}{The index date}
#' \item{outcome}{The outcome variable, which value depends on the modelType}
#' \item{time}{Optional: for survival models this is the time to event or end of risk windows, for poisson models this is the time to the end of the risk window.}
#' }
#'
#' @export
createStudyPopulation <- function(cohortMethodData,
                                  outcomeId,
                                  firstExposureOnly = TRUE,
                                  washoutPeriod = 180,
                                  removeDuplicateSubjects = TRUE,
                                  removeSubjectsWithPriorOutcome = TRUE,
                                  priorOutcomeLookback = 99999,
                                  riskWindowStart = 0,
                                  addExposureDaysToStart = FALSE,
                                  riskWindowEnd = 0,
                                  addExposureDaysToEnd = TRUE,
                                  modelType = "logistic",
                                  dropExtraColumns = TRUE) {
  if (modelType != "logistic" && modelType != "poisson" && modelType != "cox") {
    stop(paste("Unknown modelType '", modelType, "', please choose either 'logistic', 'poisson', or 'cox'", sep = ""))
  }
  population <- cohortMethodData$cohorts
  metaData <- attr(population, "metaData")
  if (firstExposureOnly) {
    writeLines("Keeping only first exposure per subject")
    population <- population[order(population$subjectId, as.Date(population$cohortStartDate)), ]
    idx <- duplicated(population[, c("subjectId", "treatment")])
    population <- population[!idx, ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, "First exposure only"))
  }
  if (washoutPeriod) {
    writeLines(paste("Requiring", washoutPeriod, "days of observation prior index date"))
    population <- population[population$daysFromObsStart >= washoutPeriod,]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("At least", washoutPeriod, "days of observation prior")))
  }
  if (removeDuplicateSubjects) {
    writeLines("Removing subject that are in both cohorts (if any)")
    targetSubjectIds <- population$subjectId[population$treatment == 1]
    comparatorSubjectIds <- population$subjectId[population$treatment == 0]
    duplicateSubjectIds <- targetSubjectIds[targetSubjectIds %in% comparatorSubjectIds]
    population <- population[!(population$subjectId %in% duplicateSubjectIds), ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("Removed subjects in both cohorts")))
  }
  if (removeSubjectsWithPriorOutcome) {
    if (missing(outcomeId) || is.null(outcomeId)){
      writeLines("No outcome specified so skipping removing people with prior outcomes")
    } else {
      writeLines("Removing subjects with prior outcomes (if any)")
      outcomes <- cohortMethodData$outcomes[cohortMethodData$outcomes$outcomeId == outcomeId, ]
      if (addExposureDaysToStart) {
        outcomes <- merge(outcomes, population[, c("rowId","daysToCohortEnd")])
        priorOutcomeRowIds <- outcomes$rowId[outcomes$daysToEvent > -priorOutcomeLookback & outcomes$daysToEvent < outcomes$daysToCohortEnd + riskWindowStart]
      } else {
        priorOutcomeRowIds <- outcomes$rowId[outcomes$daysToEvent > -priorOutcomeLookback & outcomes$daysToEvent < riskWindowStart]
      }
      population <- population[!(population$rowId %in% priorOutcomeRowIds), ]
      metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("No prior outcome")))
    }
  }
  # Create risk windows:
  population$riskStart <- riskWindowStart
  if (addExposureDaysToStart) {
    population$riskStart <- population$riskStart + population$daysToCohortEnd
  }
  population$riskEnd <- riskWindowEnd
  if (addExposureDaysToEnd) {
    population$riskEnd <- population$riskEnd + population$daysToCohortEnd
  }
  population$riskEnd[population$riskEnd > population$daysToObsEnd] <- population$daysToObsEnd[population$riskEnd > population$daysToObsEnd]

  writeLines("Removing subjects with no time at risk (if any)")
  noAtRiskTimeRowIds <- population$rowId[population$riskEnd < population$riskStart]
  population <- population[!(population$rowId %in% noAtRiskTimeRowIds), ]
  metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("Have time at risk")))

  if (missing(outcomeId) || is.null(outcomeId)){
    writeLines("No outcome specified so not creating outcome and time variables")
  } else {
    # Select outcomes during time at risk
    outcomes <- cohortMethodData$outcomes[cohortMethodData$outcomes$outcomeId == outcomeId, ]
    outcomes <- merge(outcomes, population[, c("rowId", "riskStart", "riskEnd")])
    outcomes <- outcomes[outcomes$daysToEvent >= outcomes$riskStart & outcomes$daysToEvent <= outcomes$riskEnd, ]

    # Create outcome and time columns (modelType specific):
    if (modelType == "logistic" || modelType == "cox") {
      population$outcome <- 0
      population$outcome[population$rowId %in% unique(outcomes$rowId)] <- 1
    }
    if (modelType == "cox") {
      # Keep first event per person (in risk window):
      outcomes <- outcomes[order(outcomes$rowId, outcomes$daysToEvent), ]
      outcomes <- outcomes[!duplicated(outcomes$rowId), ]

      population <- merge(population, outcomes[, c("rowId", "daysToEvent")], all.x = TRUE)
      population$time = population$riskEnd - population$riskStart
      population$time[population$outcome == 1] <- population$daysToEvent[population$outcome == 1] - population$riskStart[population$outcome == 1]
    }
    if (modelType == "poisson") {
      population$time = population$riskEnd - population$riskStart
      outcomes <- aggregate(outcomeId ~ rowId, data = outcomes, length)
      colnames(outcomes)[colnames(outcomes) == "outcomeId"] <- "outcome"
      population <- merge(population, outcomes[, c("rowId", "outcome")], all.x = TRUE)
      population$outcome[is.na(population$outcome)] <- 0
    }
    metaData$modelType <- modelType
  }
  if (dropExtraColumns) {
    if (missing(outcomeId) || is.null(outcomeId)) {
      population <- population[, c("rowId", "subjectId", "treatment", "cohortStartDate")]
    } else if (modelType == "cox" || modelType == "poisson") {
      population <- population[, c("rowId", "subjectId", "treatment", "cohortStartDate", "outcome", "time")]
    } else {
      population <- population[, c("rowId", "subjectId", "treatment", "cohortStartDate", "outcome")]
    }
  }
  attr(population, "metaData") <- metaData
  return(population)
}

limitCovariatesToPopulation <- function(covariates, rowIds) {
  idx <- !is.na(ffbase::ffmatch(covariates$rowId, rowIds))
  covariates <- covariates[ffbase::ffwhich(idx, idx == TRUE), ]
  return(covariates)
}

#' Get the attrition table for a population
#'
#' @param object   Either an object of type \code{cohortMethodData}, a population object generated by functions
#'                     like \code{createStudyPopulation}, or an object of type \code{outcomeModel}.
#'
#' @return
#' A data frame specifiyng the number of people and exposures in the population after specific sets of filtering.
#'
#'
#' @export
getAttritionTable <- function(object) {
  if (is(object, "cohortMethodData")) {
    object = object$cohorts
  }
  if (is(object, "outcomeModel")){
    return(object$attrition)
  } else {
    return(attr(object, "metaData")$attrition)
  }
}

getCounts <- function(population, description = "") {
  treatedPersons <- length(unique(population$subjectId[population$treatment == 1]))
  comparatorPersons <- length(unique(population$subjectId[population$treatment == 0]))
  treatedExposures <- length(population$subjectId[population$treatment == 1])
  comparatorExposures <- length(population$subjectId[population$treatment == 0])
  counts <- data.frame(description = description,
                       treatedPersons = treatedPersons,
                       comparatorPersons = comparatorPersons,
                       treatedExposures = treatedExposures,
                       comparatorExposures = comparatorExposures)
  return(counts)
}

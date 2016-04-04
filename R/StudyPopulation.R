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
#' @param population            If specified, this population will be used as the starting point instead of the
#'                              cohorts in the \code{cohortMethodData} object.
#' @param outcomeId             The  ID of the outcome. If not specified, no outcome-specific transformations will
#'                              be performed.
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note that
#'                                     this is typically done in the \code{createStudyPopulation} function,
#' @param removeDuplicateSubjects      Remove subjects that are in both the treated and comparator cohort?
#' @param washoutPeriod                The mininum required continuous observation time prior to index
#'                                     date for a person to be included in the cohort.
#' @param removeSubjectsWithPriorOutcome  Remove subjects that have the outcome prior to the risk window start?
#' @param priorOutcomeLookback            How many days should we look back when identifying prior outcomes?
#' @param requireTimeAtRisk      Should subject without time at risk be removed?
#' @param riskWindowStart        The start of the risk window (in days) relative to the index date (+
#'                               days of exposure if the \code{addExposureDaysToStart} parameter is
#'                               specified).
#' @param addExposureDaysToStart   Add the length of exposure the start of the risk window?
#' @param riskWindowEnd          The end of the risk window (in days) relative to the index data (+
#'                               days of exposure if the \code{addExposureDaysToEnd} parameter is
#'                               specified).
#' @param addExposureDaysToEnd   Add the length of exposure the risk window?
#'
#' @return
#' A data frame specifying the study population. This data frame will have the following columns:
#' \describe{
#' \item{rowId}{A unique identifier for an exposure}
#' \item{subjectId}{The person ID of the subject}
#' \item{cohortStartdate}{The index date}
#' \item{outcomeCount}{The number of outcomes observed during the risk window}
#' \item{timeAtRisk}{The number of days in the risk window}
#' \item{survivalTime}{The number of days until either the outcome or the end of the risk window}
#' }
#'
#' @export
createStudyPopulation <- function(cohortMethodData,
                                  population = NULL,
                                  outcomeId,
                                  firstExposureOnly = FALSE,
                                  washoutPeriod = 0,
                                  removeDuplicateSubjects = FALSE,
                                  removeSubjectsWithPriorOutcome = TRUE,
                                  priorOutcomeLookback = 99999,
                                  requireTimeAtRisk = FALSE,
                                  riskWindowStart = 0,
                                  addExposureDaysToStart = FALSE,
                                  riskWindowEnd = 0,
                                  addExposureDaysToEnd = TRUE) {
  if (is.null(population)) {
    population <- cohortMethodData$cohorts
  }
  metaData <- attr(population, "metaData")
  if (firstExposureOnly) {
    writeLines("Keeping only first exposure per subject")
    population <- population[order(population$subjectId, as.Date(population$cohortStartDate)), ]
    idx <- duplicated(population[, c("subjectId", "treatment")])
    population <- population[!idx, ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, "First exposure only"))
  }
  if (removeDuplicateSubjects) {
    writeLines("Removing subject that are in both cohorts (if any)")
    targetSubjectIds <- population$subjectId[population$treatment == 1]
    comparatorSubjectIds <- population$subjectId[population$treatment == 0]
    duplicateSubjectIds <- targetSubjectIds[targetSubjectIds %in% comparatorSubjectIds]
    population <- population[!(population$subjectId %in% duplicateSubjectIds), ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("Removed subjects in both cohorts")))
  }
  if (washoutPeriod) {
    writeLines(paste("Requiring", washoutPeriod, "days of observation prior index date"))
    population <- population[population$daysFromObsStart >= washoutPeriod,]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("At least", washoutPeriod, "days of observation prior")))
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

  if (requireTimeAtRisk) {
    writeLines("Removing subjects with no time at risk (if any)")
    noAtRiskTimeRowIds <- population$rowId[population$riskEnd < population$riskStart]
    population <- population[!(population$rowId %in% noAtRiskTimeRowIds), ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("Have time at risk")))
  }
  if (missing(outcomeId) || is.null(outcomeId)){
    writeLines("No outcome specified so not creating outcome and time variables")
  } else {
    # Select outcomes during time at risk
    outcomes <- cohortMethodData$outcomes[cohortMethodData$outcomes$outcomeId == outcomeId, ]
    outcomes <- merge(outcomes, population[, c("rowId", "riskStart", "riskEnd")])
    outcomes <- outcomes[outcomes$daysToEvent >= outcomes$riskStart & outcomes$daysToEvent <= outcomes$riskEnd, ]

    # Create outcome count column
    outcomeCount <- aggregate(outcomeId ~ rowId, data = outcomes, length)
    colnames(outcomeCount)[colnames(outcomeCount) == "outcomeId"] <- "outcomeCount"
    population <- merge(population, outcomeCount[, c("rowId", "outcomeCount")], all.x = TRUE)
    population$outcomeCount[is.na(population$outcomeCount)] <- 0

    # Create time at risk column
    population$timeAtRisk <- population$riskEnd - population$riskStart + 1

    # Create survival time column
    firstOutcomes <- outcomes[order(outcomes$rowId, outcomes$daysToEvent), ]
    firstOutcomes <- firstOutcomes[!duplicated(firstOutcomes$rowId), ]
    population <- merge(population, firstOutcomes[, c("rowId", "daysToEvent")], all.x = TRUE)
    population$survivalTime <- population$timeAtRisk
    population$survivalTime[population$outcomeCount != 0] <- population$daysToEvent[population$outcomeCount != 0] - population$riskStart[population$outcomeCount != 0] + 1
  }
  population$riskStart <- NULL
  population$riskEnd <- NULL
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
#' A data frame specifying the number of people and exposures in the population after specific steps of filtering.
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

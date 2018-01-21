# @file StudyPopulation.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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
#' Create a study population by enforcing certain inclusion and exclusion criteria, defining a risk
#' window, and determining which outcomes fall inside the risk window.
#'
#' The \code{removeduplicateSubjects} argument can have one of the following values:
#' \describe{
#'   \item{"keep all"}{Do not remove subjects that appear in both target and comparator cohort}
#'   \item{"keep first"}{When a subjects appear in both target and comparator cohort, only keep whichever cohort is first in time.}
#'   \item{"remove all"}{Remove subjects that appear in both target and comparator cohort completely from the analysis."}
#' }
#'
#' @param cohortMethodData                 An object of type \code{cohortMethodData} as generated using
#'                                         \code{getDbCohortMethodData}.
#' @param population                       If specified, this population will be used as the starting
#'                                         point instead of the cohorts in the \code{cohortMethodData}
#'                                         object.
#' @param outcomeId                        The ID of the outcome. If not specified, no outcome-specific
#'                                         transformations will be performed.
#' @param firstExposureOnly                Should only the first exposure per subject be included? Note
#'                                         that this is typically done in the
#'                                         \code{createStudyPopulation} function,
#' @param removeDuplicateSubjects          Remove subjects that are in both the treated and comparator
#'                                         cohort? See details for allowed values.
#' @param restrictToCommonPeriod           Restrict the analysis to the period when both treatments are observed?
#' @param washoutPeriod                    The mininum required continuous observation time prior to
#'                                         index date for a person to be included in the cohort.
#' @param removeSubjectsWithPriorOutcome   Remove subjects that have the outcome prior to the risk
#'                                         window start?
#' @param priorOutcomeLookback             How many days should we look back when identifying prior
#'                                         outcomes?
#' @param minDaysAtRisk                    The minimum required number of days at risk.
#' @param riskWindowStart                  The start of the risk window (in days) relative to the index
#'                                         date (+ days of exposure if the
#'                                         \code{addExposureDaysToStart} parameter is specified).
#' @param addExposureDaysToStart           Add the length of exposure the start of the risk window?
#' @param riskWindowEnd                    The end of the risk window (in days) relative to the index
#'                                         data (+ days of exposure if the \code{addExposureDaysToEnd}
#'                                         parameter is specified).
#' @param addExposureDaysToEnd             Add the length of exposure the risk window?
#' @param censorAtNewRiskWindow            If a subject is in multiple cohorts, should time-at-risk be censored
#'                                         when the new time-at-risk starts to prevent overlap?
#' @return
#' A data frame specifying the study population. This data frame will have the following columns:
#' \describe{ \item{rowId}{A unique identifier for an exposure} \item{subjectId}{The person ID of the
#' subject} \item{cohortStartdate}{The index date} \item{outcomeCount}{The number of outcomes observed
#' during the risk window} \item{timeAtRisk}{The number of days in the risk window}
#' \item{survivalTime}{The number of days until either the outcome or the end of the risk window} }
#'
#' @export
createStudyPopulation <- function(cohortMethodData,
                                  population = NULL,
                                  outcomeId,
                                  firstExposureOnly = FALSE,
                                  restrictToCommonPeriod = FALSE,
                                  washoutPeriod = 0,
                                  removeDuplicateSubjects = FALSE,
                                  removeSubjectsWithPriorOutcome = TRUE,
                                  priorOutcomeLookback = 99999,
                                  minDaysAtRisk = 1,
                                  riskWindowStart = 0,
                                  addExposureDaysToStart = FALSE,
                                  riskWindowEnd = 0,
                                  addExposureDaysToEnd = TRUE,
                                  censorAtNewRiskWindow = FALSE) {
  if (is.logical(removeDuplicateSubjects)) {
    if (removeDuplicateSubjects)
      removeDuplicateSubjects <- "remove all"
    else
      removeDuplicateSubjects <- "keep all"
  }
  if (!(removeDuplicateSubjects %in% c("keep all", "keep first", "remove all")))
    stop("removeDuplicateSubjects should have value \"keep all\", \"keep first\", or \"remove all\".")

  if (is.null(population)) {
    population <- cohortMethodData$cohorts
  }
  metaData <- attr(population, "metaData")
  if (firstExposureOnly) {
    writeLines("Keeping only first exposure per subject")
    population <- population[order(population$subjectId, population$treatment, as.Date(population$cohortStartDate)), ]
    idx <- duplicated(population[, c("subjectId", "treatment")])
    population <- population[!idx, ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, "First exposure only"))
  }
  if (restrictToCommonPeriod) {
    writeLines("Restrict to common period")
    cohortStartDate <- as.Date(population$cohortStartDate)
    periodStart <- max(aggregate(cohortStartDate ~ population$treatment, FUN = min)$cohortStartDate)
    periodEnd <- min(aggregate(cohortStartDate ~ population$treatment, FUN = max)$cohortStartDate)
    population <- population[cohortStartDate >= periodStart & cohortStartDate <= periodEnd, ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, "Restrict to common period"))
  }
  if (removeDuplicateSubjects == "remove all") {
    writeLines("Removing all subject that are in both cohorts (if any)")
    targetSubjectIds <- population$subjectId[population$treatment == 1]
    comparatorSubjectIds <- population$subjectId[population$treatment == 0]
    duplicateSubjectIds <- targetSubjectIds[targetSubjectIds %in% comparatorSubjectIds]
    population <- population[!(population$subjectId %in% duplicateSubjectIds), ]
    metaData$attrition <- rbind(metaData$attrition,
                                getCounts(population, paste("Removed subjects in both cohorts")))
  } else if (removeDuplicateSubjects == "keep first") {
    writeLines("For subject that are in both cohorts, keeping only whichever cohort is first in time.")
    population <- population[order(population$subjectId, as.Date(population$cohortStartDate)), ]
    idx <- duplicated(population[, c("subjectId")])
    population <- population[!idx, ]
    metaData$attrition <- rbind(metaData$attrition,
                                getCounts(population, paste("Restricting duplicate subjects to first cohort")))
  }

  if (washoutPeriod) {
    writeLines(paste("Requiring", washoutPeriod, "days of observation prior index date"))
    population <- population[population$daysFromObsStart >= washoutPeriod, ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("At least",
                                                                                washoutPeriod,
                                                                                "days of observation prior")))
  }
  if (removeSubjectsWithPriorOutcome) {
    if (missing(outcomeId) || is.null(outcomeId)) {
      writeLines("No outcome specified so skipping removing people with prior outcomes")
    } else {
      writeLines("Removing subjects with prior outcomes (if any)")
      outcomes <- cohortMethodData$outcomes[cohortMethodData$outcomes$outcomeId == outcomeId, ]
      if (addExposureDaysToStart) {
        outcomes <- merge(outcomes, population[, c("rowId", "daysToCohortEnd")])
        priorOutcomeRowIds <- outcomes$rowId[outcomes$daysToEvent > -priorOutcomeLookback & outcomes$daysToEvent <
                                               outcomes$daysToCohortEnd + riskWindowStart]
      } else {
        priorOutcomeRowIds <- outcomes$rowId[outcomes$daysToEvent > -priorOutcomeLookback & outcomes$daysToEvent <
                                               riskWindowStart]
      }
      population <- population[!(population$rowId %in% priorOutcomeRowIds), ]
      metaData$attrition <- rbind(metaData$attrition,
                                  getCounts(population, paste("No prior outcome")))
    }
  }
  # Create risk windows:
  population$riskStart <- rep(riskWindowStart, nrow(population))
  if (addExposureDaysToStart) {
    population$riskStart <- population$riskStart + population$daysToCohortEnd
  }
  population$riskEnd <- rep(riskWindowEnd, nrow(population))
  if (addExposureDaysToEnd) {
    population$riskEnd <- population$riskEnd + population$daysToCohortEnd
  }
  population$riskEnd[population$riskEnd > population$daysToObsEnd] <- population$daysToObsEnd[population$riskEnd >
                                                                                                population$daysToObsEnd]
  if (censorAtNewRiskWindow) {
    writeLines("Censoring time at risk of recurrent subjects at start of new time at risk")
    population$startDate <- as.Date(population$cohortStartDate) + population$riskStart
    population$endDate <- as.Date(population$cohortStartDate) + population$riskEnd
    population <- population[order(population$subjectId, population$riskStart), ]
    idx <- 1:(nrow(population)-1)
    idx <- which(population$endDate[idx] >= population$startDate[idx + 1] &
                   population$subjectId[idx] == population$subjectId[idx + 1])
    if (length(idx) > 0) {
      population$endDate[idx] <- population$startDate[idx + 1] - 1
      population$riskEnd[idx] <- population$endDate[idx] - as.Date(population$cohortStartDate[idx])
      idx <- population$riskEnd < population$riskStart
      if (any(idx)) {
        population <- population[!idx, ]
        metaData$attrition <- rbind(metaData$attrition,
                                    getCounts(population, paste("Censoring at start of new time-at-risk")))

      }
    }
    population$startDate <- NULL
    population$endDate <- NULL
  }
  if (minDaysAtRisk != 0) {
    writeLines(paste("Removing subjects with less than", minDaysAtRisk, "day(s) at risk (if any)"))
    population <- population[population$riskEnd - population$riskStart >= minDaysAtRisk, ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("Have at least",
                                                                                minDaysAtRisk,
                                                                                "days at risk")))
  }
  if (missing(outcomeId) || is.null(outcomeId)) {
    writeLines("No outcome specified so not creating outcome and time variables")
  } else {
    # Select outcomes during time at risk
    outcomes <- cohortMethodData$outcomes[cohortMethodData$outcomes$outcomeId == outcomeId, ]
    # idx <- match(outcomes$rowId, population$rowId)
    # outcomes <- outcomes[!is.na(idx), ]
    # outcomes$riskStart <- population$riskStart[idx[!is.na(idx)]]
    # outcomes$riskEnd <- population$riskEnd[idx[!is.na(idx)]]
    outcomes <- merge(outcomes, population[, c("rowId", "riskStart", "riskEnd")])
    outcomes <- outcomes[outcomes$daysToEvent >= outcomes$riskStart & outcomes$daysToEvent <= outcomes$riskEnd, ]

    # Create outcome count column
    if (nrow(outcomes) == 0) {
      population$outcomeCount <- rep(0, nrow(population))
    } else {
      outcomeCount <- aggregate(outcomeId ~ rowId, data = outcomes, length)
      colnames(outcomeCount)[colnames(outcomeCount) == "outcomeId"] <- "outcomeCount"
      population$outcomeCount <- 0
      population$outcomeCount[match(outcomeCount$rowId, population$rowId)] <- outcomeCount$outcomeCount
    }

    # Create time at risk column
    population$timeAtRisk <- population$riskEnd - population$riskStart + 1

    # Create survival time column
    firstOutcomes <- outcomes[order(outcomes$rowId, outcomes$daysToEvent), ]
    firstOutcomes <- firstOutcomes[!duplicated(firstOutcomes$rowId), ]
    # population <- merge(population, firstOutcomes[, c("rowId", "daysToEvent")], all.x = TRUE)
    population$daysToEvent <- rep(NA, nrow(population))
    population$daysToEvent[match(firstOutcomes$rowId, population$rowId)] <- firstOutcomes$daysToEvent
    population$survivalTime <- population$timeAtRisk
    population$survivalTime[population$outcomeCount != 0] <- population$daysToEvent[population$outcomeCount !=
                                                                                      0] - population$riskStart[population$outcomeCount != 0] + 1
  }
  population$riskStart <- NULL
  population$riskEnd <- NULL
  attr(population, "metaData") <- metaData
  return(population)
}

#' Get the attrition table for a population
#'
#' @param object   Either an object of type \code{cohortMethodData}, a population object generated by
#'                 functions like \code{createStudyPopulation}, or an object of type
#'                 \code{outcomeModel}.
#'
#' @return
#' A data frame specifying the number of people and exposures in the population after specific steps
#' of filtering.
#'
#'
#' @export
getAttritionTable <- function(object) {
  if (is(object, "cohortMethodData")) {
    object <- object$cohorts
  }
  if (is(object, "outcomeModel")) {
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

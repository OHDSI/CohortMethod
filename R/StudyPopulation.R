# @file StudyPopulation.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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

fastDuplicated <- function(data, columns) {
  results <- lapply(columns, function(column, data) data[2:nrow(data), column] == data[1:(nrow(data) - 1), column], data = data)
  result <- results[[1]]
  if (length(columns) > 1) {
    for (i in 2:length(columns)) {
      result <- result & results[[i]]
    }
  }
  return(c(FALSE, result))
}

#' Create a study population
#'
#' @details
#' Create a study population by enforcing certain inclusion and exclusion criteria, defining a risk
#' window, and determining which outcomes fall inside the risk window.
#'
#' The \code{removeduplicateSubjects} argument can have one of the following values:
#' \describe{
#'   \item{"keep all"}{Do not remove subjects that appear in both target and comparator cohort}
#'   \item{"keep first"}{When a subjects appear in both target and comparator cohort, only keep whichever cohort is first in time.
#'   If both cohorts start simultaneous, the person is removed from the analysis.}
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
#' @param removeDuplicateSubjects          Remove subjects that are in both the target and comparator
#'                                         cohort? See details for allowed values.
#' @param restrictToCommonPeriod           Restrict the analysis to the period when both exposures are observed?
#' @param washoutPeriod                    The mininum required continuous observation time prior to
#'                                         index date for a person to be included in the cohort.
#' @param removeSubjectsWithPriorOutcome   Remove subjects that have the outcome prior to the risk
#'                                         window start?
#' @param priorOutcomeLookback             How many days should we look back when identifying prior
#'                                         outcomes?
#' @param minDaysAtRisk                    The minimum required number of days at risk.
#' @param riskWindowStart                  The start of the risk window (in days) relative to the \code{startAnchor}.
#' @param addExposureDaysToStart           DEPRECATED: Add the length of exposure the start of the risk window?
#'                                         Use \code{startAnchor} instead.
#' @param startAnchor                      The anchor point for the start of the risk window. Can be "cohort start"
#'                                         or "cohort end".
#' @param riskWindowEnd                    The end of the risk window (in days) relative to the \code{endAnchor}.
#' @param addExposureDaysToEnd             DEPRECATED: Add the length of exposure the risk window?
#'                                         Use \code{endAnchor} instead.
#' @param endAnchor                        The anchor point for the end of the risk window. Can be "cohort start"
#'                                         or "cohort end".
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
                                  addExposureDaysToStart = NULL,
                                  startAnchor = "cohort start",
                                  riskWindowEnd = 0,
                                  addExposureDaysToEnd = NULL,
                                  endAnchor = "cohort end",
                                  censorAtNewRiskWindow = FALSE) {
  if (!missing(addExposureDaysToStart) && !is.null(addExposureDaysToStart)) {
    warning("The addExposureDaysToStart argument is deprecated. Please use the startAnchor argument instead.")
    if (addExposureDaysToStart) {
      startAnchor <- "cohort end"
    } else {
      startAnchor <- "cohort start"
    }
  }
  if (!missing(addExposureDaysToEnd) && !is.null(addExposureDaysToEnd)) {
    warning("The addExposureDaysToEnd argument is deprecated. Please use the endAnchor argument instead.")
    if (addExposureDaysToEnd) {
      endAnchor <- "cohort end"
    } else {
      endAnchor <- "cohort start"
    }
  }
  if (!grepl("start$|end$", startAnchor, ignore.case = TRUE)) {
    stop("startAnchor should have value 'cohort start' or 'cohort end'")
  }
  if (!grepl("start$|end$", endAnchor, ignore.case = TRUE)) {
    stop("endAnchor should have value 'cohort start' or 'cohort end'")
  }
  isEnd <- function(anchor) {
    return(grepl("end$", anchor, ignore.case = TRUE))
  }
  if (is.logical(removeDuplicateSubjects)) {
    if (removeDuplicateSubjects)
      removeDuplicateSubjects <- "remove all"
    else
      removeDuplicateSubjects <- "keep all"
  }
  if (!(removeDuplicateSubjects %in% c("keep all", "keep first", "remove all")))
    stop("removeDuplicateSubjects should have value \"keep all\", \"keep first\", or \"remove all\".")
  if (missing(outcomeId))
    ParallelLogger::logTrace("Creating study population without outcome ID")
  else
    ParallelLogger::logTrace("Creating study population for outcome ID ", outcomeId)

  if (is.null(population)) {
    population <- cohortMethodData$cohorts
  }
  metaData <- attr(population, "metaData")
  if (firstExposureOnly) {
    ParallelLogger::logInfo("Keeping only first exposure per subject")
    population <- population[order(population$subjectId, population$treatment, as.Date(population$cohortStartDate)), ]
    # idx <- duplicated(population[, c("subjectId", "treatment")])
    idx <- fastDuplicated(population, c("subjectId", "treatment"))
    population <- population[!idx, ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, "First exposure only"))
  }
  if (restrictToCommonPeriod) {
    ParallelLogger::logInfo("Restrict to common period")
    cohortStartDate <- as.Date(population$cohortStartDate)
    periodStart <- max(aggregate(cohortStartDate ~ population$treatment, FUN = min)$cohortStartDate)
    periodEnd <- min(aggregate(cohortStartDate ~ population$treatment, FUN = max)$cohortStartDate)
    population <- population[cohortStartDate >= periodStart & cohortStartDate <= periodEnd, ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, "Restrict to common period"))
  }
  if (removeDuplicateSubjects == "remove all") {
    ParallelLogger::logInfo("Removing all subject that are in both cohorts (if any)")
    targetSubjectIds <- population$subjectId[population$treatment == 1]
    comparatorSubjectIds <- population$subjectId[population$treatment == 0]
    duplicateSubjectIds <- targetSubjectIds[targetSubjectIds %in% comparatorSubjectIds]
    population <- population[!(population$subjectId %in% duplicateSubjectIds), ]
    metaData$attrition <- rbind(metaData$attrition,
                                getCounts(population, paste("Removed subjects in both cohorts")))
  } else if (removeDuplicateSubjects == "keep first") {
    ParallelLogger::logInfo("For subject that are in both cohorts, keeping only whichever cohort is first in time.")
    population <- population[order(population$subjectId, as.Date(population$cohortStartDate)), ]
    # Remove ties:
    # idx <- duplicated(population[, c("subjectId", "cohortStartDate")])
    idx <- fastDuplicated(population, c("subjectId", "cohortStartDate"))
    idx[1:(length(idx) - 1)] <- idx[1:(length(idx) - 1)] | idx[2:length(idx)]
    if (all(idx)) {
      stop("All cohort entries are ties, with same subject ID and cohort start date")
    }
    population <- population[!idx, ]
    # Keeping first:
    # idx <- duplicated(population[, c("subjectId")])
    idx <- fastDuplicated(population, "subjectId")
    population <- population[!idx, ]
    metaData$attrition <- rbind(metaData$attrition,
                                getCounts(population, paste("Restricting duplicate subjects to first cohort")))
  }

  if (washoutPeriod) {
    ParallelLogger::logInfo(paste("Requiring", washoutPeriod, "days of observation prior index date"))
    population <- population[population$daysFromObsStart >= washoutPeriod, ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("At least",
                                                                                washoutPeriod,
                                                                                "days of observation prior")))
  }
  if (removeSubjectsWithPriorOutcome) {
    if (missing(outcomeId) || is.null(outcomeId)) {
      ParallelLogger::logInfo("No outcome specified so skipping removing people with prior outcomes")
    } else {
      ParallelLogger::logInfo("Removing subjects with prior outcomes (if any)")
      outcomes <- cohortMethodData$outcomes[cohortMethodData$outcomes$outcomeId == outcomeId, ]
      if (isEnd(startAnchor)) {
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
  if (isEnd(startAnchor)) {
    population$riskStart <- population$riskStart + population$daysToCohortEnd
  }
  population$riskEnd <- rep(riskWindowEnd, nrow(population))
  if (isEnd(endAnchor)) {
    population$riskEnd <- population$riskEnd + population$daysToCohortEnd
  }
  population$riskEnd[population$riskEnd > population$daysToObsEnd] <- population$daysToObsEnd[population$riskEnd >
                                                                                                population$daysToObsEnd]
  if (censorAtNewRiskWindow) {
    ParallelLogger::logInfo("Censoring time at risk of recurrent subjects at start of new time at risk")
    population$startDate <- as.Date(population$cohortStartDate) + population$riskStart
    population$endDate <- as.Date(population$cohortStartDate) + population$riskEnd
    population <- population[order(population$subjectId, population$riskStart), ]
    idx <- 1:(nrow(population) - 1)
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
    ParallelLogger::logInfo(paste("Removing subjects with less than", minDaysAtRisk, "day(s) at risk (if any)"))
    population <- population[population$riskEnd - population$riskStart >= minDaysAtRisk, ]
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste("Have at least",
                                                                                minDaysAtRisk,
                                                                                "days at risk")))
  }
  if (missing(outcomeId) || is.null(outcomeId)) {
    ParallelLogger::logInfo("No outcome specified so not creating outcome and time variables")
  } else {
    # Select outcomes during time at risk
    outcomes <- cohortMethodData$outcomes[cohortMethodData$outcomes$outcomeId == outcomeId, ]
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
  # population$riskStart <- NULL
  # population$riskEnd <- NULL
  attr(population, "metaData") <- metaData
  ParallelLogger::logDebug("Study population has ", nrow(population), " rows")
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
  targetPersons <- length(unique(population$subjectId[population$treatment == 1]))
  comparatorPersons <- length(unique(population$subjectId[population$treatment == 0]))
  targetExposures <- length(population$subjectId[population$treatment == 1])
  comparatorExposures <- length(population$subjectId[population$treatment == 0])
  counts <- data.frame(description = description,
                       targetPersons = targetPersons,
                       comparatorPersons = comparatorPersons,
                       targetExposures = targetExposures,
                       comparatorExposures = comparatorExposures)
  return(counts)
}


#' Plot time-to-event
#'
#' @details
#' Creates a plot showing the number of events over time in the target and comparator cohorts, both before and after
#' index date. The plot also distinguishes between events inside and outside the time-at-risk period. This requires
#' the user to (re)specify the time-at-risk using the same arguments as the \code{\link{createStudyPopulation}} function.
#' Note that it is not possible to specify that people with the outcome prior should be removed, since the plot will
#' show these prior events.
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
#' @param removeDuplicateSubjects          Remove subjects that are in both the target and comparator
#'                                         cohort? See details for allowed values.
#' @param restrictToCommonPeriod           Restrict the analysis to the period when both exposures are observed?
#' @param washoutPeriod                    The mininum required continuous observation time prior to
#'                                         index date for a person to be included in the cohort.
#' @param minDaysAtRisk                    The minimum required number of days at risk.
#' @param riskWindowStart                  The start of the risk window (in days) relative to the \code{startAnchor}.
#' @param addExposureDaysToStart           DEPRECATED: Add the length of exposure the start of the risk window?
#'                                         Use \code{startAnchor} instead.
#' @param startAnchor                      The anchor point for the start of the risk window. Can be "cohort start"
#'                                         or "cohort end".
#' @param riskWindowEnd                    The end of the risk window (in days) relative to the \code{endAnchor}.
#' @param addExposureDaysToEnd             DEPRECATED: Add the length of exposure the risk window?
#'                                         Use \code{endAnchor} instead.
#' @param endAnchor                        The anchor point for the end of the risk window. Can be "cohort start"
#'                                         or "cohort end".
#' @param censorAtNewRiskWindow            If a subject is in multiple cohorts, should time-at-risk be censored
#'                                         when the new time-at-risk starts to prevent overlap?
#' @param periodLength                     The length in days of each period shown in the plot.
#' @param numberOfPeriods                  Number of periods to show in the plot. The periods are
#'                                         equaly divided before and after the index date.
#' @param showFittedLines                  Fit lines to the proportions and show them in the plot?
#' @param targetLabel                      A label to us for the target cohort.
#' @param comparatorLabel                  A label to us for the comparator cohort.
#' @param title                            Optional: the main title for the plot.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotTimeToEvent <- function(cohortMethodData,
                            population = NULL,
                            outcomeId,
                            firstExposureOnly = FALSE,
                            restrictToCommonPeriod = FALSE,
                            washoutPeriod = 0,
                            removeDuplicateSubjects = FALSE,
                            minDaysAtRisk = 1,
                            riskWindowStart = 0,
                            addExposureDaysToStart = NULL,
                            startAnchor = "cohort start",
                            riskWindowEnd = 0,
                            addExposureDaysToEnd = NULL,
                            endAnchor = "cohort end",
                            censorAtNewRiskWindow = FALSE,
                            periodLength = 7,
                            numberOfPeriods = 52,
                            showFittedLines = TRUE,
                            targetLabel = "Target",
                            comparatorLabel = "Comparator",
                            title = NULL,
                            fileName = NULL) {
  if (!missing(addExposureDaysToStart) && !is.null(addExposureDaysToStart)) {
    warning("The addExposureDaysToStart argument is deprecated. Please use the startAnchor argument instead.")
    if (addExposureDaysToStart) {
      startAnchor <- "cohort end"
    } else {
      startAnchor <- "cohort start"
    }
  }
  if (!missing(addExposureDaysToEnd) && !is.null(addExposureDaysToEnd)) {
    warning("The addExposureDaysToEnd argument is deprecated. Please use the endAnchor argument instead.")
    if (addExposureDaysToEnd) {
      endAnchor <- "cohort end"
    } else {
      endAnchor <- "cohort start"
    }
  }
  population <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                      population = population,
                                      outcomeId = outcomeId,
                                      firstExposureOnly = firstExposureOnly,
                                      restrictToCommonPeriod = restrictToCommonPeriod,
                                      washoutPeriod = washoutPeriod,
                                      removeDuplicateSubjects = removeDuplicateSubjects,
                                      removeSubjectsWithPriorOutcome = FALSE,
                                      minDaysAtRisk = minDaysAtRisk,
                                      riskWindowStart = riskWindowStart,
                                      startAnchor = startAnchor,
                                      riskWindowEnd = riskWindowEnd,
                                      endAnchor = endAnchor,
                                      censorAtNewRiskWindow = censorAtNewRiskWindow)
  outcomes <- cohortMethodData$outcomes[cohortMethodData$outcomes$outcomeId == outcomeId, ]
  outcomes <- merge(population[, c("rowId", "treatment", "daysFromObsStart", "daysToObsEnd", "riskStart", "riskEnd")],
                    outcomes[, c("rowId", "daysToEvent")])
  outcomes <- outcomes[-outcomes$daysFromObsStart <= outcomes$daysToEvent & outcomes$daysToObsEnd >= outcomes$daysToEvent, ]
  outcomes$exposed <- 0
  outcomes$exposed[outcomes$daysToEvent >= outcomes$riskStart & outcomes$daysToEvent <= outcomes$riskEnd] <- 1
  periods <- data.frame(number = -floor(numberOfPeriods/2):ceiling(numberOfPeriods/2))
  periods$start <- periods$number*periodLength
  periods$end <- periods$number*periodLength + periodLength
  periods$eventsExposed <- 0
  periods$eventsUnexposed <- 0
  periods$observed <- 0
  idxExposed <- outcomes$exposed == 1
  idxTarget <- outcomes$treatment == 1
  for (i in 1:nrow(periods)) {
    idxInPeriod <- outcomes$daysToEvent >= periods$start[i] & outcomes$daysToEvent < periods$end[i]
    periods$eventsExposedTarget[i] <- sum(idxInPeriod & idxExposed & idxTarget)
    periods$eventsExposedComparator[i] <- sum(idxInPeriod & idxExposed & !idxTarget)
    periods$eventsUnexposedTarget[i] <- sum(idxInPeriod & !idxExposed & idxTarget)
    periods$eventsUnexposedComparator[i] <- sum(idxInPeriod & !idxExposed & !idxTarget)
    idxInPeriod <- -population$daysFromObsStart <= periods$start[i] & population$daysToObsEnd >= periods$end[i]
    periods$observedTarget[i] <- sum(idxInPeriod & population$treatment)
    periods$observedComparator[i] <- sum(idxInPeriod & !population$treatment)
  }
  periods$rateExposedTarget <- periods$eventsExposedTarget / periods$observedTarget
  periods$rateUnexposedTarget <- periods$eventsUnexposedTarget / periods$observedTarget
  periods$rateExposedComparator <- periods$eventsExposedComparator / periods$observedComparator
  periods$rateUnexposedComparator <- periods$eventsUnexposedComparator / periods$observedComparator
  periods$rateTarget <- (periods$eventsExposedTarget + periods$eventsUnexposedTarget) / periods$observedTarget
  periods$rateComparator <- (periods$eventsExposedComparator + periods$eventsUnexposedComparator) / periods$observedComparator
  vizData <- rbind(data.frame(start = periods$start,
                              end = periods$end,
                              rate = periods$rateExposedTarget,
                              status = "Exposed events",
                              type = targetLabel),
                   data.frame(start = periods$start,
                              end = periods$end,
                              rate = periods$rateUnexposedTarget,
                              status = "Unexposed events",
                              type = targetLabel),
                   data.frame(start = periods$start,
                              end = periods$end,
                              rate = periods$rateExposedComparator,
                              status = "Exposed events",
                              type = comparatorLabel),
                   data.frame(start = periods$start,
                              end = periods$end,
                              rate = periods$rateUnexposedComparator,
                              status = "Unexposed events",
                              type = comparatorLabel))
  vizData$type <- factor(vizData$type, levels = c(targetLabel, comparatorLabel))

  plot <- ggplot2::ggplot(vizData, ggplot2::aes(x = start + periodLength / 2,
                                                y = rate * 1000,
                                                fill = status)) +
    ggplot2::geom_col(width = periodLength, alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 0, colour = "#000000", lty = 1, size = 1) +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Days since exposure start") +
    ggplot2::scale_y_continuous("Proportion (per 1,000 persons)") +
    ggplot2::facet_grid(type~., scales = "free_y") +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_line(colour = "#AAAAAA"),
                   axis.ticks = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top")

  if (showFittedLines) {
    preTarget <- periods[periods$start < 0, ]
    preTarget <- cbind(preTarget, predict(lm(rateTarget ~ poly(number, 3), data = preTarget), interval = "confidence"))
    preTarget$type <- targetLabel
    preTarget$period <- "Pre"
    postTarget <- periods[periods$start >= 0, ]
    postTarget <- cbind(postTarget, predict(lm(rateTarget ~ poly(number, 3), data = postTarget), interval = "confidence"))
    postTarget$type <- targetLabel
    postTarget$period <- "Post"
    preComparator <- periods[periods$start < 0, ]
    preComparator <- cbind(preComparator, predict(lm(rateComparator ~ poly(number, 3), data = preComparator), interval = "confidence"))
    preComparator$type <- comparatorLabel
    preComparator$period <- "Pre"
    postComparator <- periods[periods$start >= 0, ]
    postComparator <- cbind(postComparator, predict(lm(rateComparator ~ poly(number, 3), data = postComparator), interval = "confidence"))
    postComparator$type <- comparatorLabel
    postComparator$period <- "Post"
    curve <- rbind(preTarget, postTarget, preComparator, postComparator)
    curve$rate <- 0
    curve$status <- "Exposed events"
    curve$type <- factor(curve$type, levels = c(targetLabel, comparatorLabel))

    plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(x = start + periodLength/2,
                                                     ymin = lwr * 1000,
                                                     ymax = upr * 1000,
                                                     group = period),
                                        fill = rgb(0,0,0),
                                        alpha = 0.3,
                                        data = curve) +
      ggplot2::geom_line(ggplot2::aes(x = start + periodLength/2,
                                      y = fit * 1000,
                                      group = period),
                         size = 1.5,
                         alpha = 0.8,
                         data = curve)
  }

  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
    # fileName <- "S:/temp/plot1.png"
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}

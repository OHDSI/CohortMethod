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

fastDuplicated <- function(data, columns) {
  if (nrow(data) == 0) {
    return(vector())
  } else if (nrow(data) == 1) {
    return(c(FALSE))
  } else {
    results <- lapply(columns, function(column, data) data[2:nrow(data), column] == data[1:(nrow(data) - 1), column], data = data)
    result <- results[[1]]
    if (length(columns) > 1) {
      for (i in 2:length(columns)) {
        result <- result & results[[i]]
      }
    }
    return(c(FALSE, result))
  }
}

#' Create a study population
#'
#' @details
#' Create a study population by enforcing certain inclusion and exclusion criteria, defining a risk
#' window, and determining which outcomes fall inside the risk window.
#'
#' The `removeduplicateSubjects` argument can have one of the following values:
#'
#' - `"keep all"`: Do not remove subjects that appear in both target and comparator cohort
#' - `"keep first"`: When a subjects appear in both target and comparator cohort, only keep whichever cohort is first in time. If both cohorts start simultaneous, the person is removed from the analysis.
#' - `"remove all"`: Remove subjects that appear in both target and comparator cohort completely from the analysis."
#'
#' @template CohortMethodData
#'
#' @param population                       If specified, this population will be used as the starting
#'                                         point instead of the cohorts in the `cohortMethodData` object.
#' @param outcomeId                        The ID of the outcome. If NULL, no outcome-specific
#'                                         transformations will be performed.
#' @param firstExposureOnly                Should only the first exposure per subject be included?
#' @param removeDuplicateSubjects          Remove subjects that are in both the target and comparator
#'                                         cohort? See details for allowed values.
#' @param restrictToCommonPeriod           Restrict the analysis to the period when both exposures are observed?
#' @param washoutPeriod                    The minimum required continuous observation time prior to
#'                                         index date for a person to be included in the cohort.
#' @param removeSubjectsWithPriorOutcome   Remove subjects that have the outcome prior to the risk
#'                                         window start?
#' @param priorOutcomeLookback             How many days should we look back when identifying prior
#'                                         outcomes?
#' @param minDaysAtRisk                    The minimum required number of days at risk. Risk windows with fewer
#'                                         days than this number are removed from the analysis.
#' @param maxDaysAtRisk                    The maximum allowed number of days at risk. Risk windows that are
#'                                         longer will be truncated to this number of days.
#' @param riskWindowStart                  The start of the risk window (in days) relative to the `startAnchor`.
#' @param startAnchor                      The anchor point for the start of the risk window. Can be `"cohort start"`
#'                                         or `"cohort end"`.
#' @param riskWindowEnd                    The end of the risk window (in days) relative to the `endAnchor`.
#' @param endAnchor                        The anchor point for the end of the risk window. Can be `"cohort start"`
#'                                         or `"cohort end"`.
#' @param censorAtNewRiskWindow            If a subject is in multiple cohorts, should time-at-risk be censored
#'                                         when the new time-at-risk starts to prevent overlap?
#' @return
#' A `tibble` specifying the study population. This `tibble` will have the following columns:
#'
#' - `rowId`: A unique identifier for an exposure.
#' - `personSeqId`: The person sequence ID of the subject.
#' - `cohortStartdate`: The index date.
#' - `outcomeCount` The number of outcomes observed during the risk window.
#' - `timeAtRisk`: The number of days in the risk window.
#' - `survivalTime`: The number of days until either the outcome or the end of the risk window.
#'
#' @export
createStudyPopulation <- function(cohortMethodData,
                                  population = NULL,
                                  outcomeId = NULL,
                                  firstExposureOnly = FALSE,
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
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(cohortMethodData, "CohortMethodData", add = errorMessages)
  checkmate::assertDataFrame(population, null.ok = TRUE, add = errorMessages)
  checkmate::assertNumeric(outcomeId, null.ok = TRUE, add = errorMessages)
  if (!is.null(outcomeId)) checkmate::assertTRUE(all(outcomeId %% 1 == 0), add = errorMessages)
  checkmate::assertLogical(firstExposureOnly, len = 1, add = errorMessages)
  checkmate::assertLogical(restrictToCommonPeriod, len = 1, add = errorMessages)
  checkmate::assertInt(washoutPeriod, lower = 0, add = errorMessages)
  checkmate::assertChoice(removeDuplicateSubjects, c("keep all", "keep first", "remove all"), add = errorMessages)
  checkmate::assertLogical(removeSubjectsWithPriorOutcome, len = 1, add = errorMessages)
  checkmate::assertInt(priorOutcomeLookback, lower = 0, add = errorMessages)
  checkmate::assertInt(minDaysAtRisk, lower = 0, add = errorMessages)
  checkmate::assertInt(maxDaysAtRisk, lower = 0, add = errorMessages)
  checkmate::assertInt(riskWindowStart, add = errorMessages)
  checkmate::assertInt(riskWindowEnd, add = errorMessages)
  checkmate::assertLogical(censorAtNewRiskWindow, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (!grepl("start$|end$", startAnchor, ignore.case = TRUE)) {
    stop("startAnchor should have value 'cohort start' or 'cohort end'")
  }
  if (!grepl("start$|end$", endAnchor, ignore.case = TRUE)) {
    stop("endAnchor should have value 'cohort start' or 'cohort end'")
  }
  isEnd <- function(anchor) {
    return(grepl("end$", anchor, ignore.case = TRUE))
  }

  if (is.null(population)) {
    metaData <- attr(cohortMethodData, "metaData")
    population <- cohortMethodData$cohorts |>
      collect() |>
      select(-"personId")
  } else {
    metaData <- attr(population, "metaData")
  }
  metaData$targetEstimator <- "ate"

  if (firstExposureOnly) {
    message("Keeping only first exposure per subject")
    population <- population |>
      arrange(.data$personSeqId, .data$treatment, .data$cohortStartDate)

    idx <- fastDuplicated(population, c("personSeqId", "treatment"))
    population <- population[!idx, ]
    metaData$attrition <- rbind(
      metaData$attrition,
      getCounts(population, "First exposure only")
    )
  }
  if (restrictToCommonPeriod) {
    message("Restrict to common period")
    if (nrow(population) > 0) {
      population <- population |>
        group_by(.data$treatment) |>
        summarise(
          treatmentStart = min(.data$cohortStartDate),
          treatmentEnd = max(.data$cohortStartDate)
        ) |>
        ungroup() |>
        summarise(
          periodStart = max(.data$treatmentStart),
          periodEnd = min(.data$treatmentEnd)
        ) |>
        cross_join(population) |>
        filter(
          population$cohortStartDate >= .data$periodStart &
            population$cohortStartDate <= .data$periodEnd
        ) |>
        select(-"periodStart", -"periodEnd")
    }
    metaData$attrition <- rbind(
      metaData$attrition,
      getCounts(population, "Restrict to common period")
    )
  }
  if (removeDuplicateSubjects == "remove all") {
    message("Removing all subject that are in both cohorts (if any)")
    targetSubjectIds <- population$personSeqId[population$treatment == 1]
    comparatorSubjectIds <- population$personSeqId[population$treatment == 0]
    duplicateSubjectIds <- targetSubjectIds[targetSubjectIds %in% comparatorSubjectIds]
    population <- population[!(population$personSeqId %in% duplicateSubjectIds), ]
    metaData$attrition <- rbind(
      metaData$attrition,
      getCounts(population, paste("Removed subjects in both cohorts"))
    )
  } else if (removeDuplicateSubjects == "keep first") {
    message("For subject that are in both cohorts, keeping only whichever cohort is first in time.")
    if (nrow(population) > 1) {
      population <- population |>
        arrange(.data$personSeqId, .data$cohortStartDate)
      # Remove ties:
      idx <- fastDuplicated(population, c("personSeqId", "cohortStartDate"))
      # If duplicate, then we must remove both things that are a tie:
      idx[seq_len(length(idx) - 1)] <- idx[seq_len(length(idx) - 1)] | idx[seq_len(length(idx))[-1]]
      if (all(idx)) {
        warning("All cohort entries are ties, with same subject ID and cohort start date")
      }
      population <- population[!idx, ]
      # Keeping first:
      idx <- fastDuplicated(population, "personSeqId")
      population <- population[!idx, ]
    }
    metaData$attrition <- rbind(
      metaData$attrition,
      getCounts(population, paste("Restricting duplicate subjects to first cohort"))
    )
  }

  if (washoutPeriod) {
    message(paste("Requiring", washoutPeriod, "days of observation prior index date"))
    population <- population |>
      filter(.data$daysFromObsStart >= washoutPeriod)
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste(
      "At least",
      washoutPeriod,
      "days of observation prior"
    )))
  }
  if (removeSubjectsWithPriorOutcome) {
    if (is.null(outcomeId)) {
      message("No outcome specified so skipping removing people with prior outcomes")
    } else {
      message("Removing subjects with prior outcomes (if any)")
      outcomes <- cohortMethodData$outcomes |>
        filter(.data$outcomeId == !!outcomeId) |>
        collect()
      if (isEnd(startAnchor)) {
        outcomes <- merge(outcomes, population[, c("rowId", "daysToCohortEnd")])
        priorOutcomeRowIds <- outcomes |>
          filter(
            .data$daysToEvent > -priorOutcomeLookback &
              outcomes$daysToEvent < outcomes$daysToCohortEnd + riskWindowStart
          ) |>
          pull("rowId")
      } else {
        priorOutcomeRowIds <- outcomes |>
          filter(
            .data$daysToEvent > -priorOutcomeLookback &
              .data$daysToEvent < riskWindowStart
          ) |>
          pull("rowId")
      }
      population <- population |>
        filter(!(.data$rowId %in% priorOutcomeRowIds))
      metaData$attrition <- rbind(
        metaData$attrition,
        getCounts(population, paste("No prior outcome"))
      )
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
  idx <- population$riskEnd > population$daysToObsEnd
  population$riskEnd[idx] <- population$daysToObsEnd[idx]

  if (!missing(maxDaysAtRisk) && !is.null(maxDaysAtRisk)) {
    idx <- population$riskEnd > population$riskStart + maxDaysAtRisk
    if (any(idx)) {
      population$riskEnd[idx] <- population$riskStart[idx] + maxDaysAtRisk
    }
  }
  if (censorAtNewRiskWindow) {
    message("Censoring time at risk of recurrent subjects at start of new time at risk")
    if (nrow(population) > 1) {
      population$startDate <- population$cohortStartDate + population$riskStart
      population$endDate <- population$cohortStartDate + population$riskEnd
      population <- population |>
        arrange(.data$personSeqId, .data$riskStart)
      idx <- seq_len(nrow(population) - 1)
      idx <- which(population$endDate[idx] >= population$startDate[idx + 1] &
        population$personSeqId[idx] == population$personSeqId[idx + 1])
      if (length(idx) > 0) {
        population$endDate[idx] <- population$startDate[idx + 1] - 1
        population$riskEnd[idx] <- population$endDate[idx] - population$cohortStartDate[idx]
        idx <- population$riskEnd < population$riskStart
        if (any(idx)) {
          population <- population[!idx, ]
        }
      }
      population$startDate <- NULL
      population$endDate <- NULL
      metaData$attrition <- rbind(
        metaData$attrition,
        getCounts(population, paste("Censoring at start of new time-at-risk"))
      )
    }
  }
  if (minDaysAtRisk != 0) {
    message(paste("Removing subjects with less than", minDaysAtRisk, "day(s) at risk (if any)"))
    population <- population |>
      filter(.data$riskEnd - .data$riskStart >= minDaysAtRisk)
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, paste(
      "Have at least",
      minDaysAtRisk,
      "days at risk"
    )))
  }
  if (is.null(outcomeId)) {
    message("No outcome specified so not creating outcome and time variables")
  } else {
    # Select outcomes during time at risk
    outcomes <- cohortMethodData$outcomes |>
      filter(.data$outcomeId == !!outcomeId) |>
      collect()
    outcomes <- merge(outcomes, population[, c("rowId", "riskStart", "riskEnd")])
    outcomes <- outcomes |>
      filter(
        .data$daysToEvent >= .data$riskStart &
          .data$daysToEvent <= .data$riskEnd
      )

    # Create outcome count column
    if (nrow(outcomes) == 0) {
      population$outcomeCount <- rep(0, nrow(population))
    } else {
      outcomeCount <- outcomes |>
        group_by(.data$rowId) |>
        summarise(outcomeCount = length(.data$outcomeId))
      population$outcomeCount <- 0
      population$outcomeCount[match(outcomeCount$rowId, population$rowId)] <- outcomeCount$outcomeCount
    }

    # Create time at risk column
    population$timeAtRisk <- population$riskEnd - population$riskStart + 1

    # Create survival time column
    firstOutcomes <- outcomes |>
      arrange(.data$rowId, .data$daysToEvent) |>
      filter(!duplicated(.data$rowId))
    population$daysToEvent <- rep(NA, nrow(population))
    population$daysToEvent[match(firstOutcomes$rowId, population$rowId)] <- firstOutcomes$daysToEvent
    population$survivalTime <- population$timeAtRisk
    population$survivalTime[population$outcomeCount != 0] <- population$daysToEvent[population$outcomeCount !=
      0] - population$riskStart[population$outcomeCount != 0] + 1
  }
  attr(population, "metaData") <- metaData
  ParallelLogger::logDebug("Study population has ", nrow(population), " rows")
  return(population)
}

#' Get the attrition table for a population
#'
#' @param object   Either an object of type [CohortMethodData], a population object generated by
#'                 functions like [createStudyPopulation()], or an object of type
#'                 `outcomeModel`.
#'
#' @return
#' A `tibble` specifying the number of people and exposures in the population after specific steps
#' of filtering.
#'
#'
#' @export
getAttritionTable <- function(object) {
  if (is(object, "OutcomeModel")) {
    return(object$attrition)
  } else {
    return(attr(object, "metaData")$attrition)
  }
}

getCounts <- function(population, description = "") {
  targetPersons <- length(unique(population$personSeqId[population$treatment == 1]))
  comparatorPersons <- length(unique(population$personSeqId[population$treatment == 0]))
  targetExposures <- length(population$personSeqId[population$treatment == 1])
  comparatorExposures <- length(population$personSeqId[population$treatment == 0])
  counts <- tibble(
    description = description,
    targetPersons = targetPersons,
    comparatorPersons = comparatorPersons,
    targetExposures = targetExposures,
    comparatorExposures = comparatorExposures
  )
  return(counts)
}


#' Plot time-to-event
#'
#' @details
#' Creates a plot showing the number of events over time in the target and comparator cohorts, both before and after
#' index date. The plot also distinguishes between events inside and outside the time-at-risk period. This requires
#' the user to (re)specify the time-at-risk using the same arguments as the [createStudyPopulation()] function.
#' Note that it is not possible to specify that people with the outcome prior should be removed, since the plot will
#' show these prior events.
#'
#' @template CohortMethodData
#'
#' @param population                       If specified, this population will be used as the starting
#'                                         point instead of the cohorts in the `cohortMethodData` object.
#' @param outcomeId                        The ID of the outcome. If NULL, no outcome-specific
#'                                         transformations will be performed.
#' @param firstExposureOnly                (logical) Should only the first exposure per subject be included?
#' @param removeDuplicateSubjects          Remove subjects that are in both the target and comparator
#'                                         cohort? See details for allowed values.
#' @param restrictToCommonPeriod           (logical) Restrict the analysis to the period when both exposures are observed?
#' @param washoutPeriod                    The minimum required continuous observation time prior to
#'                                         index date for a person to be included in the cohort.
#' @param minDaysAtRisk                    The minimum required number of days at risk.
#' @param riskWindowStart                  The start of the risk window (in days) relative to the `startAnchor`.
#' @param startAnchor                      The anchor point for the start of the risk window. Can be `"cohort start"`
#'                                         or `"cohort end"`.
#' @param riskWindowEnd                    The end of the risk window (in days) relative to the `endAnchor`.
#' @param endAnchor                        The anchor point for the end of the risk window. Can be `"cohort start"`
#'                                         or `"cohort end"`.
#' @param censorAtNewRiskWindow            If a subject is in multiple cohorts, should time-at-risk be censored
#'                                         when the new time-at-risk starts to prevent overlap?
#' @param periodLength                     The length in days of each period shown in the plot.
#' @param numberOfPeriods                  Number of periods to show in the plot. The periods are
#'                                         equally divided before and after the index date.
#' @param highlightExposedEvents           (logical) Highlight event counts during exposure in a different color?
#' @param includePostIndexTime             (logical) Show time after the index date?
#' @param showFittedLines                  (logical) Fit lines to the proportions and show them in the plot?
#' @param targetLabel                      A label to us for the target cohort.
#' @param comparatorLabel                  A label to us for the comparator cohort.
#' @param title                            Optional: the main title for the plot.
#' @param fileName                         Name of the file where the plot should be saved, for example
#'                                         'plot.png'. See [ggplot2::ggsave()] for supported file formats.
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave()] function to save to file in a different
#' format.
#'
#' @export
plotTimeToEvent <- function(cohortMethodData,
                            population = NULL,
                            outcomeId = NULL,
                            firstExposureOnly = FALSE,
                            restrictToCommonPeriod = FALSE,
                            washoutPeriod = 0,
                            removeDuplicateSubjects = "keep all",
                            minDaysAtRisk = 1,
                            riskWindowStart = 0,
                            startAnchor = "cohort start",
                            riskWindowEnd = 0,
                            endAnchor = "cohort end",
                            censorAtNewRiskWindow = FALSE,
                            periodLength = 7,
                            numberOfPeriods = 52,
                            highlightExposedEvents = TRUE,
                            includePostIndexTime = TRUE,
                            showFittedLines = TRUE,
                            targetLabel = "Target",
                            comparatorLabel = "Comparator",
                            title = NULL,
                            fileName = NULL) {
  if (is.logical(removeDuplicateSubjects)) {
    if (removeDuplicateSubjects) {
      removeDuplicateSubjects <- "remove all"
    } else {
      removeDuplicateSubjects <- "keep all"
    }
  }
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(periodLength, lower = 0, add = errorMessages)
  checkmate::assertInt(numberOfPeriods, lower = 0, add = errorMessages)
  checkmate::assertLogical(highlightExposedEvents, len = 1, add = errorMessages)
  checkmate::assertLogical(includePostIndexTime, len = 1, add = errorMessages)
  checkmate::assertLogical(showFittedLines, len = 1, add = errorMessages)
  checkmate::assertCharacter(targetLabel, len = 1, add = errorMessages)
  checkmate::assertCharacter(comparatorLabel, len = 1, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (is.null(population)) {
    population <- cohortMethodData$cohorts |>
      collect()
  }
  population <- createStudyPopulation(
    cohortMethodData = cohortMethodData,
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
    censorAtNewRiskWindow = censorAtNewRiskWindow
  )
  outcomes <- cohortMethodData$outcomes |>
    filter(.data$outcomeId == !!outcomeId) |>
    select("rowId", "daysToEvent") |>
    collect()

  outcomes <- outcomes |>
    inner_join(select(population, "rowId", "treatment", "daysFromObsStart", "daysToObsEnd", "riskStart", "riskEnd"),
      by = "rowId"
    ) |>
    filter(-.data$daysFromObsStart <= .data$daysToEvent & .data$daysToObsEnd >= .data$daysToEvent) |>
    mutate(exposed = .data$daysToEvent >= .data$riskStart & .data$daysToEvent <= .data$riskEnd)

  idxExposed <- outcomes$exposed == 1
  idxTarget <- outcomes$treatment == 1
  createPeriod <- function(number) {
    start <- number * periodLength
    end <- number * periodLength + periodLength
    idxInPeriod <- outcomes$daysToEvent >= start & outcomes$daysToEvent < end
    idxPopInPeriod <- -population$daysFromObsStart <= start & population$daysToObsEnd >= end
    tibble(
      number = number,
      start = start,
      end = end,
      eventsExposed = 0,
      eventsUnexposed = 0,
      observed = 0,
      eventsExposedTarget = sum(idxInPeriod & idxExposed & idxTarget),
      eventsExposedComparator = sum(idxInPeriod & idxExposed & !idxTarget),
      eventsUnexposedTarget = sum(idxInPeriod & !idxExposed & idxTarget),
      eventsUnexposedComparator = sum(idxInPeriod & !idxExposed & !idxTarget),
      observedTarget = sum(idxPopInPeriod & population$treatment),
      observedComparator = sum(idxPopInPeriod & !population$treatment)
    )
  }
  periods <- lapply(-floor(numberOfPeriods / 2):ceiling(numberOfPeriods / 2), createPeriod)
  periods <- do.call("rbind", periods)
  periods <- periods |>
    filter(.data$observedTarget > 0) |>
    mutate(
      rateExposedTarget = .data$eventsExposedTarget / .data$observedTarget,
      rateUnexposedTarget = .data$eventsUnexposedTarget / .data$observedTarget,
      rateExposedComparator = .data$eventsExposedComparator / .data$observedComparator,
      rateUnexposedComparator = .data$eventsUnexposedComparator / .data$observedComparator,
      rateTarget = (.data$eventsExposedTarget + .data$eventsUnexposedTarget) / .data$observedTarget,
      rateComparator = (.data$eventsExposedComparator + .data$eventsUnexposedComparator) / .data$observedComparator
    )
  if (!includePostIndexTime) {
    periods <- periods |>
      filter(.data$end <= 0)
  }
  vizData <- rbind(
    tibble(
      start = periods$start,
      end = periods$end,
      rate = periods$rateExposedTarget,
      status = "Exposed events",
      type = targetLabel
    ),
    tibble(
      start = periods$start,
      end = periods$end,
      rate = periods$rateUnexposedTarget,
      status = "Unexposed events",
      type = targetLabel
    ),
    tibble(
      start = periods$start,
      end = periods$end,
      rate = periods$rateExposedComparator,
      status = "Exposed events",
      type = comparatorLabel
    ),
    tibble(
      start = periods$start,
      end = periods$end,
      rate = periods$rateUnexposedComparator,
      status = "Unexposed events",
      type = comparatorLabel
    )
  )
  vizData$type <- factor(vizData$type, levels = c(targetLabel, comparatorLabel))

  if (highlightExposedEvents) {
    plot <- ggplot2::ggplot(vizData, ggplot2::aes(
      x = .data$start + periodLength / 2,
      y = .data$rate * 1000,
      fill = .data$status
    )) +
      ggplot2::geom_col(width = periodLength, alpha = 0.7)
  } else {
    plot <- ggplot2::ggplot(vizData, ggplot2::aes(
      x = .data$start + periodLength / 2,
      y = .data$rate * 1000
    )) +
      ggplot2::geom_col(width = periodLength, alpha = 0.7, fill = rgb(0, 0, 0.8))
  }
  plot <- plot +
    ggplot2::geom_vline(xintercept = 0, colour = "#000000", lty = 1, size = 1) +
    ggplot2::scale_fill_manual(values = c(
      rgb(0.8, 0, 0),
      rgb(0, 0, 0.8)
    )) +
    ggplot2::scale_x_continuous("Days since exposure start") +
    ggplot2::scale_y_continuous("Proportion (per 1,000 persons)") +
    ggplot2::facet_grid(type ~ ., scales = "free_y") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#AAAAAA"),
      axis.ticks = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )

  if (showFittedLines) {
    preTarget <- periods[periods$start < 0, ]
    preTarget <- cbind(preTarget, predict(lm(rateTarget ~ poly(number, 3), data = preTarget), interval = "confidence"))
    preTarget$type <- targetLabel
    preTarget$period <- "Pre"
    preComparator <- periods[periods$start < 0, ]
    preComparator <- cbind(preComparator, predict(lm(rateComparator ~ poly(number, 3), data = preComparator), interval = "confidence"))
    preComparator$type <- comparatorLabel
    preComparator$period <- "Pre"
    curve <- bind_rows(preTarget, preComparator)
    if (includePostIndexTime) {
      postTarget <- periods[periods$start >= 0, ]
      postTarget <- cbind(postTarget, predict(lm(rateTarget ~ poly(number, 3), data = postTarget), interval = "confidence"))
      postTarget$type <- targetLabel
      postTarget$period <- "Post"
      postComparator <- periods[periods$start >= 0, ]
      postComparator <- cbind(postComparator, predict(lm(rateComparator ~ poly(number, 3), data = postComparator), interval = "confidence"))
      postComparator$type <- comparatorLabel
      postComparator$period <- "Post"
      curve <- bind_rows(curve, postTarget, postComparator)
    }
    curve <- curve |>
      mutate(
        rate = 0,
        status = "Exposed events",
        type = factor(.data$type, levels = c(targetLabel, comparatorLabel)),
        lwr = if_else(.data$lwr < 0, 0, .data$lwr)
      )


    plot <- plot + ggplot2::geom_ribbon(
      ggplot2::aes(
        x = start + periodLength / 2,
        ymin = .data$lwr * 1000,
        ymax = .data$upr * 1000,
        group = .data$period
      ),
      fill = rgb(0, 0, 0),
      alpha = 0.3,
      data = curve
    ) +
      ggplot2::geom_line(
        ggplot2::aes(
          x = start + periodLength / 2,
          y = .data$fit * 1000,
          group = .data$period
        ),
        size = 1.5,
        alpha = 0.8,
        data = curve
      )
  }

  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  }
  return(plot)
}

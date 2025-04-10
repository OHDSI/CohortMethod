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



##' Truncate low-prevalence covariates in a simulation profile
##'
##' @description
##' Set to zero all low-prevalence covariates in the supplied simulation table
##' in order to prevent identification of persons
##' @title truncateSimulationProfile
##' @param profile Object of class `CohortDataSimulationProfile`
##' @param minCellCount Number of cases below which prevalence will be set to zero
##' @return Modified copy of supplied simulation profile
.truncateSimulationProfile <- function(profile, minCellCount = 5) {
    checkmate::assertClass(profile, "CohortDataSimulationProfile")
    checkmate::assertIntegerish(minCellCount, lower = 0L, upper = profile$metaData$populationSize)

    if (minCellCount == 0) {
        warning("No truncation was done on low-prevalence covariates. Object may include low cell counts that enable identification of persons.")
        return(profile)
    }

    minObservedNonzeroPrevalence <- profile$covariatePrevalence |>
        filter(.data$prevalence > 0) |>
        summarize(min(.data$prevalence)) |>
        pull()

    message(sprintf("Before truncating simulation profile, lowest non-zero covariate prevalence is %.08f (%.0f / %.0f)",
            minObservedNonzeroPrevalence,
            round(minObservedNonzeroPrevalence * profile$metaData$populationSize),
            profile$metaData$populationSize))

    minimumAllowedPrevalence <- minCellCount / profile$metaData$populationSize

    profile$covariatePrevalence <- profile$covariatePrevalence |>
        mutate(prevalence = ifelse(.data$prevalence < minimumAllowedPrevalence,
                                   0, .data$prevalence))

    truncatedMinObservedNonzeroPrevalence <- profile$covariatePrevalence |>
        filter(.data$prevalence > 0) |>
        summarize(min(.data$prevalence)) |>
        pull()

    message(sprintf("After truncating simulation profile, lowest non-zero covariate prevalence is %.08f (%.0f / %.0f)",
            truncatedMinObservedNonzeroPrevalence,
            round(truncatedMinObservedNonzeroPrevalence * profile$metaData$populationSize),
            profile$metaData$populationSize))

    return(profile)
}


#' Create simulation profile
#'
#' @description
#' Creates a profile based on the provided
#' [CohortMethodData] object, which can be used to generate simulated data that has similar
#' characteristics.
#'
#' @template CohortMethodData
#' @param minCellCount If > 0, will apply `.truncateSimulationProfile()`
#'
#' @details
#' The output of this function is an object that can be used by the [simulateCohortMethodData()]
#' function to generate a cohortMethodData object.
#'
#' @return
#' An object of type `CohortDataSimulationProfile`.
#'
#' @export
createCohortMethodDataSimulationProfile <- function(cohortMethodData,
                                                    minCellCount = 5) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(cohortMethodData, "CohortMethodData", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (nrow_temp(cohortMethodData$cohorts) == 0) {
    stop("Cohorts are empty")
  }

  if (nrow_temp(cohortMethodData$covariates) == 0) {
    stop("Covariates are empty")
  }

  if (sum(cohortMethodData$cohorts |> select("daysToCohortEnd") |> pull()) == 0) {
    warning("Cohort data appears to be limited, check daysToCohortEnd which appears to be all zeros")
  }

  message("Computing covariate prevalence")
  # (Note: currently limiting to binary covariates)
  populationSize <- cohortMethodData$cohorts |>
    count() |>
    pull()
  covariatePrevalence <- cohortMethodData$covariates |>
    group_by(.data$covariateId) |>
    summarise(sum = sum(.data$covariateValue, na.rm = TRUE)) |>
    mutate(prevalence = .data$sum / populationSize) |>
    ungroup() |>
    inner_join(cohortMethodData$covariateRef, by = "covariateId") |>
    inner_join(cohortMethodData$analysisRef, by = "analysisId") |>
    filter(.data$isBinary == "Y") |>
    select("covariateId", "prevalence") |>
    collect()

  message("Computing propensity model")
  propensityScore <- createPs(cohortMethodData,
                              maxCohortSizeForFitting = 25000,
                              prior = Cyclops::createPrior("laplace", 0.1, exclude = 0)
  )
  propensityModel <- attr(propensityScore, "metaData")$psModelCoef

  message("Fitting outcome model(s)")
  outcomeIds <- attr(cohortMethodData, "metaData")$outcomeIds
  outcomeModels <- vector("list", length(outcomeIds))
  for (i in 1:length(outcomeIds)) {
    outcomeId <- outcomeIds[i]
    studyPop <- createStudyPopulation(
      cohortMethodData = cohortMethodData,
      population = propensityScore,
      minDaysAtRisk = 1,
      removeSubjectsWithPriorOutcome = FALSE,
      outcomeId = outcomeId
    )
    studyPop <- matchOnPs(studyPop, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)
    outcomeModel <- fitOutcomeModel(
      population = studyPop,
      cohortMethodData = cohortMethodData,
      modelType = "poisson",
      stratified = FALSE,
      useCovariates = TRUE,
      prior = Cyclops::createPrior("laplace", 0.1, exclude = 0),
      control = Cyclops::createControl(threads = 2),
      profileBounds = NULL
    )
    outcomeModels[[i]] <- outcomeModel$outcomeModelCoefficients[outcomeModel$outcomeModelCoefficients != 0]
  }

  message("Computing rates of prior outcomes")
  totalTime <- cohortMethodData$cohorts |>
    summarise(time = sum(.data$daysFromObsStart, na.rm = TRUE)) |>
    pull()

  preIndexOutcomeRates <- cohortMethodData$outcomes |>
    filter(.data$daysToEvent < 0) |>
    group_by(.data$outcomeId) |>
    summarise(n = n_distinct(.data$rowId)) |>
    mutate(rate = .data$n / totalTime) |>
    select("outcomeId", "rate") |>
    collect()

  message("Fitting models for time to observation period start, end and time to cohort end")
  cohorts <- cohortMethodData$cohorts |>
    collect()

  obsEnd <- cohorts$daysToObsEnd
  cohortEnd <- cohorts$daysToCohortEnd
  event <- as.integer(cohortEnd < obsEnd)
  time <- cohortEnd
  time[cohortEnd > obsEnd] <- obsEnd[cohortEnd > obsEnd]
  data <- tibble(time = time, event = event)
  data <- data[data$time > 0, ]
  fitCohortEnd <- survival::survreg(survival::Surv(
    time,
    event
  ) ~ 1, data = data, dist = "exponential")
  fitObsEnd <- survival::survreg(survival::Surv(obsEnd[obsEnd > 0]) ~ 1, dist = "exponential")
  data <- cohorts
  minObsTime <- min(data$daysFromObsStart)
  data$time <- data$daysFromObsStart - minObsTime + 1
  fitObsStart <- survival::survreg(survival::Surv(time) ~ 1, data = data, dist = "exponential")

  result <- list(
    covariatePrevalence = covariatePrevalence,
    propensityModel = propensityModel,
    outcomeModels = outcomeModels,
    preIndexOutcomeRates = preIndexOutcomeRates,
    metaData = attr(cohortMethodData, "metaData"),
    covariateRef = collect(cohortMethodData$covariateRef),
    analysisRef = collect(cohortMethodData$analysisRef),
    cohortEndRate = 1 / exp(coef(fitCohortEnd)),
    obsStartRate = 1 / exp(coef(fitObsStart)),
    minObsTime = minObsTime,
    obsEndRate = 1 / exp(coef(fitObsEnd))
  )

  class(result) <- "CohortDataSimulationProfile"
  result <- .truncateSimulationProfile(result, minCellCount)
  return(result)
}

#' Generate simulated data
#'
#' @description
#' Creates a [CohortMethodData] object with simulated data.
#'
#' @param profile   An object of type `CohortMethodDataSimulationProfile` as generated using the
#'                  [createCohortMethodDataSimulationProfile()] function.
#' @param n         The size of the population to be generated.
#'
#' @details
#' This function generates simulated data that is in many ways similar to the original data on which
#' the simulation profile is based. The contains same outcome, comparator, and outcome concept IDs,
#' and the covariates and their 1st order statistics should be comparable.
#'
#' @return
#' An object of type [CohortMethodData].
#'
#' @export
simulateCohortMethodData <- function(profile, n = 10000) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(profile, "CohortDataSimulationProfile", add = errorMessages)
  checkmate::assertInt(n, lower = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  message("Generating covariates")
  # Treatment variable is generated elsewhere:
  covariatePrevalence <- profile$covariatePrevalence[names(profile$covariatePrevalence) != "1"]

  personsPerCov <- rpois(n = nrow(covariatePrevalence), lambda = covariatePrevalence$prevalence * n)
  personsPerCov[personsPerCov > n] <- n
  rowId <- sapply(personsPerCov, function(x, n) sample.int(size = x, n), n = n)
  rowId <- do.call("c", rowId)
  covariateIds <- covariatePrevalence$covariateId
  covariateId <- sapply(1:length(personsPerCov),
                        function(x, personsPerCov, covariateIds) {
                          rep(
                            covariateIds[x],
                            personsPerCov[x]
                          )
                        },
                        personsPerCov = personsPerCov,
                        covariateIds = covariateIds
  )
  covariateId <- do.call("c", covariateId)
  covariateValue <- rep(1, length(covariateId))
  covariates <- tibble(
    rowId = rowId,
    covariateId = covariateId,
    covariateValue = covariateValue
  )

  message("Generating treatment variable")
  betas <- profile$propensityModel
  intercept <- betas[1]
  betas <- betas[2:length(betas)]
  betas <- tibble(
    beta = as.numeric(betas),
    covariateId = as.numeric(names(betas))
  )
  treatmentVar <- covariates |>
    inner_join(betas, by = "covariateId") |>
    mutate(value = .data$covariateValue * .data$beta) |>
    group_by(.data$rowId) |>
    summarise(value = sum(.data$value) + intercept)
  link <- function(x) {
    return(1 / (1 + exp(-x)))
  }
  treatmentVar$value <- link(treatmentVar$value)
  treatmentVar$rand <- runif(nrow(treatmentVar))
  treatmentVar$covariateValue <- as.integer(treatmentVar$rand < treatmentVar$value)
  treatmentVar <- treatmentVar[, c("rowId", "covariateValue")]
  treatmentVar$covariateId <- 1

  message("Generating cohorts")
  cohorts <- tibble(
    rowId = treatmentVar$rowId,
    treatment = treatmentVar$covariateValue,
    personId = treatmentVar$rowId,
    personSeqId = treatmentVar$rowId,
    cohortStartDate = as.Date("2000-01-01"),
    daysFromObsStart = profile$minObsTime + round(rexp(
      n,
      profile$obsStartRate
    )) - 1,
    daysToCohortEnd = round(rexp(n, profile$obsEndRate)),
    daysToObsEnd = round(rexp(n, profile$cohortEndRate))
  )

  message("Generating outcomes after index date")
  allOutcomes <- tibble()
  for (i in 1:length(profile$metaData$outcomeIds)) {
    betas <- profile$outcomeModels[[i]]
    intercept <- betas[1]
    betas <- betas[2:length(betas)]
    betas <- tibble(beta = as.numeric(betas), covariateId = as.numeric(names(betas)))
    temp <- merge(covariates, betas)
    temp$value <- temp$covariateValue * temp$beta # Currently pointless, since covariateValue is always 1
    temp <- aggregate(value ~ rowId, data = temp, sum)
    temp$value <- temp$value + intercept
    temp$value <- exp(temp$value) # Value is now the rate
    temp <- merge(temp, cohorts[, c("rowId", "daysToObsEnd")])
    temp$value <- temp$value * temp$daysToObsEnd # Value is lambda
    temp$nOutcomes <- rpois(n, temp$value)
    temp$nOutcomes[temp$nOutcomes > temp$daysToObsEnd] <- temp$daysToObsEnd[temp$nOutcomes > temp$daysToObsEnd]
    outcomeRows <- sum(temp$nOutcomes)
    outcomes <- tibble(
      rowId = rep(0, outcomeRows),
      outcomeId = rep(profile$metaData$outcomeIds[i], outcomeRows),
      daysToEvent = rep(0, outcomeRows)
    )
    cursor <- 1
    for (j in 1:nrow(temp)) {
      nOutcomes <- temp$nOutcomes[j]
      if (nOutcomes != 0) {
        outcomes$rowId[cursor:(cursor + nOutcomes - 1)] <- temp$rowId[j]
        outcomes$daysToEvent[cursor:(cursor + nOutcomes - 1)] <- sample.int(
          size = nOutcomes,
          temp$daysToObsEnd[j]
        )
        cursor <- cursor + nOutcomes
      }
    }
    allOutcomes <- rbind(allOutcomes, outcomes)
  }

  message("Generating outcomes before index date")
  for (i in 1:length(profile$metaData$outcomeIds)) {
    outcomeId <- profile$metaData$outcomeIds[i]
    rate <- profile$preIndexOutcomeRates$rate[profile$preIndexOutcomeRates$outcomeId == outcomeId]
    nOutcomes <- rpois(nrow(cohorts), rate * cohorts$daysFromObsStart)
    nOutcomes[nOutcomes > cohorts$daysFromObsStart] <- cohorts$daysFromObsStart[nOutcomes > cohorts$daysFromObsStart]
    outcomeRows <- sum(nOutcomes)
    outcomes <- tibble(
      rowId = rep(0, outcomeRows),
      outcomeId = rep(outcomeId, outcomeRows),
      daysToEvent = rep(0, outcomeRows)
    )
    cursor <- 1
    for (j in 1:length(nOutcomes)) {
      if (nOutcomes[j] != 0) {
        outcomes$rowId[cursor:(cursor + nOutcomes[j] - 1)] <- cohorts$rowId[j]
        outcomes$daysToEvent[cursor:(cursor + nOutcomes[j] - 1)] <- -sample.int(
          size = nOutcomes[j],
          cohorts$daysFromObsStart[j]
        )
        cursor <- cursor + nOutcomes[j]
      }
    }
    allOutcomes <- rbind(allOutcomes, outcomes)
  }

  result <- Andromeda::andromeda(
    outcomes = allOutcomes,
    cohorts = cohorts,
    covariates = covariates,
    covariateRef = profile$covariateRef,
    analysisRef = profile$analysisRef
  )
  metaData <- profile$metaData
  metaData$populationSize <- n
  attr(result, "metaData") <- metaData
  class(result) <- "CohortMethodData"
  attr(class(result), "package") <- "CohortMethod"
  return(result)
}

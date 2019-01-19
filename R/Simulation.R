# @file CohortMethod.R
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

#' Create simulation profile
#'
#' @description
#' \code{createCohortMethodDataSimulationProfile} creates a profile based on the provided
#' cohortMethodData object, which can be used to generate simulated data that has similar
#' characteristics.
#'
#' @param cohortMethodData   An object of type \code{cohortMethodData} as generated using
#'                           \code{getDbCohortMethodData}.
#'
#' @details
#' The output of this function is an object that can be used by the \code{simulateCohortMethodData}
#' function to generate a cohortMethodData object.
#'
#' @return
#' An object of type \code{cohortDataSimulationProfile}.
#'
#' @export
createCohortMethodDataSimulationProfile <- function(cohortMethodData) {
  ParallelLogger::logInfo("Computing covariate prevalence")  # (Note: currently assuming binary covariates)
  sums <- quickSum(cohortMethodData$covariates)
  covariatePrevalence <- sums$sum/nrow(cohortMethodData$cohorts)
  attr(covariatePrevalence, "names") <- sums$covariateId

  ParallelLogger::logInfo("Computing propensity model")
  propensityScore <- createPs(cohortMethodData,
                              prior = Cyclops::createPrior("laplace", 0.1, exclude = 0))
  propensityModel <- attr(propensityScore, "metaData")$psModelCoef

  ParallelLogger::logInfo("Fitting outcome model(s)")
  outcomeIds <- attr(cohortMethodData$outcomes, "metaData")$outcomeIds
  outcomeModels <- vector("list", length(outcomeIds))
  for (i in 1:length(outcomeIds)) {
    outcomeId <- outcomeIds[i]
    studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                      population = propensityScore,
                                      minDaysAtRisk = 1,
                                      removeSubjectsWithPriorOutcome = FALSE,
                                      outcomeId = outcomeId)
    # studyPop <- trimByPsToEquipoise(studyPop)
    studyPop <- matchOnPs(studyPop, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)
    outcomeModel <- fitOutcomeModel(population = studyPop,
                                    cohortMethodData = cohortMethodData,
                                    modelType = "poisson",
                                    stratified = FALSE,
                                    useCovariates = TRUE,
                                    prior = Cyclops::createPrior("laplace", 0.1, exclude = 0))
    outcomeModels[[i]] <- outcomeModel$outcomeModelCoefficients
  }

  ParallelLogger::logInfo("Computing rates of prior outcomes")
  totalTime <- sum(cohortMethodData$cohorts$daysFromObsStart)
  preIndexOutcomeRates <- aggregate(rowId ~ outcomeId,
                                    data = cohortMethodData$outcomes[cohortMethodData$outcomes$daysToEvent <
    0, ], length)
  preIndexOutcomeRates$rate <- preIndexOutcomeRates$rowId/totalTime
  preIndexOutcomeRates$rowId <- NULL

  ParallelLogger::logInfo("Fitting models for time to observation period start, end and time to cohort end")
  obsEnd <- cohortMethodData$cohorts$daysToObsEnd
  cohortEnd <- cohortMethodData$cohorts$daysToCohortEnd
  event <- as.integer(cohortEnd < obsEnd)
  time <- cohortEnd
  time[cohortEnd > obsEnd] <- obsEnd[cohortEnd > obsEnd]
  data <- data.frame(time = time, event = event)
  data <- data[data$time > 0, ]
  fitCohortEnd <- survival::survreg(survival::Surv(time,
                                                   event) ~ 1, data = data, dist = "exponential")
  fitObsEnd <- survival::survreg(survival::Surv(obsEnd[obsEnd > 0]) ~ 1, dist = "exponential")
  data <- cohortMethodData$cohort
  minObsTime <- min(data$daysFromObsStart)
  data$time <- data$daysFromObsStart - minObsTime + 1
  fitObsStart <- survival::survreg(survival::Surv(time) ~ 1, data = data, dist = "exponential")

  result <- list(covariatePrevalence = covariatePrevalence,
                 propensityModel = propensityModel,
                 outcomeModels = outcomeModels,
                 preIndexOutcomeRates = preIndexOutcomeRates,
                 metaData = cohortMethodData$metaData,
                 cohortsMetaData = attr(cohortMethodData$cohorts, "metaData"),
                 outcomesMetaData = attr(cohortMethodData$outcomes, "metaData"),
                 covariateRef = ff::as.ram(cohortMethodData$covariateRef),
                 analysisRef = ff::as.ram(cohortMethodData$analysisRef),
                 cohortEndRate = 1/exp(coef(fitCohortEnd)),
                 obsStartRate = 1/exp(coef(fitObsStart)),
                 minObsTime = minObsTime,
                 obsEndRate = 1/exp(coef(fitObsEnd)))
  class(result) <- "cohortDataSimulationProfile"
  return(result)
}

#' Generate simulated data
#'
#' @description
#' \code{simulateCohortMethodData} creates a cohortMethodData object with simulated data.
#'
#' @param profile   An object of type \code{cohortMethodDataSimulationProfile} as generated using the
#'                  \cr\code{createCohortMethodDataSimulationProfile} function.
#' @param n         The size of the population to be generated.
#'
#' @details
#' This function generates simulated data that is in many ways similar to the original data on which
#' the simulation profile is based. The contains same outcome, comparator, and outcome concept IDs,
#' and the covariates and their 1st order statistics should be comparable.
#'
#' @return
#' An object of type \code{cohortMethodData}.
#'
#' @export
simulateCohortMethodData <- function(profile, n = 10000) {
  # Note: currently, simulation is done completely in-memory. Could easily do batch-wise, storing in
  # ffdf
  ParallelLogger::logInfo("Generating covariates")
  # Treatment variable is generated elsewhere:
  covariatePrevalence <- profile$covariatePrevalence[names(profile$covariatePrevalence) != "1"]

  personsPerCov <- rpois(n = length(covariatePrevalence), lambda = covariatePrevalence * n)
  personsPerCov[personsPerCov > n] <- n
  rowId <- sapply(personsPerCov, function(x, n) sample.int(size = x, n), n = n)
  rowId <- do.call("c", rowId)
  covariateIds <- as.numeric(names(covariatePrevalence))
  covariateId <- sapply(1:length(personsPerCov),
                        function(x, personsPerCov, covariateIds) rep(covariateIds[x],
                                                                     personsPerCov[x]),
                        personsPerCov = personsPerCov,
                        covariateIds = covariateIds)
  covariateId <- do.call("c", covariateId)
  covariateValue <- rep(1, length(covariateId))
  covariates <- data.frame(rowId = rowId,
                           covariateId = covariateId,
                           covariateValue = covariateValue)

  ParallelLogger::logInfo("Generating treatment variable")
  betas <- profile$propensityModel
  intercept <- betas[1]
  betas <- betas[2:length(betas)]
  betas <- data.frame(beta = as.numeric(betas), covariateId = as.numeric(names(betas)))
  treatment <- merge(covariates, betas)
  treatment$value <- treatment$covariateValue * treatment$beta  #Currently pointless, since covariateValue is always 1
  treatment <- aggregate(value ~ rowId, data = treatment, sum)
  treatment$value <- treatment$value + intercept
  link <- function(x) {
    return(1/(1 + exp(-x)))
  }
  treatment$value <- link(treatment$value)
  treatment$rand <- runif(nrow(treatment))
  treatment$covariateValue <- as.integer(treatment$rand < treatment$value)
  treatment <- treatment[, c("rowId", "covariateValue")]
  treatment$covariateId <- 1

  ParallelLogger::logInfo("Generating cohorts")
  cohorts <- data.frame(rowId = treatment$rowId,
                        treatment = treatment$covariateValue,
                        subjectId = treatment$rowId,
                        cohortStartDate = "2000-01-01",
                        daysFromObsStart = profile$minObsTime + round(rexp(n,
                                                                           profile$obsStartRate)) - 1,
                        daysToCohortEnd = round(rexp(n, profile$obsEndRate)),
                        daysToObsEnd = round(rexp(n, profile$cohortEndRate)))
  attr(cohorts, "metaData") <- profile$cohortsMetaData

  ParallelLogger::logInfo("Generating outcomes after index date")
  allOutcomes <- data.frame()
  for (i in 1:length(profile$outcomesMetaData$outcomeIds)) {
    betas <- profile$outcomeModels[[i]]
    intercept <- betas[1]
    betas <- betas[2:length(betas)]
    betas <- data.frame(beta = as.numeric(betas), covariateId = as.numeric(names(betas)))
    temp <- merge(covariates, betas)
    temp$value <- temp$covariateValue * temp$beta  #Currently pointless, since covariateValue is always 1
    temp <- aggregate(value ~ rowId, data = temp, sum)
    temp$value <- temp$value + intercept
    temp$value <- exp(temp$value)  #Value is now the rate
    temp <- merge(temp, cohorts[, c("rowId", "daysToObsEnd")])
    temp$value <- temp$value * temp$daysToObsEnd  #Value is lambda
    temp$nOutcomes <- rpois(n, temp$value)
    temp$nOutcomes[temp$nOutcomes > temp$daysToObsEnd] <- temp$daysToObsEnd[temp$nOutcomes > temp$daysToObsEnd]
    outcomeRows <- sum(temp$nOutcomes)
    outcomes <- data.frame(rowId = rep(0, outcomeRows),
                           outcomeId = rep(profile$outcomesMetaData$outcomeIds[i], outcomeRows),
                           daysToEvent = rep(0, outcomeRows))
    cursor <- 1
    for (i in 1:nrow(temp)) {
      nOutcomes <- temp$nOutcomes[i]
      if (nOutcomes != 0) {
        outcomes$rowId[cursor:(cursor + nOutcomes - 1)] <- temp$rowId[i]
        outcomes$daysToEvent[cursor:(cursor + nOutcomes - 1)] <- sample.int(size = nOutcomes,
                                                                            temp$daysToObsEnd[i])
        cursor <- cursor + nOutcomes
      }
    }
    allOutcomes <- rbind(allOutcomes, outcomes)
  }

  ParallelLogger::logInfo("Generating outcomes before index date")
  for (i in 1:length(profile$outcomesMetaData$outcomeIds)) {
    outcomeId <- profile$outcomesMetaData$outcomeIds[i]
    rate <- profile$preIndexOutcomeRates$rate[profile$preIndexOutcomeRates$outcomeId == outcomeId]
    nOutcomes <- rpois(nrow(cohorts), rate * cohorts$daysFromObsStart)
    nOutcomes[nOutcomes > cohorts$daysFromObsStart] <- cohorts$daysFromObsStart[nOutcomes > cohorts$daysFromObsStart]
    outcomeRows <- sum(nOutcomes)
    outcomes <- data.frame(rowId = rep(0, outcomeRows),
                           outcomeId = rep(outcomeId, outcomeRows),
                           daysToEvent = rep(0, outcomeRows))
    cursor <- 1
    for (j in 1:length(nOutcomes)) {
      if (nOutcomes[j] != 0) {
        outcomes$rowId[cursor:(cursor + nOutcomes[j] - 1)] <- cohorts$rowId[j]
        outcomes$daysToEvent[cursor:(cursor + nOutcomes[j] - 1)] <- -sample.int(size = nOutcomes[j],
                                                                                cohorts$daysFromObsStart[j])
        cursor <- cursor + nOutcomes[j]
      }
    }
    allOutcomes <- rbind(allOutcomes, outcomes)
  }
  attr(allOutcomes, "metaData") <- profile$outcomesMetaData

  # Remove rownames else they will be copied to the ffdf objects:
  rownames(covariates) <- NULL
  rownames(profile$covariateRef) <- NULL

  result <- list(outcomes = allOutcomes,
                 cohorts = cohorts,
                 covariates = ff::as.ffdf(covariates),
                 covariateRef = ff::as.ffdf(profile$covariateRef),
                 analysisRef = ff::as.ffdf(profile$analysisRef),
                 metaData = profile$metaData)

  class(result) <- "cohortMethodData"
  return(result)
}


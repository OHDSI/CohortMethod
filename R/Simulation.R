# @file CohortMethod.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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
  writeLines("Computing covariate prevalence")  # (Note: currently assuming binary covariates)
  sums <- quickSum(cohortMethodData$covariates)
  covariatePrevalence <- sums$sum/nrow(cohortMethodData$cohorts)
  attr(covariatePrevalence, "names") <- sums$covariateId

  writeLines("Computing propensity model")
  propensityScore <- createPs(cohortMethodData, prior = Cyclops::createPrior("laplace", 0.1))
  propensityModel <- attr(propensityScore, "coefficients")

  writeLines("Fitting outcome model(s)")
  psTrimmed <- trimByPsToEquipoise(propensityScore)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)

  outcomeModels <- vector("list", length(cohortMethodData$metaData$outcomeIds))
  for (i in 1:length(cohortMethodData$metaData$outcomeIds)) {
    outcomeId <- cohortMethodData$metaData$outcomeIds[i]
    outcomeModel <- fitOutcomeModel(outcomeId,
                                    cohortMethodData,
                                    strata,
                                    useCovariates = TRUE,
                                    modelType = "pr",
                                    prior = Cyclops::createPrior("laplace", 0.1))
    outcomeModels[[i]] <- outcomeModel$coefficients
  }

  writeLines("Fitting models for time to observation period end and time to cohort end")
  obsEnd <- ff::as.ram(cohortMethodData$cohorts$timeToObsPeriodEnd)
  cohortEnd <- ff::as.ram(cohortMethodData$cohorts$timeToCohortEnd)
  event <- as.integer(cohortEnd < obsEnd)
  time <- cohortEnd
  time[cohortEnd > obsEnd] <- obsEnd[cohortEnd > obsEnd]
  data <- data.frame(time = time, event = event)
  data <- data[data$time > 0, ]
  fitCohortEnd <- survival::survreg(survival::Surv(time,
                                                   event) ~ 1, data = data, dist = "exponential")
  fitObsEnd <- survival::survreg(survival::Surv(obsEnd[obsEnd > 0]) ~ 1, dist = "exponential")

  writeLines("Computing prevalence of exlusion")
  exclusionPrevalence <- table(ff::as.ram(cohortMethodData$exclude$outcomeId))/nrow(cohortMethodData$cohorts)

  result <- list(covariatePrevalence = covariatePrevalence,
                 propensityModel = propensityModel,
                 outcomeModels = outcomeModels,
                 metaData = cohortMethodData$metaData,
                 covariateRef = ff::as.ram(cohortMethodData$covariateRef),
                 cohortEndRate = 1/exp(coef(fitCohortEnd)),
                 obsEndRate = 1/exp(coef(fitObsEnd)),
                 exclusionPrevalence = exclusionPrevalence)
  class(result) <- "cohortDataSimulationProfile"
  return(result)
}

#' Generate simulated data
#'
#' @description
#' \code{simulateCohortMethodData} creates a cohortMethodData object with simulated data.
#'
#' @param cohortDataSimulationProfile   An object of type \code{cohortDataSimulationProfile} as
#'                                      generated using the
#'                                      \cr\code{createCohortMethodDataSimulationProfile} function.
#' @param n                             The size of the population to be generated.
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
simulateCohortMethodData <- function(cohortDataSimulationProfile, n = 10000) {
  # Note: currently, simulation is done completely in-memory. Could easily do batch-wise, storing in
  # ffdf
  writeLines("Generating covariates")
  # Treatment variable is generated elsewhere:
  covariatePrevalence <- cohortDataSimulationProfile$covariatePrevalence[names(cohortDataSimulationProfile$covariatePrevalence) !=
                                                                           "1"]

  personsPerCov <- rpois(n = length(covariatePrevalence), lambda = covariatePrevalence * n)
  personsPerCov[personsPerCov > n] <- n
  covariateIds <- as.numeric(names(covariatePrevalence))
  covariateIds <- covariateIds[covariateIds != 1]
  covarRows <- sum(personsPerCov)
  covariates <- data.frame(rowId = rep(0, covarRows),
                           covariateId = rep(0, covarRows),
                           covariateValue = rep(1, covarRows))
  cursor <- 1
  for (i in 1:length(personsPerCov)) {
    nCovar <- personsPerCov[i]
    if (nCovar != 0) {
      covariates$covariateId[cursor:(cursor + nCovar - 1)] <- covariateIds[i]
      covariates$rowId[cursor:(cursor + nCovar - 1)] <- sample.int(size = nCovar, n)
      cursor <- cursor + nCovar
    }
  }

  writeLines("Generating treatment variable")
  betas <- cohortDataSimulationProfile$propensityModel
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
  covariates <- rbind(covariates, treatment[treatment$covariateValue == 1, ])

  writeLines("Generating cohorts")
  cohorts <- data.frame(rowId = treatment$rowId,
                        treatment = treatment$covariateValue,
                        personId = treatment$rowId,
                        timeToObsPeriodEnd = round(rexp(n, cohortDataSimulationProfile$obsEndRate)),
                        timeToCohortEnd = round(rexp(n, cohortDataSimulationProfile$cohortEndRate)))

  writeLines("Generating outcomes")
  allOutcomes <- data.frame()
  for (i in 1:length(cohortDataSimulationProfile$metaData$outcomeIds)) {
    betas <- cohortDataSimulationProfile$outcomeModels[[i]]
    intercept <- betas[1]
    betas <- betas[2:length(betas)]
    betas <- data.frame(beta = as.numeric(betas), covariateId = as.numeric(names(betas)))
    temp <- merge(covariates, betas)
    temp$value <- temp$covariateValue * temp$beta  #Currently pointless, since covariateValue is always 1
    temp <- aggregate(value ~ rowId, data = temp, sum)
    temp$value <- temp$value + intercept
    temp$value <- exp(temp$value)  #Value is now the rate
    temp <- merge(temp, cohorts[, c("rowId", "timeToObsPeriodEnd")])
    temp$value <- temp$value * temp$timeToObsPeriodEnd  #Value is lambda
    temp$nOutcomes <- rpois(n, temp$value)
    temp$nOutcomes[temp$nOutcomes > temp$timeToObsPeriodEnd] <- temp$timeToObsPeriodEnd[temp$nOutcomes >
                                                                                          temp$timeToObsPeriodEnd]
    outcomeRows <- sum(temp$nOutcomes)
    outcomes <- data.frame(rowId = rep(0, outcomeRows),
                           outcomeId = rep(cohortDataSimulationProfile$metaData$outcomeIds[i],
                                           outcomeRows),
                           timeToEvent = rep(0, outcomeRows))
    cursor <- 1
    for (i in 1:nrow(temp)) {
      nOutcomes <- temp$nOutcomes[i]
      if (nOutcomes != 0) {
        outcomes$rowId[cursor:(cursor + nOutcomes - 1)] <- temp$rowId[i]
        outcomes$timeToEvent[cursor:(cursor + nOutcomes - 1)] <- sample.int(size = nOutcomes,
                                                                            temp$timeToObsPeriodEnd[i])
        cursor <- cursor + nOutcomes
      }
    }
    allOutcomes <- rbind(allOutcomes, outcomes)
  }

  writeLines("Generating exclusion")
  exclude <- data.frame()
  for (i in 1:nrow(cohortDataSimulationProfile$exclusionPrevalence)) {
    sampleSize <- cohortDataSimulationProfile$exclusionPrevalence[i] * nrow(cohorts)
    rowId <- cohorts$rowId[sample(nrow(cohorts), size = sampleSize, replace = FALSE)]
    outcomeId <- as.numeric(names(cohortDataSimulationProfile$exclusionPrevalence)[i])
    exclude <- rbind(exclude, data.frame(rowId = rowId, outcomeId = outcomeId))
  }
  # Remove rownames else they will be copied to the ffdf objects:
  rownames(allOutcomes) <- NULL
  rownames(cohorts) <- NULL
  rownames(covariates) <- NULL
  rownames(exclude) <- NULL
  rownames(cohortDataSimulationProfile$covariateRef) <- NULL

  result <- list(outcomes = ff::as.ffdf(allOutcomes),
                 cohorts = ff::as.ffdf(cohorts),
                 covariates = ff::as.ffdf(covariates),
                 exclude = ff::as.ffdf(exclude),
                 covariateRef = ff::as.ffdf(cohortDataSimulationProfile$covariateRef),
                 metaData = cohortDataSimulationProfile$metaData)

  class(result) <- "cohortMethodData"
  return(result)
}

#' @export
simulateCohortMethodDataTest <- function(cohortMethodData, n = 10000,
                                         simulateTreatment = FALSE,
                                         replaceBeta = FALSE,
                                         newBeta=0,
                                         excludeCovariateIds = NULL) {

  propensityScore <- createPs(cohortMethodData, prior = Cyclops::createPrior("laplace", 0.1), excludeCovariateIds = excludeCovariateIds)
  propensityModel <- attr(propensityScore, "coefficients")

  if (simulateTreatment) {
    covariates = cohortMethodData$covariates[ffbase::ffwhich(cohortMethodData$covariates, !in.ff(cohortMethodData$covariates$covariateId, ff::as.ff(c(1, excludeCovariateIds)))),]
    covariateIds = unique(covariates$covariateId[])
    covariateIds = covariateIds[which(covariateIds!=1)]
    propensityModel = rnorm(length(covariateIds)+1)
    names(propensityModel) = c("(Intercept)", covariateIds)
    betas = ff::ffdf(covariateId = ff::as.ff(as.numeric(names(propensityModel[2:length(propensityModel)]))),
                     beta = ff::as.ff(propensityModel[2:length(propensityModel)]))
    temp = ffbase::merge.ffdf(covariates, betas)
    temp$value = temp$covariateValue * temp$beta
    temp = ff::as.ffdf(PatientLevelPrediction::bySumFf(temp$value, temp$rowId))
    names(temp) = c("rowId", "value")
    temp$value = temp$value + ff::ff(vmode = "double", initdata = propensityModel[1], length = nrow(temp))
    temp$propensityScore = 1 / (1 + exp(-1 * temp$value))
    temp$rand = ff::as.ff(runif(nrow(temp)))
    temp$value = ff::as.ff(as.integer((temp$rand < temp$propensityScore)[]))
    temp$rand <- NULL
    propensityScore = data.frame(rowId = temp$rowId[], propensityScore = temp$propensityScore[], treatment = temp$value[])
    cohorts = ffbase::merge.ffdf(cohortMethodData$cohorts, temp)
    cohorts$treatment = cohorts$value
    cohorts$value <- NULL
    cohorts$propensityScore <- NULL
    cohortMethodData$cohorts = cohorts
    t = ffbase::ffwhich(temp, temp$value == 1)
    covariates = ffbase::ffdfrbind.fill(ff::ffdf(rowId = temp$rowId[t],
                                                 covariateId = ff::ff(vmode = "double", initdata = 1, length = length(t)),
                                                 covariateValue = ff::ff(vmode = "double", initdata = 1, length = length(t))),
                                        covariates)
    cohortMethodData$covariates = covariates
  }

  psTrimmed <- trimByPsToEquipoise(propensityScore)
  #strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)
  outcomeId <- cohortMethodData$metaData$outcomeIds[1]

  # generate outcome model for outcome
  outcomeModel <- fitOutcomeModel(outcomeId,
                                  cohortMethodData,
                                  psTrimmed,
                                  useCovariates = TRUE,
                                  modelType = "cox",
                                  prior = Cyclops::createPrior("laplace", 0.1),
                                  returnFit = TRUE,
                                  stratifiedCox = FALSE)
  if (replaceBeta) {
    Cyclops::setExposureBeta(outcomeModel$fit$interface, newBeta)
    outcomeModel$coefficients[1] = newBeta
  }
  effectSize = outcomeModel$coefficients[1]

  # generate outcome model for censor
  cohortMethodData1 = invertOutcome(cohortMethodData, outcomeId)
  outcomeModel1 <- fitOutcomeModel(outcomeId,
                                   cohortMethodData1,
                                   psTrimmed,
                                   useCovariates = TRUE,
                                   modelType = "cox",
                                   prior = Cyclops::createPrior("laplace", 0.1),
                                   returnFit = TRUE,
                                   stratifiedCox = FALSE,
                                   control = createControl(maxIterations = 10000))

  ids = ff::ffdf(newId = ff::ff(vmode = "double", initdata = 1:n),
                 rowId = ff::as.ff(sample(outcomeModel$data$rowId, n, replace = TRUE)))

  # generate event times
  sTimes = simulateTimes(outcomeModel, ids)
  names(sTimes)[match("time", names(sTimes))] = "sTime"

  # generate censor times
  cTimes = simulateTimes(outcomeModel1, ids)
  names(cTimes)[match("time", names(cTimes))] = "cTime"

  # combine event and censor times
  times = ffbase::merge.ffdf(sTimes, cTimes)
  times$y = ff::ff(vmode = "double", initdata = 0, length = n)
  t = ffbase::ffwhich(times, times$sTime<times$cTime)
  times$y[t] = ff::ff(vmode = "double", initdata = 1, length = length(t))
  times$timeToEvent = times$cTime
  times$timeToEvent[t] = times$sTime[t]
  times$rowId <- NULL
  times$sTime <- NULL

  # generate cohorts
  cohorts = ffbase::merge.ffdf(ids, cohortMethodData$cohorts)
  cohorts$rowId <- NULL
  cohorts = ffbase::merge.ffdf(cohorts, times)
  cohorts$y <- NULL
  cohorts$timeToObsPeriodEnd = cohorts$cTime
  cohorts$cTime <- NULL
  cohorts$timeToEvent <- NULL
  names(cohorts)[match("newId", names(cohorts))] = "rowId"

  # generate outcomes
  outcomes = times[ffbase::ffwhich(times, times$y==1),]
  outcomes$y = ff::ff(vmode = "double", initdata = outcomeId, length = length(outcomes$y))
  names(outcomes)[match("y", names(outcomes))] = "outcomeId"
  outcomes$cTime <- NULL
  names(outcomes)[match("newId", names(outcomes))] = "rowId"

  # generate covariates
  covariates = NULL
  ids1 = ids
  while(dim(ids1)[1]!=0) {
    ids1 = ff::as.ffdf(ids1)
    covariates = if(is.null(covariates)) ffbase::merge.ffdf(cohortMethodData$covariates, ids1) else ffbase::ffdfrbind.fill(covariates, ffbase::merge.ffdf(cohortMethodData$covariates, ids1))
    t = ffbase::ffmatch(unique(ids1$rowId), ids1$rowId)
    ids1 = ids1[-t[],]
  }
  covariates$rowId <- covariates$newId
  covariates$newId <- NULL

  # generate covariate ref
  covariateRef = ffbase::merge.ffdf(cohortMethodData$covariateRef, covariates)
  covariateRef$rowId <- NULL
  covariateRef$covariateValue <- NULL

  result = list(outcomes = outcomes,
                cohorts = cohorts,
                covariates = covariates,
                exclude = NULL,
                covariateRef = covariateRef,
                metaData = cohortMethodData$metaData)

  return (list(cohortMethodData = result,
               propensityModel = propensityModel,
               effectSize = effectSize))
}

simulateTimes <- function(outcomeModel, ids) {
  fit = outcomeModel$fit
  data = outcomeModel$data
  data = data[order(data$time),]

  accDenom = ff::as.ff(Cyclops::getAccDenom(fit$interface))
  times = ff::as.ff(Cyclops::getTimes(fit$interface))
  y = ff::as.ff(Cyclops::getY(fit$interface))

  baseline = calculateBaselineSurvivalFunction(accDenom, times, y)
  x = calculateXB(rowId = ff::as.ff(data$rowId),
                  covariates = cohortMethodData$covariates,
                  coef = ff::ffdf(covariateId = ff::as.ff(as.numeric(names(outcomeModel$coefficients))),
                                  beta = ff::as.ff(outcomeModel$coefficients)))
  return(generateEventTimes(x, ids, times, baseline))
}

invertOutcome <- function(cohortMethodData, outcomeId) {
  ids = unique(cohortMethodData$outcomes$rowId)

  newOutcomes = cohortMethodData$cohorts[ffbase::ffwhich(cohortMethodData$cohorts, !in.ff(cohortMethodData$cohorts$rowId, ids)),]
  newOutcomes = ff::ffdf(rowId = newOutcomes$rowId,
                         outcomeId = ff::ff(vmode = "double", initdata = outcomeId, length = length(newOutcomes$rowId)),
                         timeToEvent = newOutcomes$timeToObsPeriodEnd)
  result = cohortMethodData
  result$outcomes = newOutcomes
  return(result)
}

calculateXB <- function(rowId, covariates, coef) {
  coef = coef[ffbase::ffwhich(coef,coef$beta>0),]
  covariates = covariates[ffbase::ffwhich(covariates, in.ff(covariates$rowId, rowId)),]
  covariates = covariates[ffbase::ffwhich(covariates, in.ff(covariates$covariateId, coef$covariateId)),]
  covariates = ffbase::merge.ffdf(covariates, coef)
  covariates$value = covariates$covariateValue * covariates$beta
  temp = ff::as.ffdf(PatientLevelPrediction::bySumFf(covariates$value, covariates$rowId))
  result = ff::ffdf(rowId = rowId, xb = ff::ff(vmode = "double", initdata = 0, length = length(rowId)))
  t = ffbase::ffmatch(temp$bins, result$rowId)
  result$xb[t] = temp$sums
  result$exb = exp(result$xb)
  return(result)
}

calculateBaselineSurvivalFunction <- function(accDenom, times, y) {
  a = times*y
  b = ff::ff(vmode = "double", initdata = c(a[-1],-1))
  c = a==b&a!=0
  d = times[ffbase::ffwhich(c, c==FALSE)]

  e = ff::ff(vmode = "double", initdata = c(d[-1],-1))
  f = d==e
  newAccDenom = accDenom[ffbase::ffwhich(f, f==FALSE)]
  newTimes = unique(times)
  n = length(newTimes)

  t = table(a[])
  t = t[-1]
  g = ffbase::ffmatch(ff::as.ff(as.numeric(names(t))), newTimes)
  newY = ff::ff(vmode = "double", initdata = 0, length = n)
  newY[g] = ff::ff(vmode = "double", initdata = t)

  l = newY/newAccDenom
  L = ff::ff(vmode = "double", initdata = 0, length = n)
  for (i in 1:n) {
    L[i] = sum(l[i:n])
  }
  return(1/exp(L))
}

generateEventTimes <- function(x, ids, times, baseline) {
  n = length(ids$newId)

  S = ffbase::merge.ffdf(ids, x)
  S$R = ff::as.ff(runif(n))
  S$value = S$R^(1/S$exb)

  S = S[ff::fforder(S$value),]
  S$timeIndex = ff::ff(vmode = "integer", initdata = 0, length = n)
  k = 1;
  K = length(baseline)

  for (i in 1:n) {
    while((k<=K) && (S$value[i] >= baseline[k])) {k=k+1}
    S$timeIndex[i] = k-1
  }

  S$times = ff::ff(vmode = "double", initdata = times[1]+1, length = n)
  t = ffbase::ffwhich(S, S$timeIndex>0)
  S$times[t] = unique(times)[S$timeIndex[t]]

  return(ff::ffdf(newId = S$newId, rowId = S$rowId, time = S$times))
}





# @file CohortMethod.R
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
  propensityScore <- createPs(cohortMethodData,
                              prior = Cyclops::createPrior("laplace", 0.1, exclude = 0))
  propensityModel <- attr(propensityScore, "metaData")$psModelCoef

  writeLines("Fitting outcome model(s)")
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

  writeLines("Computing rates of prior outcomes")
  totalTime <- sum(cohortMethodData$cohorts$daysFromObsStart)
  preIndexOutcomeRates <- aggregate(rowId ~ outcomeId,
                                    data = cohortMethodData$outcomes[cohortMethodData$outcomes$daysToEvent <
    0, ], length)
  preIndexOutcomeRates$rate <- preIndexOutcomeRates$rowId/totalTime
  preIndexOutcomeRates$rowId <- NULL

  writeLines("Fitting models for time to observation period start, end and time to cohort end")
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
  writeLines("Generating covariates")
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

  writeLines("Generating treatment variable")
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

  writeLines("Generating cohorts")
  cohorts <- data.frame(rowId = treatment$rowId,
                        treatment = treatment$covariateValue,
                        subjectId = treatment$rowId,
                        cohortStartDate = "2000-01-01",
                        daysFromObsStart = profile$minObsTime + round(rexp(n,
                                                                           profile$obsStartRate)) - 1,
                        daysToCohortEnd = round(rexp(n, profile$obsEndRate)),
                        daysToObsEnd = round(rexp(n, profile$cohortEndRate)))
  attr(cohorts, "metaData") <- profile$cohortsMetaData

  writeLines("Generating outcomes after index date")
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

  writeLines("Generating outcomes before index date")
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
                 metaData = profile$metaData)

  class(result) <- "cohortMethodData"
  return(result)
}

# psMathod: 0 (nothing), 1 (trimByPsToEquipoise), 2 (stratify), 3 (match)
#' @export
createCMDSimulationProfile <- function(cohortMethodData,
                                       outcomeId = 3,
                                       studyPop,
                                       simulateTreatment = FALSE,
                                       simulateTreatmentBinary = NULL,
                                       excludeCovariateIds = NULL,
                                       psMethod = 0,
                                       useCrossValidation = TRUE,
                                       threads = 10) {

  if (simulateTreatment & is.null(simulateTreatmentBinary)) {
    start <- Sys.time()
    covariates = cohortMethodData$covariates
    covariateIds = unique(covariates$covariateId[])
    covariateIds = covariateIds[which(!(covariateIds %in% excludeCovariateIds))]
    propensityModel = rnorm(length(covariateIds)+1)/10
    names(propensityModel) = c("(Intercept)", covariateIds)
    betas = ff::ffdf(covariateId = ff::as.ff(as.numeric(names(propensityModel[2:length(propensityModel)]))),
                     beta = ff::as.ff(propensityModel[2:length(propensityModel)]))
    temp = ffbase::merge.ffdf(covariates, betas)
    temp$value = temp$covariateValue * temp$beta
    temp = ff::as.ffdf(bySumFf(temp$value, temp$rowId))
    names(temp) = c("rowId", "value")
    temp$value = temp$value + ff::ff(vmode = "double", initdata = propensityModel[1], length = nrow(temp))
    temp$propensityScore = 0.9999 / (1 + exp(-1 * temp$value))
    temp$rand = ff::as.ff(runif(nrow(temp)))
    temp$value = ff::as.ff(as.integer((temp$rand < temp$propensityScore)[]))
    temp$rand <- NULL
    propensityScore = merge(cohortMethodData$cohorts, temp)
    propensityScore$treatment = propensityScore$value
    propensityScore$value <- NULL
    cohortMethodData$cohorts$newPropensityScore = propensityScore$propensityScore
    cohortMethodData$cohorts$treatment = propensityScore$treatment
    studyPop$treatment = cohortMethodData$cohorts$treatment[match(studyPop$rowId, cohortMethodData$cohorts$rowId)]
#     t = ffbase::ffwhich(temp, temp$value == 1)
    delta <- Sys.time() - start
    writeLines(paste("simulating treatments took", signif(delta, 3), attr(delta, "units")))
  }

  if (simulateTreatment & !is.null(simulateTreatmentBinary)) {
    cohortMethodData$cohorts$treatment = rbinom(nrow(cohortMethodData$cohorts), 1, simulateTreatmentBinary)
    studyPop$treatment = cohortMethodData$cohorts$treatment[match(studyPop$rowId, cohortMethodData$cohorts$rowId)]
    cohortMethodData$cohorts$newPropensityScore = simulateTreatmentBinary
  }

  if (psMethod != 0) {
    propensityScore <- createPs(cohortMethodData, population=studyPop,
                                excludeCovariateIds = c(1,excludeCovariateIds),
                                prior = createPrior("laplace", useCrossValidation = crossValidate),
                                control = createControl(noiseLevel = "silent",
                                                        cvType = "auto",
                                                        tolerance = 2e-07,
                                                        cvRepetitions = 10,
                                                        startingVariance = 0.01,
                                                        threads = threads))
  }

  # create populations for fitting outcome models
  if (psMethod == 0) {population = studyPop}
  if (psMethod == 1) {population = trimByPsToEquipoise(propensityScore)}
  if (psMethod == 2) {population = stratifyByPs(propensityScore)}
  if (psMethod == 3) {population = matchOnPs(propensityScore)}
  if (psMethod == 0 | psMethod == 1) {stratified = FALSE}
  if (psMethod == 2 | psMethod == 3) {stratified = TRUE}
  population1 = invertOutcome(population)

  # generate outcome model for outcome
  outcomeModel <- fitOutcomeModel(population = population,
                                  cohortMethodData = cohortMethodData,
                                  modelType = "cox",
                                  stratified = stratified,
                                  useCovariates = TRUE,
                                  excludeCovariateIds = excludeCovariateIds,
                                  prior = createPrior(priorType = "laplace", useCrossValidation = useCrossValidation),
                                  control = createControl(cvType = "auto",
                                                                    startingVariance = 0.01,
                                                                    tolerance = 2e-07,
                                                                    cvRepetitions = 10,
                                                                    selectorType = "auto",
                                                                    noiseLevel = "quiet",
                                                                    threads = threads),
                                  returnFit = TRUE)


  # generate outcome model for censor
  outcomeModel1 <- fitOutcomeModel(population = population1,
                                   cohortMethodData = cohortMethodData,
                                   modelType = "cox",
                                   stratified = stratified,
                                   useCovariates = TRUE,
                                   excludeCovariateIds = excludeCovariateIds,
                                   prior = createPrior(priorType = "laplace", useCrossValidation = useCrossValidation),
                                   control = createControl(maxIterations = 10000,
                                                           selectorType = "auto",
                                                           threads = threads),
                                   returnFit = TRUE)

  # create sData and cData
  ids = ff::as.ff(population$rowId)

  sData = calculateBaselineSurvivalFunction(outcomeModel)
  sData$XB =   calculateXB(rowId = ids,
                           covariates = cohortMethodData$covariates,
                           coef = ff::ffdf(covariateId = ff::as.ff(as.numeric(names(outcomeModel$outcomeModelCoefficients))),
                                           beta = ff::as.ff(outcomeModel$outcomeModelCoefficients)))
  cData = calculateBaselineSurvivalFunction(outcomeModel1)
  cData$XB =   calculateXB(rowId = ids,
                           covariates = cohortMethodData$covariates,
                           coef = ff::ffdf(covariateId = ff::as.ff(as.numeric(names(outcomeModel1$outcomeModelCoefficients))),
                                           beta = ff::as.ff(outcomeModel1$outcomeModelCoefficients)))

  # delete people not in the study population
  covariates = cohortMethodData$covariates[in.ff(cohortMethodData$covariates$rowId, ids),]
  covariateRef = cohortMethodData$covariateRef[in.ff(cohortMethodData$covariateRef$covariateId, unique(covariates$covariateId)),]
  cohorts = cohortMethodData$cohorts[cohortMethodData$cohorts$rowId %in% population$rowId,]
  outcomes = cohortMethodData$outcomes[cohortMethodData$outcomes$rowId %in% population$rowId,]

  result = list(sData = sData,
                cData = cData,
                observedEffectSize = coef(outcomeModel),
                observedCensoringEffectSize = coef(outcomeModel1),
                sOutcomeModelCoefficients = outcomeModel$outcomeModelCoefficients,
                cOutcomeModelCoefficients = outcomeModel1$outcomeModelCoefficients,
                studyPop = studyPop,
                outcomeId = outcomeId)
  class(result) <- "simulationProfile"

  return (result)
}

#' @export
simulateCMD <- function(cohortMethodData, sData, cData, outcomeId) {
  # generate event times
  sTimes = generateEventTimes(sData$XB, sData$times, sData$baseline)
  names(sTimes)[match("time", names(sTimes))] = "sTime"

  # generate censor times
  cTimes = generateEventTimes(cData$XB, cData$times, cData$baseline)
  names(cTimes)[match("time", names(cTimes))] = "cTime"

  # combine event and censor times
  outcomes = NULL
  times = ffbase::merge.ffdf(sTimes, cTimes)
#   t = ffbase::ffwhich(times, times$sTime<=times$cTime & times$sTime <= sData$times[1])
#   if (!is.null(t)) outcomes = times[t,]
#   outcomes$cTime <- NULL
#   names(outcomes)[match("sTime", names(outcomes))] = "daysToEvent"
#   outcomes = as.data.frame(outcomes)
#   outcomes$outcomeId = 3

  cohorts = cohortMethodData$cohorts
  cohorts = merge(cohorts, times[])
  cohorts$newOutcomeCount = 0
  cohorts$newDaysToEvent = NA
  cohorts$newSurvivalTime = cohorts$cTime + 1
  t = which(cohorts$sTime <= cohorts$cTime & cohorts$sTime <= sData$times[1])
  if(!is.null(t)) {
    cohorts$newDaysToEvent[t] = cohorts$sTime[t]
    cohorts$newSurvivalTime[t] = cohorts$sTime[t] + 1
    cohorts$newOutcomeCount[t] = 1
    outcomes = data.frame(rowId = cohorts$rowId[t], daysToEvent = cohorts$newDaysToEvent[t], outcomeId = outcomeId)
    cohorts$sTime = NULL
    cohorts$cTime = NULL
  }

  covariates = cohortMethodData$covariates[in.ff(cohortMethodData$covariates$rowId, ff::as.ff(cohorts$rowId)),]
  covariateRef = cohortMethodData$covariateRef[in.ff(cohortMethodData$covariateRef$covariateId, unique(covariates$covariateId)),]

  return (list(covariates = covariates,
               covariateRef = covariateRef,
               cohorts = cohorts,
               outcomes = outcomes,
               metaData = cohortMethodData$metaData))
}

#' @export
insertEffectSize <- function(data, effectSize, cohort) {
  data = merge(data, cohort[c("rowId", "treatment")])
  data$treatment = data$treatment*1.0
  data$xb = data$xb + data$treatment*effectSize
  data$exb = exp(data$xb)
  return(data)
}

invertOutcome <- function(studyPop) {
  t1 = which(studyPop$outcomeCount == 0)
  t2 = which(studyPop$outcomeCount > 0)
  result = studyPop
  result$outcomeCount[t1] = 1
  result$outcomeCount[t2] = 0
  return(result)
}

calculateXB <- function(rowId, covariates, coef) {
  coef = coef[ffbase::ffwhich(coef,coef$beta!=0),]
  covariates = covariates[ffbase::ffwhich(covariates, in.ff(covariates$rowId, rowId)),]
  covariates = covariates[ffbase::ffwhich(covariates, in.ff(covariates$covariateId, coef$covariateId)),]
  covariates = ffbase::merge.ffdf(covariates, coef)
  covariates$value = covariates$covariateValue * covariates$beta
  temp = ff::as.ffdf(bySumFf(covariates$value, covariates$rowId))
  result = ff::ffdf(rowId = rowId, xb = ff::ff(vmode = "double", initdata = 0, length = length(rowId)))
  t = ffbase::ffmatch(temp$bins, result$rowId)
  result$xb[t] = temp$sums
  result$exb = exp(result$xb)
  return(result)
}

calculateBaselineSurvivalFunction <- function(outcomeModel) {
  fit = outcomeModel$fit

  accDenom = ff::as.ff(Cyclops::getAccDenom(fit$interface))
  times = ff::as.ff(Cyclops::getTimes(fit$interface))
  y = ff::as.ff(Cyclops::getY(fit$interface))

  a = times*y                                                           # only keeps times for y = 1 (noncensored?)
  b = ff::ff(vmode = "double", initdata = c(a[-1],-1))
  c = a==b&a!=0                                                         # are two consecutive noncensored?
  d = times[ffbase::ffwhich(c, c==FALSE)]                               # gives times that match accDenom length

  e = ff::ff(vmode = "double", initdata = c(d[-1],-1))
  f = d==e
  newAccDenom = accDenom[ffbase::ffwhich(f, f==FALSE)]
  newTimes = unique(times)
  newTimes = newTimes[ff::fforder(newTimes, decreasing=TRUE)]
  n = length(newTimes)

  t = table(a[])
  t = t[-1]
  g = ffbase::ffmatch(ff::as.ff(as.numeric(names(t))), newTimes)
  newY = ff::ff(vmode = "double", initdata = 0, length = n)
  newY[g] = ff::ff(vmode = "double", initdata = t)

  l = newY/newAccDenom
  L = ff::ff(vmode = "double", initdata = 0, length = n)
  for (i in 1:n) {
    #L[i] = sum(l[(n+1-i):n])
    L[i] = sum(l[i:n])
  }
  return(list(times = times - 1,
              baseline = 1/exp(L)))
}

generateEventTimes <- function(x, times, baseline) {
  n = nrow(x)
  S = x
  S$R = ff::as.ff(runif(n))
  S$value = S$R^(1/S$exb)

  S = S[ff::fforder(S$value),]
  k = 1;
  K = length(baseline)

  S$timeIndex = ff::as.ff(.generateEventTimesHelper(S$value[], baseline[]))

#   S$timeIndex = ff::ff(vmode = "integer", initdata = 0, length = n)
#
#   for (i in 1:K) {
#     t = ffbase::ffwhich(S, S$value >= baseline[i])
#     if (is.null(t)) {
#       break
#     } else{
#       S$timeIndex[t] = ff::ff(vmode = "integer", initdata = i, length = length(t))
#     }
#   }

  S$times = ff::ff(vmode = "double", initdata = times[1]+1, length = n)
  t = ffbase::ffwhich(S, S$timeIndex>0)
  if(!is.null(t)) S$times[t] = unique(times)[ff::fforder(unique(times),decreasing = TRUE)][S$timeIndex[t]]

  return(ff::ffdf(rowId = S$rowId, time = S$times))
}

in.ff <- function(a, b) {
  if (length(b) == 0)
    return(ff::as.ff(rep(FALSE, length(a))))
  else return(ffbase::ffmatch(x = a, table = b,nomatch = 0L) > 0L)
}

#' @export
removeCovariates <- function(cohortMethodData,
                             covariateIds) {
  if (is.null(covariateIds)) {
    covariates = cohortMethodData$covariates
    covariateRef = cohortMethodData$covariateRef
  } else {
    covariates = cohortMethodData$covariates[!in.ff(cohortMethodData$covariates$covariateId, covariateIds),]
    covariateRef = cohortMethodData$covariateRef[!in.ff(cohortMethodData$covariateRef$covariateId, covariateIds),]
  }

  return (list(covariates = covariates,
               covariateRef = covariateRef,
               cohorts = cohortMethodData$cohorts,
               outcomes = cohortMethodData$outcomes,
               metaData = cohortMethodData$metaData))
}

#' @export
findOutcomePrevalence <- function(sData, cData, delta=1) {
  return(.findOutcomePrevalence(sData$baseline[], sData$XB$exb[]*delta, cData$baseline[], cData$XB$exb[]))
}

#' @export
saveSimulationProfile <- function(simulationProfile, file) {
  if (missing(simulationProfile))
    stop("Must specify simulationProfile")
  if (missing(file))
    stop("Must specify file")
  if (class(simulationProfile) != "simulationProfile")
    stop("Data not of class simulationProfile")

  sDataFile = paste(file,"/sData",sep="")
  XB = simulationProfile$sData$XB
  ffbase::save.ffdf(XB, dir = sDataFile, clone = TRUE)
  saveRDS(simulationProfile$sData$times[], file = file.path(sDataFile, "times.rds"))
  saveRDS(simulationProfile$sData$baseline[], file = file.path(sDataFile, "baseline.rds"))

  cDataFile = paste(file,"/cData",sep="")
  XB = simulationProfile$cData$XB
  ffbase::save.ffdf(XB, dir = cDataFile, clone = TRUE)
  saveRDS(simulationProfile$cData$times[], file = file.path(cDataFile, "times.rds"))
  saveRDS(simulationProfile$cData$baseline[], file = file.path(cDataFile, "baseline.rds"))

  saveRDS(simulationProfile$observedEffectSize, file = file.path(file, "observedEffectSize.rds"))
  saveRDS(simulationProfile$observedCensoringEffectSize, file = file.path(file, "observedCensoringEffectSize.rds"))
  saveRDS(simulationProfile$sOutcomeModelCoefficients, file = file.path(file, "sOutcomeModelCoefficients.rds"))
  saveRDS(simulationProfile$cOutcomeModelCoefficients, file = file.path(file, "cOutcomeModelCoefficients.rds"))
  saveRDS(simulationProfile$studyPop, file = file.path(file, "studyPop.rds"))
  saveRDS(simulationProfile$outcomeId, file = file.path(file, "outcomeId.rds"))
}

#' @export
loadSimulationProfile <- function(file, readOnly = TRUE) {
  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))

  e <- new.env()
  sDataFile = paste(file,"/sData",sep="")
  ffbase::load.ffdf(sDataFile, e)
  sData <- list(XB = get("XB", envir = e),
                times = ff::as.ff(readRDS(file.path(sDataFile, "times.rds"))),
                baseline = ff::as.ff(readRDS(file.path(sDataFile, "baseline.rds"))))

  e <- new.env()
  cDataFile = paste(file,"/cData",sep="")
  ffbase::load.ffdf(cDataFile, e)
  cData <- list(XB = get("XB", envir = e),
                times = ff::as.ff(readRDS(file.path(cDataFile, "times.rds"))),
                baseline = ff::as.ff(readRDS(file.path(cDataFile, "baseline.rds"))))

  observedEffectSize = readRDS(file.path(file, "observedEffectSize.rds"))
  observedCensoringEffectSize = readRDS(file.path(file, "observedCensoringEffectSize.rds"))
  sOutcomeModelCoefficients = readRDS(file.path(file, "sOutcomeModelCoefficients.rds"))
  cOutcomeModelCoefficients = readRDS(file.path(file, "cOutcomeModelCoefficients.rds"))
  studyPop = readRDS(file.path(file, "studyPop.rds"))
  outcomeId = readRDS(file.path(file, "outcomeId.rds"))

  # Open all ffdfs to prevent annoying messages later:
  open(sData$XB, readonly = readOnly)
  open(cData$XB, readonly = readOnly)
  result = list(sData = sData,
                cData = cData,
                observedEffectSize = observedEffectSize,
                observedCensoringEffectSize = observedCensoringEffectSize,
                sOutcomeModelCoefficients = sOutcomeModelCoefficients,
                cOutcomeModelCoefficients = cOutcomeModelCoefficients,
                studyPop = studyPop,
                outcomeId = outcomeId)
  class(result) <- "simulationProfile"
  rm(e)
  return(result)
}

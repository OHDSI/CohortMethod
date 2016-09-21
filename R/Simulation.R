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
  propensityScore <- createPs(cohortMethodData, prior = Cyclops::createPrior("laplace", 0.1, exclude = 0))
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
    #studyPop <- trimByPsToEquipoise(studyPop)
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
                                    data = cohortMethodData$outcomes[cohortMethodData$outcomes$daysToEvent < 0, ],
                                    length)
  preIndexOutcomeRates$rate <- preIndexOutcomeRates$rowId / totalTime
  preIndexOutcomeRates$rowId <- NULL

  writeLines("Fitting models for time to observation period start, end and time to cohort end")
  obsEnd <- cohortMethodData$cohorts$daysToObsEnd
  cohortEnd <- cohortMethodData$cohorts$daysToCohortEnd
  event <- as.integer(cohortEnd < obsEnd)
  time <- cohortEnd
  time[cohortEnd > obsEnd] <- obsEnd[cohortEnd > obsEnd]
  data <- data.frame(time = time, event = event)
  data <- data[data$time > 0, ]
  fitCohortEnd <- survival::survreg(survival::Surv(time, event) ~ 1, data = data, dist = "exponential")
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
#' @param profile   An object of type \code{cohortMethodDataSimulationProfile} as
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
                        daysFromObsStart = profile$minObsTime + round(rexp(n, profile$obsStartRate)) - 1,
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
                           outcomeId = rep(profile$outcomesMetaData$outcomeIds[i],
                                           outcomeRows),
                           daysToEvent = rep(0, outcomeRows))
    cursor <- 1
    for (i in 1:nrow(temp)) {
      nOutcomes <- temp$nOutcomes[i]
      if (nOutcomes != 0) {
        outcomes$rowId[cursor:(cursor + nOutcomes - 1)] <- temp$rowId[i]
        outcomes$daysToEvent[cursor:(cursor + nOutcomes - 1)] <- sample.int(size = nOutcomes, temp$daysToObsEnd[i])
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
        outcomes$daysToEvent[cursor:(cursor + nOutcomes[j] - 1)] <- -sample.int(size = nOutcomes[j], cohorts$daysFromObsStart[j])
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
                                       resample = FALSE,
                                       sampleSize = 10000,
                                       simulateTreatment = FALSE,
                                       replaceBeta = FALSE,
                                       newBeta = 0,
                                       excludeCovariateIds = NULL,
                                       psMethod = 0) {

  outcomeId <- cohortMethodData$outcomes$outcomeId[1]
  if (simulateTreatment) {
    start <- Sys.time()
    covariates = cohortMethodData$covariates
    covariateIds = unique(covariates$covariateId[])
    covariateIds = covariateIds[which(!(covariateIds %in% excludeCovariateIds))]
    propensityModel = rnorm(length(covariateIds)+1)
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
    cohortMethodData$cohorts$treatment = propensityScore$treatment
    t = ffbase::ffwhich(temp, temp$value == 1)
    delta <- Sys.time() - start
    writeLines(paste("simulating treatments took", signif(delta, 3), attr(delta, "units")))
  }
  studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                    outcomeId = outcomeId,
                                    firstExposureOnly = FALSE,
                                    washoutPeriod = 0,
                                    removeDuplicateSubjects = FALSE,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 1,
                                    riskWindowStart = 0,
                                    addExposureDaysToStart = FALSE,
                                    riskWindowEnd = 30,
                                    addExposureDaysToEnd = TRUE)

  if (psMethod != 0) {
    propensityScore <- createPs(cohortMethodData, population=studyPop,
                                excludeCovariateIds = c(1,excludeCovariateIds),
                                prior = Cyclops::createPrior("laplace", useCrossValidation = FALSE))
  }

  # generate outcome model for outcome
  if (psMethod == 0) {strata = studyPop}
  if (psMethod == 1) {strata = trimByPsToEquipoise(propensityScore)}
  if (psMethod == 2) {strata = stratifyByPs(propensityScore)}
  if (psMethod == 3) {strata = matchOnPs(propensityScore)}
  if (psMethod == 0 | psMethod == 1) {stratified = FALSE}
  if (psMethod == 2 | psMethod == 3) {stratified = TRUE}

  outcomeModel <- fitOutcomeModel(population = strata,
                                  cohortMethodData = cohortMethodData,
                                  modelType = "cox",
                                  stratified = stratified,
                                  useCovariates = TRUE,
                                  excludeCovariateIds = excludeCovariateIds,
                                  prior = createPrior(priorType = "laplace", useCrossValidation = FALSE),
                                  returnFit = TRUE)

  effectSize = coef(outcomeModel)
  if (replaceBeta) {
    Cyclops::setExposureBeta(outcomeModel$fit$interface, newBeta)
    outcomeModel$outcomeModelCoefficients[length(outcomeModel$outcomeModelCoefficients)] = newBeta
    effectSize = newBeta
  }

  # generate outcome model for censor
  strata1 = invertOutcome(strata)
  outcomeModel1 <- fitOutcomeModel(population = strata1,
                                   cohortMethodData = cohortMethodData,
                                   modelType = "cox",
                                   stratified = stratified,
                                   useCovariates = TRUE,
                                   excludeCovariateIds = excludeCovariateIds,
                                   prior = createPrior(priorType = "laplace", useCrossValidation = FALSE),
                                   control = createControl(maxIterations = 10000),
                                   returnFit = TRUE)

  strata = ff::as.ffdf(strata)
  rownames(strata) = NULL
  strata1 = ff::as.ffdf(strata1)
  rownames(strata1) = NULL

  if (resample == TRUE) {
    ids = ff::ffdf(newId = ff::ff(vmode = "double", initdata = 1:sampleSize),
                   rowId = ff::as.ff(sample(strata$rowId, sampleSize, replace = TRUE)))
  } else {
    ids = ff::ffdf(newId = ff::as.ff(strata$rowId),
                   rowId = ff::as.ff(strata$rowId))
  }

  # generate covariates
  covariates = NULL
  ids1 = ids
  while(!is.null(dim(ids1)) && dim(ids1)[1]>0) {
    ids1 = ff::as.ffdf(ids1)
    if(is.null(covariates)) {
      covariates = ffbase::merge.ffdf(cohortMethodData$covariates, ids1)
    } else {
      temp = ffbase::merge.ffdf(cohortMethodData$covariates, ids1)
      if (!is.null(temp)) covariates = ffbase::ffdfrbind.fill(covariates, temp)
    }
    t = ffbase::ffmatch(unique(ids1$rowId), ids1$rowId)
    ids1 = ids1[-t[],]
  }
  covariates$rowId <- covariates$newId
  covariates$newId <- NULL

  # generate covariate ref
  covariateRef = ffbase::merge.ffdf(cohortMethodData$covariateRef, covariates)
  covariateRef$rowId <- NULL
  covariateRef$covariateValue <- NULL

  # generate event and censor data
  sData = simulateTimes(outcomeModel, strata, ids)
  cData = simulateTimes(outcomeModel1, strata1, ids)

  # generate cohorts
  cohorts = merge(as.data.frame(ids), cohortMethodData$cohorts)
  cohorts$rowId <- NULL
  names(cohorts)[match("newId", names(cohorts))] = "rowId"

  result = list(cohorts = cohorts,
                covariates = covariates,
                covariateRef = covariateRef,
                outcomes = cohortMethodData$outcomes,
                metaData = cohortMethodData$metaData)

  return (list(partialCMD = result,
               trueEffectSize = effectSize,
               sData = sData,
               cData = cData))
}

#' @export
simulateCMD <- function(partialCMD, sData, cData, ignoreCensoring = FALSE) {
  # generate event times
  sTimes = generateEventTimes(sData$XB, sData$times, sData$baseline)
  names(sTimes)[match("time", names(sTimes))] = "sTime"

  # generate censor times
  cTimes = generateEventTimes(cData$XB, cData$times, cData$baseline)
  names(cTimes)[match("time", names(cTimes))] = "cTime"

  # combine event and censor times
  times = ffbase::merge.ffdf(sTimes, cTimes)
  if (ignoreCensoring) {t = ffbase::ffwhich(times, times$sTime<=sData$times[1])}
  else {t = ffbase::ffwhich(times, times$sTime<=times$cTime & times$sTime <= sData$times[1])}
  outcomes = times[t,]
  outcomes$cTime <- NULL
  names(outcomes)[match("sTime", names(outcomes))] = "daysToEvent"
  outcomes = as.data.frame(outcomes)
  outcomes$outcomeId = 3
  return (list(covariates = partialCMD$covariates,
               covariateRef = partialCMD$covariateRef,
               cohorts = partialCMD$cohorts,
               outcomes = outcomes,
               metaData = partialCMD$metaData))
}

simulateTimes <- function(outcomeModel, studyPop, ids) {
  fit = outcomeModel$fit
  studyPop = studyPop[ff::fforder(studyPop$survivalTime),]

  accDenom = ff::as.ff(Cyclops::getAccDenom(fit$interface))
  times = ff::as.ff(Cyclops::getTimes(fit$interface))
  y = ff::as.ff(Cyclops::getY(fit$interface))

  baseline = calculateBaselineSurvivalFunction(accDenom, times, y)
  t = which(cohortMethodData$cohorts$treatment==1)
  treatmentCov = ff::ffdf(rowId = ff::as.ff(cohortMethodData$cohorts$rowId[t]),
                          covariateId = ff::as.ff(rep(outcomeModel$outcomeModelTreatmentVarId, length(t))),
                          covariateValue = ff::as.ff(rep(1, length(t))))
  x = calculateXB(rowId = studyPop$rowId,
                  covariates = ffbase::ffdfrbind.fill(treatmentCov, cohortMethodData$covariates),
                  coef = ff::ffdf(covariateId = ff::as.ff(as.numeric(names(outcomeModel$outcomeModelCoefficients))),
                                  beta = ff::as.ff(outcomeModel$outcomeModelCoefficients)))
  return(list(XB = x,
              times = times,
              baseline = baseline))
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

calculateBaselineSurvivalFunction <- function(accDenom, times, y) {
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
  return(1/exp(L))
}

generateEventTimes <- function(x, times, baseline) {
  n = nrow(x)
  S = x
  S$R = ff::as.ff(runif(n))
  S$value = S$R^(1/S$exb)

  S = S[ff::fforder(S$value),]
  k = 1;
  K = length(baseline)
  S$timeIndex = ff::ff(vmode = "integer", initdata = 0, length = n)

  for (i in 1:K) {
    t = ffbase::ffwhich(S, S$value >= baseline[i])
    if (is.null(t)) {
      break
    } else{
      S$timeIndex[t] = ff::ff(vmode = "integer", initdata = i, length = length(t))
    }
  }

  S$times = ff::ff(vmode = "double", initdata = times[1]+1, length = n)
  t = ffbase::ffwhich(S, S$timeIndex>0)
  S$times[t] = unique(times)[ff::fforder(unique(times),decreasing = TRUE)][S$timeIndex[t]]

  return(ff::ffdf(rowId = S$rowId, time = S$times))
}

in.ff <- function(a, b) {
  if (length(b) == 0)
    return(ff::as.ff(rep(FALSE, length(a))))
  else return(ffbase::ffmatch(x = a, table = b,nomatch = 0L) > 0L)
}


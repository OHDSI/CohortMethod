# @file CohortMethod.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
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
#' \code{createCohortDataSimulationProfile} creates a profile based on the provided cohortData object,
#' which can be used to generate simulated data that has similar characteristics.
#'
#' @param cohortData        An object of type \code{cohortData} as generated using \code{getDbCohortData}.
#'
#' @details
#' The output of this function is an object that can be used by the \code{simulateCohortData} function
#' to generate a cohortData object.
#' 
#' @return              
#' An object of type \code{cohortDataSimulationProfile}.
#' 
#' @export
createCohortDataSimulationProfile <- function(cohortData){
  writeLines("Computing covariate prevalence") # (Note: currently assuming binary covariates)
  sums <- quickSum(cohortData$covariates)
  covariatePrevalence <- sums$sum/nrow(cohortData$cohorts)
  attr(covariatePrevalence,"names") <- sums$covariateId
  #covariatePrevalence <- covariatePrevalence[order(names(covariatePrevalence),]
  
  writeLines("Computing propensity model")
  propensityScore <- createPs(cohortData, prior=createPrior("laplace",0.1))
  propensityModel <- attr(propensityScore,"coefficients")
  
  writeLines("Fitting outcome model(s)")
  psTrimmed <- trimByPsToEquipoise(propensityScore)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=1)
  
  outcomeModels <- vector("list", length(cohortData$metaData$outcomeConceptIds))
  for (i in 1:length(cohortData$metaData$outcomeConceptIds)){
    outcomeConceptId = cohortData$metaData$outcomeConceptIds[i]
    outcomeModel <- fitOutcomeModel(outcomeConceptId, cohortData, strata, useCovariates = TRUE, modelType = "pr", prior=createPrior("laplace",0.1))
    outcomeModels[[i]] <- outcomeModel$coefficients
  }
  
  writeLines("Fitting models for time to observation period end and time to cohort end")
  obsEnd <- as.ram(cohortData$cohorts$timeToObsPeriodEnd)
  cohortEnd <- as.ram(cohortData$cohorts$timeToCohortEnd)
  event <- as.integer(cohortEnd < obsEnd)
  time <- cohortEnd
  time[cohortEnd > obsEnd] <- obsEnd[cohortEnd > obsEnd]
  data <- data.frame(time=time,event=event)
  data <- data[data$time>0,]
  fitCohortEnd = survreg (Surv(time,event) ~ 1, data=data,dist="exponential");
  fitObsEnd = survreg (Surv(obsEnd[obsEnd>0]) ~ 1, dist="exponential");
  
  result <- list(covariatePrevalence = covariatePrevalence,
                 propensityModel = propensityModel,
                 outcomeModels = outcomeModels,
                 metaData = cohortData$metaData,
                 covariateRef = as.ram(cohortData$covariateRef),
                 cohortEndRate = 1/exp(coef(fitCohortEnd)),
                 obsEndRate = 1/exp(coef(fitObsEnd))
  )
  class(result) <- "cohortDataSimulationProfile"
  return(result)
}

#' Generate simulated data
#'
#' @description
#' \code{simulateCohortData} creates a cohortData object with simulated data.
#'
#' @param cohortDataSimulationProfile        An object of type \code{cohortDataSimulationProfile} as 
#' generated using the \code{createCohortDataSimulationProfile} function.
#' @param n   The size of the population to be generated.
#'
#' @details
#' This function generates simulated data that is in many ways similar to the original data on which
#' the simulation profile is based. The contains same outcome, comparator, and outcome concept IDs,
#' and the covariates and their 1st order statistics should be comparable.
#' 
#' @return              
#' An object of type \code{cohortData}.
#' 
#' @export
simulateCohortData <- function(cohortDataSimulationProfile, n=10000){
  #Note: currently, simulation is done completely in-memory. Could easily do batch-wise, storing in ffdf
  writeLines("Generating covariates")
  personsPerCov <- rpois(n = length(cohortDataSimulationProfile$covariatePrevalence),
                         lambda = cohortDataSimulationProfile$covariatePrevalence*n)
  personsPerCov[personsPerCov > n] <- n
  covariateIds <- as.numeric(names(cohortDataSimulationProfile$covariatePrevalence))
  covarRows <- sum(personsPerCov)
  covariates <- data.frame(rowId = rep(0,covarRows), covariateId = rep(0,covarRows), covariateValue = rep(1,covarRows))
  cursor <- 1
  for (i in 1:length(personsPerCov)){
    nCovar <- personsPerCov[i]
    if (nCovar != 0){
      covariates$covariateId[cursor:(cursor+nCovar-1)] <- covariateIds[i]
      covariates$rowId[cursor:(cursor+nCovar-1)] <- sample.int(size=nCovar,n)
      cursor = cursor+nCovar
    }
  }
  
  writeLines("Generating treatment variable")
  betas <- cohortDataSimulationProfile$propensityModel
  intercept <- betas[1]
  betas <- betas[2:length(betas)]
  betas <- data.frame(beta = as.numeric(betas),covariateId = as.numeric(names(betas)))
  treatment <- merge(covariates,betas)
  treatment$value = treatment$covariateValue * treatment$beta #Currently pointless, since covariateValue is always 1
  treatment <- aggregate(value ~ rowId,data=treatment,sum)
  treatment$value = treatment$value + intercept
  link <- function(x) {
    return(1/(1+exp(-x)))
  }
  treatment$value = link(treatment$value)
  treatment$rand <- runif(nrow(treatment))
  treatment$covariateValue <- as.integer(treatment$rand < treatment$value)
  treatment <- treatment[,c("rowId","covariateValue")]
  treatment$covariateId = 1
  covariates <- rbind(covariates,treatment)
  
  writeLines("Generating cohorts")
  cohorts <- data.frame(rowId = treatment$rowId, 
                        treatment = treatment$covariateValue, 
                        personId = treatment$rowId,
                        timeToObsPeriodEnd = round(rexp(n,cohortDataSimulationProfile$obsEndRate)),
                        timeToCohortEnd = round(rexp(n,cohortDataSimulationProfile$cohortEndRate))
  )
  
  writeLines("Generating outcomes")
  allOutcomes <- data.frame()
  for (i in 1:length(cohortDataSimulationProfile$metaData$outcomeConceptIds)){
    betas <- cohortDataSimulationProfile$outcomeModels[[i]]
    intercept <- betas[1]
    betas <- betas[2:length(betas)]
    betas <- data.frame(beta = as.numeric(betas),covariateId = as.numeric(names(betas)))
    temp <- merge(covariates,betas)
    temp$value = temp$covariateValue * temp$beta #Currently pointless, since covariateValue is always 1
    temp <- aggregate(value ~ rowId,data=temp,sum)
    temp$value = temp$value + intercept
    temp$value = exp(temp$value) #Value is now the rate
    temp <- merge(temp,cohorts[,c("rowId","timeToObsPeriodEnd")])
    temp$value = temp$value*temp$timeToObsPeriodEnd  #Value is lambda
    temp$nOutcomes = rpois(n,temp$value)
    temp$nOutcomes[temp$nOutcomes > temp$timeToObsPeriodEnd] <- temp$timeToObsPeriodEnd[temp$nOutcomes > temp$timeToObsPeriodEnd]
    outcomeRows <- sum(temp$nOutcomes)
    outcomes <- data.frame(rowId = rep(0,outcomeRows),
                           outcomeId = rep(cohortDataSimulationProfile$metaData$outcomeConceptIds[i],outcomeRows),
                           timeToEvent = rep(0,outcomeRows)
    )
    cursor <- 1
    for (i in 1:nrow(temp)){
      nOutcomes <- temp$nOutcomes[i]
      if (nOutcomes != 0){
        outcomes$rowId[cursor:(cursor+nOutcomes-1)] <- temp$rowId[i]
        outcomes$timeToEvent[cursor:(cursor+nOutcomes-1)] <- sample.int(size=nOutcomes,temp$timeToObsPeriodEnd[i])
        cursor = cursor+nOutcomes
      }
    }
    allOutcomes <- rbind(allOutcomes,outcomes)
  }
  
  result <- list(outcomes = as.ffdf(allOutcomes),
                 cohorts = as.ffdf(cohorts),
                 covariates = as.ffdf(covariates),
                 exclude = NULL,
                 covariateRef = as.ffdf(cohortDataSimulationProfile$covariateRef),
                 metaData = cohortDataSimulationProfile$metaData
  )
  
  class(result) <- "cohortData"
  return(result)
}


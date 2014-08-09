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
#
# @author Observational Health Data Sciences and Informatics
# @author Patrick Ryan
# @author Marc Suchard
# @author Martijn Schuemie

#' Create an outcome model, and compute the relative risk
#'
#' @description
#' \code{estimateEffect} creates an outcome model, and computes the relative risk
#' 
#' @param cohortData          An object of type \code{cohortData} as generated using \code{dbGetCohortData}
#' @param strata              A data frame specifying the strata. This data frame should have at least the following columns:
#' \code{RowId} and \code{StratumId}
#' @param riskWindowEnd       The maximum length (in days) of the risk window.
#' @param useCovariates       Whether to use the covariate matrix in the cohortData in the outcome model
#' @param modelType           The type of model to be fitted. See details for options
#' 
#'
#' @details
#' The data frame should have a least the following two columns:
#' \tabular{ll}{  
#'   \verb{lr}      \tab Logistic regression  \cr
#'   \verb{clr}     \tab Conditional logistic regression \cr
#'   \verb{cox}     \tab Cox regression (conditional or unconditional, depending on whether \code{stata} is specified) \cr
#'   \verb{pr}      \tab Poisson regression  \cr
#'   \verb{cpr}     \tab Conditional Poisson regression \cr
#' }
#' 
#' @return
#' A data frame holding the effect estimate
#'  
#' @examples 
#' todo
#' 
#' @export
estimateEffect <- function(cohortData,
                           strata=NULL, 
                           riskWindowEnd = 9999, 
                           useCovariates = FALSE, 
                           modelType = "cox"){
  outcomes <- cohortData$outcomes
  colnames(outcomes) <- toupper(colnames(outcomes))
  
  cohorts <- cohortData$cohorts
  colnames(cohorts) <- toupper(colnames(cohorts))
  if (!is.null(strata))
    colnames(strata) <- toupper(colnames(strata))
  
  data <- merge(cohorts[,c("ROW_ID","TIME_TO_CENSOR","TREATMENT")],outcomes,by="ROW_ID",all.x=TRUE)
  data$Y[is.na(data$Y)] <- 0
  
  if (modelType == "cox"){
    data$Y[data$Y != 0] <- 1
    data$TIME <- data$TIME_TO_OUTCOME
    data$TIME[is.na(data$TIME)] <- data$TIME_TO_CENSOR[is.na(data$TIME)]
    data$Y[data$TIME > riskWindowEnd] <- 0
    data$TIME[data$TIME > riskWindowEnd] <- riskWindowEnd
    data$TIME = data$TIME + 1
    if (useCovariates) { # To implement: CCD cox regression (stratified and unstratified)
      if (is.null(strata)){ # Unstratified Cox regression
        #fit2 <- coxph( Surv(TIME, Y) ~ TREATMENT,data=data ) 
        data$STRATUM_ID <- data$ROW_ID
        
        covariates <- data[,c("ROW_ID","TREATMENT")]
        covariates$COVARIATE_ID <- 100
        colnames(covariates) [colnames(covariates) == "TREATMENT"] <- "COVARIATE_VALUE"
        covariates <- rbind(covariates,cohortData$covariates)
        covariates <- covariates[order(covariates$ROW_ID),]
        if (cohortData.useff){
          data <- as.ffdf(data)
          cyclopsData <- createCyclopsData.ffdf(data,covariates,modelType="cox")
        } else {
          cyclopsData <- createCyclopsData(data,covariates,modelType="cox")
        }
        
        fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1, exclude=100))  
        
        cfs <- data.frame(LOGRR = coef(fit), ROW_ID = as.numeric(attr(coef(fit),"names")))
        cfs[cfs$ROW_ID == 100,]
      } else { # Stratified Cox regression
        data <- merge(data,strata[,c("ROW_ID","STRATUM_ID")],by="ROW_ID")
        fit <- coxph( Surv(TIME, Y) ~ TREATMENT + strata(STRATUM_ID),data=data ) 
        #Currently using coxph until Marc fixes stratifief cox
        #cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT + strata(STRATUM_ID),data=data, modelType = "cox")
        #fit2 <- fitCyclopsModel(cyclopsData, prior = prior("none"))   
      }
    } else {
      if (is.null(strata)){ # Unstratified Cox regression
        #fit2 <- coxph( Surv(TIME, Y) ~ TREATMENT,data=data ) 
        
        cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT,data=data, modelType = "cox")
        fit <- fitCyclopsModel(cyclopsData, prior = prior("none"))        
      } else { # Stratified Cox regression
        data <- merge(data,strata[,c("ROW_ID","STRATUM_ID")],by="ROW_ID")
        #fit2 <- coxph( Surv(TIME, Y) ~ TREATMENT + strata(STRATUM_ID),data=data ) 
        cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT + strata(STRATUM_ID),data=data, modelType = "cox")
        fit <- fitCyclopsModel(cyclopsData, prior = prior("none"))   
      }
    }
  } else if (modelType == "lr" || modeltype == "clr"){
    data$Y[data$Y != 0] <- 1
    data$TIME <- data$TIME_TO_OUTCOME
    data$TIME[is.na(data$TIME)] <- data$TIME_TO_CENSOR[is.na(data$TIME)]
    data$Y[data$TIME > riskWindowEnd] <- 0
    if (useCovariates) { # To implement: CCD logistic regression (stratified and unstratified)
      
    } else {
      if (is.null(strata)){ # Unstratified logistic regression
        fit2 <- glm(Y ~ TREATMENT, data=data,family = "binomial")
        
        cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT,data=data, modelType = "lr")
        fit <- fitCyclopsModel(cyclopsData, prior = prior("none"))   
      } else {# Stratified logistic regression
        data <- merge(data,strata[,c("ROW_ID","STRATUM_ID")],by="ROW_ID")
        #fit2 <- clogit(Y ~ TREATMENT + strata(STRATUM_ID), data=data)
        
        cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT + strata(STRATUM_ID),data=data, modelType = "clr")
        fit <- fitCyclopsModel(cyclopsData, prior = prior("none"))   
      }
    }
  } else if (modelType == "pr" || modeltype == "cpr"){
    if (riskWindowEnd != 9999)
      stop("Risk window currently not supported for (conditional) Poisson regression")
    data$TIME <- data$TIME_TO_CENSOR
    data$TIME = data$TIME + 1
    if (useCovariates) { # To implement: CCD Poisson regression (stratified and unstratified)
      
    } else {
      if (is.null(strata)){ # Unstratified Poisson regression
        #fit2 <- glm(Y ~ TREATMENT + offset(log(TIME)), data=data,family = "poisson")
        
        cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT + offset(log(TIME)),data=data, modelType = "pr")
        fit <- fitCyclopsModel(cyclopsData, prior = prior("none"))   
      } else {# Stratified Poisson regression
        data <- merge(data,strata[,c("ROW_ID","STRATUM_ID")],by="ROW_ID")
        #fit2 <- glm(Y ~ TREATMENT + offset(log(TIME)) + strata(STRATUM_ID), data=data,family = "poisson")
        
        cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT + offset(log(TIME)) + strata(STRATUM_ID),data=data, modelType = "cpr")
        fit <- fitCyclopsModel(cyclopsData, prior = prior("none"))  
      }
    }  
  }
  fit
}
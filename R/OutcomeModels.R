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
#' #todo
#' 
#' @export
estimateEffect <- function(cohortData,
                           strata=NULL, 
                           riskWindowStart = 0,
                           riskWindowEnd = 9999, 
                           addExposureDaysToEnd = FALSE,
                           useCovariates = TRUE, 
                           modelType = "clr"){
  useStrata = (modelType == "clr" | modelType == "cpr")
  if (useStrata & is.null(strata))
    stop("Strata parameter missing, but analysis requires strata")
  
  outcomes <- cohortData$outcomes
  colnames(outcomes) <- toupper(colnames(outcomes))
  
  cohorts <- as.ram(cohortData$cohorts)
  colnames(cohorts) <- toupper(colnames(cohorts))
  
  if (useStrata) {
    colnames(strata) <- toupper(colnames(strata))
    cohorts <- merge(strata,cohorts) #keeping only persons that have been matched
  }
  
  if (useCovariates){
    covariates <- cohortData$covariates
    colnames(covariates) <- toupper(colnames(covariates))  
    treatment <- data.frame(ROW_ID = cohorts$ROW_ID[cohorts$TREATMENT == 1],COVARIATE_ID = 100, COVARIATE_VALUE = 1)
    covariates = ffdfappend(covariates,treatment)
    if (useStrata) { #covariates need to be merged by stratum_id then row_id
      covariates <- merge(covariates,as.ffdf(cohorts[c("ROW_ID","STRATUM_ID")]))    
      covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","ROW_ID")]),]
    } else {#covariates need to be merged by  row_id
      covariates <- covariates[ffdforder(covariates[c("ROW_ID")]),]
    }
  }
  
  #todo: handle exposure days if addExposureDaysToEnd == TRUE
  outcomes <- subset(outcomes, TIME_TO_OUTCOME >= riskWindowStart & TIME_TO_OUTCOME <= riskWindowEnd)  
  
  if (modelType == "cox"){
    outcomes <- aggregate(TIME_TO_OUTCOME ~ ROW_ID,data=outcomes,min) #keep first outcome per person
    data <- merge(cohorts,outcomes, all.x=TRUE)
    data$Y <- 0
    data$Y[!is.na(data$TIME_TO_OUTCOME)] <- 1
    data$TIME <- data$TIME_TO_OUTCOME
    data$TIME[is.na(data$TIME)] <- data$TIME_TO_CENSOR[is.na(data$TIME)]
    
    if (useCovariates) { 
      if (useStrata){
        data <- data[order(data$STRATUM_ID,data$ROW_ID),]
        #todo: stratified Cox regression using covariates
      } else {
        data <- data[order(data$ROW_ID),]
        cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cox")
        fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1, exclude=100))  
        
      }
      #Doesn't seem to converge
    } else {# don't use covariates
      
      if (useStrata){
        #todo: stratified Cox regression without using covariates
        
      } else {
        cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT,data=data, modelType = "cox")
        fit <- fitCyclopsModel(cyclopsData, prior=prior("none"))  
        #fit2 <- coxph( Surv(TIME, Y) ~ TREATMENT,data=data ) 
        
      }
    }
  } else if (modelType == "lr" | modelType == "clr"){
    data <- cohorts
    data$Y <- 0
    data$Y[data$ROW_ID %in% as.ram(outcomes$ROW_ID)] <- 1
    if (useCovariates) { 
      if (useStrata){
        data <- data[order(data$STRATUM_ID,data$ROW_ID),]
        cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="clr")
        fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1)) 
        
        cyclopsFit <- fitCyclopsModel(cyclopsData, prior = prior("laplace", useCrossValidation = TRUE), control = control(cvType = "auto", cvRepetitions = 2, noiseLevel = "quiet"))
        #Doesn't seem to converge
      } else {
        data <- data[order(data$ROW_ID),]
        cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="lr")
        fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1, exclude=100))  
        ci <- confint(fit,parm=100)
        #se <- getSEs(fit,100)
        #coef(fit)[index] - 1.96*se
        index = which(names(coef(fit)) == "100")
        effectSize <-  data.frame(LOGRR = coef(fit)[index], LOGLB95 = ci[2], LOGUB95 = ci[3])
      }
    } else {# don't use covariates
      
      if (useStrata){
        
        
      } else {
        cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT,data=data, modelType = "cox")
        fit <- fitCyclopsModel(cyclopsData, prior=prior("none"))  
        #fit2 <- coxph( Surv(TIME, Y) ~ TREATMENT,data=data ) 
        effectSize <-  data.frame(LOGRR = coef(fit)[1], LOGLB95 = confint(fit,parm=1)[2], LOGUB95 = confint(fit,parm=1)[3])
      }
    }
  }
}
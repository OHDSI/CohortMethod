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
#'   \verb{cox}     \tab Cox regression (stratified or not, depending on whether \code{stata} is specified) \cr
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
estimateEffect <- function(outcomeConceptId,
                           cohortData,
                           strata=NULL, 
                           riskWindowStart = 0,
                           riskWindowEnd = 9999, 
                           addExposureDaysToEnd = FALSE,
                           useCovariates = TRUE, 
                           fitModel = TRUE,
                           returnOutcomeData = FALSE,
                           modelType = "cox"){
  if (!(modelType %in% c("lr","clr","pr","cpr","cox")))
    stop("Unknown model type")
  if (modelType != "cox" && modelType != "pr" && modelType != "cpr")
    stop("Currently only Cox model and (conditional) Poisson regression are implemented")
  if ((modelType == "clr" | modelType == "cpr") & is.null(strata))
    stop("Conditional regression specified, but no strate provided")
  
  useStrata = (modelType == "clr" | modelType == "cpr" | (modelType == "cox" & !is.null(strata)))
  
  #Keep only outcome information for this outcome:
  outcomes <- subset(cohortData$outcomes,OUTCOME_ID == as.double(outcomeConceptId))
  colnames(outcomes) <- toupper(colnames(outcomes))
  
  #Remove people from cohorts based on exclusion data for this outcome:
  t <- in.ff(cohortData$cohorts$ROW_ID ,cohortData$exclude$ROW_ID[cohortData$exclude$OUTCOME_ID == outcomeConceptId])
  cohorts <- as.ram(cohortData$cohort[ffwhich(t,t == FALSE),])
  colnames(cohorts) <- toupper(colnames(cohorts))
  
  if (useStrata) {
    colnames(strata) <- toupper(colnames(strata))
    cohorts <- merge(strata,cohorts) #keeping only persons that have been matched
  }
  
  if (useCovariates){
    covariates <- cohortData$covariates
    colnames(covariates) <- toupper(colnames(covariates))  
  }
  
  #Censor outcomes outside of risk window:
  cohorts$TIME_TO_CENSOR <- riskWindowEnd
  if (addExposureDaysToEnd)
    cohorts$TIME_TO_CENSOR <- cohorts$TIME_TO_CENSOR + cohorts$TIME_TO_COHORT_END
  cohorts$TIME_TO_CENSOR[cohorts$TIME_TO_CENSOR > cohorts$TIME_TO_OBS_PERIOD_END] <-  cohorts$TIME_TO_OBS_PERIOD_END
  outcomes <- merge(outcomes,as.ffdf(cohorts))
  outcomes <- subset(outcomes, TIME_TO_EVENT >= riskWindowStart & TIME_TO_EVENT <= TIME_TO_CENSOR)  
  
  if (modelType == "cox"){
    outcomes <- aggregate(TIME_TO_EVENT ~ ROW_ID,data=outcomes,min) #keep first outcome per person
    data <- merge(cohorts,outcomes, all.x=TRUE)
    data$Y <- 0
    data$Y[!is.na(data$TIME_TO_EVENT)] <- 1
    data$TIME <- data$TIME_TO_EVENT
    data$TIME[is.na(data$TIME)] <- data$TIME_TO_CENSOR[is.na(data$TIME)]
    data <- data[data$TIME > 0,]
    
    if (useCovariates) { 
      if (useStrata){
        data <- data[order(data$STRATUM_ID,-data$TIME,data$Y,data$ROW_ID),]
        covariates <- merge(covariates,as.ffdf(data[,c("ROW_ID","Y","TIME","STRATUM_ID")]))
        covariates$MINTIME <- 0-covariates$TIME
        covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","MINTIME","Y","ROW_ID")]),]
        cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cox")
        treatmentVariable <- 1
      } else {
        data <- data[order(data$ROW_ID,-data$TIME,data$Y),]
        covariates <- merge(covariates,as.ffdf(data[,c("ROW_ID","Y","TIME")]))
        covariates$MINTIME <- 0-covariates$TIME
        covariates <- covariates[ffdforder(covariates[c("ROW_ID","MINTIME","Y")]),]
        cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cox")
        treatmentVariable <- 1
      }
    } else {# don't use covariates    
      if (useStrata){
        cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT + strata(STRATUM_ID),data=data, modelType = "cox")
        treatmentVariable <- "TREATMENT"
      } else {
        cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT,data=data, modelType = "cox")
        treatmentVariable <- "TREATMENT"
      }
    }
  }
  
  if (modelType == "pr" | modelType == "cpr"){
    outcomes <- as.ram(outcomes)
    outcomes$Y <- 1
    outcomes <- aggregate(Y ~ ROW_ID + STRATUM_ID,data=outcomes,sum) #count outcome per person
    data <- merge(cohorts[,c("TREATMENT","ROW_ID","STRATUM_ID","TIME_TO_CENSOR")],outcomes, all.x=TRUE)
    data$Y[is.na(data$Y)] <- 0
    colnames(data)[colnames(data) == "TIME_TO_CENSOR"] <- "TIME"
    data <- data[data$TIME > 0,]
    
    if (useCovariates) { 
      if (useStrata){
        data <- data[order(data$STRATUM_ID,data$TIME,data$Y,data$ROW_ID),]
        covariates <- merge(covariates,as.ffdf(data[,c("ROW_ID","Y","TIME","STRATUM_ID")]))
        covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","TIME","Y","ROW_ID")]),]
        cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cpr",addIntercept=FALSE)
        treatmentVariable <- 1
      } else {
        data <- data[order(data$ROW_ID,data$TIME,data$Y),]
        covariates <- merge(covariates,as.ffdf(data[,c("ROW_ID","Y","TIME")]))
        covariates <- covariates[ffdforder(covariates[c("ROW_ID","TIME","Y")]),]
        cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="pr")
        treatmentVariable <- 1
      }
      
    } else {# don't use covariates  
      data$LOGTIME <- log(data$TIME)
      if (useStrata){
        cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT + strata(STRATUM_ID) + offset(LOGTIME),data=data, modelType = "cpr")
        treatmentVariable <- "TREATMENT"
      } else {
        cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT + offset(LOGTIME),data=data, modelType = "pr")
        treatmentVariable <- "TREATMENT"
      }
    }
  }
  
  df <- NULL
  if (fitModel) {
    fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1,  exclude=treatmentVariable))  
    ci <- confint(fit,parm=treatmentVariable)
    df <- data.frame(LOGRR=coef(fit)[names(coef(fit)) == treatmentVariable], LOGLB95 = ci[2], LOGUB95 = ci[3])
  }
  
  if (fitModel) {
    if (returnOutcomeData) {
        return(list(estimates = df, data = data))
    } else {
        return(df)
    }
  } else {
    if (returnOutcomeData) {
      return(data)
    }
  }
  return(NULL)
}

getOutcomeSummaryStatistics <- function(data,
                     modelType = "cox",
                     plot = FALSE,
                     timeBy = 1000) {
  labels <- c("Comparator", "Treated")
  
  patientTable <- table(data$TREATMENT)
  eventTable <- table(data$TREATMENT, data$Y)
  timeTable <- aggregate(TIME ~ TREATMENT, FUN = sum, data = data)[,2]
  
  tmp <- matrix(0, nrow=3, ncol=2)
  tmp[1,] <- patientTable
  tmp[2,] <- eventTable[,2]
  tmp[3,] <- timeTable
  
  km <- NULL
  
  if (plot && modelType == "cox") {
    require(plyr)
    km <- .ggkm(survfit(Surv(TIME, Y) ~ TREATMENT, data), 
                pval = FALSE, 
                timeby = timeBy, 
                ystratalabs=c("Comparator","Treated"), return = TRUE)  
  }
  
  return (list(info = data.frame(Comparator = tmp[,1], 
                                 Treated = tmp[,2],
                                 row.names = c("Patients","Events","Time")),
               plot = km))
}

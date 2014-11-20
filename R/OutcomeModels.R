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


createDataForModelFitCox <- function(useStrata,
                                     useCovariates,
                                     cohorts,
                                     covariates,
                                     outcomes) {
  outcomes <- aggregate(TIME_TO_EVENT ~ ROW_ID,data=outcomes,min) #keep first outcome per person
  data <- merge(cohorts,outcomes, all.x=TRUE)
  data$Y <- 0
  data$Y[!is.na(data$TIME_TO_EVENT)] <- 1
  data$TIME <- data$TIME_TO_EVENT
  data$TIME[is.na(data$TIME)] <- data$TIME_TO_CENSOR[is.na(data$TIME)]
  data <- data[data$TIME > 0,]
  result <- list(outcomeData = NULL, cyclopsData = NULL, treatmentVariable = NULL)
  if (useCovariates) { 
    if (useStrata){
      data <- data[order(data$STRATUM_ID,-data$TIME,data$Y,data$ROW_ID),]
      covariates <- merge(covariates,as.ffdf(data[,c("ROW_ID","Y","TIME","STRATUM_ID")]))
      covariates$MINTIME <- 0-covariates$TIME
      covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","MINTIME","Y","ROW_ID")]),]
      result$cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cox")
      result$data <- data
      result$treatmentVariable <- 1
    } else {
      data <- data[order(data$ROW_ID,-data$TIME,data$Y),]
      covariates <- merge(covariates,as.ffdf(data[,c("ROW_ID","Y","TIME")]))
      covariates$MINTIME <- 0-covariates$TIME
      covariates <- covariates[ffdforder(covariates[c("ROW_ID","MINTIME","Y")]),]
      result$cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cox")
      result$data <- data
      result$treatmentVariable <- 1
    }
  } else {# don't use covariates    
    if (useStrata){
      result$cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT + strata(STRATUM_ID),data=data, modelType = "cox")
      result$data <- data
      result$treatmentVariable <- "TREATMENT"
    } else {
      result$cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT,data=data, modelType = "cox")
      result$data <- data
      result$treatmentVariable <-"TREATMENT"
    }
  }
  return(result)
}

createDataForModelFitPoisson <- function(useStrata,
                                         useCovariates,
                                         cohorts,
                                         covariates,
                                         outcomes) {
  outcomes <- as.ram(outcomes)
  outcomes$Y <- 1
  outcomes <- aggregate(Y ~ ROW_ID + STRATUM_ID,data=outcomes,sum) #count outcome per person
  data <- merge(cohorts[,c("TREATMENT","ROW_ID","STRATUM_ID","TIME_TO_CENSOR")],outcomes, all.x=TRUE)
  data$Y[is.na(data$Y)] <- 0
  colnames(data)[colnames(data) == "TIME_TO_CENSOR"] <- "TIME"
  data <- data[data$TIME > 0,]
  result <- list(outcomeData = NULL, cyclopsData = NULL, treatmentVariable = NULL)
  if (useCovariates) { 
    if (useStrata){
      data <- data[order(data$STRATUM_ID,data$TIME,data$Y,data$ROW_ID),]
      covariates <- merge(covariates,as.ffdf(data[,c("ROW_ID","Y","TIME","STRATUM_ID")]))
      covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","TIME","Y","ROW_ID")]),]
      result$cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cpr",addIntercept=FALSE)
      result$data <- data
      result$treatmentVariable <- 1
    } else {
      data <- data[order(data$ROW_ID,data$TIME,data$Y),]
      covariates <- merge(covariates,as.ffdf(data[,c("ROW_ID","Y","TIME")]))
      covariates <- covariates[ffdforder(covariates[c("ROW_ID","TIME","Y")]),]
      result$cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="pr")
      result$data <- data
      result$treatmentVariable <- 1
    }
    
  } else {# don't use covariates  
    data$LOGTIME <- log(data$TIME)
    if (useStrata){
      result$cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT + strata(STRATUM_ID) + offset(LOGTIME),data=data, modelType = "cpr")
      result$data <- data
      result$treatmentVariable <-"TREATMENT"
    } else {
      result$cyclopsData <- createCyclopsDataFrame(Y ~ TREATMENT + offset(LOGTIME),data=data, modelType = "pr")
      result$data <- data
      result$treatmentVariable <-"TREATMENT"
    }
  }
  return(result)
}

createDataForModelFit <- function(outcomeConceptId,
                                  cohortData,
                                  strata=NULL, 
                                  riskWindowStart = 0,
                                  riskWindowEnd = 9999, 
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = TRUE, 
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
  cohorts$TIME_TO_CENSOR[cohorts$TIME_TO_CENSOR > cohorts$TIME_TO_OBS_PERIOD_END] <-  cohorts$TIME_TO_OBS_PERIOD_END[cohorts$TIME_TO_CENSOR > cohorts$TIME_TO_OBS_PERIOD_END]
  outcomes <- merge(outcomes,as.ffdf(cohorts))
  outcomes <- subset(outcomes, TIME_TO_EVENT >= riskWindowStart & TIME_TO_EVENT <= TIME_TO_CENSOR)  
  
  if (modelType == "cox"){
    return(createDataForModelFitCox(useStrata,useCovariates,cohorts,covariates,outcomes))
  } 
  
  if (modelType == "pr" | modelType == "cpr"){
    return(createDataForModelFitPoisson(useStrata,useCovariates,cohorts,covariates,outcomes))
  }
}


#' Create an outcome model, and compute the relative risk
#'
#' @description
#' \code{fitOutcomeModel} creates an outcome model, and computes the relative risk
#' 
#' @param cohortData          An object of type \code{cohortData} as generated using \code{dbGetCohortData}.
#' @param strata              A data frame specifying the strata. This data frame should have at least the following columns:
#' \code{RowId} and \code{StratumId}.
#' @param riskWindowEnd       The maximum length (in days) of the risk window.
#' @param useCovariates       Whether to use the covariate matrix in the cohortData in the outcome model.
#' @param fitModel            If false, the model will not be fit, and only summary statistics are available.
#' @param modelType           The type of model to be fitted. See details for options.
#' 
#'
#' @details
#' The model type can be one of these:
#' \tabular{ll}{  
#'   \verb{lr}      \tab Logistic regression  \cr
#'   \verb{clr}     \tab Conditional logistic regression \cr
#'   \verb{cox}     \tab Cox regression (stratified or not, depending on whether \code{stata} is specified) \cr
#'   \verb{pr}      \tab Poisson regression  \cr
#'   \verb{cpr}     \tab Conditional Poisson regression \cr
#' }
#' 
#' @return
#' An object of class \code{outcomeModel}. Generic function \code{summary}, \code{coef}, and \code{confint}
#' are available.
#'  
#' @examples 
#' #todo
#' 
#' @export
fitOutcomeModel <- function(outcomeConceptId,
                            cohortData,
                            strata=NULL, 
                            riskWindowStart = 0,
                            riskWindowEnd = 9999, 
                            addExposureDaysToEnd = FALSE,
                            useCovariates = TRUE, 
                            fitModel = TRUE,
                            modelType = "cox"){
  dataObject <- createDataForModelFit(outcomeConceptId,cohortData,strata,riskWindowStart,riskWindowEnd,addExposureDaysToEnd,useCovariates,modelType)
  
  treatmentEstimate <- NULL
  coefficients <- NULL
  fit <- NULL
  if (fitModel) {
    fit <- fitCyclopsModel(dataObject$cyclopsData, prior=prior("laplace",0.1,  exclude=dataObject$treatmentVariable))  
    coefficients <- coef(fit)
    logRr <- coef(fit)[names(coef(fit)) == dataObject$treatmentVariable]
    ci <- confint(fit,parm=dataObject$treatmentVariable)
    seLogRr <- (ci[3] - logRr)/qnorm(.975)
    treatmentEstimate <- data.frame(logRr=logRr, logLb95 = ci[2], logUb95 = ci[3], seLogRr = seLogRr)
  }
  outcomeModel <- list(modelType = modelType, coefficients = coefficients, treatmentEstimate = treatmentEstimate, data = dataObject$data)
  class(outcomeModel) <- "outcomeModel"
  return(outcomeModel)
}

summary.outcomeModel <- function(outcomeModel){
  patientTable <- table(outcomeModel$data$TREATMENT)
  eventTable <- table(outcomeModel$data$TREATMENT, outcomeModel$data$Y)
  timeTable <- aggregate(TIME ~ TREATMENT, FUN = sum, data = outcomeModel$data)[,2]
  
  counts <- matrix(0, nrow=3, ncol=2)
  counts[1,] <- patientTable
  counts[2,] <- eventTable[,2]
  counts[3,] <- timeTable
  colnames(counts) <- c("Comparator","Treated")
  rownames(counts) <- c("Nr. of persons","Nr. of events","Person time (days)")
  
  if (is.null(outcomeModel$coefficients)){
    result <- list(modelType = outcomeModel$modelType,
                   counts = counts)
  } else {
    model <- c(length(outcomeModel$coefficients),sum(outcomeModel$coefficients != 0))
    names(model) <- c("Nr. of betas","Nr. of non-zero betas")
    if (!is.null(outcomeModel$data$STRATUM_ID)){
      model <- c(model,length(unique(outcomeModel$data$STRATUM_ID)))
      names(model)[length(model)] <- "Number of strata"
    }
    
    result <- list(modelType = outcomeModel$modelType,
                   counts = counts, 
                   model = model,
                   coefficients = outcomeModel$treatmentEstimate)
  }
  class(result) <- "summary.outcomeModel"
  return(result);
}

print.summary.outcomeModel <- function(data){
  writeLines(paste("Model type:",data$modelType))
  writeLines("")
  writeLines("Counts")
  printCoefmat(data$counts)
  if (!is.null(data$model)){
    writeLines("")
    writeLines("Model")
    print(data$model)
    
    writeLines("")
    writeLines("Coefficients")
    d <- data$coefficients
    output <- data.frame(exp(d$logRr), 
                         exp(d$logLb95),
                         exp(d$logUb95),
                         d$logRr,
                         d$seLogRr)
    
    colnames(output) <- c("Estimate", "lower .95", "upper .95", "logRr","seLogRr")
    rownames(output) <- "Treatment"
    printCoefmat(output)
  }
}

coef.outcomeModel <- function(outcomeModel){
  return(outcomeModel$treatmentEstimate$logRr)
}

confint.outcomeModel <- function(outcomeModel){
  return(c(outcomeModel$treatmentEstimate$logLb95,outcomeModel$treatmentEstimate$logUb95))
}

print.outcomeModel <- function(outcomeModel){
  writeLines(paste("Model type:",outcomeModel$modelType))
  d <- outcomeModel$treatmentEstimate
  output <- data.frame(exp(d$logRr), 
                       exp(d$logLb95),
                       exp(d$logUb95),
                       d$logRr,
                       d$seLogRr)
  
  colnames(output) <- c("Estimate", "lower .95", "upper .95", "logRr","seLogRr")
  rownames(output) <- "Treatment"
  printCoefmat(output)
}

#' Get the full outcome model
#'
#' @description
#' \code{getFullOutcomeModel} shows the full outcome model, so showing the betas of all variables
#' included in the outcome model, not just the treatment variable.
#' 
#' @param outcomeModel        An object of type \code{outcomeModel} as generated using he \code{createOutcomeMode} function.
#' @param cohortData          An object of type \code{cohortData} as generated using \code{dbGetCohortData}.
#'
#' @details
#' Shows the coefficients and names of the covariates with non-zero coefficients.
#'  
#' @examples 
#' #todo
#' 
#' @export
getFullOutcomeModel <- function(outcomeModel,cohortData){
  cfs <- outcomeModel$coefficients
  cfs <- cfs[cfs != 0]
  #attr(cfs,"names")[1] <- 0
  cfs <- data.frame(coefficient = cfs, id = as.numeric(attr(cfs,"names")))
  
  cfs <- merge(as.ffdf(cfs),cohortData$covariateRef,by.x="id",by.y="COVARIATE_ID")
  cfs <- as.ram(cfs[,c("coefficient","id","COVARIATE_NAME")])
  cfs <- cfs[order(-abs(cfs$coefficient)),]
  colnames(cfs) <- toupper(colnames(cfs))
  return(cfs)
}

#' Plot the Kaplan-Meier curve
#'
#' @description
#' \code{plotKaplanMeier} creates the Kaplain-Meier survival plot
#' 
#' @param outcomeModel        An object of type \code{outcomeModel} as generated using he \code{createOutcomeMode} function.
#' @param censorMarks         Whether or not to include censor marks in the plot.
#' @param legend              Whether or not to include a legend in the plot.
#' @param labelsInGraph       If true, the labels identifying the two curves will be added to the graph.
#' @param fileName            Name of the file where the plot should be saved, for example 'plot.png'. See 
#' the function \code{ggsave} in the ggplot2 package for supported file formats.
#' 
#' @examples 
#' #todo
#' 
#' @export
plotKaplanMeier <- function(outcomeModel,
                            censorMarks = FALSE, 
                            legend=FALSE,
                            labelsInGraph=TRUE,
                            fileName = NULL){
  if (class(outcomeModel) != "outcomeModel")
    stop("Object not of class outcomeModel")
  if (outcomeModel$modelType != "cox")
    stop("Outcome model is not a Cox model")
  
  plot <- .ggkm(survfit(Surv(TIME, Y) ~ TREATMENT, outcomeModel$data), 
                marks = censorMarks, 
                legend = legend,
                labelsInGraph = labelsInGraph)  
  if (!is.null(fileName))
    ggsave(fileName,plot,width=5,height=5,dpi=400) 
  return(plot)
}
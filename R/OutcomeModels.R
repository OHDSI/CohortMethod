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
  outcomes <- aggregate(timeToEvent ~ rowId,data=outcomes,min) #keep first outcome per person
  data <- merge(cohorts,outcomes, all.x=TRUE)
  data$y <- 0
  data$y[!is.na(data$timeToEvent)] <- 1
  data$time <- data$timeToEvent
  data$time[is.na(data$time)] <- data$timeToCensor[is.na(data$time)]
  data <- data[data$time > 0,]
  result <- list(outcomeData = NULL, cyclopsData = NULL, treatmentVariable = NULL)
  if (useCovariates) { 
    if (useStrata){
      covariates <- merge(covariates,as.ffdf(data[,c("rowId","y","time","stratumId")]))
      result$cyclopsData <- convertToCyclopsDataObject(as.ffdf(data),covariates,modelType="cox",quiet=TRUE)
      result$data <- data
      result$treatmentVariable <- 1
    } else {
      outcomes$stratumId <- NULL
      covariates <- merge(covariates,as.ffdf(data[,c("rowId","y","time")]))
      result$cyclopsData <- convertToCyclopsDataObject(as.ffdf(data),covariates,modelType="cox",quiet=TRUE)
      result$data <- data
      result$treatmentVariable <- 1
    }
  } else {# don't use covariates    
    if (useStrata){
      result$cyclopsData <- createCyclopsDataFrame(Surv(time, y) ~ treatment + strata(stratumId),data=data, modelType = "cox")
      result$data <- data
      result$treatmentVariable <- "treatment"
    } else {
      result$cyclopsData <- createCyclopsDataFrame(Surv(time, y) ~ treatment,data=data, modelType = "cox")
      result$data <- data
      result$treatmentVariable <-"treatment"
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
  outcomes$y <- 1
  outcomes <- aggregate(Y ~ rowId + stratumId,data=outcomes,sum) #count outcome per person
  data <- merge(cohorts[,c("treatment","rowId","stratumId","timeToCensor")],outcomes, all.x=TRUE)
  data$y[is.na(data$y)] <- 0
  colnames(data)[colnames(data) == "timeToCensor"] <- "time"
  data <- data[data$time > 0,]
  result <- list(outcomeData = NULL, cyclopsData = NULL, treatmentVariable = NULL)
  if (useCovariates) { 
    if (useStrata){
      covariates <- merge(covariates,as.ffdf(data[,c("rowId","y","time","stratumId")]))
      result$cyclopsData <- convertToCyclopsDataObject(as.ffdf(data),covariates,modelType="cpr",addIntercept=FALSE,quiet=TRUE)
      result$data <- data
      result$treatmentVariable <- 1
    } else {
      covariates <- merge(covariates,as.ffdf(data[,c("rowId","y","time")]))
      result$cyclopsData <- convertToCyclopsDataObject(as.ffdf(data),covariates,modelType="pr",quiet=TRUE)
      result$data <- data
      result$treatmentVariable <- 1
    }
  } else {# don't use covariates  
    data$LOGtime <- log(data$time)
    if (useStrata){
      result$cyclopsData <- createCyclopsDataFrame(y ~ treatment + strata(stratumId) + offset(LOGtime),data=data, modelType = "cpr")
      result$data <- data
      result$treatmentVariable <-"treatment"
    } else {
      result$cyclopsData <- createCyclopsDataFrame(y ~ treatment + offset(LOGtime),data=data, modelType = "pr")
      result$data <- data
      result$treatmentVariable <-"treatment"
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
  outcomes <- subset(cohortData$outcomes,outcomeId == as.double(outcomeConceptId))
  
  #Remove people from cohorts based on exclusion data for this outcome:
  t <- in.ff(cohortData$cohorts$rowId ,cohortData$exclude$rowId[cohortData$exclude$outcomeId == outcomeConceptId])
  cohorts <- as.ram(cohortData$cohort[ffwhich(t,t == FALSE),])
  
  if (useStrata) {
    cohorts <- merge(strata,cohorts) #keeping only persons that have been matched
  }
  
  if (useCovariates){
    covariates <- cohortData$covariates
  }
  
  #Censor outcomes outside of risk window:
  cohorts$timeToCensor <- riskWindowEnd
  if (addExposureDaysToEnd)
    cohorts$timeToCensor <- cohorts$timeToCensor + cohorts$timeToCohortEnd
  cohorts$timeToCensor[cohorts$timeToCensor > cohorts$timeToObsPeriodEnd] <-  cohorts$timeToObsPeriodEnd[cohorts$timeToCensor > cohorts$timeToObsPeriodEnd]
  outcomes <- merge(outcomes,as.ffdf(cohorts))
  outcomes <- subset(outcomes, timeToEvent >= riskWindowStart & timeToEvent <= timeToCensor)  
  
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
                            modelType = "cox",
                            prior = prior("laplace", useCrossValidation = TRUE)){
  dataObject <- createDataForModelFit(outcomeConceptId,cohortData,strata,riskWindowStart,riskWindowEnd,addExposureDaysToEnd,useCovariates,modelType)
  
  treatmentEstimate <- NULL
  coefficients <- NULL
  fit <- NULL
  if (fitModel) {
    prior$exclude = dataObject$treatmentVariable 
    fit <- fitCyclopsModel(dataObject$cyclopsData, prior=prior)  
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
  patientTable <- table(outcomeModel$data$treatment)
  eventTable <- table(outcomeModel$data$treatment, outcomeModel$data$y)
  timeTable <- aggregate(time ~ treatment, FUN = sum, data = outcomeModel$data)[,2]
  
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
    if (!is.null(outcomeModel$data$stratumId)){
      model <- c(model,length(unique(outcomeModel$data$stratumId)))
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
    rownames(output) <- "treatment"
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
  rownames(output) <- "treatment"
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
  
  cfs <- merge(as.ffdf(cfs),cohortData$covariateRef,by.x="id",by.y="covariateId")
  cfs <- as.ram(cfs[,c("coefficient","id","covariateName")])
  cfs <- cfs[order(-abs(cfs$coefficient)),]
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
  
  plot <- .ggkm(survfit(Surv(time, y) ~ treatment, outcomeModel$data), 
                marks = censorMarks, 
                legend = legend,
                labelsInGraph = labelsInGraph)  
  if (!is.null(fileName))
    ggsave(fileName,plot,width=5,height=5,dpi=400) 
  return(plot)
}
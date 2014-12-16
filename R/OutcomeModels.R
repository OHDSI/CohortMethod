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
  if (nrow(outcomes) == 0){
    data <- as.ram(cohorts)
    data$timeToEvent <- NA
  } else {
    outcomes <- aggregate(timeToEvent ~ rowId,data=outcomes,min) #keep first outcome per person
    data <- merge(cohorts,outcomes, all.x=TRUE)
  }
  data$y <- 0
  data$y[!is.na(data$timeToEvent)] <- 1
  data$time <- data$timeToEvent
  data$time[is.na(data$time)] <- data$timeToCensor[is.na(data$time)]
  data <- data[data$time > 0,]
  result <- list(outcomeData = NULL, cyclopsData = NULL, treatmentVariable = NULL)
  if (useCovariates) { 
    if (useStrata){
      covariates <- merge(covariates,as.ffdf(data[,c("rowId","y","time","stratumId")]))
      result$cyclopsData <- convertToCyclopsData(as.ffdf(data),covariates,modelType="cox",quiet=TRUE)
      result$data <- data
      result$treatmentVariable <- 1
    } else {
      data$stratumId <- NULL
      covariates <- merge(covariates,as.ffdf(data[,c("rowId","y","time")]))
      result$cyclopsData <- convertToCyclopsData(as.ffdf(data),covariates,modelType="cox",quiet=TRUE)
      result$data <- data
      result$treatmentVariable <- 1
    }
  } else {# don't use covariates    
    if (useStrata){
      result$cyclopsData <- createCyclopsData(Surv(time, y) ~ treatment + strata(stratumId),data=data, modelType = "cox")
      result$data <- data
      result$treatmentVariable <- "treatment"
    } else {
      result$cyclopsData <- createCyclopsData(Surv(time, y) ~ treatment,data=data, modelType = "cox")
      result$data <- data
      result$data$stratumId <- NULL
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
  if (nrow(outcomes) == 0){
    data <- as.ram(cohorts[,c("treatment","rowId","stratumId","timeToCensor")])
    data$y <- 0
  } else {
    outcomes <- ff::as.ram(outcomes)
    outcomes$y <- 1
    outcomes <- aggregate(y ~ rowId,data=outcomes,sum) #count outcome per person
    if (useStrata) {
      data <- merge(cohorts[,c("treatment","rowId","stratumId","timeToCensor")],outcomes, all.x=TRUE)  
    } else {
      data <- merge(cohorts[,c("treatment","rowId","timeToCensor")],outcomes, all.x=TRUE)  
    }
    data$y[is.na(data$y)] <- 0
  }
  
  colnames(data)[colnames(data) == "timeToCensor"] <- "time"
  data <- data[data$time > 0,]
  result <- list(outcomeData = NULL, cyclopsData = NULL, treatmentVariable = NULL)
  if (useCovariates) { 
    if (useStrata){
      covariates <- merge(covariates,as.ffdf(data[,c("rowId","y","time","stratumId")]))
      result$cyclopsData <- convertToCyclopsData(as.ffdf(data),covariates,modelType="cpr",addIntercept=FALSE,quiet=TRUE)
      result$data <- data
      result$treatmentVariable <- 1
    } else {
      covariates <- merge(covariates,as.ffdf(data[,c("rowId","y","time")]))
      result$cyclopsData <- convertToCyclopsData(as.ffdf(data),covariates,modelType="pr",quiet=TRUE)
      result$data <- data
      result$treatmentVariable <- 1
    }
  } else {# don't use covariates  
    data$logTime <- log(data$time)
    if (useStrata){
      result$cyclopsData <- createCyclopsData(y ~ treatment + strata(stratumId) + offset(logTime),data=data, modelType = "cpr")
      result$data <- data
      result$treatmentVariable <-"treatment"
    } else {
      result$cyclopsData <- createCyclopsData(y ~ treatment + offset(logTime),data=data, modelType = "pr")
      result$data <- data
      result$treatmentVariable <-"treatment"
    }
  }
  return(result)
}

createDataForModelFitLogistic <- function(useStrata,
                                          useCovariates,
                                          cohorts,
                                          covariates,
                                          outcomes) {
  if (nrow(outcomes) == 0){
    data <- as.ram(cohorts[,c("treatment","rowId","stratumId")])
    data$y <- 0
  } else {
    outcomes <- ff::as.ram(outcomes)
    outcomes$y <- 1
    outcomes <- aggregate(y ~ rowId,data=outcomes,max) #Keep one outcome per person
    if (useStrata) {
      data <- merge(cohorts[,c("treatment","rowId","stratumId")],outcomes, all.x=TRUE)  
    } else {
      data <- merge(cohorts[,c("treatment","rowId")],outcomes, all.x=TRUE)  
    }
    data$y[is.na(data$y)] <- 0
  }
  result <- list(outcomeData = NULL, cyclopsData = NULL, treatmentVariable = NULL)
  if (useCovariates) { 
    if (useStrata){
      covariates <- merge(covariates,as.ffdf(data[,c("rowId","stratumId")]))
      result$cyclopsData <- convertToCyclopsData(as.ffdf(data),covariates,modelType="clr",addIntercept=FALSE,quiet=TRUE)
      result$data <- data
      result$treatmentVariable <- 1
    } else {
      #Restrict covariates to those of people in data: (can't use merge cause only 1 column to match)
      #mapping <- ffmatch(covariates$rowId,as.ff(data[,c("rowId")]))
      #covariateRowsWithMapping <- ffwhich(mapping, !is.na(mapping))
      #covariates <- covariates[covariateRowsWithMapping,]
      
      result$cyclopsData <- convertToCyclopsData(as.ffdf(data),covariates,modelType="lr",addIntercept=TRUE,quiet=TRUE)
      result$data <- data
      result$treatmentVariable <- 1
    }
  } else {# don't use covariates  
    if (useStrata){
      result$cyclopsData <- createCyclopsData(y ~ treatment + strata(stratumId),data=data, modelType = "clr")
      result$data <- data
      result$treatmentVariable <-"treatment"
    } else {
      result$cyclopsData <- createCyclopsData(y ~ treatment,data=data, modelType = "lr")
      result$data <- data
      result$treatmentVariable <-"treatment"
    }
  }
  return(result)
}

createDataForModelFit <- function(outcomeConceptId,
                                  cohortData,
                                  subPopulation, 
                                  useStrata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 9999, 
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = TRUE, 
                                  modelType = "cox"){
  if (!(modelType %in% c("lr","clr","pr","cpr","cox")))
    stop("Unknown model type")
  if (!any(cohortData$outcomes$outcomeId == outcomeConceptId))
    stop("Outcome concept ID has no events. Cannot fit model.")
  
  if ((modelType == "lr" | modelType == "pr"))
    useStrata = FALSE
  if ((modelType == "clr" | modelType == "cpr"))
    useStrata = TRUE
  
  if (useStrata & (is.null(subPopulation) | is.null(subPopulation$stratumId)))
    stop("Conditional regression specified, but no strata provided")
  
  if (useStrata)
    writeLines("Fitting stratified model")
  else 
    writeLines("Fitting unstratified model")
  if (is.null(outcomeConceptId) | is.null(cohortData$exclude)){
    outcomes <- cohortData$outcomes
    cohorts <- ff::as.ram(cohortData$cohort)
  } else {
    outcomes <- ffbase::subset.ffdf(cohortData$outcomes,outcomeId == as.double(outcomeConceptId))
    t <- in.ff(cohortData$cohorts$rowId ,cohortData$exclude$rowId[cohortData$exclude$outcomeId == outcomeConceptId])
    cohorts <- ff::as.ram(cohortData$cohort[ffwhich(t,t == FALSE),])
  }
  
  if (!is.null(subPopulation))
    cohorts <- merge(subPopulation,cohorts) #keeping only persons that have been matched
  
  if (useCovariates){
    covariates <- cohortData$covariates
  }
  
  #Censor outcomes outside of risk window:
  cohorts$timeToCensor <- riskWindowEnd
  if (addExposureDaysToEnd)
    cohorts$timeToCensor <- cohorts$timeToCensor + cohorts$timeToCohortEnd
  cohorts$timeToCensor[cohorts$timeToCensor > cohorts$timeToObsPeriodEnd] <-  cohorts$timeToObsPeriodEnd[cohorts$timeToCensor > cohorts$timeToObsPeriodEnd]
  
  outcomes <- tryCatch({
    merge(outcomes,as.ffdf(cohorts))
  }, warning = function(w){
    if (w$message == "No match found, returning NULL as ffdf can not contain 0 rows")
      data.frame() #No events within selected population, return empty data.frame
    else 
      merge(outcomes,as.ffdf(cohorts))
  })
  if (nrow(outcomes) != 0)
    outcomes <- tryCatch({
      ffbase::subset.ffdf(outcomes, timeToEvent >= riskWindowStart & timeToEvent <= timeToCensor)  
    }, error = function(e){
      if (e$message == "no applicable method for 'as.hi' applied to an object of class \"NULL\"") {
        data.frame(as.ram(outcomes)[0,]) #subset.ffdf throws an error if zero rows meet all criteria, so just return empty data.frame with same columns
      } else {
        stop(as.character(e$message))
      }
    })
  
  if (modelType == "cox"){
    return(createDataForModelFitCox(useStrata,useCovariates,cohorts,covariates,outcomes))
  } 
  
  if (modelType == "pr" | modelType == "cpr"){
    return(createDataForModelFitPoisson(useStrata,useCovariates,cohorts,covariates,outcomes))
  }
  
  if (modelType == "lr" | modelType == "clr"){
    return(createDataForModelFitLogistic(useStrata,useCovariates,cohorts,covariates,outcomes))
  }
}


#' Create an outcome model, and compute the relative risk
#'
#' @description
#' \code{fitOutcomeModel} creates an outcome model, and computes the relative risk
#' 
#' @param cohortData          An object of type \code{cohortData} as generated using \code{getDbCohortData}.
#' @param subPopulation       A data frame specifying the (matched and/or trimmed) subpopulation to be 
#' used in the study, as well as their strata (for conditional models). This data frame should have at 
#' least a \code{RowId}, and a \code{StratumId} when including stratification.
#' @param stratifiedCox           Specifically for Cox regressions: specify whether to use the strata defined in 
#' \code{subPopulation} in the analysis. For Poisson regression and logistic regression, this is implied in 
#' 'clr' and 'cpr'.
#' @param riskWindowEnd       The maximum length (in days) of the risk window.
#' @param useCovariates       Whether to use the covariate matrix in the cohortData in the outcome model.
#' @param fitModel            If false, the model will not be fit, and only summary statistics are available.
#' @param modelType           The type of model to be fitted. See details for options.
#' @param prior   The prior used to fit the model. See \code{\link[Cyclops]{createPrior}} for details.
#' @param control The control object used to control the cross-validation used to determine the 
#' hyperparameters of the prior (if applicable). See \code{\link[Cyclops]{createControl}}  for details.
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
                            subPopulation=NULL, 
                            stratifiedCox=TRUE,
                            riskWindowStart = 0,
                            riskWindowEnd = 9999, 
                            addExposureDaysToEnd = FALSE,
                            useCovariates = TRUE, 
                            fitModel = TRUE,
                            modelType = "cox",
                            prior = createPrior("laplace", useCrossValidation = TRUE),
                            control = createControl(lowerLimit=0.01, upperLimit=10, fold=5, noiseLevel = "quiet")){
  dataObject <- createDataForModelFit(outcomeConceptId,cohortData,subPopulation,stratifiedCox,riskWindowStart,riskWindowEnd,addExposureDaysToEnd,useCovariates,modelType)
  
  treatmentEstimate <- NULL
  coefficients <- NULL
  fit <- NULL
  priorVariance <- NULL
  status <- "OK"
  if (fitModel) {
    if (useCovariates)
      prior$exclude = dataObject$treatmentVariable 
    else
      prior <- createPrior("none") #Only one variable, which we're not going to regularize, so effectively no prior
    fit <- fitCyclopsModel(dataObject$cyclopsData, 
                           prior = prior,
                           control = control)  
    if (fit$return_flag == "ILLCONDITIONED"){
      coefficients <- c(0)
      treatmentEstimate <- data.frame(logRr=0, logLb95 = -Inf, logUb95 = Inf, seLogRr = Inf)
      priorVariance <- 0   
      status <- "ILL CONDITIONED, CANNOT FIT"
    } else {
      coefficients <- coef(fit)
      logRr <- coef(fit)[names(coef(fit)) == dataObject$treatmentVariable]
      ci <- tryCatch({
        confint(fit,parm = dataObject$treatmentVariable, includePenalty = TRUE)
      }, error = function(e) {
        c(0,-Inf,Inf)
      })
      if (identical(ci, c(0,-Inf,Inf)))
        status <- "ERROR COMPUTING CI"
      seLogRr <- (ci[3] - logRr)/qnorm(.975)
      treatmentEstimate <- data.frame(logRr=logRr, logLb95 = ci[2], logUb95 = ci[3], seLogRr = seLogRr)
      priorVariance <- fit$variance[1]
    }
  }
  outcomeModel <- list(outcomeConceptId = outcomeConceptId,
                       modelType = modelType, 
                       coefficients = coefficients,
                       priorVariance = priorVariance,
                       treatmentEstimate = treatmentEstimate, 
                       data = dataObject$data,
                       status = status)
  class(outcomeModel) <- "outcomeModel"
  return(outcomeModel)
}

summary.outcomeModel <- function(outcomeModel){
  if (outcomeModel$modelType == "clr" || outcomeModel$modelType == "lr"){
    patientTable <- table(outcomeModel$data$treatment)
    eventTable <- table(outcomeModel$data$treatment, outcomeModel$data$y)
    
    counts <- matrix(0, nrow=2, ncol=2)
    counts[1,] <- patientTable
    if (ncol(eventTable) == 1)
      counts[2,] <- c(0,0)
    else
      counts[2,] <- eventTable[,2]
    colnames(counts) <- c("Comparator","Treated")
    rownames(counts) <- c("Nr. of persons","Nr. of events")
  } else {
    patientTable <- table(outcomeModel$data$treatment)
    eventTable <- table(outcomeModel$data$treatment, outcomeModel$data$y)
    timeTable <- aggregate(time ~ treatment, FUN = sum, data = outcomeModel$data)[,2]
    
    counts <- matrix(0, nrow=3, ncol=2)
    counts[1,] <- patientTable
    if (ncol(eventTable) == 1)
      counts[2,] <- c(0,0)
    else
      counts[2,] <- eventTable[,2]
    counts[3,] <- timeTable
    colnames(counts) <- c("Comparator","Treated")
    rownames(counts) <- c("Nr. of persons","Nr. of events","Person time (days)")
  }
  
  
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
                   priorVariance = outcomeModel$priorVariance,
                   coefficients = outcomeModel$treatmentEstimate,
                   status = outcomeModel$status)
  }
  class(result) <- "summary.outcomeModel"
  return(result);
}

print.summary.outcomeModel <- function(data){
  writeLines(paste("Model type:",data$modelType))
  writeLines(paste("Status:",outcomeModel$status))
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
    writeLines("")
    writeLines(paste("Prior variance:",data$priorVariance))
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
  writeLines(paste("Status:",outcomeModel$status))
  
  d <- outcomeModel$treatmentEstimate
  output <- data.frame(exp(d$logRr), 
                       exp(d$logLb95),
                       exp(d$logUb95),
                       d$logRr,
                       d$seLogRr)
  colnames(output) <- c("Estimate", "lower .95", "upper .95", "logRr","seLogRr")
  rownames(output) <- "treatment"
  writeLines("")
  writeLines(paste("Prior variance:",outcomeModel$priorVariance))
  printCoefmat(output)
}

#' Get the outcome model
#'
#' @description
#' \code{getFullOutcomeModel} shows the full outcome model, so showing the betas of all variables
#' included in the outcome model, not just the treatment variable.
#' 
#' @param outcomeModel        An object of type \code{outcomeModel} as generated using he \code{createOutcomeMode} function.
#' @param cohortData          An object of type \code{cohortData} as generated using \code{getDbCohortData}.
#'
#' @details
#' Shows the coefficients and names of the covariates with non-zero coefficients.
#'  
#' @examples 
#' #todo
#' 
#' @export
getOutcomeModel <- function(outcomeModel,cohortData){
  cfs <- outcomeModel$coefficients
  
  cfs <- cfs[cfs != 0]
  attr(cfs,"names")[attr(cfs,"names") == "(Intercept)"] <- 0
  attr(cfs,"names")[attr(cfs,"names") == "treatment"] <- 1
  cfs <- data.frame(coefficient = cfs, id = as.numeric(attr(cfs,"names")))
  
  cfs <- merge(as.ffdf(cfs), cohortData$covariateRef, by.x = "id", by.y = "covariateId", all.x = TRUE)
  cfs <- ff::as.ram(cfs[,c("coefficient","id","covariateName")])
  cfs$covariateName <- as.character(cfs$covariateName)
  cfs <- cfs[order(-abs(cfs$coefficient)),]
  cfs$covariateName[cfs$id == 0] <- "Intercept"
  return(cfs)
}

#' Plot the Kaplan-Meier curve
#'
#' @description
#' \code{plotKaplanMeier} creates the Kaplain-Meier survival plot
#' 
#' @param outcomeModel        An object of type \code{outcomeModel} as generated using he \code{fitOutcomeModel} function.
#' @param censorMarks         Whether or not to include censor marks in the plot.
#' @param legend              Whether or not to include a legend in the plot.
#' @param labelsInGraph       If true, the labels identifying the two curves will be added to the graph.
#' @param fileName            Name of the file where the plot should be saved, for example 'plot.png'. See 
#' the function \code{ggsave} in the ggplot2 package for supported file formats.
#' 
#' @return A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different format.
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
    ggplot2::ggsave(fileName,plot,width=5,height=5,dpi=400) 
  return(plot)
}
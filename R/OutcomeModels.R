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


createDataForModelFitCox <- function(useStrata, useCovariates, cohorts, covariates, outcomes) {
  if (nrow(outcomes) == 0) {
    data <- ff::as.ram(cohorts)
    data$timeToEvent <- NA
  } else {
    outcomes <- aggregate(timeToEvent ~ rowId, data = outcomes, min)  #keep first outcome per person
    data <- merge(cohorts, outcomes, all.x = TRUE)
  }
  data$y <- 0
  data$y[!is.na(data$timeToEvent)] <- 1
  data$time <- data$timeToEvent
  data$time[is.na(data$time)] <- data$timeToCensor[is.na(data$time)]
  data <- data[data$time > 0, ]
  if (!useStrata)
    data$stratumId <- NULL
  result <- list(data = data,
                 cyclopsData = NULL,
                 zeroOutcomes = (nrow(outcomes) == 0),
                 useStrata = useStrata,
                 useCovariates = useCovariates)
  if (result$zeroOutcomes)
    return(result)
  if (useCovariates) {
    if (useStrata) {
      informativeStrata <- unique(data$stratumId[data$y == 1])
      informativeData <- data[data$stratumId %in% informativeStrata, ]
      covariates <- merge(covariates,
                          ff::as.ffdf(informativeData[, c("rowId", "y", "time", "stratumId")]))
      result$cyclopsData <- Cyclops::convertToCyclopsData(ff::as.ffdf(informativeData),
                                                          covariates,
                                                          modelType = "cox",
                                                          quiet = TRUE)
    } else {
      covariates <- merge(covariates, ff::as.ffdf(data[, c("rowId", "y", "time")]))
      result$cyclopsData <- Cyclops::convertToCyclopsData(ff::as.ffdf(data),
                                                          covariates,
                                                          modelType = "cox",
                                                          quiet = TRUE)
    }
  } else {
    # don't use covariates
    if (useStrata) {
      covariates <- data[, c("rowId", "y", "time", "stratumId", "treatment")]
      covariates$covariateId <- 1
      colnames(covariates)[colnames(covariates) == "treatment"] <- "covariateValue"
      result$cyclopsData <- Cyclops::convertToCyclopsData(data,
                                                          covariates,
                                                          modelType = "cox",
                                                          quiet = TRUE)
    } else {
      data$stratumId <- NULL
      covariates <- data[, c("rowId", "y", "time", "treatment")]
      covariates$covariateId <- 1
      colnames(covariates)[colnames(covariates) == "treatment"] <- "covariateValue"
      result$cyclopsData <- Cyclops::convertToCyclopsData(data,
                                                          covariates,
                                                          modelType = "cox",
                                                          quiet = TRUE)
    }
  }
  return(result)
}

createDataForModelFitPoisson <- function(useStrata, useCovariates, cohorts, covariates, outcomes) {
  if (nrow(outcomes) == 0) {
    if (useStrata) {
      data <- ff::as.ram(cohorts[, c("treatment", "rowId", "stratumId", "timeToCensor")])
    } else {
      data <- ff::as.ram(cohorts[, c("treatment", "rowId", "timeToCensor")])
    }
    data$y <- 0
  } else {
    outcomes <- ff::as.ram(outcomes)
    outcomes$y <- 1
    outcomes <- aggregate(y ~ rowId, data = outcomes, sum)  #count outcome per person
    if (useStrata) {
      data <- merge(cohorts[,
                    c("treatment", "rowId", "stratumId", "timeToCensor")],
                    outcomes,
                    all.x = TRUE)
    } else {
      data <- merge(cohorts[, c("treatment", "rowId", "timeToCensor")], outcomes, all.x = TRUE)
    }
    data$y[is.na(data$y)] <- 0
  }

  colnames(data)[colnames(data) == "timeToCensor"] <- "time"
  data <- data[data$time > 0, ]
  result <- list(data = data,
                 cyclopsData = NULL,
                 zeroOutcomes = (nrow(outcomes) == 0),
                 useStrata = useStrata,
                 useCovariates = useCovariates)
  if (result$zeroOutcomes)
    return(result)
  if (useCovariates) {
    if (useStrata) {
      informativeStrata <- unique(data$stratumId[data$y == 1])
      informativeData <- data[data$stratumId %in% informativeStrata, ]
      covariates <- merge(covariates,
                          ff::as.ffdf(informativeData[, c("rowId", "y", "time", "stratumId")]))
      result$cyclopsData <- Cyclops::convertToCyclopsData(ff::as.ffdf(informativeData),
                                                          covariates,
                                                          modelType = "cpr",
                                                          addIntercept = FALSE,
                                                          quiet = TRUE)
    } else {
      covariates <- merge(covariates, ff::as.ffdf(data[, c("rowId", "y", "time")]))
      result$cyclopsData <- Cyclops::convertToCyclopsData(ff::as.ffdf(data),
                                                          covariates,
                                                          modelType = "pr",
                                                          quiet = TRUE)
    }
  } else {
    # don't use covariates
    if (useStrata) {
      covariates <- data[, c("rowId", "y", "time", "stratumId", "treatment")]
      covariates$covariateId <- 1
      colnames(covariates)[colnames(covariates) == "treatment"] <- "covariateValue"
      result$cyclopsData <- Cyclops::convertToCyclopsData(data,
                                                          covariates,
                                                          modelType = "cpr",
                                                          quiet = TRUE)
    } else {
      covariates <- data[, c("rowId", "y", "time", "treatment")]
      covariates$covariateId <- 1
      colnames(covariates)[colnames(covariates) == "treatment"] <- "covariateValue"
      result$cyclopsData <- Cyclops::convertToCyclopsData(data,
                                                          covariates,
                                                          modelType = "pr",
                                                          quiet = TRUE)
    }
  }
  return(result)
}

createDataForModelFitLogistic <- function(useStrata, useCovariates, cohorts, covariates, outcomes) {
  if (nrow(outcomes) == 0) {
    if (useStrata) {
      data <- ff::as.ram(cohorts[, c("treatment", "rowId", "stratumId")])
    } else {
      data <- ff::as.ram(cohorts[, c("treatment", "rowId")])
    }
    data$y <- 0
  } else {
    outcomes <- ff::as.ram(outcomes)
    outcomes$y <- 1
    outcomes <- aggregate(y ~ rowId, data = outcomes, max)  #Keep one outcome per person
    if (useStrata) {
      data <- merge(cohorts[, c("treatment", "rowId", "stratumId")], outcomes, all.x = TRUE)
    } else {
      data <- merge(cohorts[, c("treatment", "rowId")], outcomes, all.x = TRUE)
    }
    data$y[is.na(data$y)] <- 0
  }
  result <- list(data = data,
                 cyclopsData = NULL,
                 zeroOutcomes = (nrow(outcomes) == 0),
                 useStrata = useStrata,
                 useCovariates = useCovariates)
  if (result$zeroOutcomes)
    return(result)
  if (useCovariates) {
    if (useStrata) {
      informativeStrata <- unique(data$stratumId[data$y == 1])
      informativeData <- data[data$stratumId %in% informativeStrata, ]
      covariates <- merge(covariates, ff::as.ffdf(informativeData[, c("rowId", "stratumId")]))
      result$cyclopsData <- Cyclops::convertToCyclopsData(ff::as.ffdf(informativeData),
                                                          covariates,
                                                          modelType = "clr",
                                                          addIntercept = FALSE,
                                                          quiet = TRUE)
    } else {
      t <- ffbase::ffmatch(x = covariates$rowId, table = ff::as.ff(data$rowId), nomatch = 0L) >
        0L
      covariates <- covariates[ffbase::ffwhich(t, t == TRUE), ]
      result$cyclopsData <- Cyclops::convertToCyclopsData(ff::as.ffdf(data),
                                                          covariates,
                                                          modelType = "lr",
                                                          addIntercept = TRUE,
                                                          quiet = TRUE)
    }
  } else {
    # don't use covariates
    if (useStrata) {
      covariates <- data[, c("rowId", "stratumId", "treatment")]
      covariates$covariateId <- 1
      colnames(covariates)[colnames(covariates) == "treatment"] <- "covariateValue"
      result$cyclopsData <- Cyclops::convertToCyclopsData(data,
                                                          covariates,
                                                          modelType = "clr",
                                                          quiet = TRUE)
    } else {
      covariates <- data[, c("rowId", "treatment")]
      covariates$covariateId <- 1
      colnames(covariates)[colnames(covariates) == "treatment"] <- "covariateValue"
      result$cyclopsData <- Cyclops::convertToCyclopsData(data,
                                                          covariates,
                                                          modelType = "lr",
                                                          quiet = TRUE)
    }
  }
  return(result)
}

createDataForModelFit <- function(outcomeId,
                                  cohortMethodData,
                                  subPopulation,
                                  useStrata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 9999,
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = TRUE,
                                  modelType = "cox") {
  if (!(modelType %in% c("lr", "clr", "pr", "cpr", "cox")))
    stop("Unknown model type")
  if ((modelType == "lr" | modelType == "pr"))
    useStrata <- FALSE
  if ((modelType == "clr" | modelType == "cpr"))
    useStrata <- TRUE
  if (useStrata & (is.null(subPopulation) | is.null(subPopulation$stratumId)))
    stop("Conditional regression specified, but no strata provided")
  if (useStrata)
    writeLines("Fitting stratified model") else writeLines("Fitting unstratified model")
  if (is.null(outcomeId) | is.null(cohortMethodData$exclude) | !ffbase::any.ff(cohortMethodData$outcomes$outcomeId ==
    outcomeId)) {
    outcomes <- cohortMethodData$outcomes
    cohorts <- ff::as.ram(cohortMethodData$cohort)
  } else {
    t <- cohortMethodData$outcomes$outcomeId == outcomeId
    t <- ffbase::ffwhich(t, t == TRUE)
    outcomes <- cohortMethodData$outcomes[t, ]
    t <- cohortMethodData$exclude$outcomeId == outcomeId
    t <- ffbase::ffwhich(t, t == TRUE)
    if (is.null(t)) {
      # None need to be excluded
      cohorts <- ff::as.ram(cohortMethodData$cohort)
    } else {
      t <- in.ff(cohortMethodData$cohorts$rowId, cohortMethodData$exclude$rowId[t])
      cohorts <- ff::as.ram(cohortMethodData$cohort[ffbase::ffwhich(t, t == FALSE), ])
    }
  }

  if (!is.null(subPopulation))
    cohorts <- merge(subPopulation, cohorts)  #keeping only persons that have been matched

  if (useCovariates) {
    covariates <- cohortMethodData$covariates
  }

  # Censor outcomes outside of risk window:
  cohorts$timeToCensor <- riskWindowEnd
  if (addExposureDaysToEnd)
    cohorts$timeToCensor <- cohorts$timeToCensor + cohorts$timeToCohortEnd
  cohorts$timeToCensor[cohorts$timeToCensor > cohorts$timeToObsPeriodEnd] <- cohorts$timeToObsPeriodEnd[cohorts$timeToCensor >
    cohorts$timeToObsPeriodEnd]

  outcomes <- tryCatch({
    merge(outcomes, ff::as.ffdf(cohorts))
  }, warning = function(w) {
    if (w$message == "No match found, returning NULL as ffdf can not contain 0 rows")
      data.frame()  #No events within selected population, return empty data.frame
 else merge(outcomes, ff::as.ffdf(cohorts))
  })
  if (nrow(outcomes) != 0)
    outcomes <- tryCatch({
      ffbase::subset.ffdf(outcomes, timeToEvent >= riskWindowStart & timeToEvent <= timeToCensor)
    }, error = function(e) {
      if (e$message == "no applicable method for 'as.hi' applied to an object of class \"NULL\"") {
        data.frame(ff::as.ram(outcomes)[0,
                   ])  #subset.ffdf throws an error if zero rows meet all criteria, so just return empty data.frame with same columns
      } else {
        stop(as.character(e$message))
      }
    })
  if (modelType == "cox") {
    return(createDataForModelFitCox(useStrata, useCovariates, cohorts, covariates, outcomes))
  }
  if (modelType == "pr" | modelType == "cpr") {
    return(createDataForModelFitPoisson(useStrata, useCovariates, cohorts, covariates, outcomes))
  }
  if (modelType == "lr" | modelType == "clr") {
    return(createDataForModelFitLogistic(useStrata, useCovariates, cohorts, covariates, outcomes))
  }
}

#' Create an outcome model, and compute the relative risk
#'
#' @description
#' \code{fitOutcomeModel} creates an outcome model, and computes the relative risk
#'
#' @param outcomeId              The concept ID of the outcome. Persons marked for removal for the
#'                               outcome will be removed prior to creating the outcome model.
#' @param cohortMethodData       An object of type \code{cohortMethodData} as generated using
#'                               \code{getDbCohortMethodData}.
#' @param subPopulation          A data frame specifying the (matched and/or trimmed) subpopulation to
#'                               be used in the study, as well as their strata (for conditional
#'                               models). This data frame should have at least a \code{RowId}, and a
#'                               \code{StratumId} when including stratification.
#' @param stratifiedCox          Specifically for Cox regressions: specify whether to use the strata
#'                               defined in \code{subPopulation} in the analysis. For Poisson
#'                               regression and logistic regression, this is implied in 'clr' and
#'                               'cpr'.
#' @param riskWindowStart        The start of the risk window (in days) relative to the index data.
#' @param riskWindowEnd          The end of the risk window (in days) relative to the index data (+
#'                               days of exposure if the \code{addExposureDaysToEnd} parameter is
#'                               specified).
#' @param addExposureDaysToEnd   Add the length of exposure the risk window?
#' @param useCovariates          Whether to use the covariate matrix in the cohortMethodData in the
#'                               outcome model.
#' @param fitModel               If false, the model will not be fit, and only summary statistics are
#'                               available.
#' @param modelType              The type of model to be fitted. See details for options.
#' @param prior                  The prior used to fit the model. See
#'                               \code{\link[Cyclops]{createPrior}} for details.
#' @param control                The control object used to control the cross-validation used to
#'                               determine the hyperparameters of the prior (if applicable). See
#'                               \code{\link[Cyclops]{createControl}} for details.
#'
#' @details
#' The model type can be one of these: \tabular{ll}{ \verb{lr} \tab Logistic regression \cr \verb{clr}
#' \tab Conditional logistic regression \cr \verb{cox} \tab Cox regression (stratified or not,
#' depending on whether \code{stata} is specified) \cr \verb{pr} \tab Poisson regression \cr
#' \verb{cpr} \tab Conditional Poisson regression \cr }
#'
#' @return
#' An object of class \code{outcomeModel}. Generic function \code{summary}, \code{coef}, and
#' \code{confint} are available.
#'
#' @examples
#' # todo
#'
#' @export
fitOutcomeModel <- function(outcomeId,
                            cohortMethodData,
                            subPopulation = NULL,
                            stratifiedCox = TRUE,
                            riskWindowStart = 0,
                            riskWindowEnd = 9999,
                            addExposureDaysToEnd = FALSE,
                            useCovariates = TRUE,
                            fitModel = TRUE,
                            modelType = "cox",
                            prior = createPrior("laplace", useCrossValidation = TRUE),
                            control = createControl(cvType = "auto",
                                                    startingVariance = 0.1,
                                                    selectorType = "byPid",
                                                    noiseLevel = "quiet")) {
  dataObject <- createDataForModelFit(outcomeId,
                                      cohortMethodData,
                                      subPopulation,
                                      stratifiedCox,
                                      riskWindowStart,
                                      riskWindowEnd,
                                      addExposureDaysToEnd,
                                      useCovariates,
                                      modelType)
  treatmentEstimate <- NULL
  coefficients <- NULL
  fit <- NULL
  priorVariance <- NULL
  status <- "NO MODEL FITTED"
  if (dataObject$zeroOutcomes)
    status <- "NO OUTCOMES REMAINING AFTER RESTRICTING TO SUBPOPULATION, CANNOT FIT"
  if (fitModel & !is.null(dataObject$cyclopsData)) {
    if (useCovariates) {
      if (dataObject$useStrata | modelType == "cox")
        prior$exclude <- 1  # Exclude treatment variable from regularization
 else prior$exclude <- c(0, 1)  # Exclude treatment variable and intercept from regularization
    } else prior <- createPrior("none")  #Only one variable, which we're not going to regularize, so effectively no prior
    if (sum(dataObject$data$y[dataObject$data$treatment == 0]) == 0 || sum(dataObject$data$y[dataObject$data$treatment ==
      1]) == 0) {
      fit <- "TREATMENT IS PERFECTLY PREDICTIVE OF OUTCOME, CANNOT FIT"
    } else {
      fit <- tryCatch({
        Cyclops::fitCyclopsModel(dataObject$cyclopsData, prior = prior, control = control)
      }, error = function(e) {
        e$message
      })
    }
    if (is.character(fit)) {
      coefficients <- c(0)
      treatmentEstimate <- data.frame(logRr = 0, logLb95 = -Inf, logUb95 = Inf, seLogRr = Inf)
      priorVariance <- 0
      status <- fit
    } else if (fit$return_flag == "ILLCONDITIONED") {
      coefficients <- c(0)
      treatmentEstimate <- data.frame(logRr = 0, logLb95 = -Inf, logUb95 = Inf, seLogRr = Inf)
      priorVariance <- 0
      status <- "ILL CONDITIONED, CANNOT FIT"
    } else {
      status <- "OK"
      coefficients <- coef(fit)
      logRr <- coef(fit)[names(coef(fit)) == "1"]
      ci <- tryCatch({
        confint(fit, parm = 1, includePenalty = TRUE)
      }, error = function(e) {
        missing(e)  # suppresses R CMD check note
        c(0, -Inf, Inf)
      })
      if (identical(ci, c(0, -Inf, Inf)))
        status <- "ERROR COMPUTING CI"
      seLogRr <- (ci[3] - logRr)/qnorm(0.975)
      treatmentEstimate <- data.frame(logRr = logRr,
                                      logLb95 = ci[2],
                                      logUb95 = ci[3],
                                      seLogRr = seLogRr)
      priorVariance <- fit$variance[1]
    }
  }
  counts <- cohortMethodData$metaData$counts

  if (!is.null(outcomeId) & !is.null(cohortMethodData$exclude)) {
    t <- cohortMethodData$exclude$outcomeId == outcomeId
    t <- ffbase::ffwhich(t, t == TRUE)
    if (is.null(t)) {
      cohortSubset <- cohortMethodData$cohort
    } else {
      t <- in.ff(cohortMethodData$cohorts$rowId, cohortMethodData$exclude$rowId[t])
      cohortSubset <- cohortMethodData$cohort[ffbase::ffwhich(t, t == TRUE), ]
    }
    treatedWithPriorOutcome <- ffbase::sum.ff(cohortSubset$treatment)
    comparatorWithPriorOutcome <- nrow(cohortSubset) - treatedWithPriorOutcome
    notPriorCount <- data.frame(treatment = c(0, 1),
                                notPriorCount = c(counts$notExcludedCount[counts$treatment ==
      0] - comparatorWithPriorOutcome, counts$notExcludedCount[counts$treatment == 1] - treatedWithPriorOutcome))
    counts <- merge(counts, notPriorCount)
  }
  if (!is.null(subPopulation)) {
    matchedTrimmedCount <- aggregate(rowId ~ treatment, data = subPopulation, length)
    names(matchedTrimmedCount) <- c("treatment", "matchedTrimmedCount")
    counts <- merge(counts, matchedTrimmedCount)
  }
  outcomeModel <- list(outcomeId = outcomeId,
                       modelType = modelType,
                       coefficients = coefficients,
                       priorVariance = priorVariance,
                       stratified = dataObject$useStrata,
                       usedCovariates = dataObject$useCovariates,
                       treatmentEstimate = treatmentEstimate,
                       data = dataObject$data,
                       counts = counts,
                       status = status)
  class(outcomeModel) <- "outcomeModel"
  return(outcomeModel)
}

#' @export
summary.outcomeModel <- function(object, ...) {
  if (object$modelType == "clr" || object$modelType == "lr") {
    patientTable <- table(object$data$treatment)
    eventTable <- table(object$data$treatment, object$data$y)

    counts <- matrix(0, nrow = 2, ncol = 2)
    counts[1, ] <- patientTable
    if (ncol(eventTable) == 1)
      counts[2, ] <- c(0, 0) else counts[2, ] <- eventTable[, 2]
    colnames(counts) <- c("Comparator", "Treated")
    rownames(counts) <- c("Nr. of persons", "Nr. of events")
  } else {
    patientTable <- table(object$data$treatment)
    eventTable <- table(object$data$treatment, object$data$y)
    timeTable <- aggregate(time ~ treatment, FUN = sum, data = object$data)[, 2]

    counts <- matrix(0, nrow = 3, ncol = 2)
    counts[1, ] <- patientTable
    if (ncol(eventTable) == 1)
      counts[2, ] <- c(0, 0) else counts[2, ] <- eventTable[, 2]
    counts[3, ] <- timeTable
    colnames(counts) <- c("Comparator", "Treated")
    rownames(counts) <- c("Nr. of persons", "Nr. of events", "Person time (days)")
  }


  if (is.null(object$coefficients)) {
    result <- list(modelType = object$modelType, counts = counts)
  } else {
    model <- c(length(object$coefficients), sum(object$coefficients != 0))
    names(model) <- c("Nr. of betas", "Nr. of non-zero betas")
    if (!is.null(object$data$stratumId)) {
      model <- c(model, length(unique(object$data$stratumId)))
      names(model)[length(model)] <- "Number of strata"
    }

    result <- list(modelType = object$modelType,
                   counts = counts,
                   model = model,
                   priorVariance = object$priorVariance,
                   coefficients = object$treatmentEstimate,
                   status = object$status)
  }
  class(result) <- "summary.outcomeModel"
  return(result)
}

#' @export
print.summary.outcomeModel <- function(x, ...) {
  writeLines(paste("Model type:", x$modelType))
  writeLines(paste("Status:", x$status))
  writeLines("")
  writeLines("Counts")
  printCoefmat(x$counts)
  if (!is.null(x$model)) {
    writeLines("")
    writeLines("Model")
    print(x$model)

    writeLines("")
    writeLines("Coefficients")
    d <- x$coefficients
    output <- data.frame(exp(d$logRr), exp(d$logLb95), exp(d$logUb95), d$logRr, d$seLogRr)

    colnames(output) <- c("Estimate", "lower .95", "upper .95", "logRr", "seLogRr")
    rownames(output) <- "treatment"
    printCoefmat(output)
    writeLines("")
    writeLines(paste("Prior variance:", x$priorVariance))
  }
}

#' @export
coef.outcomeModel <- function(object, ...) {
  return(object$treatmentEstimate$logRr)
}

#' @export
confint.outcomeModel <- function(object, parm, level = 0.95, ...) {
  missing(parm)  # suppresses R CMD check note
  if (level != 0.95)
    stop("Only supporting 95% confidence interval")
  return(c(object$treatmentEstimate$logLb95, object$treatmentEstimate$logUb95))
}

#' @export
print.outcomeModel <- function(x, ...) {
  writeLines(paste("Model type:", x$modelType))
  writeLines(paste("Status:", x$status))

  d <- x$treatmentEstimate
  output <- data.frame(exp(d$logRr), exp(d$logLb95), exp(d$logUb95), d$logRr, d$seLogRr)
  colnames(output) <- c("Estimate", "lower .95", "upper .95", "logRr", "seLogRr")
  rownames(output) <- "treatment"
  writeLines("")
  writeLines(paste("Prior variance:", x$priorVariance))
  printCoefmat(output)
}

#' Get the outcome model
#'
#' @description
#' \code{getOutcomeModel} shows the full outcome model, so showing the betas of all variables included
#' in the outcome model, not just the treatment variable.
#'
#' @param outcomeModel       An object of type \code{outcomeModel} as generated using he
#'                           \code{createOutcomeMode} function.
#' @param cohortMethodData   An object of type \code{cohortMethodData} as generated using
#'                           \code{getDbCohortMethodData}.
#'
#' @details
#' Shows the coefficients and names of the covariates with non-zero coefficients.
#'
#' @examples
#' # todo
#'
#' @export
getOutcomeModel <- function(outcomeModel, cohortMethodData) {
  cfs <- outcomeModel$coefficients

  cfs <- cfs[cfs != 0]
  attr(cfs, "names")[attr(cfs, "names") == "(Intercept)"] <- 0
  attr(cfs, "names")[attr(cfs, "names") == "treatment"] <- 1
  cfs <- data.frame(coefficient = cfs, id = as.numeric(attr(cfs, "names")))

  cfs <- merge(ff::as.ffdf(cfs),
               cohortMethodData$covariateRef,
               by.x = "id",
               by.y = "covariateId",
               all.x = TRUE)
  cfs <- ff::as.ram(cfs[, c("coefficient", "id", "covariateName")])
  cfs$covariateName <- as.character(cfs$covariateName)
  cfs <- cfs[order(-abs(cfs$coefficient)), ]
  cfs$covariateName[cfs$id == 0] <- "Intercept"
  return(cfs)
}

#' Plot the Kaplan-Meier curve
#'
#' @description
#' \code{plotKaplanMeier} creates the Kaplain-Meier survival plot
#'
#' @param outcomeModel          An object of type \code{outcomeModel} as generated using he
#'                              \code{fitOutcomeModel} function.
#' @param censorMarks           Whether or not to include censor marks in the plot.
#' @param confidenceIntervals   Plot 95 percent confidence intervals?
#' @param includeZero           Should the y axis include zero, or only go down to the lowest observed
#'                              survival?
#' @param dataCutoff            Fraction of the data (number censored) after which the graph will not
#'                              be shown.
#' @param treatmentLabel        A label to us for the treated cohort.
#' @param comparatorLabel       A label to us for the comparator cohort.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @examples
#' # todo
#'
#' @export
plotKaplanMeier <- function(outcomeModel,
                            censorMarks = FALSE,
                            confidenceIntervals = TRUE,
                            includeZero = TRUE,
                            dataCutoff = 0.99,
                            treatmentLabel = "Treated",
                            comparatorLabel = "Comparator",
                            fileName = NULL) {
  if (class(outcomeModel) != "outcomeModel")
    stop("Object not of class outcomeModel")
  if (outcomeModel$modelType != "cox")
    stop("Outcome model is not a Cox model")
  if (outcomeModel$stratified)
    warning("The outcome model is stratified, but the stratification is not visible in the plot")

  sv <- survival::survfit(survival::Surv(time, y) ~ treatment, outcomeModel$data, conf.int = TRUE)
  data <- data.frame(time = sv$time,
                     n.risk = sv$n.risk,
                     n.event = sv$n.event,
                     n.censor = sv$n.censor,
                     surv = sv$surv,
                     strata = summary(sv, censored = T)$strata,
                     upper = sv$upper,
                     lower = sv$lower)
  levels(data$strata)[levels(data$strata) == "treatment=0"] <- comparatorLabel
  levels(data$strata)[levels(data$strata) == "treatment=1"] <- treatmentLabel
  data$strata <- factor(data$strata, levels = c(treatmentLabel, comparatorLabel))
  dataAtT <- aggregate(n.censor ~ time, data, sum)
  dataAtT$cumSum <- cumsum(dataAtT$n.censor)
  cutoff <- min(dataAtT$time[dataAtT$cumSum >= dataCutoff * sum(dataAtT$n.censor)])
  xlabs <- "Time in days"
  ylabs <- "Survival probability"
  main <- "Kaplan-Meier Plot"
  xlims <- c(0, cutoff)
  data <- data[data$time <= cutoff, ]
  if (includeZero) {
    ylims <- c(0, 1)
  } else if (confidenceIntervals) {
    ylims <- c(min(data$lower), 1)
  } else {
    ylims <- c(min(data$surv), 1)
  }
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = time,
                                             y = surv,
                                             color = strata,
                                             fill = strata,
                                             ymin = lower,
                                             ymax = upper))

  if (confidenceIntervals)
    plot <- plot + ggplot2::geom_ribbon(color = rgb(0, 0, 0, alpha = 0))

  plot <- plot +
          ggplot2::geom_step(size = 1) +
          ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.8),
                                                 rgb(0, 0, 0.8, alpha = 0.8))) +
          ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.3),
                                                rgb(0, 0, 0.8, alpha = 0.3))) +
          ggplot2::scale_x_continuous(xlabs, limits = xlims) +
          ggplot2::scale_y_continuous(ylabs, limits = ylims) +
          ggplot2::ggtitle(main) +
          ggplot2::theme(legend.title = ggplot2::element_blank())

  if (censorMarks == TRUE)
    plot <- plot + ggplot2::geom_point(data = subset(data, n.censor >= 1),
                                       ggplot2::aes(x = time, y = surv),
                                       shape = "|",
                                       size = 3)

  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}

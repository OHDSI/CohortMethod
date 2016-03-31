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
#
# @author Observational Health Data Sciences and Informatics
# @author Patrick Ryan
# @author Marc Suchard
# @author Martijn Schuemie

#' Create an outcome model, and compute the relative risk
#'
#' @description
#' \code{fitOutcomeModel} creates an outcome model, and computes the relative risk
#'
#' @param outcomeId              The concept ID of the outcome. Persons marked for removal for the
#'                               outcome will be removed prior to creating the outcome model.
#' @param cohortMethodData       An object of type \code{cohortMethodData} as generated using
#'                               \code{getDbCohortMethodData}.
#' @param excludePriorOutcome    Remove people that have the outcome prior to the risk window start date?
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
fitOutcomeModel <- function(population,
                            cohortMethodData,
                            stratified = TRUE,
                            useCovariates = TRUE,
                            prior = createPrior("laplace", useCrossValidation = TRUE),
                            control = createControl(cvType = "auto",
                                                    startingVariance = 0.1,
                                                    selectorType = "byPid",
                                                    noiseLevel = "quiet")) {
  if (stratified && is.null(population$stratumId))
    stop("Requested stratified analysis, but no stratumId column found in population. Please use matchOnPs or stratifyByPs to create strata.")
  if (is.null(population$outcome))
    stop("No outcome variable found in population object. Use createStudyPopulation to create variable.")
  if (missing(cohortMethodData) && useCovariates)
    stop("Requested all covariates for model, but no cohortMethodData object specified")

  treatmentEstimate <- NULL
  coefficients <- NULL
  fit <- NULL
  priorVariance <- NULL
  status <- "NO MODEL FITTED"
  if (nrow(population) == 0) {
    status <- "NO SUBJECTS IN POPULATION, CANNOT FIT"
  } else if (sum(population$outcome) == 0) {
    status <- "NO OUTCOMES FOUND FOR POPULATION, CANNOT FIT"
  } else if (sum(population$outcome[population$treatment == 0]) == 0 || sum(population$outcome[population$treatment == 1]) == 0) {
    fit <- "TREATMENT IS PERFECTLY PREDICTIVE OF OUTCOME, CANNOT FIT"
  } else {
    cohorts <- ff::as.ffdf(population)
    colnames(cohorts)[colnames(cohorts) == "outcome"] <- "y"
    if (!stratified) {
      cohorts$stratumId <- NULL
    }
    modelType <- attr(population, "metaData")$modelType
    if (useCovariates) {
      if (stratified || modelType == "cox") {
        prior$exclude <- 1  # Exclude treatment variable from regularization
      } else {
        prior$exclude <- c(0, 1)  # Exclude treatment variable and intercept from regularization
      }
      covariates <- limitCovariatesToPopulation(cohortMethodData$covariates, cohorts$rowId)
      if (stratified) {
        cols <- which(colnames(cohorts) %in% c("rowId","stratumId"))
        covariates <- ffbase::merge.ffdf(covariates, ff::ffdf(rowId = ff::clone.ff(cohorts$rowId), stratumId = ff::clone.ff(cohorts$stratumId)))
      }
    } else {
      prior <- createPrior("none")  #Only one variable, which we're not going to regularize, so effectively no prior
      covariates <- ff::ffdf(rowId = ff::clone.ff(cohorts$rowId),
                             covariateId = ff::ff(1, length = nrow(cohorts)),
                             covariateValue = ff::clone.ff(cohorts$treatment),
                             row.names = row.names(cohorts))
      if (stratified) {
        covariates$stratumId <- ff::clone.ff(cohorts$stratumId)
      }
    }
    if (stratified || modelType == "cox") {
      addIntercept <- FALSE
    } else {
      addIntercept <- TRUE
    }
    cyclopsData <- Cyclops::convertToCyclopsData(outcomes = cohorts,
                                                 covariates = covariates,
                                                 addIntercept = addIntercept,
                                                 modelType = modelTypeToCyclopsModelType(modelType, stratified),
                                                 checkSorting = TRUE,
                                                 checkRowIds = FALSE,
                                                 normalize = NULL,
                                                 quiet = TRUE)
    fit <- tryCatch({
      Cyclops::fitCyclopsModel(cyclopsData, prior = prior, control = control)
    }, error = function(e) {
      e$message
    })
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
    } else if (fit$return_flag == "MAX_ITERATIONS") {
      coefficients <- c(0)
      treatmentEstimate <- data.frame(logRr = 0, logLb95 = -Inf, logUb95 = Inf, seLogRr = Inf)
      priorVariance <- 0
      status <- "REACHED MAXIMUM NUMBER OF ITERATIONS, CANNOT FIT"
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
      seLogRr <- (ci[3] - ci[2])/(2*qnorm(0.975))
      treatmentEstimate <- data.frame(logRr = logRr,
                                      logLb95 = ci[2],
                                      logUb95 = ci[3],
                                      seLogRr = seLogRr)
      priorVariance <- fit$variance[1]
    }
  }
  outcomeModel <- attr(population, "metaData")
  outcomeModel$outcomeModelCoefficients <- coefficients
  outcomeModel$outcomeModelPriorVariance <- priorVariance
  outcomeModel$outcomeModelStratified <- stratified
  outcomeModel$outcomeModelUseCovariates <- useCovariates
  outcomeModel$outcomeModelTreatmentEstimate <- treatmentEstimate
  outcomeModel$outcomeModelStatus <- status
  outcomeModel$populationCounts <- getCounts(population, "Population count")
  outcomeCounts <- data.frame(treatedPersons = length(unique(population$subjectId[population$treatment == 1 & population$outcome != 0])),
                              comparatorPersons = length(unique(population$subjectId[population$treatment == 0 & population$outcome != 0])),
                              treatedExposures = length(population$subjectId[population$treatment == 1 & population$outcome != 0]),
                              comparatorExposures = length(population$subjectId[population$treatment == 0 & population$outcome != 0]),
                              treatedOutcomes = sum(population$outcome[population$treatment == 1]),
                              comparatorOutcomes = sum(population$outcome[population$treatment == 0]))
  outcomeModel$outcomeCounts <- outcomeCounts
  if (modelType == "poisson" || modelType == "cox") {
    timeAtRisk <- data.frame(treatedDays = sum(population$time[population$treatment == 1]),
                             comparatorDays = sum(population$time[population$treatment == 0]))
    outcomeModel$timeAtRisk <- timeAtRisk
  }
  class(outcomeModel) <- "outcomeModel"
  return(outcomeModel)
}

modelTypeToCyclopsModelType <- function(modelType, stratified) {
  if (modelType == "logistic") {
    if (stratified)
      return("clr")
    else
      return("lr")
  } else if (modelType == "poisson") {
    if (stratified)
      return("cpr")
    else
      return("pr")
  } else if (modelType == "cox") {
    return("cox")
  } else
    stop(paste("Unknown model type:", modelType))
}

#' @export
summary.outcomeModel <- function(object, ...) {
  class(object) <- "summary.outcomeModel"
  return(object)
}

#' @export
print.summary.outcomeModel <- function(x, ...) {
  print.outcomeModel(x)

  writeLines("")
  writeLines("Population counts")
  d <- x$populationCounts
  row.names(d) <- "Count"
  d$description <- NULL
  printCoefmat(d)

  writeLines("")
  writeLines("Outcome counts")
  d <- x$outcomeCounts
  row.names(d) <- "Count"
  printCoefmat(d)

  if (x$modelType == "poisson" || x$modelType == "cox") {
    writeLines("")
    writeLines("Time at risk")
    d <- x$timeAtRisk
    row.names(d) <- "Days"
    printCoefmat(d)
  }
}

#' @export
coef.outcomeModel <- function(object, ...) {
  return(object$outcomeModelTreatmentEstimate$logRr)
}

#' @export
confint.outcomeModel <- function(object, parm, level = 0.95, ...) {
  missing(parm)  # suppresses R CMD check note
  if (level != 0.95)
    stop("Only supporting 95% confidence interval")
  return(c(object$outcomeModelTreatmentEstimate$logLb95, object$outcomeModelTreatmentEstimate$logUb95))
}

#' @export
print.outcomeModel <- function(x, ...) {
  writeLines(paste("Model type:", x$modelType))
  writeLines(paste("Stratified:", x$outcomeModelStratified))
  writeLines(paste("Use covariates:", x$outcomeModelUseCovariates))
  writeLines(paste("Status:", x$outcomeModelStatus))

  d <- x$outcomeModelTreatmentEstimate
  output <- data.frame(exp(d$logRr), exp(d$logLb95), exp(d$logUb95), d$logRr, d$seLogRr)
  colnames(output) <- c("Estimate", "lower .95", "upper .95", "logRr", "seLogRr")
  rownames(output) <- "treatment"
  writeLines("")
  if (!is.na(x$outcomeModelPriorVariance)) {
    writeLines(paste("Prior variance:", x$outcomeModelPriorVariance))
  }
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
  cfs <- outcomeModel$outcomeModelCoefficients

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
  cfs$covariateName[cfs$id == 1] <- "Treatment"
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
#' @param title                 The main title of the plot.
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
                            title = "Kaplan-Meier Plot",
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
    ggplot2::ggtitle(title) +
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

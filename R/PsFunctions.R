# Copyright 2025 Observational Health Data Sciences and Informatics
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

#' Create propensity scores
#'
#' @description
#' Creates propensity scores and inverse probability of treatment weights (IPTW) using a regularized logistic regression.
#'
#' @details
#' IPTW estimates either the average treatment effect (ate) or average treatment effect in the treated
#' (att) using stabilized inverse propensity scores (Xu et al. 2010).
#'
#' @param cohortMethodData         An object of type [CohortMethodData] as generated using
#'                                 [getDbCohortMethodData()].
#' @param population               A data frame describing the population. This should at least have a
#'                                 `rowId` column corresponding to the `rowId` column in the
#'                                 [CohortMethodData] covariates object and a `treatment` column.
#'                                 If population is not specified, the full population in the
#'                                 [CohortMethodData] will be used.
#' @param excludeCovariateIds      Exclude these covariates from the propensity model.
#' @param includeCovariateIds      Include only these covariates in the propensity model.
#' @param maxCohortSizeForFitting  If the target or comparator cohort are larger than this number, they
#'                                 will be downsampled before fitting the propensity model. The model
#'                                 will be used to compute propensity scores for all subjects. The
#'                                 purpose of the sampling is to gain speed. Setting this number to 0
#'                                 means no downsampling will be applied.
#' @param errorOnHighCorrelation   If true, the function will test each covariate for correlation with
#'                                 the treatment assignment. If any covariate has an unusually high
#'                                 correlation (either positive or negative), this will throw and
#'                                 error.
#' @param stopOnError              If an error occur, should the function stop? Else, the two cohorts
#'                                 will be assumed to be perfectly separable.
#' @param prior                    The prior used to fit the model. See
#'                                 [Cyclops::createPrior()] for details.
#' @param control                  The control object used to control the cross-validation used to
#'                                 determine the hyperparameters of the prior (if applicable). See
#'                                 [Cyclops::createControl()] for details.
#' @param estimator                The type of estimator for the IPTW. Options are `estimator = "ate"`
#'                                 for the average treatment effect, `estimator = "att"` for the
#'                                 average treatment effect in the treated, and `estimator = "ato"`
#'                                 for the average treatment effect in the overlap population.
#'
#' @references
#' Xu S, Ross C, Raebel MA, Shetterly S, Blanchette C, Smith D. Use of stabilized inverse propensity scores
#' as weights to directly estimate relative risk and its confidence intervals. Value Health.
#' 2010;13(2):273-277. doi:10.1111/j.1524-4733.2009.00671.x
#'
#' @examples
#' data(cohortMethodDataSimulationProfile)
#' cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = 1000)
#' ps <- createPs(cohortMethodData)
#'
#' @export
createPs <- function(cohortMethodData,
                     population = NULL,
                     excludeCovariateIds = c(),
                     includeCovariateIds = c(),
                     maxCohortSizeForFitting = 250000,
                     errorOnHighCorrelation = TRUE,
                     stopOnError = TRUE,
                     prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
                     control = createControl(
                       noiseLevel = "silent",
                       cvType = "auto",
                       seed = 1,
                       resetCoefficients = TRUE,
                       tolerance = 2e-07,
                       cvRepetitions = 10,
                       startingVariance = 0.01
                     ),
                     estimator = "att") {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(cohortMethodData, "CohortMethodData", add = errorMessages)
  checkmate::assertDataFrame(population, null.ok = TRUE, add = errorMessages)
  .assertCovariateId(excludeCovariateIds, null.ok = TRUE, add = errorMessages)
  .assertCovariateId(includeCovariateIds, null.ok = TRUE, add = errorMessages)
  checkmate::assertInt(maxCohortSizeForFitting, lower = 0, add = errorMessages)
  checkmate::assertLogical(errorOnHighCorrelation, len = 1, add = errorMessages)
  checkmate::assertLogical(stopOnError, len = 1, add = errorMessages)
  checkmate::assertClass(prior, "cyclopsPrior", add = errorMessages)
  checkmate::assertClass(control, "cyclopsControl", add = errorMessages)
  checkmate::assertChoice(estimator, c("ate", "att", "ato"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (is.null(population)) {
    population <- cohortMethodData$cohorts |>
      collect()
  }
  if (!("rowId" %in% colnames(population))) {
    stop("Missing column rowId in population")
  }
  if (!("treatment" %in% colnames(population))) {
    stop("Missing column treatment in population")
  }

  start <- Sys.time()
  population <- population[order(population$rowId), ]
  if (nrow(population) == 0) {
    error <- "No subjects in population, so cannot fit model"
    sampled <- FALSE
    ref <- NULL
  } else if (all(population$treatment == 1) || all(population$treatment == 0)) {
    error <- "Target or comparator cohort empty, so cannot fit model"
    sampled <- FALSE
    ref <- NULL
  } else if (nrow_temp(cohortMethodData$covariates) == 0) {
    error <- "No covariate data, so cannot fit model"
    sampled <- FALSE
    ref <- NULL
  } else {
    rowIds <- cohortMethodData$covariates |>
      distinct(.data$rowId) |>
      pull()
    if (all(rowIds %in% population$rowId) &&
        length(includeCovariateIds) == 0 &&
        length(excludeCovariateIds) == 0) {
      # No filtering necessary, send to tidyCovariateData:
      covariateData <- FeatureExtraction::tidyCovariateData(cohortMethodData)
    } else {
      # Need filtering here before sending it to tidyCovariateData:
      covariates <- cohortMethodData$covariates |>
        filter(.data$rowId %in% local(population$rowId))

      if (length(includeCovariateIds) != 0) {
        covariates <- covariates |>
          filter(.data$covariateId %in% includeCovariateIds)
      }
      if (length(excludeCovariateIds) != 0) {
        covariates <- covariates |>
          filter(!.data$covariateId %in% excludeCovariateIds)
      }
      filteredCovariateData <- Andromeda::andromeda(
        covariates = covariates,
        covariateRef = cohortMethodData$covariateRef,
        analysisRef = cohortMethodData$analysisRef
      )
      metaData <- attr(cohortMethodData, "metaData")
      metaData$populationSize <- nrow(population)
      attr(filteredCovariateData, "metaData") <- metaData
      class(filteredCovariateData) <- "CovariateData"
      covariateData <- FeatureExtraction::tidyCovariateData(filteredCovariateData)
      close(filteredCovariateData)
    }
    on.exit(close(covariateData))
    covariates <- covariateData$covariates
    attr(population, "metaData")$deletedInfrequentCovariateIds <- attr(covariateData, "metaData")$deletedInfrequentCovariateIds
    attr(population, "metaData")$deletedRedundantCovariateIds <- attr(covariateData, "metaData")$deletedRedundantCovariateIds
    sampled <- FALSE
    if (maxCohortSizeForFitting != 0) {
      set.seed(0)
      targetRowIds <- population$rowId[population$treatment == 1]
      if (length(targetRowIds) > maxCohortSizeForFitting) {
        message(paste0("Downsampling target cohort from ", length(targetRowIds), " to ", maxCohortSizeForFitting, " before fitting"))
        targetRowIds <- sample(targetRowIds, size = maxCohortSizeForFitting, replace = FALSE)
        sampled <- TRUE
      }
      comparatorRowIds <- population$rowId[population$treatment == 0]
      if (length(comparatorRowIds) > maxCohortSizeForFitting) {
        message(paste0("Downsampling comparator cohort from ", length(comparatorRowIds), " to ", maxCohortSizeForFitting, " before fitting"))
        comparatorRowIds <- sample(comparatorRowIds, size = maxCohortSizeForFitting, replace = FALSE)
        sampled <- TRUE
      }
      if (sampled) {
        fullPopulation <- population
        fullCovariates <- covariates
        population <- population[population$rowId %in% c(targetRowIds, comparatorRowIds), ]
        covariates <- covariates |>
          filter(.data$rowId %in% local(population$rowId))
      }
    }
    population <- population[order(population$rowId), ]
    outcomes <- population
    colnames(outcomes)[colnames(outcomes) == "treatment"] <- "y"
    covariateData$outcomes <- outcomes
    floatingPoint <- getOption("floatingPoint")
    if (is.null(floatingPoint)) {
      floatingPoint <- 64
    } else {
      message("Cyclops using precision of ", floatingPoint)
    }
    cyclopsData <- Cyclops::convertToCyclopsData(covariateData$outcomes, covariates, modelType = "lr", quiet = TRUE, floatingPoint = floatingPoint)
    error <- NULL
    ref <- NULL
    if (errorOnHighCorrelation) {
      suspect <- Cyclops::getUnivariableCorrelation(cyclopsData, threshold = 0.5)
      suspect <- suspect[!is.na(suspect)]
      if (length(suspect) != 0) {
        covariateIds <- as.numeric(names(suspect))
        ref <- cohortMethodData$covariateRef |>
          filter(.data$covariateId %in% covariateIds) |>
          collect()
        message("High correlation between covariate(s) and treatment detected:")
        message(paste(colnames(ref), collapse = "\t"))
        for (i in 1:nrow(ref)) {
          message(paste(ref[i, ], collapse = "\t"))
        }
        message <- "High correlation between covariate(s) and treatment detected. Perhaps you forgot to exclude part of the exposure definition from the covariates?"
        if (stopOnError) {
          stop(message)
        } else {
          error <- message
        }
      }
    }
  }
  if (is.null(error)) {
    cyclopsFit <- tryCatch(
      {
        Cyclops::fitCyclopsModel(cyclopsData, prior = prior, control = control)
      },
      error = function(e) {
        e$message
      }
    )
    if (is.character(cyclopsFit)) {
      if (stopOnError) {
        stop(cyclopsFit)
      } else {
        error <- cyclopsFit
      }
    } else if (cyclopsFit$return_flag != "SUCCESS") {
      if (stopOnError) {
        stop(cyclopsFit$return_flag)
      } else {
        error <- cyclopsFit$return_flag
      }
    }
  }
  if (is.null(error)) {
    error <- "OK"
    cfs <- coef(cyclopsFit)
    if (all(cfs[2:length(cfs)] == 0)) {
      warning("All coefficients (except maybe the intercept) are zero. Either the covariates are completely uninformative or completely predictive of the treatment. Did you remember to exclude the treatment variables from the covariates?")
    }
    if (sampled) {
      # Adjust intercept to non-sampled population:
      yBar <- mean(population$treatment)
      yOdds <- yBar / (1 - yBar)
      yBarNew <- mean(fullPopulation$treatment)
      yOddsNew <- yBarNew / (1 - yBarNew)
      delta <- log(yOdds) - log(yOddsNew)
      cfs[1] <- cfs[1] - delta # Equation (7) in King and Zeng (2001)
      cyclopsFit$estimation$estimate[1] <- cfs[1]
      covariateData$fullOutcomes <- fullPopulation
      population <- fullPopulation
      propensityScore <- predict(cyclopsFit, newOutcomes = covariateData$fullOutcomes, newCovariates = fullCovariates)
    } else {
      propensityScore <- predict(cyclopsFit)
    }
    propensityScore <- tibble(rowId = as.numeric(names(propensityScore)),
                              propensityScore = propensityScore)
    population <- population |>
      inner_join(propensityScore, by = join_by("rowId"))
    attr(population, "metaData")$psModelCoef <- coef(cyclopsFit)
    attr(population, "metaData")$psModelPriorVariance <- cyclopsFit$variance[1]
  } else {
    if (sampled) {
      population <- fullPopulation
    }
    population$propensityScore <- population$treatment
    attr(population, "metaData")$psError <- error
    if (!is.null(ref)) {
      attr(population, "metaData")$psHighCorrelation <- ref
    }
  }
  population$propensityScore <- round(population$propensityScore, 10)
  population <- computePreferenceScore(population)
  population <- computeIptw(population, estimator)
  attr(population, "metaData")$iptwEstimator <- estimator
  delta <- Sys.time() - start
  ParallelLogger::logDebug("Propensity model fitting finished with status ", error)
  message("Creating propensity scores took ", signif(delta, 3), " ", attr(delta, "units"))
  return(population)
}

computeIptw <- function(population, estimator = "ate") {
  # Unstabilized:
  # ATE:
  # population$iptw <- ifelse(population$treatment == 1,
  #                                        1/population$propensityScore,
  #                                        1/(1 - population$propensityScore))
  # ATT:
  # population$iptw <- ifelse(population$treatment == 1,
  #                                        1,
  #                                        population$propensityScore/(1 - population$propensityScore))
  # ATO:
  # Average treatment effect in the overlap population
  # https://doi.org/10.1093/aje/kwy201
  # population$iptw <- ifelse(population$treatment == 1,
  #                                        1 - population$propensityScore,
  #                                        population$propensityScore)
  if (estimator == "ate") {
    # 'Stabilized' ATE:
    population$iptw <- ifelse(population$treatment == 1,
                              mean(population$treatment == 1) / population$propensityScore,
                              mean(population$treatment == 0) / (1 - population$propensityScore)
    )
  } else if (estimator == "att") {
    # 'Stabilized' ATT:
    population$iptw <- ifelse(population$treatment == 1,
                              mean(population$treatment == 1),
                              mean(population$treatment == 0) * population$propensityScore / (1 - population$propensityScore)
    )
  } else if (estimator == "ato") {
    population$iptw <- ifelse(population$treatment == 1,
                              1 - population$propensityScore,
                              population$propensityScore)
  } else stop("The estimator argument should be either 'ate', 'att', or 'ato'.")
  return(population)
}

#' Get the propensity model
#'
#' @description
#' Returns the coefficients and names of the covariates with non-zero coefficients.
#'
#' @param propensityScore    The propensity scores as generated using the [createPs()] function.
#'
#' @template CohortMethodData
#'
#' @return
#' A tibble.
#'
#' @export
getPsModel <- function(propensityScore, cohortMethodData) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(propensityScore, null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(cohortMethodData, "CohortMethodData", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  coefficients <- attr(propensityScore, "metaData")$psModelCoef
  if (is.null(coefficients)) {
    return(tibble(
      coefficient = NA,
      covariateId = NA,
      covariateName = NA
    ))
  }
  result <- tibble(
    coefficient = coefficients[1],
    covariateId = NA,
    covariateName = "(Intercept)"
  )
  coefficients <- coefficients[2:length(coefficients)]
  coefficients <- coefficients[coefficients != 0]
  if (length(coefficients) != 0) {
    covariateIdIsInteger64 <- cohortMethodData$covariateRef |>
      pull(.data$covariateId) |>
      is("integer64")
    if (covariateIdIsInteger64) {
      coefficients <- tibble(
        coefficient = coefficients,
        covariateId = bit64::as.integer64(attr(coefficients, "names"))
      )
    } else {
      coefficients <- tibble(
        coefficient = coefficients,
        covariateId = as.numeric(attr(coefficients, "names"))
      )
    }
    covariateRef <- cohortMethodData$covariateRef |>
      collect()
    coefficients <- coefficients |>
      inner_join(covariateRef, by = "covariateId") |>
      select("coefficient", "covariateId", "covariateName")
    result <- bind_rows(result, coefficients) |>
      arrange(-abs(.data$coefficient))
  }
  return(result)
}

computePreferenceScore <- function(data, unfilteredData = NULL) {
  if (is.null(unfilteredData)) {
    proportion <- sum(data$treatment) / nrow(data)
  } else {
    proportion <- sum(unfilteredData$treatment) / nrow(unfilteredData)
  }
  propensityScore <- data$propensityScore
  propensityScore[propensityScore > 0.9999999] <- 0.9999999
  x <- exp(log(propensityScore / (1 - propensityScore)) - log(proportion / (1 - proportion)))
  data$preferenceScore <- x / (x + 1)
  return(data)
}

#' Compute fraction in equipoise
#'
#' @param data              A data frame with at least the two columns described below.
#' @param equipoiseBounds   The bounds on the preference score to determine whether a subject is in
#'                          equipoise.
#'
#' @details
#' Computes the fraction of the population (the union of the target and comparator cohorts) who are in clinical
#' equipoise (i.e. who had a reasonable chance of receiving either target or comparator, based on the baseline
#' characteristics).
#'
#' The data frame should have a least the following two columns:
#'
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group
#' - propensityScore (numeric): Propensity score
#'
#' @return
#' A numeric value (fraction in equipoise) between 0 and 1.
#'
#' @references
#' Walker AM, Patrick AR, Lauer MS, Hornbrook MC, Marin MG, Platt R, Roger VL, Stang P, and
#' Schneeweiss S. (2013) A tool for assessing the feasibility of comparative effectiveness research,
#' Comparative Effective Research, 3, 11-20
#'
#' @export
computeEquipoise <- function(data, equipoiseBounds = c(0.3, 0.7)) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(data, add = errorMessages)
  checkmate::assertNames(colnames(data), must.include = c("treatment", "propensityScore"), add = errorMessages)
  checkmate::assertNumeric(equipoiseBounds, lower = 0, upper = 1, len = 2, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!"preferenceScore" %in% colnames(data)) {
    data <- computePreferenceScore(data)
  }
  equipoise <- mean(data$preferenceScore >= equipoiseBounds[1] & data$preferenceScore <= equipoiseBounds[2])
  return(equipoise)
}

#' Plot the propensity score distribution
#'
#' @description
#' Plots the propensity (or preference) score distribution.
#'
#' @param data              A data frame with at least the two columns described below
#' @param unfilteredData    To be used when computing preference scores on data from which subjects
#'                          have already been removed, e.g. through trimming and/or matching. This data
#'                          frame should have the same structure as `data`.
#' @param scale             The scale of the graph. Two scales are supported: `scale =
#'                          'propensity'` or `scale = 'preference'`. The preference score scale is
#'                          defined by Walker et al (2013).
#' @param type              Type of plot. Four possible values: `type = 'density'`  `type =
#'                          'histogram'`, `type = 'histogramCount'`,
#'                          or `type = 'histogramProportion'`. `'histogram'` defaults to `'histogramCount'`.
#' @param binWidth          For histograms, the width of the bins
#' @param targetLabel       A label to us for the target cohort.
#' @param comparatorLabel   A label to us for the comparator cohort.
#' @param showCountsLabel   Show subject counts?
#' @param showAucLabel      Show the AUC?
#' @param showEquiposeLabel Show the percentage of the population in equipoise?
#' @param equipoiseBounds   The bounds on the preference score to determine whether a subject is in
#'                          equipoise.
#' @param unitOfAnalysis    The unit of analysis in the input data. Defaults to 'subjects'.
#' @param title             Optional: the main title for the plot.
#' @param fileName          Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function [ggplot2::ggsave()] for supported file formats.
#'
#' @details
#' The data frame should have a least the following two columns:
#'
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group
#' - propensityScore (numeric): Propensity score
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave()] function to save to file in a different
#' format.
#'
#' @examples
#' treatment <- rep(0:1, each = 100)
#' propensityScore <- c(rnorm(100, mean = 0.4, sd = 0.25), rnorm(100, mean = 0.6, sd = 0.25))
#' data <- data.frame(treatment = treatment, propensityScore = propensityScore)
#' data <- data[data$propensityScore > 0 & data$propensityScore < 1, ]
#' plotPs(data)
#'
#' @references
#' Walker AM, Patrick AR, Lauer MS, Hornbrook MC, Marin MG, Platt R, Roger VL, Stang P, and
#' Schneeweiss S. (2013) A tool for assessing the feasibility of comparative effectiveness research,
#' Comparative Effective Research, 3, 11-20
#'
#' @export
plotPs <- function(data,
                   unfilteredData = NULL,
                   scale = "preference",
                   type = "density",
                   binWidth = 0.05,
                   targetLabel = "Target",
                   comparatorLabel = "Comparator",
                   showCountsLabel = FALSE,
                   showAucLabel = FALSE,
                   showEquiposeLabel = FALSE,
                   equipoiseBounds = c(0.3, 0.7),
                   unitOfAnalysis = "subjects",
                   title = NULL,
                   fileName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(data, add = errorMessages)
  checkmate::assertNames(colnames(data), must.include = c("treatment", "propensityScore"), add = errorMessages)
  checkmate::assertDataFrame(unfilteredData, null.ok = TRUE, add = errorMessages)
  if (!is.null(unfilteredData)) {
    checkmate::assertNames(colnames(unfilteredData), must.include = c("treatment", "propensityScore"), add = errorMessages)
  }
  checkmate::assertChoice(scale, c("propensity", "preference"), add = errorMessages)
  checkmate::assertChoice(type, c("density", "histogram", "histogramCount", "histogramProportion"), add = errorMessages)
  checkmate::assertNumber(binWidth, lower = 0, upper = 1, add = errorMessages)
  checkmate::assertCharacter(targetLabel, len = 1, add = errorMessages)
  checkmate::assertCharacter(comparatorLabel, len = 1, add = errorMessages)
  checkmate::assertLogical(showCountsLabel, len = 1, add = errorMessages)
  checkmate::assertLogical(showAucLabel, len = 1, add = errorMessages)
  checkmate::assertLogical(showEquiposeLabel, len = 1, add = errorMessages)
  checkmate::assertNumeric(equipoiseBounds, lower = 0, upper = 1, len = 2, add = errorMessages)
  checkmate::assertCharacter(unitOfAnalysis, len = 1, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (type == "histogram") {
    type <- "histogramCount"
  }

  if (scale == "preference") {
    data <- computePreferenceScore(data, unfilteredData)
    data$score <- data$preferenceScore
    label <- "Preference score"
  } else {
    data$score <- data$propensityScore
    label <- "Propensity score"
  }
  if (showAucLabel || showCountsLabel || showEquiposeLabel) {
    yMultiplier <- 1.25
  } else {
    yMultiplier <- 1
  }
  if (type == "density") {
    d1 <- density(data$score[data$treatment == 1], from = 0, to = 1, n = 200)
    d0 <- density(data$score[data$treatment == 0], from = 0, to = 1, n = 200)
    d <- data.frame(x = c(d1$x, d0$x), y = c(d1$y, d0$y), treatment = c(
      rep(targetLabel, length(d1$x)),
      rep(comparatorLabel, length(d0$x))
    ))
    d$treatment <- factor(d$treatment, levels = c(targetLabel, comparatorLabel))
    plot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_density(stat = "identity", ggplot2::aes(color = .data$treatment, group = .data$treatment, fill = .data$treatment)) +
      ggplot2::scale_fill_manual(values = c(
        rgb(0.8, 0, 0, alpha = 0.5),
        rgb(0, 0, 0.8, alpha = 0.5)
      )) +
      ggplot2::scale_color_manual(values = c(
        rgb(0.8, 0, 0, alpha = 0.5),
        rgb(0, 0, 0.8, alpha = 0.5)
      )) +
      ggplot2::scale_x_continuous(label, limits = c(0, 1)) +
      ggplot2::scale_y_continuous("Density", limits = c(0, max(d$y) * yMultiplier)) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = "top",
        legend.text = ggplot2::element_text(margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))
      )
    if (!is.null(attr(data, "strata"))) {
      strata <- data.frame(propensityScore = attr(data, "strata"))
      if (scale == "preference") {
        if (is.null(unfilteredData)) {
          strata <- computePreferenceScore(strata, data)
        } else {
          strata <- computePreferenceScore(strata, unfilteredData)
        }
        strata$score <- strata$preferenceScore
      } else {
        strata$score <- strata$propensityScore
      }
      plot <- plot +
        ggplot2::geom_vline(xintercept = strata$score, color = rgb(0, 0, 0, alpha = 0.5))
    }
  } else {
    x <- seq(from = 0, to = 1, by = binWidth)
    d1 <- data.frame(xmin = cut(data$score[data$treatment == 1], x, labels = x[1:(length(x) - 1)]), y = 1)
    d1 <- aggregate(y ~ xmin, d1, sum)
    d1$xmin <- as.numeric(as.character(d1$xmin))
    d0 <- data.frame(xmin = cut(data$score[data$treatment == 0], x, labels = x[1:(length(x) - 1)]), y = 1)
    d0 <- aggregate(y ~ xmin, d0, sum)
    d0$xmin <- as.numeric(as.character(d0$xmin))
    d <- data.frame(xmin = c(d1$xmin, d0$xmin), y = c(d1$y, d0$y), treatment = c(
      rep(targetLabel, nrow(d1)),
      rep(comparatorLabel, nrow(d0))
    ))
    d$xmax <- d$xmin + binWidth
    d$treatment <- factor(d$treatment, levels = c(targetLabel, comparatorLabel))
    yAxisScale <- "Number"
    if (type == "histogramProportion") {
      d$y <- d$y / sum(d$y)
      yAxisScale <- "Proportion"
    }
    plot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$xmin)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = 0, ymax = .data$y, color = .data$treatment, group = .data$treatment, fill = .data$treatment)) +
      ggplot2::scale_fill_manual(values = c(
        rgb(0.8, 0, 0, alpha = 0.5),
        rgb(0, 0, 0.8, alpha = 0.5)
      )) +
      ggplot2::scale_color_manual(values = c(
        rgb(0.8, 0, 0, alpha = 0.5),
        rgb(0, 0, 0.8, alpha = 0.5)
      )) +
      ggplot2::scale_x_continuous(label, limits = c(0, 1)) +
      ggplot2::scale_y_continuous(paste(yAxisScale, "of", unitOfAnalysis), limits = c(0, max(d$y) * 1.25)) +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "top")
  }
  if (showAucLabel || showCountsLabel || showEquiposeLabel) {
    labelsLeft <- c()
    labelsRight <- c()
    if (showCountsLabel) {
      labelsLeft <- c(labelsLeft, sprintf("%s: %s %s", targetLabel, format(sum(data$treatment == 1), big.mark = ",", scientific = FALSE), unitOfAnalysis))
      labelsLeft <- c(labelsLeft, sprintf("%s: %s %s", comparatorLabel, format(sum(data$treatment == 0), big.mark = ",", scientific = FALSE), unitOfAnalysis))
    }

    if (showAucLabel) {
      auc <- computePsAuc(data, confidenceIntervals = FALSE)
      labelsRight <- c(labelsRight, sprintf("AUC:\t\t%0.2f", auc))
    }
    if (showEquiposeLabel) {
      if (is.null(data$preferenceScore)) {
        data <- computePreferenceScore(data, unfilteredData)
      }
      equipoise <- computeEquipoise(data = data, equipoiseBounds = equipoiseBounds)
      labelsRight <- c(labelsRight, sprintf("%2.1f%% is in equipoise", equipoise * 100))
    }
    if (length(labelsLeft) > 0) {
      dummy <- data.frame(text = paste(labelsLeft, collapse = "\n"))
      plot <- plot + ggplot2::geom_label(x = 0, y = max(d$y) * 1.24, hjust = "left", vjust = "top", alpha = 0.8, ggplot2::aes(label = text), data = dummy, size = 3.5)
    }
    if (length(labelsRight) > 0) {
      dummy <- data.frame(text = paste(labelsRight, collapse = "\n"))
      plot <- plot + ggplot2::geom_label(x = 1, y = max(d$y) * 1.24, hjust = "right", vjust = "top", alpha = 0.8, ggplot2::aes(label = text), data = dummy, size = 3.5)
    }
  }
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  }
  return(plot)
}

#' Compute the area under the ROC curve
#'
#' @description
#' Compute the area under the ROC curve of the propensity score.
#'
#' @param data                  A data frame with at least the two columns described below
#' @param confidenceIntervals   Compute 95 percent confidence intervals (computationally expensive for
#'                              large data sets)
#' @param maxRows               Maximum number of rows to use. If the number of rows is larger, a random sample
#'                              will be taken. This can increase speed, with minor cost to precision. Set to 0
#'                              to use all data.
#'
#' @details
#' The data frame should have a least the following two columns:
#'
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - propensityScore (numeric): Propensity score.
#'
#' @return
#' A tibble holding the AUC and its 95 percent confidence interval
#'
#' @examples
#' treatment <- rep(0:1, each = 100)
#' propensityScore <- c(rnorm(100, mean = 0.4, sd = 0.25), rnorm(100, mean = 0.6, sd = 0.25))
#' data <- data.frame(treatment = treatment, propensityScore = propensityScore)
#' data <- data[data$propensityScore > 0 & data$propensityScore < 1, ]
#' computePsAuc(data)
#'
#' @export
computePsAuc <- function(data, confidenceIntervals = FALSE, maxRows = 100000) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(data, add = errorMessages)
  checkmate::assertNames(colnames(data), must.include = c("treatment", "propensityScore"), add = errorMessages)
  checkmate::assertLogical(confidenceIntervals, len = 1, add = errorMessages)
  checkmate::assertInt(maxRows, lower = 0, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (nrow(data) > maxRows) {
    message(sprintf("Downsampling study population from %d to %d before computing AUC", nrow(data), maxRows))
    idx <- sample.int(nrow(data), maxRows, replace = FALSE)
    data <- data[idx, ]
  }

  if (confidenceIntervals) {
    aucCi <- aucWithCi(data$propensityScore, data$treatment)
    return(tibble(auc = aucCi[1], aucLb95ci = aucCi[2], aucUb95ci = aucCi[3]))
  } else {
    auc <- aucWithoutCi(data$propensityScore, data$treatment)
    return(auc)
  }
}

#' Trim persons by propensity score
#'
#' @description
#' Use the provided propensity scores to trim subjects with extreme scores.
#'
#' @param population     A data frame with the three columns described below
#' @param trimFraction   This fraction will be removed from each treatment group. In the target
#'                       group, persons with the highest propensity scores will be removed, in the
#'                       comparator group person with the lowest scores will be removed.
#'
#' @details
#' The data frame should have the following three columns:
#'
#' - rowId (numeric): A unique identifier for each row (e.g. the person ID).
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - propensityScore (numeric): Propensity score.
#'
#' @return
#' Returns a tibble with the same three columns as the input.
#'
#' @examples
#' rowId <- 1:2000
#' treatment <- rep(0:1, each = 1000)
#' propensityScore <- c(runif(1000, min = 0, max = 1), runif(1000, min = 0, max = 1))
#' data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
#' result <- trimByPs(data, 0.05)
#'
#' @export
trimByPs <- function(population, trimFraction = 0.05) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(population, add = errorMessages)
  checkmate::assertNames(colnames(population), must.include = c("treatment", "propensityScore"), add = errorMessages)
  checkmate::assertNumber(trimFraction, lower = 0, upper = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  cutoffTarget <- quantile(population$propensityScore[population$treatment == 1], trimFraction)
  cutoffComparator <- quantile(population$propensityScore[population$treatment == 0], 1 - trimFraction)
  result <- population[(population$propensityScore >= cutoffTarget & population$treatment == 1) |
                         (population$propensityScore <= cutoffComparator & population$treatment == 0), ]
  if (!is.null(attr(result, "metaData"))) {
    attr(
      result,
      "metaData"
    )$attrition <- rbind(attr(result, "metaData")$attrition, getCounts(result, paste("Trimmed by PS")))
  }
  ParallelLogger::logDebug("Population size after trimming is ", nrow(result))
  return(result)
}

#' Keep only persons in clinical equipoise
#'
#' @description
#' Use the preference score to trim subjects that are not in clinical equipoise
#'
#' @param population   A data frame with at least the three columns described below.
#' @param bounds       The upper and lower bound on the preference score for keeping persons.
#'
#' @details
#' The data frame should have the following three columns:
#'
#' - rowId (numeric): A unique identifier for each row (e.g. the person ID).
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - propensityScore (numeric): Propensity score.
#'
#' @return
#' Returns a tibble with the same three columns as the input.
#'
#' @examples
#' rowId <- 1:2000
#' treatment <- rep(0:1, each = 1000)
#' propensityScore <- c(runif(1000, min = 0, max = 1), runif(1000, min = 0, max = 1))
#' data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
#' result <- trimByPsToEquipoise(data)
#'
#' @references
#' Walker AM, Patrick AR, Lauer MS, Hornbrook MC, Marin MG, Platt R, Roger VL, Stang P, and
#' Schneeweiss S. (2013) A tool for assessing the feasibility of comparative effectiveness research,
#' Comparative Effective Research, 3, 11-20
#'
#' @export
trimByPsToEquipoise <- function(population, bounds = c(0.3, 0.7)) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(population, add = errorMessages)
  checkmate::assertNames(colnames(population), must.include = c("treatment", "propensityScore"), add = errorMessages)
  checkmate::assertNumeric(bounds, len = 2, lower = 0, upper = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  temp <- computePreferenceScore(population)
  population <- population[temp$preferenceScore >= bounds[1] & temp$preferenceScore <= bounds[2], ]
  if (!is.null(attr(population, "metaData"))) {
    attr(
      population,
      "metaData"
    )$attrition <- rbind(attr(population, "metaData")$attrition, getCounts(population, paste("Trimmed to equipoise")))
  }
  ParallelLogger::logDebug("Population size after trimming is ", nrow(population))
  return(population)
}


#' Remove subjects with a high IPTW
#'
#' @description
#' Remove subjects having a weight higher than the user-specified threshold.
#'
#' @param population   A data frame with at least the two columns described in the details
#' @param maxWeight    The maximum allowed IPTW.
#'
#' @details
#' The data frame should have the following two columns:
#'
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - iptw (numeric): Propensity score.
#'
#' @return
#' Returns a tibble with the same  columns as the input.
#'
#' @examples
#' rowId <- 1:2000
#' treatment <- rep(0:1, each = 1000)
#' iptw <- 1 / c(runif(1000, min = 0, max = 1), runif(1000, min = 0, max = 1))
#' data <- data.frame(rowId = rowId, treatment = treatment, iptw = iptw)
#' result <- trimByIptw(data)
#'
#' @export
trimByIptw <- function(population, maxWeight = 10) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(population, add = errorMessages)
  checkmate::assertNames(colnames(population), must.include = c("treatment", "iptw"), add = errorMessages)
  checkmate::assertNumber(maxWeight, lower = 0, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  population <- population |>
    filter(.data$iptw <= maxWeight)
  if (!is.null(attr(population, "metaData"))) {
    metaData <- attr(population, "metaData")
    metaData$attrition <- bind_rows(
      metaData$attrition,
      getCounts(population, paste("Trimmed by IPTW"))
    )
    attr(population, "metaData") <- metaData
  }
  ParallelLogger::logDebug("Population size after trimming is ", nrow(population))
  return(population)
}

#' Truncate IPTW values
#'
#' @description
#' Set the inverse probability of treatment weights (IPTW) to the user-specified threshold if it exceeds
#' said threshold.
#'
#' @param population   A data frame with at least the two columns described in the details
#' @param maxWeight    The maximum allowed IPTW.
#'
#' @details
#' The data frame should have the following two columns:
#'
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - iptw (numeric): Propensity score.
#'
#' @return
#' Returns a tibble with the same  columns as the input.
#'
#' @examples
#' rowId <- 1:2000
#' treatment <- rep(0:1, each = 1000)
#' iptw <- 1 / c(runif(1000, min = 0, max = 1), runif(1000, min = 0, max = 1))
#' data <- data.frame(rowId = rowId, treatment = treatment, iptw = iptw)
#' result <- truncateIptw(data)
#'
#' @export
truncateIptw <- function(population, maxWeight = 10) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(population, add = errorMessages)
  checkmate::assertNames(colnames(population), must.include = c("treatment", "iptw"), add = errorMessages)
  checkmate::assertNumber(maxWeight, lower = 0, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  nTruncated <- sum(population$iptw > maxWeight)
  message(sprintf("Truncating %s (%0.1f%%) IPTW values", nTruncated, 100 * nTruncated / nrow(population)))
  population <- population |>
    mutate(iptw = ifelse(.data$iptw > maxWeight, maxWeight, .data$iptw))
  return(population)
}

mergeCovariatesWithPs <- function(data, cohortMethodData, covariateIds) {
  covariates <- cohortMethodData$covariates |>
    filter(.data$covariateId %in% covariateIds) |>
    collect()

  for (covariateId in covariateIds) {
    values <- covariates[covariates$covariateId == covariateId, c("rowId", "covariateValue")]
    colnames(values)[colnames(values) == "covariateValue"] <- paste("covariateId", covariateId, sep = "_")
    data <- merge(data, values, all.x = TRUE)
    col <- which(colnames(data) == paste("covariateId", covariateId, sep = "_"))
    data[is.na(data[, col]), col] <- 0
  }
  return(data)
}

logit <- function(p) {
  log(p / (1 - p))
}

#' Match persons by propensity score
#'
#' @description
#' Use the provided propensity scores to match target to comparator persons.
#'
#' @param population              A data frame with the three columns described below.
#' @param caliper                 The caliper for matching. A caliper is the distance which is
#'                                acceptable for any match. Observations which are outside of the
#'                                caliper are dropped. A caliper of 0 means no caliper is used.
#' @param caliperScale            The scale on which the caliper is defined. Three scales are supported:
#'                                `caliperScale = 'propensity score'`, `caliperScale =
#'                                'standardized'`, or `caliperScale = 'standardized logit'`.
#'                                On the standardized scale, the caliper is interpreted in standard
#'                                deviations of the propensity score distribution. 'standardized logit'
#'                                is similar, except that the propensity score is transformed to the logit
#'                                scale because the PS is more likely to be normally distributed on that scale
#'                                (Austin, 2011).
#' @param maxRatio                The maximum number of persons in the comparator arm to be matched to
#'                                each person in the treatment arm. A maxRatio of 0 means no maximum:
#'                                all comparators will be assigned to a target person.
#' @param allowReverseMatch       Allows n-to-1 matching if target arm is larger
#' @param stratificationColumns   Names or numbers of one or more columns in the `data` data.frame
#'                                on which subjects should be stratified prior to matching. No persons
#'                                will be matched with persons outside of the strata identified by the
#'                                values in these columns.
#'
#' @details
#' The data frame should have the following three columns:
#'
#' - rowId (numeric): A unique identifier for each row (e.g. the person ID).
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - propensityScore (numeric): Propensity score.
#'
#' The default caliper (0.2 on the standardized logit scale) is the one recommended by Austin (2011).
#'
#' @return
#' Returns a date frame with the same columns as the input data plus one extra column: stratumId. Any
#' rows that could not be matched are removed
#'
#' @examples
#' rowId <- 1:5
#' treatment <- c(1, 0, 1, 0, 1)
#' propensityScore <- c(0, 0.1, 0.3, 0.4, 1)
#' age_group <- c(1, 1, 1, 1, 1)
#' data <- data.frame(
#'   rowId = rowId,
#'   treatment = treatment,
#'   propensityScore = propensityScore,
#'   age_group = age_group
#' )
#' result <- matchOnPs(data, caliper = 0, maxRatio = 1, stratificationColumns = "age_group")
#'
#' @references
#' Rassen JA, Shelat AA, Myers J, Glynn RJ, Rothman KJ, Schneeweiss S. (2012) One-to-many propensity
#' score matching in cohort studies, Pharmacoepidemiology and Drug Safety, May, 21 Suppl 2:69-80.
#'
#' Austin, PC. (2011) Optimal caliper widths for propensity-score matching when estimating differences in
#' means and differences in proportions in observational studies, Pharmaceutical statistics, March, 10(2):150-161.
#'
#' @export
matchOnPs <- function(population,
                      caliper = 0.2,
                      caliperScale = "standardized logit",
                      maxRatio = 1,
                      allowReverseMatch = FALSE,
                      stratificationColumns = c()) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(population, add = errorMessages)
  checkmate::assertNames(colnames(population), must.include = c("rowId", "treatment", "propensityScore"), add = errorMessages)
  checkmate::assertNumber(caliper, lower = 0, add = errorMessages)
  checkmate::assertChoice(caliperScale, c("standardized", "propensity score", "standardized logit"), add = errorMessages)
  checkmate::assertInt(maxRatio, lower = 0, add = errorMessages)
  checkmate::assertLogical(allowReverseMatch, len = 1, add = errorMessages)
  checkmate::assertCharacter(stratificationColumns, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  reverseTreatment <- (allowReverseMatch && sum(population$treatment == 1) > sum(population$treatment == 0))
  if (reverseTreatment) {
    population$treatment <- 1 - population$treatment
  }

  population <- population[order(population$propensityScore), ]
  propensityScore <- population$propensityScore

  if (caliper <= 0 || nrow(population) == 0 || min(population$propensityScore) == max(population$propensityScore)) {
    caliper <- 9999
  } else if (caliperScale == "standardized") {
    caliper <- caliper * sd(population$propensityScore)
  } else if (caliperScale == "standardized logit") {
    propensityScore <- logit(propensityScore)
    caliper <- caliper * sd(propensityScore[is.finite(propensityScore)])
  }
  if (maxRatio == 0) {
    maxRatio <- 999
  }
  if (length(stratificationColumns) == 0) {
    result <- matchPsInternal(
      propensityScore,
      population$treatment,
      maxRatio,
      caliper
    )
    result <- as_tibble(result)
    population$stratumId <- result$stratumId
    population <- population[population$stratumId != -1, ]
  } else {
    f <- function(subset, maxRatio, caliper) {
      subResult <- matchPsInternal(
        subset$propensityScore,
        subset$treatment,
        maxRatio,
        caliper
      )
      subResult <- as_tibble(subResult)
      subset$stratumId <- subResult$stratumId
      subset <- subset[subset$stratumId != -1, ]
      return(subset)
    }
    results <- plyr::dlply(
      .data = population,
      .variables = stratificationColumns,
      .drop = TRUE,
      .fun = f,
      maxRatio = maxRatio,
      caliper = caliper
    )
    if (length(results) == 0) {
      result <- population |>
        mutate(stratumId = 0)
    } else {
      maxStratumId <- 0
      for (i in seq_len(length(results))) {
        if (nrow(results[[i]]) > 0) {
          if (maxStratumId != 0) {
            results[[i]]$stratumId <- results[[i]]$stratumId + maxStratumId + 1
          }
          maxStratumId <- max(results[[i]]$stratumId)
        }
      }
      result <- do.call(rbind, results)
    }
    attr(result, "metaData") <- attr(population, "metaData")
    population <- result
  }
  if (reverseTreatment) {
    population$treatment <- 1 - population$treatment
  }

  if (!is.null(attr(population, "metaData"))) {
    attr(population, "metaData")$attrition <- rbind(
      attr(population, "metaData")$attrition,
      getCounts(population, paste("Matched on propensity score"))
    )
    if (reverseTreatment) {
      attr(population, "metaData")$targetEstimator <- "atu"
    } else {
      attr(population, "metaData")$targetEstimator <- "att"
    }
  }
  ParallelLogger::logDebug("Population size after matching is ", nrow(population))
  return(population)
}

#' Match by propensity score as well as other covariates
#'
#' @description
#' Use the provided propensity scores and a set of covariates to match
#' target to comparator persons.
#'
#' @param population         A data frame with the three columns described below.
#' @param caliper            The caliper for matching. A caliper is the distance which is acceptable
#'                           for any match. Observations which are outside of the caliper are dropped.
#'                           A caliper of 0 means no caliper is used.
#' @param caliperScale            The scale on which the caliper is defined. Three scales are supported:
#'                                `caliperScale = 'propensity score'`, `caliperScale =
#'                                'standardized'`, or `caliperScale = 'standardized logit'`.
#'                                On the standardized scale, the caliper is interpreted in standard
#'                                deviations of the propensity score distribution. 'standardized logit'
#'                                is similar, except that the propensity score is transformed to the logit
#'                                scale because the PS is more likely to be normally distributed on that scale
#'                                (Austin, 2011).
#' @param maxRatio           The maximum number of persons in the comparator arm to be matched to each
#'                           person in the treatment arm. A maxRatio of 0 means no maximum: all
#'                           comparators will be assigned to a target person.
#' @param allowReverseMatch  Allows n-to-1 matching if target arm is larger
#' @param covariateIds       One or more covariate IDs in the `cohortMethodData` object on which
#'                           subjects should be also matched.
#'
#' @template CohortMethodData
#'
#' @details
#' The data frame should have the following three columns:
#'
#' - rowId (numeric): A unique identifier for each row (e.g. the person ID).
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - propensityScore (numeric): Propensity score.
#'
#' The default caliper (0.2 on the standardized logit scale) is the one recommended by Austin (2011).
#'
#' @return
#' Returns a tibble with the same columns as the input data plus one extra column: stratumId. Any
#' rows that could not be matched are removed
#'
#' @references
#' Rassen JA, Shelat AA, Myers J, Glynn RJ, Rothman KJ, Schneeweiss S. (2012) One-to-many propensity
#' score matching in cohort studies, Pharmacoepidemiology and Drug Safety, May, 21 Suppl 2:69-80.
#'
#' Austin, PC. (2011) Optimal caliper widths for propensity-score matching when estimating differences in
#' means and differences in proportions in observational studies, Pharmaceutical statistics, March, 10(2):150-161.
#'
#' @export
matchOnPsAndCovariates <- function(population,
                                   caliper = 0.2,
                                   caliperScale = "standardized logit",
                                   maxRatio = 1,
                                   allowReverseMatch = FALSE,
                                   cohortMethodData,
                                   covariateIds) {
  errorMessages <- checkmate::makeAssertCollection()
  .assertCovariateId(covariateIds, min.len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  population <- mergeCovariatesWithPs(population, cohortMethodData, covariateIds)
  stratificationColumns <- colnames(population)[colnames(population) %in% paste("covariateId",
                                                                                covariateIds,
                                                                                sep = "_"
  )]
  return(matchOnPs(population, caliper, caliperScale, maxRatio, allowReverseMatch, stratificationColumns))
}

#' Stratify persons by propensity score
#'
#' @description
#' Use the provided propensity scores to stratify persons. Additional
#' stratification variables for stratifications can also be used.
#'
#' @param population              A data frame with the three columns described below
#' @param numberOfStrata          How many strata? The boundaries of the strata are automatically
#'                                defined to contain equal numbers of target persons.
#' @param stratificationColumns   Names of one or more columns in the `data` data.frame on which
#'                                subjects should also be stratified in addition to stratification on
#'                                propensity score.
#' @param baseSelection           What is the base selection of subjects where the strata bounds are
#'                                to be determined? Strata are defined as equally-sized strata inside
#'                                this selection. Possible values are "all", "target", and "comparator".
#'
#' @details
#' The data frame should have the following three columns:
#'
#' - rowId (numeric): A unique identifier for each row (e.g. the person ID).
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - propensityScore (numeric): Propensity score.
#'
#' @return
#' Returns a tibble with the same columns as the input data plus one extra column: stratumId.
#'
#' @examples
#' rowId <- 1:200
#' treatment <- rep(0:1, each = 100)
#' propensityScore <- c(runif(100, min = 0, max = 1), runif(100, min = 0, max = 1))
#' data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
#' result <- stratifyByPs(data, 5)
#'
#' @export
stratifyByPs <- function(population, numberOfStrata = 5, stratificationColumns = c(), baseSelection = "all") {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(population, add = errorMessages)
  checkmate::assertNames(colnames(population), must.include = c("treatment", "propensityScore"), add = errorMessages)
  checkmate::assertInt(numberOfStrata, lower = 1, add = errorMessages)
  checkmate::assertCharacter(stratificationColumns, null.ok = TRUE, add = errorMessages)
  checkmate::assertChoice(baseSelection, c("all", "target", "comparator"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (nrow(population) == 0) {
    return(population)
  }
  baseSelection <- tolower(baseSelection)
  if (baseSelection == "all") {
    basePop <- population$propensityScore
    targetEstimator <- "ate"
  } else if (baseSelection == "target") {
    basePop <- population$propensityScore[population$treatment == 1]
    targetEstimator <- "att"
  } else if (baseSelection == "comparator") {
    basePop <- population$propensityScore[population$treatment == 0]
    targetEstimator <- "atu"
  } else {
    stop(paste0("Unknown base selection: '", baseSelection, "'. Please choose 'all', 'target', or 'comparator'"))
  }
  if (!is.null(attr(population, "metaData"))) {
    attr(population, "metaData")$targetEstimator <- targetEstimator
  }
  if (length(basePop) == 0) {
    psStrata <- c()
  } else {
    psStrata <- unique(quantile(basePop, (1:(numberOfStrata - 1)) / numberOfStrata))
  }
  attr(population, "strata") <- psStrata
  breaks <- unique(c(0, psStrata, 1))
  breaks[1] <- -1 # So 0 is included in the left-most stratum
  if (length(breaks) - 1 < numberOfStrata) {
    warning("Specified ", numberOfStrata, " strata, but only ", length(breaks) - 1, " could be created")
  }
  if (length(stratificationColumns) == 0) {
    if (length(breaks) - 1 == 1) {
      population$stratumId <- rep(1, nrow(population))
    } else {
      population$stratumId <- as.integer(as.character(cut(population$propensityScore,
                                                          breaks = breaks,
                                                          labels = 1:(length(breaks) - 1)
      )))
    }
    return(population)
  } else {
    f <- function(subset, psStrata, numberOfStrata) {
      if (length(breaks) - 1 == 1) {
        subset$stratumId <- rep(1, nrow(subset))
      } else {
        subset$stratumId <- as.integer(as.character(cut(subset$propensityScore,
                                                        breaks = breaks,
                                                        labels = 1:(length(breaks) - 1)
        )))
      }
      return(subset)
    }

    results <- plyr::dlply(
      .data = population,
      .variables = stratificationColumns,
      .drop = TRUE,
      .fun = f,
      psStrata = psStrata,
      numberOfStrata = numberOfStrata
    )
    maxStratumId <- 0
    for (i in 1:length(results)) {
      if (nrow(results[[i]]) > 0) {
        if (maxStratumId != 0) {
          results[[i]]$stratumId <- results[[i]]$stratumId + maxStratumId + 1
        }
        maxStratumId <- max(results[[i]]$stratumId)
      }
    }
    result <- bind_rows(results)
    attr(result, "metaData") <- attr(population, "metaData")
    return(result)
  }
}

#' Stratify persons by propensity score and other covariates
#'
#' @description
#' Use the provided propensity scores and covariates to stratify
#' persons.
#'
#' @param population         A data frame with the three columns described below
#' @param numberOfStrata     Into how many strata should the propensity score be divided? The
#'                           boundaries of the strata are automatically defined to contain equal
#'                           numbers of target persons.
#' @param baseSelection      What is the base selection of subjects where the strata bounds are
#'                           to be determined? Strata are defined as equally-sized strata inside
#'                           this selection. Possible values are "all", "target", and "comparator".
#' @param covariateIds       One or more covariate IDs in the `cohortMethodData` object on which
#'                           subjects should also be stratified.
#'
#' @template CohortMethodData
#'
#' @details
#' The data frame should have the following three columns:
#'
#' - rowId (numeric): A unique identifier for each row (e.g. the person ID).
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - propensityScore (numeric): Propensity score.
#'
#' @return
#' Returns a date frame with the same columns as the input population plus one extra column:
#' stratumId.
#'
#' @export
stratifyByPsAndCovariates <- function(population,
                                      numberOfStrata = 5,
                                      baseSelection = "all",
                                      cohortMethodData,
                                      covariateIds) {
  errorMessages <- checkmate::makeAssertCollection()
  .assertCovariateId(covariateIds, min.len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  population <- mergeCovariatesWithPs(population, cohortMethodData, covariateIds)
  stratificationColumns <- colnames(population)[colnames(population) %in% paste("covariateId",
                                                                                covariateIds,
                                                                                sep = "_"
  )]
  return(stratifyByPs(
    population = population,
    numberOfStrata = numberOfStrata,
    stratificationColumns = stratificationColumns,
    baseSelection = baseSelection
  ))
}

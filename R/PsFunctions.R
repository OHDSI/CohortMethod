# @file PsFunctions.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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
#' \code{createPs} creates propensity scores using a regularized logistic regression.
#'
#' @param cohortMethodData         An object of type \code{cohortMethodData} as generated using
#'                                 \code{getDbCohortMethodData}.
#' @param population               A data frame describing the population. This should at least have a
#'                                 'rowId' column corresponding to the rowId column in the
#'                                 \code{cohortMethodData} covariates object and a 'treatment' column.
#'                                 If population is not specified, the full population in the
#'                                 \code{cohortMethodData} will be used.
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
#' @param stopOnError              If an error occurrs, should the function stop? Else, the two cohorts
#'                                 will be assumed to be perfectly separable.
#' @param prior                    The prior used to fit the model. See
#'                                 \code{\link[Cyclops]{createPrior}} for details.
#' @param control                  The control object used to control the cross-validation used to
#'                                 determine the hyperparameters of the prior (if applicable). See
#'                                 \code{\link[Cyclops]{createControl}} for details.
#'
#' @details
#' \code{createPs} creates propensity scores using a regularized logistic regression.
#'
#' @examples
#' data(cohortMethodDataSimulationProfile)
#' cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = 1000)
#' ps <- createPs(cohortMethodData)
#'
#' @export
createPs <- function(cohortMethodData,
                     population,
                     excludeCovariateIds = c(),
                     includeCovariateIds = c(),
                     maxCohortSizeForFitting = 250000,
                     errorOnHighCorrelation = TRUE,
                     stopOnError = TRUE,
                     prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
                     control = createControl(noiseLevel = "silent",
                                             cvType = "auto",
                                             seed = 1,
                                             tolerance = 2e-07,
                                             cvRepetitions = 10,
                                             startingVariance = 0.01)) {
  if (missing(population))
    population <- cohortMethodData$cohorts
  if (!("rowId" %in% colnames(population)))
    stop("Missing column rowId in population")
  if (!("treatment" %in% colnames(population)))
    stop("Missing column treatment in population")

  start <- Sys.time()
  if (is.data.frame(cohortMethodData$covariates)) {
    error <- "No covariate data, so cannot fit model"
    sampled <- FALSE
    ref <- NULL
  } else {
    covariates <- FeatureExtraction::filterByRowId(cohortMethodData$covariates, population$rowId)
    if (length(includeCovariateIds) != 0) {
      idx <- !is.na(ffbase::ffmatch(covariates$covariateId, ff::as.ff(includeCovariateIds)))
      covariates <- covariates[ffbase::ffwhich(idx, idx == TRUE), ]
    }
    if (length(excludeCovariateIds) != 0) {
      idx <- is.na(ffbase::ffmatch(covariates$covariateId, ff::as.ff(excludeCovariateIds)))
      covariates <- covariates[ffbase::ffwhich(idx, idx == TRUE), ]
    }
    covariateData <- FeatureExtraction::tidyCovariateData(covariates = covariates,
                                                          covariateRef = cohortMethodData$covariateRef,
                                                          populationSize = nrow(population),
                                                          minFraction = 0.001,
                                                          normalize = TRUE,
                                                          removeRedundancy = TRUE)
    covariates <- covariateData$covariates
    attr(population, "metaData")$deletedInfrequentCovariateIds <- covariateData$metaData$deletedInfrequentCovariateIds
    attr(population, "metaData")$deletedRedundantCovariateIds <- covariateData$metaData$deletedRedundantCovariateIds
    sampled <- FALSE
    if (maxCohortSizeForFitting != 0) {
      set.seed(0)
      targetRowIds <- population$rowId[population$treatment == 1]
      if (length(targetRowIds) > maxCohortSizeForFitting) {
        ParallelLogger::logInfo(paste0("Downsampling target cohort from ", length(targetRowIds), " to ", maxCohortSizeForFitting, " before fitting"))
        targetRowIds <- sample(targetRowIds, size = maxCohortSizeForFitting, replace = FALSE)
        sampled <- TRUE
      }
      comparatorRowIds <- population$rowId[population$treatment == 0]
      if (length(comparatorRowIds) > maxCohortSizeForFitting) {
        ParallelLogger::logInfo(paste0("Downsampling comparator cohort from ", length(comparatorRowIds), " to ", maxCohortSizeForFitting, " before fitting"))
        comparatorRowIds <- sample(comparatorRowIds, size = maxCohortSizeForFitting, replace = FALSE)
        sampled <- TRUE
      }
      if (sampled) {
        fullPopulation <- population
        fullCovariates <- covariates
        population <- population[population$rowId %in% c(targetRowIds, comparatorRowIds), ]
        covariates <- FeatureExtraction::filterByRowId(covariates, population$rowId)
      }
    }
    if (!Cyclops::isSorted(population, "rowId")) {
      population <- population[order(population$rowId), ]
    }
    outcomes <- ff::as.ffdf(population)
    colnames(outcomes)[colnames(outcomes) == "treatment"] <- "y"
    floatingPoint <- getOption("floatingPoint")
    if (is.null(floatingPoint)) {
      floatingPoint <- 64
    } else {
       ParallelLogger::logInfo("Cyclops using precision of ", floatingPoint)
    }
    cyclopsData <- convertToCyclopsData(outcomes, covariates, modelType = "lr", quiet = TRUE, floatingPoint = floatingPoint)
    ff::close.ffdf(outcomes)
    ff::close.ffdf(covariates)
    rm(outcomes)
    rm(covariates)
    rm(covariateData)
    error <- NULL
    ref <- NULL
    if (errorOnHighCorrelation) {
      suspect <- Cyclops::getUnivariableCorrelation(cyclopsData, threshold = 0.5)
      suspect <- suspect[!is.na(suspect)]
      if (length(suspect) != 0) {
        covariateIds <- as.numeric(names(suspect))
        idx <- !is.na(ffbase::ffmatch(cohortMethodData$covariateRef$covariateId,
                                      ff::as.ff(covariateIds)))
        ref <- ff::as.ram(cohortMethodData$covariateRef[ffbase::ffwhich(idx, idx == TRUE), ])
        ParallelLogger::logInfo("High correlation between covariate(s) and treatment detected:")
        ParallelLogger::logInfo(paste(colnames(ref), collapse = "\t"))
        ref$covariateName <- as.character(ref$covariateName)
        for (i in 1:nrow(ref))
          ParallelLogger::logInfo(paste(ref[i, ], collapse = "\t"))
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
    cyclopsFit <- tryCatch({
      Cyclops::fitCyclopsModel(cyclopsData, prior = prior, control = control)
    }, error = function(e) {
      e$message
    })
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
      y.bar <- mean(population$treatment)
      y.odds <- y.bar/(1 - y.bar)
      y.bar.new <- mean(fullPopulation$treatment)
      y.odds.new <- y.bar.new/(1 - y.bar.new)
      delta <- log(y.odds) - log(y.odds.new)
      cfs[1] <- cfs[1] - delta  # Equation (7) in King and Zeng (2001)
      cyclopsFit$estimation$estimate[1] <- cfs[1]
      fullOutcomes <- ff::as.ffdf(fullPopulation)
      population <- fullPopulation
      population$propensityScore <- predict(cyclopsFit, newOutcomes = fullOutcomes, newCovariates = fullCovariates)
      ff::close.ffdf(fullOutcomes)
      ff::close.ffdf(fullCovariates)
      rm(fullOutcomes)
      rm(fullCovariates)
    } else {
      population$propensityScore <- predict(cyclopsFit)
    }
    attr(population, "metaData")$psModelCoef <- coef(cyclopsFit)
    attr(population, "metaData")$psModelPriorVariance <- cyclopsFit$variance[1]
  } else {
    if (sampled) {
      population <- fullPopulation
      ff::close.ffdf(fullCovariates)
      rm(fullCovariates)
    }
    population$propensityScore <- population$treatment
    attr(population, "metaData")$psError <- error
    if (!is.null(ref)) {
      attr(population, "metaData")$psHighCorrelation <- ref
    }
  }
  population <- computePreferenceScore(population)
  delta <- Sys.time() - start
  ParallelLogger::logDebug("Propensity model fitting finished with status ", error)
  ParallelLogger::logInfo(paste("Creating propensity scores took", signif(delta, 3), attr(delta, "units")))
  return(population)
}

#' Get the propensity model
#'
#' @description
#' \code{getPsModel} shows the propensity score model
#'
#' @param propensityScore    The propensity scores as generated using the \code{createPs} function.
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
getPsModel <- function(propensityScore, cohortMethodData) {
  coefficients <- attr(propensityScore, "metaData")$psModelCoef
  result <- data.frame(coefficient = coefficients[1],
                       covariateId = NA,
                       covariateName = "(Intercept)")
  coefficients <- coefficients[2:length(coefficients)]
  coefficients <- coefficients[coefficients != 0]
  if (length(coefficients) != 0) {
    coefficients <- data.frame(coefficient = coefficients,
                               covariateId = as.numeric(attr(coefficients, "names")))
    coefficients <- merge(ff::as.ffdf(coefficients), cohortMethodData$covariateRef)
    coefficients <- ff::as.ram(coefficients[, c("coefficient", "covariateId", "covariateName")])
    result <- rbind(result, coefficients)
    result <- result[order(-abs(result$coefficient)), ]
  }
  return(result)
}

computePreferenceScore <- function(data, unfilteredData = NULL) {
  if (is.null(unfilteredData)) {
    proportion <- sum(data$treatment)/nrow(data)
  } else {
    proportion <- sum(unfilteredData$treatment)/nrow(unfilteredData)
  }
  propensityScore <- data$propensityScore
  propensityScore[propensityScore > 0.9999999] <- 0.9999999
  x <- exp(log(propensityScore/(1 - propensityScore)) - log(proportion/(1 - proportion)))
  data$preferenceScore <- x/(x + 1)
  return(data)
}

#' Plot the propensity score distribution
#'
#' @description
#' \code{plotPs} shows the propensity (or preference) score distribution
#'
#' @param data              A data frame with at least the two columns described below
#' @param unfilteredData    To be used when computing preference scores on data from which subjects
#'                          have already been removed, e.g. through trimming and/or matching. This data
#'                          frame should have the same structure as \code{data}.
#' @param scale             The scale of the graph. Two scales are supported: \code{ scale =
#'                          'propensity'} or \code{scale = 'preference'}. The preference score scale is
#'                          defined by Walker et al (2013).
#' @param type              Type of plot. Four possible values: \code{type = 'density'} or \code{type =
#'                          'histogram'} or \code{type = 'histogramCount'}
#'                          or \code{type = 'histogramProportion'}. 'histogram' defaults to 'histogramCount'.
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
#'                          See the function \code{ggsave} in the ggplot2 package for supported file
#'                          formats.
#'
#' @details
#' The data frame should have a least the following two columns: \tabular{lll}{ \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the target (1) or comparator\cr \tab
#' \tab (0) group \cr \verb{propensityScore} \tab(numeric) \tab Propensity score \cr }
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
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
                   equipoiseBounds = c(0.25, 0.75),
                   unitOfAnalysis = "subjects",
                   title = NULL,
                   fileName = NULL) {
  if (!("treatment" %in% colnames(data)))
    stop("Missing column treatment in data")
  if (!("propensityScore" %in% colnames(data)))
    stop("Missing column propensityScore in data")
  if (!is.null(unfilteredData)) {
    if (!("treatment" %in% colnames(unfilteredData)))
      stop("Missing column treatment in unfilteredData")
    if (!("propensityScore" %in% colnames(unfilteredData)))
      stop("Missing column propensityScore in unfilteredData")
  }
  if (type != "density" && type != "histogram" && type != "histogramCount" && type != "histogramProportion")
    stop(paste("Unknown type '", type, "', please choose either 'density', 'histogram', 'histogramCount', or 'histogramProportion'"),
         sep = "")
  if (type == "histogram")
    type <- "histogramCount"
  if (scale != "propensity" && scale != "preference")
    stop(paste("Unknown scale '", scale, "', please choose either 'propensity' or 'preference'"),
         sep = "")
  targetLabel <- as.character(targetLabel)
  comparatorLabel <- as.character(comparatorLabel)

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
    d <- data.frame(x = c(d1$x, d0$x), y = c(d1$y, d0$y), treatment = c(rep(as.character(targetLabel), length(d1$x)),
                                                                        rep(as.character(comparatorLabel), length(d0$x))))
    d$treatment <- factor(d$treatment, levels = c(targetLabel, comparatorLabel))
    plot <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_density(stat = "identity", ggplot2::aes(color = treatment, group = treatment, fill = treatment)) +
      ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                            rgb(0, 0, 0.8, alpha = 0.5))) +
      ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                             rgb(0, 0, 0.8, alpha = 0.5))) +
      ggplot2::scale_x_continuous(label, limits = c(0, 1)) +
      ggplot2::scale_y_continuous("Density", limits = c(0, max(d$y)*yMultiplier)) +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     legend.position = "top",
                     legend.text = ggplot2::element_text(margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm")))
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
    d1 <- aggregate(y ~   xmin, d1, sum)
    d1$xmin <- as.numeric(as.character(d1$xmin))
    d0 <- data.frame(xmin = cut(data$score[data$treatment == 0], x, labels = x[1:(length(x) - 1)]), y = 1)
    d0 <- aggregate(y ~   xmin, d0, sum)
    d0$xmin <- as.numeric(as.character(d0$xmin))
    d <- data.frame(xmin = c(d1$xmin, d0$xmin), y = c(d1$y, d0$y), treatment = c(rep(as.character(targetLabel), nrow(d1)),
                                                                                 rep(as.character(comparatorLabel), nrow(d0))))
    d$xmax <- d$xmin + binWidth
    d$treatment <- factor(d$treatment, levels = c(targetLabel, comparatorLabel))
    yAxisScale <- "Number"
    if (type == "histogramProportion") {
      d$y <- d$y / sum(d$y)
      yAxisScale <- "Proportion"
    }
    plot <- ggplot2::ggplot(d, ggplot2::aes(x = xmin)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = y, color = treatment, group = treatment, fill = treatment)) +
      ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                            rgb(0, 0, 0.8, alpha = 0.5))) +
      ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                             rgb(0, 0, 0.8, alpha = 0.5))) +
      ggplot2::scale_x_continuous(label, limits = c(0, 1)) +
      ggplot2::scale_y_continuous(paste(yAxisScale, "of", unitOfAnalysis), limits = c(0, max(d$y)*1.25)) +
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
      equipoise <- mean(data$preferenceScore >= equipoiseBounds[1] & data$preferenceScore <= equipoiseBounds[2])
      labelsRight <- c(labelsRight, sprintf("%2.1f%% is in equipoise", equipoise*100))
    }
    # maxY <- ggplot2::ggplot_build(plot)$layout$panel_ranges[[1]]$y.range[2]
    if (length(labelsLeft) > 0) {
      dummy <- data.frame(text = paste(labelsLeft, collapse = "\n"))
      plot <- plot + ggplot2::geom_label(x = 0, y = max(d$y) * 1.24, hjust = "left", vjust = "top", alpha = 0.8, ggplot2::aes(label = text), data = dummy, size = 3.5)
    }
    if (length(labelsRight) > 0) {
      dummy <- data.frame(text = paste(labelsRight, collapse = "\n"))
      plot <- plot + ggplot2::geom_label(x = 1, y =  max(d$y) * 1.24, hjust = "right", vjust = "top", alpha = 0.8, ggplot2::aes(label = text), data = dummy, size = 3.5)
    }
  }
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}

#' Compute the area under the ROC curve
#'
#' @description
#' \code{computePsAuc} computes the area under the ROC curve of the propensity score
#'
#' @param data                  A data frame with at least the two columns described below
#' @param confidenceIntervals   Compute 95 percent confidence intervals (computationally expensive for
#'                              large data sets)
#' @details
#' The data frame should have a least the following two columns: \tabular{lll}{ \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the target (1) or comparator\cr \tab
#' \tab (0) group \cr \verb{propensityScore} \tab(numeric) \tab Propensity score \cr }
#'
#' @return
#' A data frame holding the AUC and its 95 percent confidence interval
#'
#' @examples
#' treatment <- rep(0:1, each = 100)
#' propensityScore <- c(rnorm(100, mean = 0.4, sd = 0.25), rnorm(100, mean = 0.6, sd = 0.25))
#' data <- data.frame(treatment = treatment, propensityScore = propensityScore)
#' data <- data[data$propensityScore > 0 & data$propensityScore < 1, ]
#' computePsAuc(data)
#'
#' @export
computePsAuc <- function(data, confidenceIntervals = FALSE) {
  if (!("treatment" %in% colnames(data)))
    stop("Missing column treatment in data")
  if (!("propensityScore" %in% colnames(data)))
    stop("Missing column propensityScore in data")

  if (confidenceIntervals) {
    auc <- aucWithCi(data$propensityScore, data$treatment)
    return(data.frame(auc = auc[1], auc_lb95ci = auc[2], auc_lb95ci = auc[3]))
  } else {
    auc <- aucWithoutCi(data$propensityScore, data$treatment)
    return(auc)
  }
}

#' Trim persons by propensity score
#'
#' @description
#' \code{trimByPs} uses the provided propensity scores to trim subjects with extreme scores.
#'
#' @param population     A data frame with the three columns described below
#' @param trimFraction   This fraction will be removed from each treatment group. In the target
#'                       group, persons with the highest propensity scores will be removed, in the
#'                       comparator group person with the lowest scores will be removed.
#'
#' @details
#' The data frame should have the following three columns: \tabular{lll}{ \verb{rowId} \tab(numeric)
#' \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment} \tab(integer) \tab
#' Column indicating whether the person is in the target (1) or comparator\cr \tab \tab (0) group \cr
#' \verb{propensityScore} \tab(numeric) \tab Propensity score \cr }
#'
#' @return
#' Returns a date frame with the same three columns as the input.
#' @examples
#' rowId <- 1:2000
#' treatment <- rep(0:1, each = 1000)
#' propensityScore <- c(runif(1000, min = 0, max = 1), runif(1000, min = 0, max = 1))
#' data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
#' result <- trimByPs(data, 0.05)
#'
#' @export
trimByPs <- function(population, trimFraction = 0.05) {
  if (!("rowId" %in% colnames(population)))
    stop("Missing column rowId in population")
  if (!("treatment" %in% colnames(population)))
    stop("Missing column treatment in population")
  if (!("propensityScore" %in% colnames(population)))
    stop("Missing column propensityScore in population")
  ParallelLogger::logTrace("Trimming based on propensity score")
  cutoffTarget <- quantile(population$propensityScore[population$treatment == 1], trimFraction)
  cutoffComparator <- quantile(population$propensityScore[population$treatment == 0], 1 - trimFraction)
  result <- population[(population$propensityScore >= cutoffTarget & population$treatment == 1) |
                         (population$propensityScore <= cutoffComparator & population$treatment == 0), ]
  if (!is.null(attr(result, "metaData"))) {
    attr(result,
         "metaData")$attrition <- rbind(attr(result, "metaData")$attrition, getCounts(result, paste("Trimmed by PS")))
  }
  ParallelLogger::logDebug("Population size after trimming is ", nrow(result))
  return(result)
}

#' Keep only persons in clinical equipoise
#'
#' @description
#' \code{trimByPsToEquipoise} uses the preference score to trim subjects that are not in clinical
#' equipoise
#'
#' @param population   A data frame with at least the three columns described below
#' @param bounds       The upper and lower bound on the preference score for keeping persons
#'
#' @details
#' The data frame should have the following three columns: \tabular{lll}{ \verb{rowId} \tab(numeric)
#' \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment} \tab(integer) \tab
#' Column indicating whether the person is in the target (1) or comparator\cr \tab \tab (0) group \cr
#' \verb{propensityScore} \tab(numeric) \tab Propensity score \cr }
#'
#' @return
#' Returns a date frame with the same three columns as the input.
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
trimByPsToEquipoise <- function(population, bounds = c(0.25, 0.75)) {
  if (!("rowId" %in% colnames(population)))
    stop("Missing column rowId in population")
  if (!("treatment" %in% colnames(population)))
    stop("Missing column treatment in population")
  if (!("propensityScore" %in% colnames(population)))
    stop("Missing column propensityScore in population")
  ParallelLogger::logTrace("Trimming to equipoise")
  temp <- computePreferenceScore(population)
  population <- population[temp$preferenceScore >= bounds[1] & temp$preferenceScore <= bounds[2], ]
  if (!is.null(attr(population, "metaData"))) {
    attr(population,
         "metaData")$attrition <- rbind(attr(population, "metaData")$attrition, getCounts(population, paste("Trimmed to equipoise")))
  }
  ParallelLogger::logDebug("Population size after trimming is ", nrow(population))
  return(population)
}

mergeCovariatesWithPs <- function(data, cohortMethodData, covariateIds) {
  for (covariateId in covariateIds) {
    t <- cohortMethodData$covariates$covariateId == covariateId
    if (ffbase::any.ff(t)) {
      values <- ff::as.ram(cohortMethodData$covariates[ffbase::ffwhich(t, t == TRUE), c(1, 3)])
      colnames(values)[colnames(values) == "covariateValue"] <- paste("covariateId", covariateId, sep = "_")
      data <- merge(data, values, all.x = TRUE)
      col <- which(colnames(data) == paste("covariateId", covariateId, sep = "_"))
      data[is.na(data[, col]), col] <- 0
    } else {
      warning(paste("Not matching on covariate", covariateId, "because value is always 0"))
    }
  }
  return(data)
}

logit <- function(p){
  log(p / (1 - p))
}

#' Match persons by propensity score
#'
#' @description
#' \code{matchOnPs} uses the provided propensity scores to match target to comparator persons.
#'
#' @param population              A data frame with the three columns described below.
#' @param caliper                 The caliper for matching. A caliper is the distance which is
#'                                acceptable for any match. Observations which are outside of the
#'                                caliper are dropped. A caliper of 0 means no caliper is used.
#' @param caliperScale            The scale on which the caliper is defined. Three scales are supported:
#'                                \cr\code{caliperScale = 'propensity score'}, \code{caliperScale =
#'                                'standardized'}, or \cr\code{caliperScale = 'standardized logit'}.
#'                                On the standardized scale, the caliper is interpreted in standard
#'                                deviations of the propensity score distribution. 'standardized logit'
#'                                is similar, except that the propensity score is transformed to the logit
#'                                scale because the PS is more likely to be normally distributed on that scale
#'                                (Austin, 2011).
#' @param maxRatio                The maximum number of persons int the comparator arm to be matched to
#'                                each person in the treatment arm. A maxRatio of 0 means no maximum:
#'                                all comparators will be assigned to a target person.
#' @param stratificationColumns   Names or numbers of one or more columns in the \code{data} data.frame
#'                                on which subjects should be stratified prior to matching. No persons
#'                                will be matched with persons outside of the strata identified by the
#'                                values in these columns.
#'
#' @details
#' The data frame should have at least the following three columns: \tabular{lll}{ \verb{rowId}
#' \tab(numeric) \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the target (1) or comparator\cr \tab
#' \tab (0) group \cr \verb{propensityScore} \tab(numeric) \tab Propensity score \cr } This function
#' implements the greedy variable-ratio matching algorithm described in Rassen et al (2012).
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
#' data <- data.frame(rowId = rowId,
#'                    treatment = treatment,
#'                    propensityScore = propensityScore,
#'                    age_group = age_group)
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
                      stratificationColumns = c()) {
  if (!("rowId" %in% colnames(population)))
    stop("Missing column rowId in population")
  if (!("treatment" %in% colnames(population)))
    stop("Missing column treatment in population")
  if (!("propensityScore" %in% colnames(population)))
    stop("Missing column propensityScore in population")
  if (caliperScale != "standardized" && caliperScale != "propensity score" && caliperScale != "standardized logit")
    stop(paste("Unknown caliperScale '",
               caliperScale,
               "', please choose either 'standardized', 'propensity score', or 'standardized logit'"), sep = "")

  population <- population[order(population$propensityScore), ]
  propensityScore <- population$propensityScore
  if (caliper <= 0 || min(population$propensityScore) == max(population$propensityScore)) {
    caliper <- 9999
  } else if (caliperScale == "standardized") {
    caliper <- caliper * sd(population$propensityScore)
  } else if (caliperScale == "standardized logit"){
    propensityScore <- logit(propensityScore)
    caliper <- caliper * sd(propensityScore)
  }
  if (maxRatio == 0) {
    maxRatio <- 999
  }
  if (length(stratificationColumns) == 0) {
    result <- matchPsInternal(propensityScore,
                              population$treatment,
                              maxRatio,
                              caliper)
    population$stratumId <- result$stratumId
    population <- population[population$stratumId != -1, ]
    if (!is.null(attr(population, "metaData"))) {
      attr(population, "metaData")$attrition <- rbind(attr(population, "metaData")$attrition,
                                                      getCounts(population, paste("Matched on propensity score")))
    }
    ParallelLogger::logDebug("Population size after matching is ", nrow(result))
    return(population)
  } else {
    f <- function(subset, maxRatio, caliper) {
      subResult <- matchPsInternal(subset$propensityScore,
                                   subset$treatment,
                                   maxRatio,
                                   caliper)
      subset$stratumId <- subResult$stratumId
      subset <- subset[subset$stratumId != -1, ]
      return(subset)
    }
    results <- plyr::dlply(.data = population,
                           .variables = stratificationColumns,
                           .drop = TRUE,
                           .fun = f,
                           maxRatio = maxRatio,
                           caliper = caliper)
    maxStratumId <- 0
    for (i in 1:length(results)) {
      if (nrow(results[[i]]) > 0) {
        if (maxStratumId != 0)
          results[[i]]$stratumId <- results[[i]]$stratumId + maxStratumId + 1
        maxStratumId <- max(results[[i]]$stratumId)
      }
    }
    result <- do.call(rbind, results)
    if (!is.null(attr(result, "metaData"))) {
      attr(result, "metaData")$attrition <- rbind(attr(result, "metaData")$attrition,
                                                  getCounts(result, paste("Trimmed to equipoise")))
    }
    ParallelLogger::logDebug("Population size after matching is ", nrow(result))
    return(result)
  }
}

#' Match by propensity score as well as other covariates
#'
#' @description
#' \code{matchOnPsAndCovariates} uses the provided propensity scores and a set of covariates to match
#' target to comparator persons.
#'
#' @param population         A data frame with the three columns described below.
#' @param caliper            The caliper for matching. A caliper is the distance which is acceptable
#'                           for any match. Observations which are outside of the caliper are dropped.
#'                           A caliper of 0 means no caliper is used.
#' @param caliperScale            The scale on which the caliper is defined. Three scales are supported:
#'                                \cr\code{caliperScale = 'propensity score'}, \code{caliperScale =
#'                                'standardized'}, or \cr\code{caliperScale = 'standardized logit'}.
#'                                On the standardized scale, the caliper is interpreted in standard
#'                                deviations of the propensity score distribution. 'standardized logit'
#'                                is similar, except that the propensity score is transformed to the logit
#'                                scale because the PS is more likely to be normally distributed on that scale
#'                                (Austin, 2011).
#' @param maxRatio           The maximum number of persons int the comparator arm to be matched to each
#'                           person in the treatment arm. A maxRatio of 0 means no maximum: all
#'                           comparators will be assigned to a target person.
#' @param cohortMethodData   An object of type \code{cohortMethodData} as generated using
#'                           \code{getDbCohortMethodData}.
#' @param covariateIds       One or more covariate IDs in the \code{cohortMethodData} object on which
#'                           subjects should be also matched.
#'
#' @details
#' The data frame should have at least the following three columns: \tabular{lll}{ \verb{rowId}
#' \tab(numeric) \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the target (1) or comparator\cr \tab
#' \tab (0) group \cr \verb{propensityScore} \tab(numeric) \tab Propensity score \cr } This function
#' implements the greedy variable-ratio matching algorithm described in Rassen et al (2012).
#'
#' The default caliper (0.2 on the standardized logit scale) is the one recommended by Austin (2011).
#'
#' @return
#' Returns a date frame with the same columns as the input data plus one extra column: stratumId. Any
#' rows that could not be matched are removed
#'
#' @examples
#' # todo
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
                                   cohortMethodData,
                                   covariateIds) {
  if (caliperScale != "standardized" && caliperScale != "propensity score" && caliperScale != "standardized logit")
    stop(paste("Unknown caliperScale '",
               caliperScale,
               "', please choose either 'standardized', 'propensity score', or 'standardized logit'"), sep = "")

  population <- mergeCovariatesWithPs(population, cohortMethodData, covariateIds)
  stratificationColumns <- colnames(population)[colnames(population) %in% paste("covariateId",
                                                                                covariateIds,
                                                                                sep = "_")]
  return(matchOnPs(population, caliper, caliperScale, maxRatio, stratificationColumns))
}

#' Stratify persons by propensity score
#'
#' @description
#' \code{stratifyByPs} uses the provided propensity scores to stratify persons. Additional
#' stratification variables for stratifications can also be used.
#'
#' @param population              A data frame with the three columns described below
#' @param numberOfStrata          How many strata? The boundaries of the strata are automatically
#'                                defined to contain equal numbers of target persons.
#' @param stratificationColumns   Names of one or more columns in the \code{data} data.frame on which
#'                                subjects should also be stratified in addition to stratification on
#'                                propensity score.
#' @param baseSelection           What is the base selection of subjects where the strata bounds are
#'                                to be determined? Strata are defined as equally-sized strata inside
#'                                this selection. Possible values are "all", "target", and "comparator".
#'
#' @details
#' The data frame should have the following three columns: \tabular{lll}{ \verb{rowId} \tab(numeric)
#' \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment} \tab(integer) \tab
#' Column indicating whether the person is in the target (1) or comparator\cr \tab \tab (0) group \cr
#' \verb{propensityScore} \tab(numeric) \tab Propensity score \cr }
#'
#' @return
#' Returns a date frame with the same columns as the input data plus one extra column: stratumId.
#' @examples
#' rowId <- 1:200
#' treatment <- rep(0:1, each = 100)
#' propensityScore <- c(runif(100, min = 0, max = 1), runif(100, min = 0, max = 1))
#' data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
#' result <- stratifyByPs(data, 5)
#'
#' @export
stratifyByPs <- function(population, numberOfStrata = 5, stratificationColumns = c(), baseSelection = "all") {
  if (!("rowId" %in% colnames(population)))
    stop("Missing column rowId in population")
  if (!("treatment" %in% colnames(population)))
    stop("Missing column treatment in population")
  if (!("propensityScore" %in% colnames(population)))
    stop("Missing column propensityScore in population")
  ParallelLogger::logTrace("Stratifying by propensity score")
  if (nrow(population) == 0) {
    return(population)
  }
  baseSelection <- tolower(baseSelection)
  if (baseSelection == "all") {
    basePop <- population$propensityScore
  } else if (baseSelection == "target") {
    basePop <- population$propensityScore[population$treatment == 1]
  } else if (baseSelection == "target") {
    basePop <- population$propensityScore[population$treatment == 0]
  } else {
    stop(paste0("Unknown base selection: '", baseSelection, "'. Please choose 'all', 'target', or 'comparator'"))
  }
  psStrata <- unique(quantile(basePop,
                              (1:(numberOfStrata - 1))/numberOfStrata))
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
                                                          labels = 1:(length(breaks) - 1))))
    }
    return(population)
  } else {
    f <- function(subset, psStrata, numberOfStrata) {
      if (length(breaks) - 1 == 1) {
        subset$stratumId <- rep(1, nrow(subset))
      } else {
        subset$stratumId <- as.integer(as.character(cut(subset$propensityScore,
                                                        breaks = breaks,
                                                        labels = 1:(length(breaks) - 1))))
      }
      return(subset)
    }

    results <- plyr::dlply(.data = population,
                           .variables = stratificationColumns,
                           .drop = TRUE,
                           .fun = f,
                           psStrata = psStrata,
                           numberOfStrata = numberOfStrata)
    maxStratumId <- 0
    for (i in 1:length(results)) {
      if (nrow(results[[i]]) > 0) {
        if (maxStratumId != 0)
          results[[i]]$stratumId <- results[[i]]$stratumId + maxStratumId + 1
        maxStratumId <- max(results[[i]]$stratumId)
      }
    }
    result <- do.call(rbind, results)
    return(result)
  }
}

#' Stratify persons by propensity score and other covariates
#'
#' @description
#' \code{stratifyByPsAndCovariates} uses the provided propensity scores and covariatesto stratify
#' persons.
#'
#' @param population         A data frame with the three columns described below
#' @param numberOfStrata     Into how many strata should the propensity score be divided? The
#'                           boundaries of the strata are automatically defined to contain equal
#'                           numbers of target persons.
#' @param baseSelection      What is the base selection of subjects where the strata bounds are
#'                           to be determined? Strata are defined as equally-sized strata inside
#'                           this selection. Possible values are "all", "target", and "comparator".
#' @param cohortMethodData   An object of type \code{cohortMethodData} as generated using
#'                           \code{getDbCohortMethodData}.
#' @param covariateIds       One or more covariate IDs in the \code{cohortMethodData} object on which
#'                           subjects should also be stratified.
#'
#' @details
#' The data frame should have the following three columns: \tabular{lll}{ \verb{rowId} \tab(integer)
#' \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment} \tab(integer) \tab
#' Column indicating whether the person is in the target (1) or comparator\cr \tab \tab (0) group \cr
#' \verb{propensityScore} \tab(numeric) \tab Propensity score \cr }
#'
#' @return
#' Returns a date frame with the same columns as the input population plus one extra column:
#' stratumId.
#' @examples
#' # todo
#'
#' @export
stratifyByPsAndCovariates <- function(population,
                                      numberOfStrata = 5,
                                      baseSelection = "all",
                                      cohortMethodData,
                                      covariateIds) {
  population <- mergeCovariatesWithPs(population, cohortMethodData, covariateIds)
  stratificationColumns <- colnames(population)[colnames(population) %in% paste("covariateId",
                                                                                covariateIds,
                                                                                sep = "_")]
  return(stratifyByPs(population = population,
                      numberOfStrata = numberOfStrata,
                      stratificationColumns = stratificationColumns,
                      baseSelection = baseSelection))
}

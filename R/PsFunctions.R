# @file PsFunctions.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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
  sampled <- FALSE
  if (maxCohortSizeForFitting != 0) {
    set.seed(0)
    targetRowIds <- population$rowId[population$treatment == 1]
    if (length(targetRowIds) > maxCohortSizeForFitting) {
      writeLines(paste0("Downsampling target cohort from ", length(targetRowIds), " to ", maxCohortSizeForFitting, " before fitting"))
      targetRowIds <- sample(targetRowIds, size = maxCohortSizeForFitting, replace = FALSE)
      sampled <- TRUE
    }
    comparatorRowIds <- population$rowId[population$treatment == 0]
    if (length(comparatorRowIds) > maxCohortSizeForFitting) {
      writeLines(paste0("Downsampling comparator cohort from ", length(comparatorRowIds), " to ", maxCohortSizeForFitting, " before fitting"))
      comparatorRowIds <- sample(comparatorRowIds, size = maxCohortSizeForFitting, replace = FALSE)
      sampled <- TRUE
    }
    if (sampled) {
      fullPopulation <- population
      population <- population[population$rowId %in% c(targetRowIds, comparatorRowIds), ]
    }
  }
  if (!Cyclops::isSorted(population, "rowId")) {
    population <- population[order(population$rowId), ]
  }
  cohortSubset <- ff::as.ffdf(population)
  colnames(cohortSubset)[colnames(cohortSubset) == "treatment"] <- "y"
  covariateSubset <- FeatureExtraction::filterByRowId(cohortMethodData$covariates, cohortSubset$rowId)

  if (length(includeCovariateIds) != 0) {
    idx <- !is.na(ffbase::ffmatch(covariateSubset$covariateId, ff::as.ff(includeCovariateIds)))
    covariateSubset <- covariateSubset[ffbase::ffwhich(idx, idx == TRUE), ]
  }
  if (length(excludeCovariateIds) != 0) {
    idx <- is.na(ffbase::ffmatch(covariateSubset$covariateId, ff::as.ff(excludeCovariateIds)))
    covariateSubset <- covariateSubset[ffbase::ffwhich(idx, idx == TRUE), ]
  }
  covariateData <- FeatureExtraction::tidyCovariateData(covariates = covariateSubset,
                                                        covariateRef = cohortMethodData$covariateRef,
                                                        populationSize = nrow(cohortSubset),
                                                        normalize = TRUE,
                                                        removeRedundancy = TRUE)
  covariateSubset <- covariateData$covariates
  attr(population, "metaData")$deletedCovariateIdsforPs <- covariateData$metaData$deletedCovariateIds
  rm(covariateData)
  cyclopsData <- convertToCyclopsData(cohortSubset, covariateSubset, modelType = "lr", quiet = TRUE)
  ff::close.ffdf(cohortSubset)
  ff::close.ffdf(covariateSubset)
  rm(cohortSubset)
  rm(covariateSubset)
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
      writeLines("High correlation between covariate(s) and treatment detected:")
      print(ref)
      message <- "High correlation between covariate(s) and treatment detected. Perhaps you forgot to exclude part of the exposure definition from the covariates?"
      if (stopOnError) {
        stop(message)
      } else {
        error <- message
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
      population <- fullPopulation
      cohortSubset <- ff::as.ffdf(population)
      covariateSubset <- FeatureExtraction::filterByRowId(cohortMethodData$covariates, cohortSubset$rowId)
      population$propensityScore <- predict(cyclopsFit, newOutcomes = cohortSubset, newCovariates = covariateSubset)
      ff::close.ffdf(cohortSubset)
      ff::close.ffdf(covariateSubset)
      rm(cohortSubset)
      rm(covariateSubset)
    } else {
      population$propensityScore <- predict(cyclopsFit)
    }
    attr(population, "metaData")$psModelCoef <- coef(cyclopsFit)
    attr(population, "metaData")$psModelPriorVariance <- cyclopsFit$variance[1]
  } else {
    population$propensityScore <- population$treatment
    attr(population, "metaData")$psError <- error
    if (!is.null(ref)) {
      attr(population, "metaData")$psHighCorrelation <- ref
    }
  }
  population <- computePreferenceScore(population)
  delta <- Sys.time() - start
  writeLines(paste("Creating propensity scores took", signif(delta, 3), attr(delta, "units")))
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
  cfs <- attr(propensityScore, "metaData")$psModelCoef
  cfs <- cfs[cfs != 0]
  attr(cfs, "names")[1] <- 0  #Rename intercept to 0
  cfs <- data.frame(coefficient = cfs, id = as.numeric(attr(cfs, "names")))
  cfs <- merge(ff::as.ffdf(cfs), cohortMethodData$covariateRef, by.x = "id", by.y = "covariateId")
  cfs <- ff::as.ram(cfs[, c("coefficient", "id", "covariateName")])
  if (length(cfs$coefficient) > 1) {
    cfs <- cfs[order(-abs(cfs$coefficient)), ]
  }
  return(cfs)
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
#' @param type              Type of plot. Two possible values: \code{type = 'density'} or \code{type =
#'                          'histogram'}
#' @param binWidth          For histograms, the width of the bins
#' @param treatmentLabel    A label to us for the treated cohort.
#' @param comparatorLabel   A label to us for the comparator cohort.
#' @param fileName          Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function \code{ggsave} in the ggplot2 package for supported file
#'                          formats.
#'
#' @details
#' The data frame should have a least the following two columns: \tabular{lll}{ \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator\cr \tab
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
                   treatmentLabel = "Treated",
                   comparatorLabel = "Comparator",
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
  if (type != "density" && type != "histogram")
    stop(paste("Unknown type '", type, "', please choose either 'density' or 'histogram'"),
         sep = "")
  if (scale != "propensity" && scale != "preference")
    stop(paste("Unknown scale '", scale, "', please choose either 'propensity' or 'preference'"),
         sep = "")

  if (scale == "preference") {
    data <- computePreferenceScore(data, unfilteredData)
    data$SCORE <- data$preferenceScore
    label <- "Preference score"
  } else {
    data$SCORE <- data$propensityScore
    label <- "Propensity score"
  }
  data$GROUP <- treatmentLabel
  data$GROUP[data$treatment == 0] <- comparatorLabel
  data$GROUP <- factor(data$GROUP, levels = c(treatmentLabel, comparatorLabel))
  if (type == "density") {
    plot <- ggplot2::ggplot(data,
                            ggplot2::aes(x = SCORE, color = GROUP, group = GROUP, fill = GROUP)) +
      ggplot2::geom_density() +
      ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                            rgb(0, 0, 0.8, alpha = 0.5))) +
      ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                             rgb(0, 0, 0.8, alpha = 0.5))) +
      ggplot2::scale_x_continuous(label, limits = c(0, 1)) +
      ggplot2::scale_y_continuous("Density") +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    if (!is.null(attr(data, "strata"))) {
      strata <- data.frame(propensityScore = attr(data, "strata"))
      if (scale == "preference") {
        if (is.null(unfilteredData)) {
          strata <- computePreferenceScore(strata, data)
        } else {
          strata <- computePreferenceScore(strata, unfilteredData)
        }
        strata$SCORE <- strata$preferenceScore
      } else {
        strata$SCORE <- strata$propensityScore
      }
      plot <- plot +
        ggplot2::geom_vline(xintercept = strata$SCORE, color = rgb(0, 0, 0, alpha = 0.5))
    }
  } else {
    plot <- ggplot2::ggplot(data,
                            ggplot2::aes(x = SCORE, color = GROUP, group = GROUP, fill = GROUP)) +
      ggplot2::geom_histogram(binwidth = binWidth, position = "identity") +
      ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                            rgb(0, 0, 0.8, alpha = 0.5))) +
      ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                             rgb(0, 0, 0.8, alpha = 0.5))) +
      ggplot2::scale_x_continuous(label, limits = c(0, 1)) +
      ggplot2::scale_y_continuous("Number of subjects") +
      ggplot2::theme(legend.title = ggplot2::element_blank())
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
#' \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator\cr \tab
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
    auc <- auc(data$propensityScore, data$treatment)
    return(auc)
  }
}

#' Trim persons by propensity score
#'
#' @description
#' \code{trimByPs} uses the provided propensity scores to trim subjects with extreme scores.
#'
#' @param population     A data frame with the three columns described below
#' @param trimFraction   This fraction will be removed from each treatment group. In the treatment
#'                       group, persons with the highest propensity scores will be removed, in the
#'                       comparator group person with the lowest scores will be removed.
#'
#' @details
#' The data frame should have the following three columns: \tabular{lll}{ \verb{rowId} \tab(numeric)
#' \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment} \tab(integer) \tab
#' Column indicating whether the person is in the treated (1) or comparator\cr \tab \tab (0) group \cr
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
  cutoffTreated <- quantile(population$propensityScore[population$treatment == 1], 1 - trimFraction)
  cutoffComparator <- quantile(population$propensityScore[population$treatment == 0], trimFraction)
  result <- population[(population$propensityScore <= cutoffTreated & population$treatment == 1) |
                         (population$propensityScore >= cutoffComparator & population$treatment == 0), ]
  if (!is.null(attr(result, "metaData"))) {
    attr(result,
         "metaData")$attrition <- rbind(attr(result, "metaData")$attrition, getCounts(population, paste("Trimmed by PS")))
  }
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
#' Column indicating whether the person is in the treated (1) or comparator\cr \tab \tab (0) group \cr
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

  temp <- computePreferenceScore(population)
  population <- population[temp$preferenceScore >= bounds[1] & temp$preferenceScore <= bounds[2], ]
  if (!is.null(attr(population, "metaData"))) {
    attr(population,
         "metaData")$attrition <- rbind(attr(population, "metaData")$attrition, getCounts(population, paste("Trimmed to equipoise")))
  }
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
#' \code{matchOnPs} uses the provided propensity scores to match treated to comparator persons.
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
#'                                all comparators will be assigned to a treated person.
#' @param stratificationColumns   Names or numbers of one or more columns in the \code{data} data.frame
#'                                on which subjects should be stratified prior to matching. No persons
#'                                will be matched with persons outside of the strata identified by the
#'                                values in these columns.
#'
#' @details
#' The data frame should have at least the following three columns: \tabular{lll}{ \verb{rowId}
#' \tab(numeric) \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator\cr \tab
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
    return(result)
  }
}

#' Match by propensity score as well as other covariates
#'
#' @description
#' \code{matchOnPsAndCovariates} uses the provided propensity scores and a set of covariates to match
#' treated to comparator persons.
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
#'                           comparators will be assigned to a treated person.
#' @param cohortMethodData   An object of type \code{cohortMethodData} as generated using
#'                           \code{getDbCohortMethodData}.
#' @param covariateIds       One or more covariate IDs in the \code{cohortMethodData} object on which
#'                           subjects should be also matched.
#'
#' @details
#' The data frame should have at least the following three columns: \tabular{lll}{ \verb{rowId}
#' \tab(numeric) \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator\cr \tab
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
#'                                defined to contain equal numbers of treated persons.
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
#' Column indicating whether the person is in the treated (1) or comparator\cr \tab \tab (0) group \cr
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
  if (length(psStrata) == 1) {
    warning("Only 1 strata detected")
  }
  if (length(stratificationColumns) == 0) {
    if (length(psStrata) == 1) {
      population$stratumId <- 1
    } else {
      population$stratumId <- as.integer(as.character(cut(population$propensityScore,
                                                          breaks = c(0, psStrata, 1),
                                                          labels = 1:(length(psStrata) + 1))))
    }
    return(population)
  } else {
    f <- function(subset, psStrata, numberOfStrata) {
      if (length(psStrata) == 1) {
        subset$stratumId <- 1
      } else {
        subset$stratumId <- as.integer(as.character(cut(subset$propensityScore,
                                                        breaks = c(0, psStrata, 1),
                                                        labels = 1:(length(psStrata) + 1))))
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
#'                           numbers of treated persons.
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
#' Column indicating whether the person is in the treated (1) or comparator\cr \tab \tab (0) group \cr
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

quickSum <- function(data, squared = FALSE) {
  if (squared) {
    x <- bySumFf(data$covariateValue^2, data$covariateId)
    colnames(x) <- c("covariateId", "sumSqr")
  } else {
    x <- bySumFf(data$covariateValue, data$covariateId)
    colnames(x) <- c("covariateId", "sum")
  }
  x$covariateId <- as.numeric(x$covariateId)
  return(x)
}


computeMeanAndSd <- function(cohorts, covariates, treatment) {
  if (!is.null(cohorts$stratumId)) {
    # Rownames needs to be null or else next command will crash
    rownames(cohorts) <- NULL
    t <- cohorts[cohorts$treatment == treatment, ]
    t <- aggregate(rowId ~ stratumId, t, length)
  }
  if (!is.null(cohorts$stratumId) && max(t$rowId) > 1) {
    # Variable strata sizes detected: weigh by size of strata set
    w <- t
    w$weight <- 1/w$rowId
    w$rowId <- NULL
    w <- merge(w, cohorts[cohorts$treatment == treatment, ])
    w <- w[, c("rowId", "weight")]
    w$weight <- w$weight/sum(w$weight)  # Normalize so sum(w) == 1
    t <- cohorts$treatment == treatment
    t <- !is.na(ffbase::ffmatch(covariates$rowId, cohorts$rowId[ffbase::ffwhich(t, t == TRUE)]))
    covariatesSubset <- covariates[ffbase::ffwhich(t, t == TRUE), ]
    covariatesSubset <- ffbase::merge.ffdf(covariatesSubset, ff::as.ffdf(w))
    covariatesSubset$wValue <- covariatesSubset$weight * covariatesSubset$covariateValue
    covariatesSubset$wValueSquared <- covariatesSubset$wValue * covariatesSubset$covariateValue

    # Compute sum
    result <- bySumFf(covariatesSubset$covariateValue, covariatesSubset$covariateId)
    colnames(result)[colnames(result) == "bins"] <- "covariateId"
    colnames(result)[colnames(result) == "sums"] <- "sum"

    # Compute weighted mean (no need to divide by sum(w) because it is 1)
    wMean <- bySumFf(covariatesSubset$wValue, covariatesSubset$covariateId)
    colnames(wMean)[colnames(wMean) == "bins"] <- "covariateId"
    colnames(wMean)[colnames(wMean) == "sums"] <- "mean"
    result <- merge(result, wMean)

    # Compute weighted standard deviation
    wValueSquared <- bySumFf(covariatesSubset$wValueSquared, covariatesSubset$covariateId)
    colnames(wValueSquared)[colnames(wValueSquared) == "bins"] <- "covariateId"
    colnames(wValueSquared)[colnames(wValueSquared) == "sums"] <- "wValueSquared"
    result <- merge(result, wMean)
    sumW <- 1
    sumWSquared <- sum(w$weight^2)
    result <- merge(result, wValueSquared)
    result$variance <- (result$wValueSquared - result$mean^2) * sumW/(sumW^2 - sumWSquared)
    result$sd <- sqrt(result$variance)
  } else {
    # Used uniform strata size, no need to weigh
    t <- cohorts$treatment == treatment
    personCount <- sum(t)
    t <- !is.na(ffbase::ffmatch(covariates$rowId, cohorts$rowId[ffbase::ffwhich(t, t == TRUE)]))
    covariatesSubset <- covariates[ffbase::ffwhich(t, t == TRUE), ]
    result <- quickSum(covariatesSubset)
    resultSqr <- quickSum(covariatesSubset, squared = TRUE)
    result <- merge(result, resultSqr)
    result$sd <- sqrt((result$sumSqr - (result$sum^2/personCount))/personCount)
    result$mean <- result$sum/personCount
  }
  return(result)
}

computeMeansPerGroup <- function(cohorts, covariates) {
  nOverall <- nrow(cohorts)
  nTreated <- ffbase::sum.ff(cohorts$treatment == 1)
  nComparator <- nOverall - nTreated

  treated <- computeMeanAndSd(cohorts, covariates, treatment = 1)
  colnames(treated)[colnames(treated) == "sum"] <- "sumTreated"
  colnames(treated)[colnames(treated) == "mean"] <- "meanTreated"
  colnames(treated)[colnames(treated) == "sd"] <- "sdTreated"

  comparator <- computeMeanAndSd(cohorts, covariates, treatment = 0)
  colnames(comparator)[colnames(comparator) == "sum"] <- "sumComparator"
  colnames(comparator)[colnames(comparator) == "mean"] <- "meanComparator"
  colnames(comparator)[colnames(comparator) == "sd"] <- "sdComparator"

  result <- merge(treated[,
                          c("covariateId", "meanTreated", "sumTreated", "sdTreated")],
                  comparator[,
                             c("covariateId", "meanComparator", "sumComparator", "sdComparator")],
                  all = TRUE)
  result$sd <- sqrt((result$sdTreated^2 + result$sdComparator^2)/2)
  result <- result[, c("covariateId",
                       "meanTreated",
                       "meanComparator",
                       "sumTreated",
                       "sumComparator",
                       "sd")]
  return(result)
}


#' Compute covariate balance before and after matching and trimming
#'
#' @description
#' For every covariate, prevalence in treatment and comparator groups before and after
#' matching/trimming are computed. When variable ratio matching was used the balance score will be
#' corrected according the method described in Austin et al (2008).
#'
#' @param population         A data frame containing the people that are remaining after matching
#'                           and/or trimming.
#' @param cohortMethodData   An object of type \code{cohortMethodData} as generated using
#'                           \code{getDbCohortMethodData}.
#' @details
#' The population data frame should have at least the following columns: \tabular{lll}{ \verb{rowId}
#' \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0)\cr
#' \tab \tab group \cr }
#'
#' @return
#' Returns a date frame describing the covariate balance before and after matching/trimming.
#'
#' @references
#' Austin, P.C. (2008) Assessing balance in measured baseline covariates when using many-to-one
#' matching on the propensity-score. Pharmacoepidemiology and Drug Safety, 17: 1218-1225.
#'
#' @export
computeCovariateBalance <- function(population, cohortMethodData) {
  start <- Sys.time()
  cohorts <- ff::as.ffdf(cohortMethodData$cohorts[, c("rowId", "treatment")])
  covariates <- cohortMethodData$covariates

  # Try to undo normalization of covariate values:
  # normFactors <- attr(cohortMethodData$covariates, "normFactors")
  # if (!is.null(normFactors)) {
  #   covariates <- ffbase::merge.ffdf(covariates, ff::as.ffdf(normFactors))
  #   covariates$covariateValue <- covariates$covariateValue * covariates$maxs
  #   covariates$maxs <- NULL
  # }

  beforeMatching <- computeMeansPerGroup(cohorts, covariates)
  cohortsAfterMatching <- ff::as.ffdf(population[, c("rowId", "treatment", "stratumId")])
  afterMatching <- computeMeansPerGroup(cohortsAfterMatching, covariates)

  ff::close.ffdf(cohorts)
  ff::close.ffdf(cohortsAfterMatching)
  rm(cohorts)
  rm(cohortsAfterMatching)

  colnames(beforeMatching)[colnames(beforeMatching) == "meanTreated"] <- "beforeMatchingMeanTreated"
  colnames(beforeMatching)[colnames(beforeMatching) == "meanComparator"] <- "beforeMatchingMeanComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumTreated"] <- "beforeMatchingSumTreated"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumComparator"] <- "beforeMatchingSumComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sd"] <- "beforeMatchingSd"
  colnames(afterMatching)[colnames(afterMatching) == "meanTreated"] <- "afterMatchingMeanTreated"
  colnames(afterMatching)[colnames(afterMatching) == "meanComparator"] <- "afterMatchingMeanComparator"
  colnames(afterMatching)[colnames(afterMatching) == "sumTreated"] <- "afterMatchingSumTreated"
  colnames(afterMatching)[colnames(afterMatching) == "sumComparator"] <- "afterMatchingSumComparator"
  colnames(afterMatching)[colnames(afterMatching) == "sd"] <- "afterMatchingSd"
  balance <- merge(beforeMatching, afterMatching)
  balance <- merge(balance, ff::as.ram(cohortMethodData$covariateRef))
  balance$beforeMatchingStdDiff <- (balance$beforeMatchingMeanTreated - balance$beforeMatchingMeanComparator)/balance$beforeMatchingSd
  balance$afterMatchingStdDiff <- (balance$afterMatchingMeanTreated - balance$afterMatchingMeanComparator)/balance$afterMatchingSd
  balance$beforeMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance$afterMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  delta <- Sys.time() - start
  writeLines(paste("Computing covariate balance took", signif(delta, 3), attr(delta, "units")))
  return(balance)
}

#' Create a scatterplot of the covariate balance
#'
#' @description
#' Create a scatterplot of the covariate balance, showing all variables with balance before and after
#' matching on the x and y axis respectively. Requires running \code{computeCovariateBalance} first.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @param balance     A data frame created by the \code{computeCovariateBalance} funcion.
#' @param absolute    Should the absolute value of the difference be used?
#' @param threshold   Show a threshold value for after matching standardized difference.
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function \code{ggsave} in the ggplot2 package for supported file formats.
#' @param beforeLabel Label for the x-axis.
#' @param afterLabel  Label for the y-axis.
#'
#' @export
plotCovariateBalanceScatterPlot <- function(balance,
                                            absolute = TRUE,
                                            threshold = 0,
                                            fileName = NULL,
                                            beforeLabel = "Before matching",
                                            afterLabel = "After matching") {
  if (absolute) {
    balance$beforeMatchingStdDiff <- abs(balance$beforeMatchingStdDiff)
    balance$afterMatchingStdDiff <- abs(balance$afterMatchingStdDiff)
  }
  limits <- c(min(c(balance$beforeMatchingStdDiff, balance$afterMatchingStdDiff), na.rm = TRUE),
              max(c(balance$beforeMatchingStdDiff, balance$afterMatchingStdDiff), na.rm = TRUE))
  plot <- ggplot2::ggplot(balance,
                          ggplot2::aes(x = beforeMatchingStdDiff, y = afterMatchingStdDiff)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::ggtitle("Standardized difference of mean") +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits)
  if (threshold != 0) {
    plot <- plot + ggplot2::geom_hline(yintercept = c(threshold,
                                                      -threshold), alpha = 0.5, linetype = "dotted")
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 4, height = 4, dpi = 400)
  }
  return(plot)
}

.truncRight <- function(x, n) {
  nc <- nchar(x)
  x[nc > (n - 3)] <- paste("...",
                           substr(x[nc > (n - 3)], nc[nc > (n - 3)] - n + 1, nc[nc > (n - 3)]),
                           sep = "")
  x
}

#' Plot variables with largest imbalance
#'
#' @description
#' Create a plot showing those variables having the largest imbalance before matching, and those
#' variables having the largest imbalance after matching. Requires running
#' \code{computeCovariateBalance} first.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @param balance        A data frame created by the \code{computeCovariateBalance} funcion.
#' @param n              Count of variates to plot.
#' @param maxNameWidth   Covariate names longer than this number of characters are truncated to create
#'                       a nicer plot.
#' @param fileName       Name of the file where the plot should be saved, for example 'plot.png'. See
#'                       the function \code{ggsave} in the ggplot2 package for supported file formats.
#' @param beforeLabel    Label for identifying data before matching / stratification / trimming.
#' @param afterLabel     Label for identifying data after matching / stratification / trimming.
#'
#' @export
plotCovariateBalanceOfTopVariables <- function(balance,
                                               n = 20,
                                               maxNameWidth = 100,
                                               fileName = NULL,
                                               beforeLabel = "before matching",
                                               afterLabel = "after matching") {
  topBefore <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  topBefore <- topBefore[1:n, ]
  topBefore$facet <- paste("Top", n, beforeLabel)
  topAfter <- balance[order(-abs(balance$afterMatchingStdDiff)), ]
  topAfter <- topAfter[1:n, ]
  topAfter$facet <- paste("Top", n, afterLabel)
  filtered <- rbind(topBefore, topAfter)

  data <- data.frame(covariateId = rep(filtered$covariateId, 2),
                     covariate = rep(filtered$covariateName, 2),
                     difference = c(filtered$beforeMatchingStdDiff, filtered$afterMatchingStdDiff),
                     group = rep(c(beforeLabel, afterLabel), each = nrow(filtered)),
                     facet = rep(filtered$facet, 2),
                     rowId = rep(nrow(filtered):1, 2))
  filtered$covariateName <- .truncRight(as.character(filtered$covariateName), maxNameWidth)
  data$facet <- factor(data$facet, levels = rev(levels(data$facet)))
  data$group <- factor(data$group, levels = rev(levels(data$group)))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = difference,
                                             y = rowId,
                                             color = group,
                                             group = group,
                                             fill = group,
                                             shape = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Standardized difference of mean") +
    ggplot2::scale_y_continuous(breaks = nrow(filtered):1, labels = filtered$covariateName) +
    ggplot2::facet_grid(facet ~ ., scales = "free", space = "free") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.direction = "vertical",
                   legend.title = ggplot2::element_blank())
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 10, height = max(2 + n * 0.2, 5), dpi = 400)
  return(plot)
}


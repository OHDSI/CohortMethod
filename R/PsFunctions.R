# @file PsFunctions.R
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

#' Create propensity scores
#'
#' @description
#' \code{createPs} creates propensity scores using a regularized logistic regression.
#'
#' @param cohortMethodData      An object of type \code{cohortMethodData} as generated using
#'                              \code{getDbCohortMethodData}.
#' @param outcomeConceptId      The concept ID of the outcome. Persons marked for removal for the
#'                              outcome will be removed prior to creating the propensity score model.
#' @param excludeCovariateIds   Exclude these covariates from the propensity model.
#' @param prior                 The prior used to fit the model. See \code{\link[Cyclops]{createPrior}}
#'                              for details.
#' @param control               The control object used to control the cross-validation used to
#'                              determine the hyperparameters of the prior (if applicable). See
#'                              \code{\link[Cyclops]{createControl}} for details.
#'
#' @details
#' \code{createPs} creates propensity scores using a regularized logistic regression.
#'
#' @examples
#' data(cohortDataSimulationProfile)
#' cohortMethodData <- simulateCohortMethodData(cohortDataSimulationProfile, n = 1000)
#' ps <- createPs(cohortMethodData)
#'
#' @export
createPs <- function(cohortMethodData,
                     outcomeConceptId = NULL,
                     excludeCovariateIds = NULL,
                     prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
                     control = createControl(noiseLevel = "silent",
                                             cvType = "auto",
                                             startingVariance = 0.1)) {
  if (is.null(outcomeConceptId) | is.null(cohortMethodData$exclude)) {
    cohortSubset <- cohortMethodData$cohorts
    if (is.null(excludeCovariateIds)) {
      covariateSubset <- ffbase::subset.ffdf(cohortMethodData$covariates, covariateId != 1)
    } else {
      excludeCovariateIds <- c(excludeCovariateIds, 1)
      t <- in.ff(cohortMethodData$covariates$covariateId, ff::as.ff(excludeCovariateIds))
      covariateSubset <- cohortMethodData$covariates[ffbase::ffwhich(t, t == FALSE), ]
    }
  } else {
    t <- cohortMethodData$exclude$outcomeId == outcomeConceptId
    t <- in.ff(cohortMethodData$cohorts$rowId,
               cohortMethodData$exclude$rowId[ffbase::ffwhich(t, t == TRUE)])
    cohortSubset <- cohortMethodData$cohort[ffbase::ffwhich(t, t == FALSE), ]
    t <- cohortMethodData$exclude$outcomeId == outcomeConceptId
    t <- in.ff(cohortMethodData$covariates$rowId,
               cohortMethodData$exclude$rowId[ffbase::ffwhich(t, t == TRUE)])
    if (is.null(excludeCovariateIds)) {
      excludeCovariateIds <- c(1)
    } else {
      excludeCovariateIds <- c(excludeCovariateIds, 1)
    }
    t <- t | in.ff(cohortMethodData$covariates$covariateId, ff::as.ff(excludeCovariateIds))
    covariateSubset <- cohortMethodData$covariates[ffbase::ffwhich(t, t == FALSE), ]
  }
  colnames(cohortSubset)[colnames(cohortSubset) == "treatment"] <- "y"
  cyclopsData <- convertToCyclopsData(cohortSubset, covariateSubset, modelType = "lr", quiet = TRUE)
  ps <- ff::as.ram(cohortSubset[, c("y", "rowId")])
  cyclopsFit <- fitCyclopsModel(cyclopsData, prior = prior, control = control)
  pred <- predict(cyclopsFit)

  colnames(ps)[colnames(ps) == "y"] <- "treatment"
  data <- data.frame(propensityScore = pred, rowId = as.numeric(attr(pred, "names")))
  data <- merge(data, ps, by = "rowId")
  attr(data, "coefficients") <- coef(cyclopsFit)
  attr(data, "priorVariance") <- cyclopsFit$variance[1]
  return(data)
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
  cfs <- attr(propensityScore, "coefficients")
  cfs <- cfs[cfs != 0]
  attr(cfs, "names")[1] <- 0  #Rename intercept to 0
  cfs <- data.frame(coefficient = cfs, id = as.numeric(attr(cfs, "names")))
  cfs <- merge(ff::as.ffdf(cfs), cohortMethodData$covariateRef, by.x = "id", by.y = "covariateId")
  cfs <- ff::as.ram(cfs[, c("coefficient", "id", "covariateName")])
  cfs <- cfs[order(-abs(cfs$coefficient)), ]
  return(cfs)
}

computePreferenceScore <- function(data, unfilteredData = NULL) {
  if (is.null(unfilteredData))
    proportion <- sum(data$treatment)/nrow(data) else proportion <- sum(unfilteredData$treatment)/nrow(unfilteredData)
  x <- exp(log(data$propensityScore/(1 - data$propensityScore)) - log(proportion/(1 - proportion)))
  data$preferenceScore <- x/(x + 1)
  return(data)
}

#' Plot the propensity score distribution
#'
#' @description
#' \code{plotPs} shows the propensity (or preference) score distribution
#'
#' @param data             A data frame with at least the two columns described below
#' @param unfilteredData   To be used when computing preference scores on data from which subjects have
#'                         already been removed, e.g. through trimming and/or matching. This data frame
#'                         should have the same structure as \code{data}.
#' @param scale            The scale of the graph. Two scales are supported: \code{ scale =
#'                         'propensity'} or \code{scale = 'preference'}. The preference score scale is
#'                         defined by Walker et al (2013).
#' @param type             Type of plot. Two possible values: \code{type = 'density'} or \code{type =
#'                         'histogram'}
#' @param binWidth         For histograms, the width of the bins
#' @param fileName         Name of the file where the plot should be saved, for example 'plot.png'. See
#'                         the function \code{ggsave} in the ggplot2 package for supported file
#'                         formats.
#'
#' @details
#' The data frame should have a least the following two columns: \tabular{lll}{ \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator\cr \tab
#' \tab (0) group \cr \verb{propensityScore} \tab(real) \tab Propensity score \cr }
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

  if (scale == "preference") {
    data <- computePreferenceScore(data, unfilteredData)
    data$SCORE <- data$preferenceScore
    label <- "Preference score"
  } else {
    data$SCORE <- data$propensityScore
    label <- "Propensity score"
  }
  data$GROUP <- "Treated"
  data$GROUP[data$treatment == 0] <- "Comparator"
  data$GROUP <- factor(data$GROUP, levels = c("Treated", "Comparator"))
  if (type == "density") {
    plot <- ggplot2::ggplot(data,
                            ggplot2::aes(x = SCORE, color = GROUP, group = GROUP, fill = GROUP)) +
            ggplot2::geom_density() +
            ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                                  rgb(0, 0, 0.8, alpha = 0.5))) +
            ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                                   rgb(0, 0, 0.8, alpha = 0.5))) +
            ggplot2::scale_x_continuous(label, limits = c(0, 1)) +
            ggplot2::scale_y_continuous("Density")
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
      plot <- plot + ggplot2::geom_vline(x = strata$SCORE, color = rgb(0, 0, 0, alpha = 0.5))
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
            ggplot2::scale_y_continuous("Number of subjects")
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
#' \tab (0) group \cr \verb{propensityScore} \tab(real) \tab Propensity score \cr }
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
    auc <- .Call("CohortMethod_aucWithCi",
                 PACKAGE = "CohortMethod",
                 data$propensityScore,
                 data$treatment)
    return(data.frame(auc = auc[1], auc_lb95ci = auc[2], auc_lb95ci = auc[3]))
  } else {
    auc <- .Call("CohortMethod_auc", PACKAGE = "CohortMethod", data$propensityScore, data$treatment)
    return(auc)
  }
}

#' Trim persons by propensity score
#'
#' @description
#' \code{trimByPs} uses the provided propensity scores to trim subjects with extreme scores.
#'
#' @param data           A data frame with the three columns described below
#' @param trimFraction   This fraction will be removed from each treatment group. In the treatment
#'                       group, persons with the highest propensity scores will be removed, in the
#'                       comparator group person with the lowest scores will be removed.
#'
#' @details
#' The data frame should have the following three columns: \tabular{lll}{ \verb{rowId} \tab(integer)
#' \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment} \tab(integer) \tab
#' Column indicating whether the person is in the treated (1) or comparator\cr \tab \tab (0) group \cr
#' \verb{propensityScore} \tab(real) \tab Propensity score \cr }
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
trimByPs <- function(data, trimFraction = 0.05) {
  if (!("rowId" %in% colnames(data)))
    stop("Missing column rowId in data")
  if (!("treatment" %in% colnames(data)))
    stop("Missing column treatment in data")
  if (!("propensityScore" %in% colnames(data)))
    stop("Missing column propensityScore in data")
  cutoffTreated <- quantile(data$propensityScore[data$treatment == 1], 1 - trimFraction)
  cutoffComparator <- quantile(data$propensityScore[data$treatment == 0], trimFraction)
  result <- data[(data$propensityScore <= cutoffTreated & data$treatment == 1) | (data$propensityScore >=
    cutoffComparator & data$treatment == 0), ]
  return(result)
}

#' Keep only persons in clinical equipoise
#'
#' @description
#' \code{trimByPsToEquipoise} uses the preference score to trim subjects that are not in clinical
#' equipoise
#'
#' @param data     A data frame with at least the three columns described below
#' @param bounds   The upper and lower bound on the preference score for keeping persons
#'
#' @details
#' The data frame should have the following three columns: \tabular{lll}{ \verb{rowId} \tab(integer)
#' \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment} \tab(integer) \tab
#' Column indicating whether the person is in the treated (1) or comparator\cr \tab \tab (0) group \cr
#' \verb{propensityScore} \tab(real) \tab Propensity score \cr }
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
trimByPsToEquipoise <- function(data, bounds = c(0.25, 0.75)) {
  if (!("rowId" %in% colnames(data)))
    stop("Missing column rowId in data")
  if (!("treatment" %in% colnames(data)))
    stop("Missing column treatment in data")
  if (!("propensityScore" %in% colnames(data)))
    stop("Missing column propensityScore in data")

  data <- computePreferenceScore(data)
  return(data[data$preferenceScore >= bounds[1] & data$preferenceScore <= bounds[2], ])
}

mergeCovariatesWithPs <- function(data, cohortMethodData, covariateIds) {
  for (covariateId in covariateIds) {
    t <- cohortMethodData$covariates$covariateId == covariateId
    if (any.ff(t)) {
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

#' Match persons by propensity score
#'
#' @description
#' \code{matchOnPs} uses the provided propensity scores to match treated to comparator persons.
#'
#' @param data                    A data frame with the three columns described below.
#' @param caliper                 The caliper for matching. A caliper is the distance which is
#'                                acceptable for any match. Observations which are outside of the
#'                                caliper are dropped. A caliper of 0 means no caliper is used.
#' @param caliperScale            The scale on which the caliper is defined. Two scales are supported:
#'                                \cr\code{caliperScale = 'propensity score'} or \code{caliperScale =
#'                                'standardized'}. On the standardized scale, the caliper is
#'                                interpreted in standard deviations of the propensity score
#'                                distribution.
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
#' \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator\cr \tab
#' \tab (0) group \cr \verb{propensityScore} \tab(real) \tab Propensity score \cr } This function
#' implements the greedy variable-ratio matching algorithm described in Rassen et al (2012).
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
#' @export
matchOnPs <- function(data,
                      caliper = 0.25,
                      caliperScale = "standardized",
                      maxRatio = 1,
                      stratificationColumns = c()) {
  if (!("rowId" %in% colnames(data)))
    stop("Missing column rowId in data")
  if (!("treatment" %in% colnames(data)))
    stop("Missing column treatment in data")
  if (!("propensityScore" %in% colnames(data)))
    stop("Missing column propensityScore in data")

  data <- data[order(data$propensityScore), ]
  if (caliper <= 0) {
    caliper <- 9999
  } else if (caliperScale == "standardized")
    caliper <- caliper * sd(data$propensityScore)
  if (maxRatio == 0) {
    maxRatio <- 999
  }
  if (length(stratificationColumns) == 0) {
    result <- .Call("CohortMethod_matchOnPs",
                    PACKAGE = "CohortMethod",
                    data$propensityScore,
                    data$treatment,
                    maxRatio,
                    caliper)
    result$rowId <- data$rowId
    return(result[result$stratumId != -1, ])
  } else {
    f <- function(subset, maxRatio, caliper) {
      subResult <- .Call("CohortMethod_matchOnPs",
                         PACKAGE = "CohortMethod",
                         subset$propensityScore,
                         subset$treatment,
                         maxRatio,
                         caliper)
      subResult$rowId <- subset$rowId
      subResult[, stratificationColumns] <- subset[, stratificationColumns]
      subResult <- subResult[subResult$stratumId != -1, ]
      return(subResult)
    }

    results <- plyr::dlply(.data = data,
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
    return(result)
  }
}

#' Match by propensity score as well as other covariates
#'
#' @description
#' \code{matchOnPsAndCovariates} uses the provided propensity scores and a set of covariates to match
#' treated to comparator persons.
#'
#' @param data               A data frame with the three columns described below.
#' @param caliper            The caliper for matching. A caliper is the distance which is acceptable
#'                           for any match. Observations which are outside of the caliper are dropped.
#'                           A caliper of 0 means no caliper is used.
#' @param caliperScale       The scale on which the caliper is defined. Two scales are
#'                           supported:\cr\code{caliperScale = 'propensity score'} or
#'                           \code{caliperScale = 'standardized'}. On the standardized scale, the
#'                           caliper is interpreted in standard deviations of the propensity score
#'                           distribution.
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
#' \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator\cr \tab
#' \tab (0) group \cr \verb{propensityScore} \tab(real) \tab Propensity score \cr } This function
#' implements the greedy variable-ratio matching algorithm described in Rassen et al (2012).
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
#' @export
matchOnPsAndCovariates <- function(data,
                                   caliper = 0.25,
                                   caliperScale = "standardized",
                                   maxRatio = 1,
                                   cohortMethodData,
                                   covariateIds) {
  data <- mergeCovariatesWithPs(data, cohortMethodData, covariateIds)
  stratificationColumns <- colnames(data)[colnames(data) %in% paste("covariateId",
                                                                    covariateIds,
                                                                    sep = "_")]
  return(matchOnPs(data, caliper, caliperScale, maxRatio, stratificationColumns))
}

#' Stratify persons by propensity score
#'
#' @description
#' \code{stratifyByPs} uses the provided propensity scores to stratify persons. Additional
#' stratification variables for stratifications can also be used.
#'
#' @param data                    A data frame with the three columns described below
#' @param numberOfStrata          How many strata? The boundaries of the strata are automatically
#'                                defined to contain equal numbers of treated persons.
#' @param stratificationColumns   Names of one or more columns in the \code{data} data.frame on which
#'                                subjects should also be stratified in addition to stratification on
#'                                propensity score.
#'
#' @details
#' The data frame should have the following three columns: \tabular{lll}{ \verb{rowId} \tab(integer)
#' \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment} \tab(integer) \tab
#' Column indicating whether the person is in the treated (1) or comparator\cr \tab \tab (0) group \cr
#' \verb{propensityScore} \tab(real) \tab Propensity score \cr }
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
stratifyByPs <- function(data, numberOfStrata = 5, stratificationColumns = c()) {
  if (!("rowId" %in% colnames(data)))
    stop("Missing column rowId in data")
  if (!("treatment" %in% colnames(data)))
    stop("Missing column treatment in data")
  if (!("propensityScore" %in% colnames(data)))
    stop("Missing column propensityScore in data")

  psStrata <- quantile(data$propensityScore[data$treatment == 1],
                       (1:(numberOfStrata - 1))/numberOfStrata)
  attr(data, "strata") <- psStrata
  if (length(stratificationColumns) == 0) {
    data$stratumId <- as.integer(as.character(cut(data$propensityScore,
                                                  breaks = c(0, psStrata, 1),
                                                  labels = (1:numberOfStrata) - 1)))
    return(data)
  } else {
    f <- function(subset, psStrata, numberOfStrata) {
      subset$stratumId <- as.integer(as.character(cut(subset$propensityScore,
                                                      breaks = c(0, psStrata, 1),
                                                      labels = (1:numberOfStrata) - 1)))
      return(subset)
    }

    results <- plyr::dlply(.data = data,
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
#' @param data               A data frame with the three columns described below
#' @param numberOfStrata     Into how many strata should the propensity score be divided? The
#'                           boundaries of the strata are automatically defined to contain equal
#'                           numbers of treated persons.
#' @param cohortMethodData   An object of type \code{cohortMethodData} as generated using
#'                           \code{getDbCohortMethodData}.
#' @param covariateIds       One or more covariate IDs in the \code{cohortMethodData} object on which
#'                           subjects should also be stratified.
#'
#' @details
#' The data frame should have the following three columns: \tabular{lll}{ \verb{rowId} \tab(integer)
#' \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment} \tab(integer) \tab
#' Column indicating whether the person is in the treated (1) or comparator\cr \tab \tab (0) group \cr
#' \verb{propensityScore} \tab(real) \tab Propensity score \cr }
#'
#' @return
#' Returns a date frame with the same columns as the input data plus one extra column: stratumId.
#' @examples
#' # todo
#'
#' @export
stratifyByPsAndCovariates <- function(data, numberOfStrata = 5, cohortMethodData, covariateIds) {
  data <- mergeCovariatesWithPs(data, cohortMethodData, covariateIds)
  stratificationColumns <- colnames(data)[colnames(data) %in% paste("covariateId",
                                                                    covariateIds,
                                                                    sep = "_")]
  return(stratifyByPs(data, numberOfStrata, stratificationColumns))
}

# A faster version of ffbase::bySum. Doesn't require the bins to be converted to characters
cmBySum <- function(values, bins) {
  .Call("CohortMethod_bySum", PACKAGE = "CohortMethod", values, bins)
}

quickSum <- function(data, squared = FALSE) {
  if (squared) {
    x <- cmBySum(data$covariateValue^2, data$covariateId)
    colnames(x) <- c("covariateId", "sumSqr")
  } else {
    x <- cmBySum(data$covariateValue, data$covariateId)
    colnames(x) <- c("covariateId", "sum")
  }
  x$covariateId <- as.numeric(x$covariateId)
  return(x)
}

computeMeansPerGroup <- function(cohorts, covariates) {
  nOverall <- nrow(cohorts)
  nTreated <- ffbase::sum.ff(cohorts$treatment == 1)
  nComparator <- nOverall - nTreated

  t <- cohorts$treatment == 1
  t <- in.ff(covariates$rowId, cohorts$rowId[ffbase::ffwhich(t, t == TRUE)])
  treated <- quickSum(covariates[ffbase::ffwhich(t, t == TRUE), ])
  treated$meanTreated <- treated$sum/nTreated
  colnames(treated)[colnames(treated) == "sum"] <- "sumTreated"

  t <- cohorts$treatment == 0
  t <- in.ff(covariates$rowId, cohorts$rowId[ffbase::ffwhich(t, t == TRUE)])
  comparator <- quickSum(covariates[ffbase::ffwhich(t, t == TRUE), ])
  comparator$meanComparator <- comparator$sum/nComparator
  colnames(comparator)[colnames(comparator) == "sum"] <- "sumComparator"

  t <- in.ff(covariates$rowId, cohorts$rowId)
  covsInCohorts <- covariates[ffbase::ffwhich(t, t == TRUE), ]
  overall <- quickSum(covsInCohorts)
  overallSqr <- quickSum(covsInCohorts, squared = TRUE)
  overall <- merge(overall, overallSqr)
  overall$sd <- sqrt((overall$sumSqr - (overall$sum^2/nOverall))/nOverall)
  overall <- data.frame(covariateId = overall$covariateId, sd = overall$sd)

  result <- merge(treated, comparator)
  result <- merge(result, overall)
  return(result)
}


#' Compute covariate balance before and after matching and trimming
#'
#' @description
#' For every covariate, prevalence in treatment and comparator groups before and after
#' matching/trimming are computed.
#'
#' @param restrictedCohorts   A data frame containing the people that are remaining after matching
#'                            and/or trimming.
#' @param cohortMethodData    An object of type \code{cohortMethodData} as generated using
#'                            \code{getDbCohortMethodData}.
#' @param outcomeConceptId    The concept ID of the outcome. Persons marked for removal for the outcome
#'                            will be removed when computing the balance before matching/trimming.
#'
#' @details
#' The restrictedCohorts data frame should have at least the following columns: \tabular{lll}{
#' \verb{rowId} \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr
#' \verb{treatment} \tab(integer) \tab Column indicating whether the person is in the treated (1) or
#' comparator (0)\cr \tab \tab group \cr }
#'
#' @return
#' Returns a date frame describing the covariate balance before and after matching/trimming.
#'
#' @export
computeCovariateBalance <- function(restrictedCohorts, cohortMethodData, outcomeConceptId = NULL) {
  if (is.null(outcomeConceptId) | is.null(cohortMethodData$exclude)) {
    cohorts <- cohortMethodData$cohorts
    covariates <- ffbase::subset.ffdf(cohortMethodData$covariates, covariateId != 1)
  } else {
    t <- cohortMethodData$exclude$outcomeId == outcomeConceptId
    t <- in.ff(cohortMethodData$cohorts$rowId,
               cohortMethodData$exclude$rowId[ffbase::ffwhich(t, t == TRUE)])
    cohorts <- cohortMethodData$cohort[ffbase::ffwhich(t, t == FALSE), ]
    t <- cohortMethodData$exclude$outcomeId == outcomeConceptId
    t <- in.ff(cohortMethodData$covariates$rowId,
               cohortMethodData$exclude$rowId[ffbase::ffwhich(t, t == TRUE)])
    t <- t | cohortMethodData$covariates$covariateId == 1
    covariates <- cohortMethodData$covariates[ffbase::ffwhich(t, t == FALSE), ]
  }

  beforeMatching <- computeMeansPerGroup(cohorts, covariates)
  afterMatching <- computeMeansPerGroup(ff::as.ffdf(restrictedCohorts), covariates)

  colnames(beforeMatching)[colnames(beforeMatching) == "meanTreated"] <- "beforeMatchingMeanTreated"
  colnames(beforeMatching)[colnames(beforeMatching) == "meanComparator"] <- "beforeMatchingMeanComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumTreated"] <- "beforeMatchingSumTreated"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumComparator"] <- "beforeMatchingsumComparator"
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
#' @param balance    A data frame created by the \code{computeCovariateBalance} funcion.
#' @param fileName   Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                   function \code{ggsave} in the ggplot2 package for supported file formats.
#'
#' @export
plotCovariateBalanceScatterPlot <- function(balance, fileName = NULL) {
  balance$beforeMatchingStdDiff <- abs(balance$beforeMatchingStdDiff)
  balance$afterMatchingStdDiff <- abs(balance$afterMatchingStdDiff)
  limits <- c(0, max(c(balance$beforeMatchingStdDiff, balance$afterMatchingStdDiff), na.rm = TRUE))
  plot <- ggplot2::ggplot(balance,
                          ggplot2::aes(x = beforeMatchingStdDiff, y = afterMatchingStdDiff)) +
          ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3)) +
          ggplot2::geom_abline(a = 1) +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::ggtitle("Standardized difference of mean") +
          ggplot2::scale_x_continuous("Before matching", limits = limits) +
          ggplot2::scale_y_continuous("After matching", limits = limits)
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 4, height = 4, dpi = 400)
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
#'
#' @export
plotCovariateBalanceOfTopVariables <- function(balance,
                                               n = 20,
                                               maxNameWidth = 100,
                                               fileName = NULL) {
  topBefore <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  topBefore <- topBefore[1:n, ]
  topBefore$facet <- paste("Top", n, "before matching")
  topAfter <- balance[order(-abs(balance$afterMatchingStdDiff)), ]
  topAfter <- topAfter[1:n, ]
  topAfter$facet <- paste("Top", n, "after matching")
  filtered <- rbind(topBefore, topAfter)

  data <- data.frame(covariateId = rep(filtered$covariateId, 2),
                     covariate = rep(filtered$covariateName, 2),
                     difference = c(filtered$beforeMatchingStdDiff, filtered$afterMatchingStdDiff),
                     group = rep(c("before matching", "after matching"), each = nrow(filtered)),
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


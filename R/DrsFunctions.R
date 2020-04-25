# Copyright 2020 Observational Health Data Sciences and Informatics
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

#' Create disease risk scores
#'
#' @description
#' \code{createDrs} creates disease risk scores.
#'
#' @param cohortMethodData         An object of type \code{cohortMethodData} as generated using
#'                                 \code{getDbCohortMethodData}.
#' @param population               A data frame describing the population. This should at least have a
#'                                 'rowId' column corresponding to the rowId column in the
#'                                 \code{cohortMethodData} covariates object and a 'treatment' column.
#'                                 If population is not specified, the full population in the
#'                                 \code{cohortMethodData} will be used.
#' @param coefficients             When using an external disease risk model, the coefficients. See details for
#'                                 requirements.
#' @param modelType                The type of disease risk model. Can be either 'logistic' or 'survival'.
#'
#' @details
#' The \code{coefficients} argument must be a named vector, where the first element is the intercept, and for
#' the other elements the names are the covariate IDs.
#'
#' @export
createDrs <- function(cohortMethodData,
                      population = NULL,
                      coefficients,
                      modelType = "logistic") {
  if (!modelType %in% c("logistic", "survival"))
    stop("Model type should be 'logistic' or 'survival'")

  metaData <- list(drsModelCoef = coefficients,
                   drsModelType = modelType)

  if (missing(population))
    population <- cohortMethodData$cohorts

  intercept <- coefficients[1]
  if (length(coefficients) == 1) {
    population$diseaseRiskScore <- rep(intercept, nrow(population))
  } else {
    coefficients <- coefficients[2:length(coefficients)]
    coefficients <- data.frame(beta = as.numeric(coefficients),
                               covariateId = as.numeric(names(coefficients)))
    prediction <- ffbase::merge.ffdf(cohortMethodData$covariates, ff::as.ffdf(coefficients), by = "covariateId")
    prediction$value <- prediction$covariateValue * prediction$beta
    prediction <- FeatureExtraction::bySumFf(prediction$value, prediction$rowId)
    colnames(prediction) <- c("rowId", "diseaseRiskScore")
    population <- merge(population, prediction, by = "rowId", all.x = TRUE)
    population$diseaseRiskScore[is.na(population$diseaseRiskScore)] <- 0
    population$diseaseRiskScore <- population$diseaseRiskScore + intercept
  }
  if (modelType == "logistic") {
    link <- function(x) {
      return(1/(1 + exp(0 - x)))
    }
    population$diseaseRiskScore <- link(population$diseaseRiskScore)
  } else {
    # survival
    population$diseaseRiskScore <- exp(population$diseaseRiskScore)
  }
  attr(population, "metaData") <- metaData
  return(population)
}

#' Get the disease risk model
#'
#' @description
#' \code{getDrsModel} shows the disease risk model
#'
#' @param diseaseRiskScore    The disease risk scores as generated using the \code{\link{createDrs}} function.
#' @param cohortMethodData   An object of type \code{cohortMethodData} as generated using
#'                           \code{\link{getDbCohortMethodData}}.
#'
#' @details
#' Shows the coefficients and names of the covariates with non-zero coefficients.
#'
#' @export
getDrsModel <- function(diseaseRiskScore, cohortMethodData) {
  coefficients <- attr(diseaseRiskScore, "metaData")$drsModelCoef
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

#' Plot the disease risk score distribution
#'
#' @description
#' \code{plotDrs} shows the disease risk score distribution
#'
#' @param data              A data frame with at least the two columns described below
#' @param targetLabel       A label to us for the target cohort.
#' @param comparatorLabel   A label to us for the comparator cohort.
#' @param title             Optional: the main title for the plot.
#' @param fileName          Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function \code{ggsave} in the ggplot2 package for supported file
#'                          formats.
#'
#' @details
#' The data frame should have a least the following two columns: \tabular{lll}{ \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the target (1) or comparator\cr \tab
#' \tab (0) group \cr \verb{diseaseRiskScore} \tab(numeric) \tab disease risk score \cr }
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @examples
#' treatment <- rep(0:1, each = 100)
#' diseaseRiskScore <- c(rnorm(100, mean = 0.4, sd = 0.25), rnorm(100, mean = 0.6, sd = 0.25))
#' data <- data.frame(treatment = treatment, diseaseRiskScore = diseaseRiskScore)
#' data <- data[data$diseaseRiskScore > 0 & data$diseaseRiskScore < 1, ]
#' plotDrs(data)
#'
#' @export
plotDrs <- function(data,
                    targetLabel = "Target",
                    comparatorLabel = "Comparator",
                    title = NULL,
                    fileName = NULL) {
  if (!("treatment" %in% colnames(data)))
    stop("Missing column treatment in data")
  if (!("diseaseRiskScore" %in% colnames(data)))
    stop("Missing column diseaseRiskScore in data")

  data$group <- as.character(targetLabel)
  data$group[data$treatment == 0] <- as.character(comparatorLabel)
  data$group <- factor(data$group, levels = c(targetLabel, comparatorLabel))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym("diseaseRiskScore"))) +
    ggplot2::geom_density(ggplot2::aes(color = !!rlang::sym("group"),
                                       group = !!rlang::sym("group"),
                                       fill = !!rlang::sym("group"))) +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_log10("Disease risk score") +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.text = ggplot2::element_text(margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm")))
  if (!is.null(attr(data, "strata"))) {
    strata <- data.frame(score = attr(data, "strata"))
    plot <- plot +
      ggplot2::geom_vline(xintercept = strata$score, color = rgb(0, 0, 0, alpha = 0.5))
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
#' \code{computeDrsAuc} computes the area under the ROC curve of the disease risk score within the union of
#' the target and comparator cohorts.
#'
#' @param data                  A data frame with at least the two columns described below
#' @param confidenceIntervals   Compute 95 percent confidence intervals (computationally expensive for
#'                              large data sets)
#' @details
#' The data frame should have a least the following two columns: \tabular{lll}{ \verb{outcomeCount}
#' \tab(integer) \tab Column counting the number of outcomes observed during the time at risk\cr \tab
#' \tab (0) group \cr \verb{diseaseRiskScore} \tab(numeric) \tab disease risk score \cr }
#'
#' @return
#' A data frame holding the AUC and its 95 percent confidence interval
#'
#' @examples
#' outcomeCount <- rep(0:1, each = 100)
#' propensityScore <- c(rnorm(100, mean = 0.4, sd = 0.25), rnorm(100, mean = 0.6, sd = 0.25))
#' data <- data.frame(outcomeCount = outcomeCount, diseaseRiskScore = diseaseRiskScore)
#' data <- data[data$diseaseRiskScore > 0 & data$diseaseRiskScore < 1, ]
#' computeDrsAuc(data)
#'
#' @export
computeDrsAuc <- function(data, confidenceIntervals = FALSE) {
  if (!("outcomeCount" %in% colnames(data)))
    stop("Missing column outcomeCount in data")
  if (!("diseaseRiskScore" %in% colnames(data)))
    stop("Missing column diseaseRiskScore in data")

  y <- as.integer(data$outcomeCount > 0)
  if (confidenceIntervals) {
    auc <- aucWithCi(data$diseaseRiskScore, y)
    return(data.frame(auc = auc[1], auc_lb95ci = auc[2], auc_lb95ci = auc[3]))
  } else {
    auc <- aucWithoutCi(data$diseaseRiskScore, y)
    return(auc)
  }
}


#' Match persons by disease risk score
#'
#' @description
#' \code{matchOnPs} uses the provided disease risk scores to match target to comparator persons.
#'
#' @param population              A data frame with the three columns described below.
#' @param caliper                 The caliper for matching. A caliper is the distance which is
#'                                acceptable for any match. Observations which are outside of the
#'                                caliper are dropped. A caliper of 0 means no caliper is used.
#' @param caliperScale            The scale on which the caliper is defined. Three scales are supported:
#'                                \cr\code{caliperScale = 'disease risk score'}, \code{caliperScale =
#'                                'standardized'}, or \cr\code{caliperScale = 'standardized logit'}.
#'                                On the standardized scale, the caliper is interpreted in standard
#'                                deviations of the disease risk score distribution. 'standardized logit'
#'                                is similar, except that the disease risk score is transformed to the logit
#'                                scale because the DRS is more likely to be normally distributed on that scale.
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
#' \tab (0) group \cr \verb{diseaseRiskScore} \tab(numeric) \tab disease risk score \cr } This function
#' implements the greedy variable-ratio matching algorithm described in Rassen et al (2012).
#'
#'
#' @return
#' Returns a date frame with the same columns as the input data plus one extra column: stratumId. Any
#' rows that could not be matched are removed
#'
#' @examples
#' rowId <- 1:5
#' treatment <- c(1, 0, 1, 0, 1)
#' diseaseRiskScore <- c(0, 0.1, 0.3, 0.4, 1)
#' age_group <- c(1, 1, 1, 1, 1)
#' data <- data.frame(rowId = rowId,
#'                    treatment = treatment,
#'                    diseaseRiskScore = diseaseRiskScore,
#'                    age_group = age_group)
#' result <- matchOnDrs(data, caliper = 0, maxRatio = 1, stratificationColumns = "age_group")
#'
#' @references
#' Rassen JA, Shelat AA, Myers J, Glynn RJ, Rothman KJ, Schneeweiss S. (2012) One-to-many disease risk
#' score matching in cohort studies, Pharmacoepidemiology and Drug Safety, May, 21 Suppl 2:69-80.
#'
#' @export
matchOnDrs <- function(population,
                       caliper = 0.2,
                       caliperScale = "standardized logit",
                       maxRatio = 1,
                       stratificationColumns = c()) {
  if (!("diseaseRiskScore" %in% colnames(population)))
    stop("Missing column diseaseRiskScore in population")
  temp <- population$propensityScore
  population$propensityScore <- population$diseaseRiskScore
  population <- matchOnPs(population = population,
                          caliper = caliper,
                          caliperScale = caliperScale,
                          maxRatio = maxRatio,
                          stratificationColumns = stratificationColumns)
  population$diseaseRiskScore <- population$propensityScore
  population$propensityScore <- temp
  return(population)
}

#' Stratify persons by disease risk score
#'
#' @description
#' \code{stratifyByDrs} uses the provided disease risk scores to stratify persons. Additional
#' stratification variables for stratifications can also be used.
#'
#' @param population              A data frame with the three columns described below
#' @param numberOfStrata          How many strata? The boundaries of the strata are automatically
#'                                defined to contain equal numbers of target persons.
#' @param stratificationColumns   Names of one or more columns in the \code{data} data.frame on which
#'                                subjects should also be stratified in addition to stratification on
#'                                disease risk score.
#' @param baseSelection           What is the base selection of subjects where the strata bounds are
#'                                to be determined? Strata are defined as equally-sized strata inside
#'                                this selection. Possible values are "all", "target", and "comparator".
#'
#' @details
#' The data frame should have the following three columns: \tabular{lll}{ \verb{rowId} \tab(numeric)
#' \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment} \tab(integer) \tab
#' Column indicating whether the person is in the target (1) or comparator\cr \tab \tab (0) group \cr
#' \verb{diseaseRiskScore} \tab(numeric) \tab disease risk score \cr }
#'
#' @return
#' Returns a date frame with the same columns as the input data plus one extra column: stratumId.
#' @examples
#' rowId <- 1:200
#' treatment <- rep(0:1, each = 100)
#' diseaseRiskScore <- c(runif(100, min = 0, max = 1), runif(100, min = 0, max = 1))
#' data <- data.frame(rowId = rowId, treatment = treatment, diseaseRiskScore = diseaseRiskScore)
#' result <- stratifyByDrs(data, 5)
#'
#' @export
stratifyByDrs <- function(population, numberOfStrata = 5, stratificationColumns = c(), baseSelection = "all") {
  if (!("diseaseRiskScore" %in% colnames(population)))
    stop("Missing column diseaseRiskScore in population")
  temp <- population$propensityScore
  population$propensityScore <- population$diseaseRiskScore
  population <- stratifyByPs(population = population,
                             numberOfStrata = numberOfStrata,
                             stratificationColumns = stratificationColumns,
                             baseSelection = baseSelection)
  population$diseaseRiskScore <- population$propensityScore
  population$propensityScore <- temp
  return(population)
}

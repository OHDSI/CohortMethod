# @file Balance.R
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
    covariatesSubset <- covariates[ffbase::`%in%`(covariates$rowId,
                                                  cohorts$rowId[cohorts$treatment == treatment]), ]
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
    personCount <- ffbase::sum.ff(cohorts$treatment == treatment)
    covariatesSubset <- covariates[ffbase::`%in%`(covariates$rowId,
                                                  cohorts$rowId[cohorts$treatment == treatment]), ]
    result <- quickSum(covariatesSubset)
    resultSqr <- quickSum(covariatesSubset, squared = TRUE)
    result <- merge(result, resultSqr)
    result$sd <- sqrt((result$sumSqr - (result$sum^2/personCount))/personCount)
    result$mean <- result$sum/personCount
  }
  return(result)
}

computeMeansPerGroup <- function(cohorts, covariates) {
  # nOverall <- nrow(cohorts)
  # nTarget <- ffbase::sum.ff(cohorts$treatment == 1)
  # nComparator <- nOverall - nTarget

  target <- computeMeanAndSd(cohorts, covariates, treatment = 1)
  colnames(target)[colnames(target) == "sum"] <- "sumTarget"
  colnames(target)[colnames(target) == "mean"] <- "meanTarget"
  colnames(target)[colnames(target) == "sd"] <- "sdTarget"

  comparator <- computeMeanAndSd(cohorts, covariates, treatment = 0)
  colnames(comparator)[colnames(comparator) == "sum"] <- "sumComparator"
  colnames(comparator)[colnames(comparator) == "mean"] <- "meanComparator"
  colnames(comparator)[colnames(comparator) == "sd"] <- "sdComparator"

  result <- merge(target[,
                         c("covariateId", "meanTarget", "sumTarget", "sdTarget")],
                  comparator[,
                             c("covariateId", "meanComparator", "sumComparator", "sdComparator")],
                  all = TRUE)
  result$sd <- sqrt((result$sdTarget^2 + result$sdComparator^2)/2)
  result <- result[, c("covariateId",
                       "meanTarget",
                       "meanComparator",
                       "sumTarget",
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
#' @param subgroupCovariateId  Optional: a covariate ID of a binary covariate that indicates a subgroup of
#'                             interest. Both the before and after populations will be restricted to this
#'                             subgroup before computing covariate balance.
#' @details
#' The population data frame should have at least the following columns: \tabular{lll}{ \verb{rowId}
#' \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr \verb{treatment}
#' \tab(integer) \tab Column indicating whether the person is in the target (1) or comparator (0)\cr
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
computeCovariateBalance <- function(population, cohortMethodData, subgroupCovariateId = NULL) {
  ParallelLogger::logTrace("Computing covariate balance")
  start <- Sys.time()
  cohorts <- ff::as.ffdf(cohortMethodData$cohorts[, c("rowId", "treatment")])
  cohortsAfterMatching <- ff::as.ffdf(population[, c("rowId", "treatment", "stratumId")])
  covariates <- cohortMethodData$covariates
  if (!is.null(subgroupCovariateId)) {
    idx <- covariates$covariateId == subgroupCovariateId
    if (!ffbase::any.ff(idx)) {
      stop("Cannot find covariate with ID ", subgroupCovariateId)
    }
    subGroupRowIds <- covariates$rowId[idx]
    row.names(cohorts) <- NULL
    idx <- ffbase::`%in%`(cohorts$rowId, subGroupRowIds)
    if (!ffbase::any.ff(idx)) {
      stop("Cannot find covariate with ID ", subgroupCovariateId, " in population before matching/trimming")
    }
    cohorts <- cohorts[idx, ]
    sumTreatment <- sum(cohorts$treatment)
    if (sumTreatment == 0 || sumTreatment == nrow(cohorts)) {
      stop("Subgroup population before matching/trimming doesn't have both target and comparator")
    }
    row.names(cohortsAfterMatching) <- NULL
    idx <- ffbase::`%in%`(cohortsAfterMatching$rowId, subGroupRowIds)
    if (!ffbase::any.ff(idx)) {
      stop("Cannot find covariate with ID ", subgroupCovariateId, " in population after matching/trimming")
    }
    cohortsAfterMatching <- cohortsAfterMatching[idx, ]
    sumTreatment <- sum(cohortsAfterMatching$treatment)
    if (sumTreatment == 0 || sumTreatment == nrow(cohortsAfterMatching)) {
      stop("Subgroup population before matching/trimming doesn't have both target and comparator")
    }
  }
  beforeMatching <- computeMeansPerGroup(cohorts, covariates)
  afterMatching <- computeMeansPerGroup(cohortsAfterMatching, covariates)

  ff::close.ffdf(cohorts)
  ff::close.ffdf(cohortsAfterMatching)
  rm(cohorts)
  rm(cohortsAfterMatching)

  colnames(beforeMatching)[colnames(beforeMatching) == "meanTarget"] <- "beforeMatchingMeanTarget"
  colnames(beforeMatching)[colnames(beforeMatching) == "meanComparator"] <- "beforeMatchingMeanComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumTarget"] <- "beforeMatchingSumTarget"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumComparator"] <- "beforeMatchingSumComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sd"] <- "beforeMatchingSd"
  colnames(afterMatching)[colnames(afterMatching) == "meanTarget"] <- "afterMatchingMeanTarget"
  colnames(afterMatching)[colnames(afterMatching) == "meanComparator"] <- "afterMatchingMeanComparator"
  colnames(afterMatching)[colnames(afterMatching) == "sumTarget"] <- "afterMatchingSumTarget"
  colnames(afterMatching)[colnames(afterMatching) == "sumComparator"] <- "afterMatchingSumComparator"
  colnames(afterMatching)[colnames(afterMatching) == "sd"] <- "afterMatchingSd"
  balance <- merge(beforeMatching, afterMatching)
  balance <- merge(balance, ff::as.ram(cohortMethodData$covariateRef))
  balance$covariateName <- as.character(balance$covariateName)
  balance$beforeMatchingStdDiff <- (balance$beforeMatchingMeanTarget - balance$beforeMatchingMeanComparator)/balance$beforeMatchingSd
  balance$afterMatchingStdDiff <- (balance$afterMatchingMeanTarget - balance$afterMatchingMeanComparator)/balance$afterMatchingSd
  balance$beforeMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance$afterMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Computing covariate balance took", signif(delta, 3), attr(delta, "units")))
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
#' @param title       The main title for the plot.
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function \code{ggsave} in the ggplot2 package for supported file formats.
#' @param beforeLabel Label for the x-axis.
#' @param afterLabel  Label for the y-axis.
#' @param showCovariateCountLabel  Show a label with the number of covariates included in the plot?
#' @param showMaxLabel Show a label with the maximum absolute standardized difference after matching/stratification?
#'
#' @export
plotCovariateBalanceScatterPlot <- function(balance,
                                            absolute = TRUE,
                                            threshold = 0,
                                            title = "Standardized difference of mean",
                                            fileName = NULL,
                                            beforeLabel = "Before matching",
                                            afterLabel = "After matching",
                                            showCovariateCountLabel = FALSE,
                                            showMaxLabel = FALSE) {
  beforeLabel <- as.character(beforeLabel)
  afterLabel <- as.character(afterLabel)
  if (absolute) {
    balance$beforeMatchingStdDiff <- abs(balance$beforeMatchingStdDiff)
    balance$afterMatchingStdDiff <- abs(balance$afterMatchingStdDiff)
  }
  limits <- c(min(c(balance$beforeMatchingStdDiff, balance$afterMatchingStdDiff), na.rm = TRUE),
              max(c(balance$beforeMatchingStdDiff, balance$afterMatchingStdDiff), na.rm = TRUE))
  plot <- ggplot2::ggplot(balance,
                          ggplot2::aes(x = beforeMatchingStdDiff, y = afterMatchingStdDiff)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits)
  if (threshold != 0) {
    plot <- plot + ggplot2::geom_hline(yintercept = c(threshold,
                                                      -threshold), alpha = 0.5, linetype = "dotted")
  }
  if (showCovariateCountLabel || showMaxLabel) {
    labels <- c()
    if (showCovariateCountLabel) {
      labels <- c(labels, sprintf("Number of covariates: %s", format(nrow(balance), big.mark = ",", scientific = FALSE)))
    }
    if (showMaxLabel) {
      labels <- c(labels, sprintf("%s max(absolute): %.2f", afterLabel, max(abs(balance$afterMatchingStdDiff), na.rm = TRUE)))
    }
    dummy <- data.frame(text = paste(labels, collapse = "\n"))
    plot <- plot + ggplot2::geom_label(x = limits[1] + 0.01, y = limits[2], hjust = "left", vjust = "top", alpha = 0.8, ggplot2::aes(label = text), data = dummy, size = 3.5)

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
#' @param n              (Maximum) count of variates to plot.
#' @param maxNameWidth   Covariate names longer than this number of characters are truncated to create
#'                       a nicer plot.
#' @param title          Optional: the main title for the plot.
#' @param fileName       Name of the file where the plot should be saved, for example 'plot.png'. See
#'                       the function \code{ggsave} in the ggplot2 package for supported file formats.
#' @param beforeLabel    Label for identifying data before matching / stratification / trimming.
#' @param afterLabel     Label for identifying data after matching / stratification / trimming.
#'
#' @export
plotCovariateBalanceOfTopVariables <- function(balance,
                                               n = 20,
                                               maxNameWidth = 100,
                                               title = NULL,
                                               fileName = NULL,
                                               beforeLabel = "before matching",
                                               afterLabel = "after matching") {
  n <- min(n, nrow(balance))
  beforeLabel <- as.character(beforeLabel)
  afterLabel <- as.character(afterLabel)
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
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 10, height = max(2 + n * 0.2, 5), dpi = 400)
  return(plot)
}


# @file Table1.R
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

#' Get the default table 1 specifications
#'
#' @description
#' Loads the default specifications for a table 1, to be used with the \code{\link{createTable1}}
#' function.
#'
#' Important: currently only works for binary covariates.
#'
#' @return
#' A specifications objects.
#'
#' @export
getDefaultCmTable1Specifications <- function() {
  fileName <- system.file("csv", "Table1Specs.csv", package = "CohortMethod")
  specifications <- read.csv(fileName, stringsAsFactors = FALSE)
  return(specifications)
}

#' Create a table 1
#'
#' @description
#' Creates a formatted table of cohort characteristics, to be included in publications or reports.
#'
#' @param balance                 A data frame created by the \code{computeCovariateBalance} funcion.
#' @param specifications          Specifications of which covariates to display, and how.
#' @param beforeTargetPopSize     The number of people in the target cohort before matching/stratification/trimming,
#'                                to mention in the table header. If not provide, no number will be included in the header.
#' @param beforeComparatorPopSize The number of people in the comparator cohort before matching/stratification/trimming,
#'                                to mention in the table header. If not provide, no number will be included in the header.
#' @param afterTargetPopSize      The number of people in the target cohort after matching/stratification/trimming,
#'                                to mention in the table header. If not provide, no number will be included in the header.
#' @param afterComparatorPopSize  The number of people in the comparator cohort after matching/stratification/trimming,
#'                                to mention in the table header. If not provide, no number will be included in the header.
#' @param beforeLabel             Label for identifying columns before matching / stratification / trimming.
#' @param afterLabel              Label for identifying columns after matching / stratification / trimming.
#' @param targetLabel             Label for identifying columns of the target cohort.
#' @param comparatorLabel         Label for identifying columns of the comparator cohort.
#' @param percentDigits           Number of digits to be used for percentages.
#' @param stdDiffDigits           Number of digits to be used for the standardized differences.
#'
#' @return
#' A data frame with the formatted table 1.
#'
#' @export
createCmTable1 <- function(balance,
                           specifications = getDefaultCmTable1Specifications(),
                           beforeTargetPopSize,
                           beforeComparatorPopSize,
                           afterTargetPopSize,
                           afterComparatorPopSize,
                           beforeLabel = "Before matching",
                           afterLabel = "After matching",
                           targetLabel = "Target",
                           comparatorLabel = "Comparator",
                           percentDigits = 1,
                           stdDiffDigits = 2) {
  fixCase <- function(label) {
    idx <- (toupper(label) == label)
    if (any(idx)) {
      label[idx] <- paste0(substr(label[idx], 1, 1),
                           tolower(substr(label[idx], 2, nchar(label[idx]))))
    }
    return(label)
  }

  formatPercent <- function(x) {
    result <- format(round(100*x, percentDigits), digits = percentDigits+1, justify = "right")
    result <- gsub("NA", "", result)
    result <- gsub(" ", " ", result)
    return(result)
  }

  formatStdDiff <- function(x) {
    result <- format(round(x, stdDiffDigits), digits = stdDiffDigits+1, justify = "right")
    result <- gsub("NA", "", result)
    result <- gsub(" ", " ", result)
    return(result)
  }

  resultsTable <- data.frame()
  for (i in 1:nrow(specifications)) {
    if (specifications$analysisId[i] == "") {
      resultsTable <- rbind(resultsTable,
                            data.frame(Characteristic = specifications$label[i], value = ""))
    } else {
      idx <- balance$analysisId == specifications$analysisId[i]
      if (any(idx)) {
        if (specifications$covariateIds[i] != "") {
          covariateIds <- as.numeric(strsplit(as.character(specifications$covariateIds[i]), ";")[[1]])
          idx <- balance$covariateId %in% covariateIds
        } else {
          covariateIds <- NULL
        }
        if (any(idx)) {
          balanceSubset <- balance[idx, ]
          if (is.null(covariateIds)) {
            balanceSubset <- balanceSubset[order(balanceSubset$covariateId), ]
          } else {
            balanceSubset <- merge(balanceSubset, data.frame(covariateId = covariateIds,
                                                             rn = 1:length(covariateIds)))
            balanceSubset <- balanceSubset[order(balanceSubset$rn,
                                                 balanceSubset$covariateId), ]
          }
          balanceSubset$covariateName <- fixCase(gsub("^.*: ",
                                                      "",
                                                      balanceSubset$covariateName))
          if (specifications$covariateIds[i] == "" || length(covariateIds) > 1) {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           beforeMatchingMeanTarget = NA,
                                                           beforeMatchingMeanComparator = NA,
                                                           beforeMatchingStdDiff = NA,
                                                           afterMatchingMeanTarget = NA,
                                                           afterMatchingMeanComparator = NA,
                                                           afterMatchingStdDiff = NA,
                                                           stringsAsFactors = FALSE))
            resultsTable <- rbind(resultsTable,
                                  data.frame(Characteristic = paste0("  ", balanceSubset$covariateName),
                                             beforeMatchingMeanTarget = balanceSubset$beforeMatchingMeanTarget,
                                             beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                             beforeMatchingStdDiff = balanceSubset$beforeMatchingStdDiff,
                                             afterMatchingMeanTarget = balanceSubset$afterMatchingMeanTarget,
                                             afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                             afterMatchingStdDiff = balanceSubset$afterMatchingStdDiff,
                                             stringsAsFactors = FALSE))
          } else {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           beforeMatchingMeanTarget = balanceSubset$beforeMatchingMeanTarget,
                                                           beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                                           beforeMatchingStdDiff = balanceSubset$beforeMatchingStdDiff,
                                                           afterMatchingMeanTarget = balanceSubset$afterMatchingMeanTarget,
                                                           afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                                           afterMatchingStdDiff = balanceSubset$afterMatchingStdDiff,
                                                           stringsAsFactors = FALSE))
          }
        }
      }
    }
  }
  resultsTable$beforeMatchingMeanTarget <- formatPercent(resultsTable$beforeMatchingMeanTarget)
  resultsTable$beforeMatchingMeanComparator <- formatPercent(resultsTable$beforeMatchingMeanComparator)
  resultsTable$beforeMatchingStdDiff <- formatStdDiff(resultsTable$beforeMatchingStdDiff)
  resultsTable$afterMatchingMeanTarget <- formatPercent(resultsTable$afterMatchingMeanTarget)
  resultsTable$afterMatchingMeanComparator <- formatPercent(resultsTable$afterMatchingMeanComparator)
  resultsTable$afterMatchingStdDiff <- formatStdDiff(resultsTable$afterMatchingStdDiff)

  headerRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(headerRow) <- colnames(resultsTable)
  headerRow$beforeMatchingMeanTarget <- targetLabel
  headerRow$beforeMatchingMeanComparator <- comparatorLabel
  headerRow$afterMatchingMeanTarget <- targetLabel
  headerRow$afterMatchingMeanComparator <- comparatorLabel

  subHeaderRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(subHeaderRow) <- colnames(resultsTable)
  subHeaderRow$Characteristic <- "Characteristic"
  if (missing(beforeTargetPopSize)) {
    subHeaderRow$beforeMatchingMeanTarget <- "%"
  } else {
    subHeaderRow$beforeMatchingMeanTarget <- paste0("% (n = ",format(beforeTargetPopSize, big.mark = ","), ")")
  }
  if (missing(beforeComparatorPopSize)) {
    subHeaderRow$beforeMatchingMeanComparator <- "%"
  } else {
    subHeaderRow$beforeMatchingMeanComparator <- paste0("% (n = ",format(beforeComparatorPopSize, big.mark = ","), ")")
  }
  subHeaderRow$beforeMatchingStdDiff <- "Std. diff"
  if (missing(afterTargetPopSize)) {
    subHeaderRow$afterMatchingMeanTarget <- "%"
  } else {
    subHeaderRow$afterMatchingMeanTarget <- paste0("% (n = ",format(afterTargetPopSize, big.mark = ","), ")")
  }
  if (missing(afterComparatorPopSize)) {
    subHeaderRow$afterMatchingMeanComparator <- "%"
  } else {
    subHeaderRow$afterMatchingMeanComparator <- paste0("% (n = ",format(afterComparatorPopSize, big.mark = ","), ")")
  }
  subHeaderRow$afterMatchingStdDiff <- "Std. diff"
  resultsTable <- rbind(headerRow, subHeaderRow, resultsTable)
  colnames(resultsTable) <- rep("", ncol(resultsTable))
  colnames(resultsTable)[2] <- beforeLabel
  colnames(resultsTable)[5] <- afterLabel
  return(resultsTable)
}

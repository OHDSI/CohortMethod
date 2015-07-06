# @file ComparatorUnderSampling.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' Sample the target or comparator group down
#'
#' @details
#' When the target and/or comparator cohorts are extremely large, it may be more efficient to only use
#' a sample to fit the propensity model. This function creates a new cohortMethodData object with
#' sampled populations.
#'
#' @param cohortMethodData       The original cohortMethodData.
#' @param treatedSampleSize      The sampe size for the treated cohort.
#' @param comparatorSampleSize   The sample size for the comparator cohort.
#'
#' @return
#' An object of type \code{cohortMethodData} with the sampled populations.
#'
#' @export
sampleCohorts <- function(cohortMethodData,
                          treatedSampleSize = 10000,
                          comparatorSampleSize = 20000) {
  comparators <- cohortMethodData$cohorts$rowId[cohortMethodData$cohorts$treatment == 0]
  treated <- cohortMethodData$cohorts$rowId[cohortMethodData$cohorts$treatment == 1]
  treatedSample <- sample(treated, treatedSampleSize)
  comparatorSample <- sample(comparators, comparatorSampleSize)
  allowedRowIds <- ff::as.ff(c(treatedSample, comparatorSample))

  in.ff <- function(a, b) {
    if (length(b) == 0) {
      return(ff::as.ff(rep(FALSE, length(a))))
    } else {
      return(ffbase::ffmatch(x = a, table = b, nomatch = 0L) > 0L)
    }
  }

  t <- in.ff(cohortMethodData$outcomes$rowId, allowedRowIds)
  outcomesSample <- cohortMethodData$outcomes[ffbase::ffwhich(t, t == TRUE), ]

  t <- in.ff(cohortMethodData$cohorts$rowId, allowedRowIds)
  cohortsSample <- cohortMethodData$cohorts[ffbase::ffwhich(t, t == TRUE), ]

  t <- in.ff(cohortMethodData$covariates$rowId, allowedRowIds)
  covariatesSample <- cohortMethodData$covariates[ffbase::ffwhich(t, t == TRUE), ]

  t <- in.ff(cohortMethodData$exclude$rowId, allowedRowIds)
  excludeSample <- cohortMethodData$exclude[ffbase::ffwhich(t, t == TRUE), ]

  cohortMethodDataSample <- list(outcomes = outcomesSample,
                                 cohorts = cohortsSample,
                                 covariates = covariatesSample,
                                 exclude = excludeSample,
                                 covariateRef = ff::clone(cohortMethodData$covariateRef),
                                 metaData = cohortMethodData$metaData)
  class(cohortMethodDataSample) <- "cohortMethodData"
  return(cohortMethodDataSample)
}

#' Sample the comparator group down
#'
#' @details
#' When the comparator group is extremely large, it may be more efficient to only use a sample to fit
#' the propensity model. This function creates a new cohortMethodData object where to comparator group
#' is sampled down to a size relative to the treated group.
#'
#' @param cohortMethodData           The original cohortMethodData.
#' @param comparatorToTreatedRatio   The ratio between comparator and treated group.
#'
#' @return
#' An object of type \code{cohortMethodData} with the sampled populations.
#'
#' @export
sampleComparator <- function(cohortMethodData, comparatorToTreatedRatio = 2) {
  treatedSampleSize <- sum(cohortMethodData$cohorts$treatment)
  comparatorSampleSize <- nrow(cohortMethodData$cohorts) - treatedSampleSize
  comparatorSampleSize <- min(comparatorSampleSize, comparatorToTreatedRatio * treatedSampleSize)
  cohortMethodDataSample <- sampleCohorts(cohortMethodData, treatedSampleSize, comparatorSampleSize)
  return(cohortMethodDataSample)
}

#' Recompute the PS for the full data set
#'
#' @details
#' After using the \code{\link{sampleCohorts}} or \code{\link{sampleComparator}} to reduce the
#' population size, this function can be used to apply a propensity model fitted on the sample to the
#' full data.
#'
#' @param psSample                 The propensity scores as created on the sample data.
#' @param cohortMethodDataSample   The sample data on which the PS model was fitted.
#' @param cohortMethodData         The full data.
#'
#' @return
#' A new propensity score object.
#'
#' @export
recomputePsForFullData <- function(psSample, cohortMethodDataSample, cohortMethodData) {
  # Adjust intercept:
  coefficients <- attr(psSample, "coefficients")
  y.bar <- mean(cohortMethodDataSample$cohorts$treatment)
  y.odds <- y.bar/(1 - y.bar)
  y.bar.new <- mean(cohortMethodData$cohorts$treatment)
  y.odds.new <- y.bar.new/(1 - y.bar.new)
  delta <- log(y.odds) - log(y.odds.new)
  coefficients[1] <- coefficients[1] - delta  # Equation (7) in King and Zeng (2001)

  predictOnFullData <- function(coefficients, cohortMethodData) {
    intercept <- coefficients[1]
    coefficients <- coefficients[2:length(coefficients)]
    coefficients <- data.frame(beta = as.numeric(coefficients),
                               covariateId = as.numeric(names(coefficients)))
    coefficients <- coefficients[coefficients$beta != 0, ]
    prediction <- merge(cohortMethodData$covariates, ff::as.ffdf(coefficients), by = "covariateId")
    prediction$value <- prediction$covariateValue * prediction$beta
    prediction <- cmBySum(prediction$value, prediction$rowId)
    colnames(prediction) <- c("rowId", "value")
    prediction$value <- prediction$value + intercept
    link <- function(x) {
      return(1/(1 + exp(0 - x)))
    }
    prediction$value <- link(prediction$value)
    return(prediction)
  }
  prediction <- predictOnFullData(coefficients, cohortMethodData)
  prediction <- merge(prediction,
                      ffbase::subset.ffdf(cohortMethodData$cohorts, select = c(rowId, treatment)))

  ps <- data.frame(rowId = ff::as.ram(prediction$rowId),
                   propensityScore = ff::as.ram(prediction$value),
                   treatment = ff::as.ram(prediction$treatment))
  attr(ps, "coefficients") <- coefficients
  attr(ps, "priorVariance") <- attr(psSample, "priorVariance")
  return(ps)
}

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

filterCovariates <- function(covariates, covariateRef, covariateFilter) {
  if (is.null(covariateFilter)) {
    return(covariates)
  } else if (is.numeric(covariateFilter)) {
    covariates <- covariates |>
      filter(.data$covariateId %in% covariateFilter)
    return(covariates)
  } else if (is.data.frame(covariateFilter) && all(c("analysisId", "covariateIds") %in% colnames(covariateFilter))) {
    analysisIds <- covariateFilter |>
      filter(is.na(.data$covariateIds)) |>
      pull(.data$analysisId)
    covariateIds1 <- covariateRef |>
      filter(.data$analysisId %in% analysisIds) |>
      pull(.data$covariateId)
    covariateIds2 <- covariateFilter |>
      filter(!is.na(.data$covariateIds)) |>
      pull(.data$covariateIds) |>
      strsplit(",|;") |>
      unlist() |>
      as.numeric()
    covariates <- covariates |>
      filter(.data$covariateId %in% c(covariateIds1, covariateIds2))
    return(covariates)
  } else {
    stop("Unknown covariateFilter type")
  }
}

computeMeansPerGroup <- function(cohorts, cohortMethodData, covariateFilter) {
  hasStrata <- "stratumId" %in% colnames(cohorts)
  hasIptw <- "iptw" %in% colnames(cohorts)

  if (hasStrata) {
    stratumSize <- cohorts |>
      group_by(.data$stratumId, .data$treatment) |>
      count() |>
      ungroup() |>
      collect()
  }

  useWeighting <- (hasStrata && any(stratumSize |> pull(.data$n) > 1)) ||
    (!hasStrata && hasIptw)

  # By definition:
  sumW <- 1

  covariates <- filterCovariates(cohortMethodData$covariates, cohortMethodData$covariateRef, covariateFilter)

  if (useWeighting) {
    if (hasStrata) {
      # Variable strata sizes detected: weigh by size of strata set
      w <- stratumSize |>
        mutate(weight = 1 / .data$n) |>
        inner_join(cohorts, by = c("stratumId", "treatment"), copy = TRUE) |>
        select("rowId", "treatment", "weight")
      # Overall weight is for computing mean and SD across T and C
      overallW <-  stratumSize |>
        group_by(.data$stratumId) |>
        summarise(weight = 1 / sum(.data$n, na.rm = TRUE)) |>
        ungroup() |>
        inner_join(cohorts, by = c("stratumId"), copy = TRUE) |>
        select("rowId", "weight")
    } else {
      w <- cohorts |>
        mutate(weight = .data$iptw) |>
        select("rowId", "treatment", "weight") |>
        collect()
      overallW <- w
    }
    # Normalize so sum(weight) == 1 per treatment arm:
    wSum <- w |>
      group_by(.data$treatment) |>
      summarize(wSum = sum(.data$weight, na.rm = TRUE)) |>
      ungroup()
    overallWSum <- overallW |>
      summarize(overallWSum = sum(.data$weight, na.rm = TRUE)) |>
      pull()

    cohortMethodData$w <- w |>
      inner_join(wSum, by = "treatment") |>
      mutate(weight = .data$weight / .data$wSum) |>
      select("rowId", "treatment", "weight")

    cohortMethodData$overallW <- overallW |>
      mutate(overallWeight = .data$weight / overallWSum) |>
      select("rowId", "overallWeight")

    cohortMethodData$sumWSqr <- cohortMethodData$w |>
      group_by(.data$treatment) |>
      summarise(sumWSqr = sum(.data$weight^2, na.rm = TRUE))

    overallSumWSqr <- cohortMethodData$overallW |>
      summarise(overallSumWSqr = sum(.data$overallWeight^2, na.rm = TRUE)) |>
      pull()

    # Note: using abs() because due to rounding to machine precision number can become slightly negative:
    result <- covariates |>
      inner_join(cohortMethodData$w, by = c("rowId")) |>
      inner_join(cohortMethodData$overallW, by = c("rowId")) |>
      group_by(.data$covariateId, .data$treatment) |>
      summarise(
        sum = sum(as.numeric(.data$covariateValue), na.rm = TRUE),
        mean = sum(.data$weight * as.numeric(.data$covariateValue), na.rm = TRUE),
        overallMean = sum(.data$overallWeight * as.numeric(.data$covariateValue), na.rm = TRUE),
        sumSqr = sum(.data$weight * as.numeric(.data$covariateValue)^2, na.rm = TRUE),
        overallSumSqr = sum(.data$overallWeight * as.numeric(.data$covariateValue)^2, na.rm = TRUE),
        .groups = "drop"
      ) |>
      inner_join(cohortMethodData$sumWSqr, join_by("treatment")) |>
      mutate(
        sd = sqrt(abs(.data$sumSqr - .data$mean^2) * sumW / (sumW^2 - .data$sumWSqr)),
        overallSumWSqr = overallSumWSqr
      ) |>
      ungroup() |>
      select("covariateId", "treatment", "sum", "mean", "sd", "overallMean", "overallSumSqr", "overallSumWSqr") |>
      collect()

    # Compute variance of SDM:
    if (hasStrata) {
      totalStratumSize <- stratumSize |>
        group_by(.data$stratumId) |>
        summarise(nInStratum = sum(.data$n))
      nTotal <- sum(totalStratumSize$nInStratum)

      variances <- covariates |>
        inner_join(cohorts, by = join_by("rowId")) |>
        group_by(.data$treatment, .data$stratumId, .data$covariateId) |>
        summarise(
          sumX = sum(.data$covariateValue, na.rm = TRUE),
          sumXsqr = sum(.data$covariateValue * .data$covariateValue, na.rm = TRUE),
          .groups = "drop"
        ) |>
        inner_join(stratumSize, by = join_by("treatment", "stratumId"), copy = TRUE) |>
        mutate(
          sumX = coalesce(.data$sumX, 0),
          sumXsqr = coalesce(.data$sumXsqr, 0)
        ) |>
        mutate(
          sumSqrDiffs = .data$sumXsqr - (.data$sumX * .data$sumX) / .data$n,
          variance = case_when(
            .data$n <= 1 ~ 0.0,
            TRUE ~ .data$sumSqrDiffs / (.data$n - 1)
          )
        )
      numerators <- variances |>
        inner_join(totalStratumSize, by = join_by("stratumId"), copy = TRUE) |>
        mutate(s = (.data$variance / .data$n) * ((.data$nInStratum / nTotal) ^ 2)) |>
        group_by(.data$covariateId) |>
        summarise(numerator = sum(.data$s, na.rm = TRUE)) |>
        collect()
    } else {
      # Using IPTW:
      numerators <- covariates |>
        inner_join(cohortMethodData$w, by = c("rowId")) |>
        inner_join(result, by = c("covariateId", "treatment"), copy = TRUE) |>
        group_by(.data$covariateId, .data$treatment) |>
        summarise(variance = sum(.data$weight^2 * (.data$covariateValue - .data$mean) ^2, na.rm = TRUE), .groups = "drop") |>
        group_by(.data$covariateId) |>
        summarise(numerator = sum(.data$variance, na.rm = TRUE)) |>
        collect()
    }

    cohortMethodData$w <- NULL
    cohortMethodData$overallW <- NULL
  } else {
    # Don't use weighting
    cohortCounts <- cohorts |>
      group_by(.data$treatment) |>
      count()
    overallCount <- cohorts |>
      count() |>
      pull()

    result <- covariates |>
      inner_join(select(cohorts, "rowId", "treatment"), by = "rowId") |>
      group_by(.data$covariateId, .data$treatment) |>
      summarise(
        sum = sum(as.numeric(.data$covariateValue), na.rm = TRUE),
        sumSqr = sum(as.numeric(.data$covariateValue)^2, na.rm = TRUE),
        overallSumWSqr = sum(1 / overallCount^2, na.rm = TRUE),
        .groups = "drop"
      ) |>
      inner_join(cohortCounts, by = "treatment") |>
      mutate(
        sd = sqrt((.data$sumSqr - (.data$sum^2 / .data$n)) / .data$n),
        mean = .data$sum / .data$n,
        overallMean = .data$sum / overallCount,
        overallSumSqr = .data$sumSqr / overallCount
      ) |>
      ungroup() |>
      select("covariateId", "treatment", "sum", "mean", "sd", "overallMean", "overallSumSqr", "overallSumWSqr") |>
      collect()
  }
  target <- result |>
    filter(.data$treatment == 1) |>
    select("covariateId",
           sumTarget = "sum",
           meanTarget = "mean",
           sdTarget = "sd",
           overallMeanTarget = "overallMean",
           overallSumSqrTarget = "overallSumSqr",
           overallSumWSqrTarget = "overallSumWSqr"
    )

  comparator <- result |>
    filter(.data$treatment == 0) |>
    select("covariateId",
           sumComparator = "sum",
           meanComparator = "mean",
           sdComparator = "sd",
           overallMeanComparator = "overallMean",
           overallSumSqrComparator = "overallSumSqr",
           overallSumWSqrComparator = "overallSumWSqr"
    )

  result <- target |>
    full_join(comparator, by = "covariateId") |>
    mutate(
      sumTarget = coalesce(.data$sumTarget, 0),
      meanTarget = coalesce(.data$meanTarget, 0),
      sdTarget = coalesce(.data$sdTarget, 0),
      overallMeanTarget = coalesce(.data$overallMeanTarget, 0),
      overallSumSqrTarget = coalesce(.data$overallSumSqrTarget, 0),
      overallSumWSqrTarget = coalesce(.data$overallSumWSqrTarget, 0),
      sumComparator = coalesce(.data$sumComparator, 0),
      meanComparator = coalesce(.data$meanComparator, 0),
      sdComparator = coalesce(.data$sdComparator, 0),
      overallMeanComparator = coalesce(.data$overallMeanComparator, 0),
      overallSumSqrComparator = coalesce(.data$overallSumSqrComparator, 0),
      overallSumWSqrComparator = coalesce(.data$overallSumWSqrComparator, 0)
    ) |>
    mutate(mean = .data$overallMeanTarget + .data$overallMeanComparator,
           overallSumSqr = .data$overallSumSqrTarget + .data$overallSumSqrComparator,
           overallSumWSqr = .data$overallSumWSqrTarget + .data$overallSumWSqrComparator) |>
    mutate(
      sd = sqrt(abs(.data$overallSumSqr - .data$mean^2) * sumW / (sumW^2 - .data$overallSumWSqr)),
      denominatorSd = sqrt((.data$sdTarget^2 + .data$sdComparator^2) / 2)
    ) |>
    mutate(
      stdDiff = (meanTarget - meanComparator) / denominatorSd
    )

  if (useWeighting) {
    result <- result |>
      inner_join(numerators, by = join_by("covariateId")) |>
      mutate(sdmVariance = .data$numerator / .data$denominatorSd ^ 2) |>
      select(-"numerator")
  } else {
    cohortCounts <- cohorts |>
      group_by(.data$treatment) |>
      count() |>
      collect()
    count1 <- cohortCounts |>
      filter(.data$treatment == 1) |>
      pull(.data$n)
    count0 <- cohortCounts |>
      filter(.data$treatment == 0) |>
      pull(.data$n)
    result <- result |>
      mutate(sdmVariance = (count1 + count0) / (count1 * count0) + (.data$stdDiff^2) / (2 * (count1 + count0 - 2)))
  }

  result <- result |>
    select(
      -"overallMeanTarget",
      -"overallMeanComparator",
      -"overallSumSqrTarget",
      -"overallSumSqrComparator",
      -"overallSumWSqrTarget",
      -"overallSumWSqrComparator",
      -"denominatorSd"
    )
  return(result)
}

#' Compute covariate balance before and after PS adjustment
#'
#' @description
#' For every covariate, prevalence in treatment and comparator groups before and after
#' matching/trimming/weighting are computed. When variable ratio matching was used
#' the balance score will be corrected according the method described in Austin et
#' al (2008).
#'
#' @template CohortMethodData
#'
#' @param population         A data frame containing the people that are remaining after PS adjustment.
#' @param subgroupCovariateId  Optional: a covariate ID of a binary covariate that indicates a subgroup of
#'                             interest. Both the before and after populations will be restricted to this
#'                             subgroup before computing covariate balance.
#' @param maxCohortSize  If the target or comparator cohort are larger than this number, they
#'                                 will be downsampled before computing covariate balance to save time.
#'                                 Setting this number to 0 means no downsampling will be applied.
#' @param covariateFilter   Determines the covariates for which to compute covariate balance. Either a vector
#'                          of covariate IDs, or a table 1 specifications object as generated for example using
#'                          [FeatureExtraction::getDefaultTable1Specifications()]. If `covariateFilter = NULL`,
#'                          balance will be computed for all variables found in the data.
#' @param threshold   Threshold value for the absolute value of the standardized difference of means (ASDM).
#'                    If the ASDM exceeds this threshold it will be marked as unbalanced. (Hripcsak et al. 2025)
#' @param alpha       The family-wise alpha for testing whether the absolute value of the standardized
#'                    difference of means is greater than the threshold. If not provided, any value greater
#'                    than the threshold will be marked as unbalanced.
#'                    highlighted in the plot.
#' @details
#' The population data frame should have the following three columns:
#'
#' - rowId (numeric): A unique identifier for each row (e.g. the person ID).
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - propensityScore (numeric): Propensity score.
#'
#' @return
#' Returns a tibble describing the covariate balance before and after PS adjustment,
#' with one row per covariate, with the same data as the `covariateRef` table in the `CohortMethodData` object,
#' and the following additional columns:
#'
#' - beforeMatchingMeanTarget: The (weighted) mean value in the target before PS adjustment.
#' - beforeMatchingMeanComparator: The (weighted) mean value in the comparator before PS adjustment.
#' - beforeMatchingSumTarget: The (weighted) sum value in the target before PS adjustment.
#' - beforeMatchingSumComparator: The (weighted) sum value in the comparator before PS adjustment.
#' - beforeMatchingSdTarget: The standard deviation of the value in the target before PS adjustment.
#' - beforeMatchingSdComparator: The standard deviation of the value in the comparator before PS adjustment.
#' - beforeMatchingMean: The mean of the value across target and comparator before PS adjustment.
#' - beforeMatchingSd: The standard deviation of the value across target and comparator before PS adjustment.
#' - beforeMatchingStdDiff: The standardized difference of means when comparing the target to
#'                          the comparator before PS adjustment.
#' - beforeMatchingSdmVariance: The variance of the standardized difference of the means when comparing the target to
#'                          the comparator before PS adjustment.
#' - beforeMatchingSdmP : The P-value for whether abs(beforeMatchingStdDiff) exceeds the threshold.
#' - beforeMatchingBalanced : TRUE if the covariate is considered balanced between the target and comparator before PS
#'                            adjustment (depending on the threshold and alpha settings).
#' - afterMatchingMeanTarget: The (weighted) mean value in the target after PS adjustment.
#' - afterMatchingMeanComparator: The (weighted) mean value in the comparator after PS adjustment.
#' - afterMatchingSumTarget: The (weighted) sum value in the target after PS adjustment.
#' - afterMatchingSumComparator: The (weighted) sum value in the comparator after PS adjustment.
#' - afterMatchingSdTarget: The standard deviation of the value in the target after PS adjustment.
#' - afterMatchingSdComparator: The standard deviation of the value in the comparator after PS adjustment.
#' - afterMatchingMean: The mean of the value across target and comparator after PS adjustment.
#' - afterMatchingSd: The standard deviation of the value across target and comparator after PS adjustment.
#' - afterMatchingStdDiff: The standardized difference of means when comparing the target to
#'                         the comparator after PS adjustment.
#' - afterMatchingSdmVariance: The variance of the standardized difference of the means when comparing the target to
#'                          the comparator after PS adjustment.
#' - afteMatchingSdmP : The P-value for whether abs(beforeMatchingStdDiff) exceeds the threshold.
#' - afteMatchingBalanced : TRUE if the covariate is considered balanced between the target and comparator before PS
#'                            adjustment (depending on the threshold and alpha settings).
#' - targetStdDiff: The standardized difference of means when comparing the target
#'                  before PS adjustment to the target after PS adjustment.
#' - comparatorStdDiff: The standardized difference of means when comparing the comparator
#'                      before PS adjustment to the comparator after PS adjustment.
#'  -targetComparatorStdDiff:  The standardized difference of means when comparing the entire
#'                             population before PS adjustment to the entire population after
#'                             PS adjustment.
#'
#' The 'beforeMatchingStdDiff' and 'afterMatchingStdDiff' columns inform on the balance:
#' are the target and comparator sufficiently similar in terms of baseline covariates to
#' allow for valid causal estimation?
#'
#' The 'targetStdDiff', 'comparatorStdDiff', and 'targetComparatorStdDiff' columns inform on
#' the generalizability: are the cohorts after PS adjustment sufficiently similar to the cohorts
#' before adjustment to allow generalizing the findings to the original cohorts?
#'
#' @references
#' Austin, PC (2008) Assessing balance in measured baseline covariates when using many-to-one
#' matching on the propensity-score. Pharmacoepidemiology and Drug Safety, 17: 1218-1225.
#'
#' Hripcsak G, Zhang L, Chen Y, Li K, Suchard MA, Ryan PB, Schuemie MJ (2025)
#' Assessing Covariate Balance with Small Sample Sizes. medRxiv. Feb 21:2024.04.23.24306230.
#'
#' @export
computeCovariateBalance <- function(population,
                                    cohortMethodData,
                                    subgroupCovariateId = NULL,
                                    maxCohortSize = 250000,
                                    covariateFilter = NULL,
                                    threshold = 0.1,
                                    alpha = 0.05) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(population, add = errorMessages)
  checkmate::assertClass(cohortMethodData, "CohortMethodData", add = errorMessages)
  .assertCovariateId(subgroupCovariateId, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertInt(maxCohortSize, lower = 0, add = errorMessages)
  if (is.numeric(covariateFilter)) {
    checkmate::assertIntegerish(covariateFilter, add = errorMessages)
  } else if (!is.null(covariateFilter)) {
    checkmate::assertDataFrame(covariateFilter, add = errorMessages)
    checkmate::assertNames(colnames(covariateFilter), must.include = c("analysisId", "covariateIds"), add = errorMessages)
  }
  checkmate::assertNumber(threshold, lower = 0, add = errorMessages)
  checkmate::assertNumber(alpha, lower = 0, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  start <- Sys.time()

  if (!is.null(subgroupCovariateId)) {
    subGroupCovariate <- cohortMethodData$covariates |>
      filter(.data$covariateId == subgroupCovariateId) |>
      collect()

    if (nrow(subGroupCovariate) == 0) {
      stop("Cannot find covariate with ID ", subgroupCovariateId)
    }

    tempCohorts <- cohortMethodData$cohorts |>
      collect() |>
      filter(.data$rowId %in% subGroupCovariate$rowId) |>
      sampleCohorts(maxCohortSize = maxCohortSize)

    if (nrow(tempCohorts) == 0) {
      stop("Cannot find covariate with ID ", subgroupCovariateId, " in population before PS adjustment")
    }

    sumTreatment <- sum(tempCohorts$treatment)
    if (sumTreatment == 0 || sumTreatment == nrow(tempCohorts)) {
      stop("Subgroup population before PS adjustment doesn't have both target and comparator")
    }

    tempCohortsAfterMatching <- population |>
      filter(.data$rowId %in% subGroupCovariate$rowId) |>
      sampleCohorts(maxCohortSize = maxCohortSize)

    if (nrow(tempCohortsAfterMatching) == 0) {
      stop("Cannot find covariate with ID ", subgroupCovariateId, " in population after PS adjustment")
    }
    sumTreatment <- sum(tempCohortsAfterMatching$treatment)
    if (sumTreatment == 0 || sumTreatment == nrow(tempCohortsAfterMatching)) {
      stop("Subgroup population before PS adjustment doesn't have both target and comparator")
    }

    cohortMethodData$tempCohorts <- tempCohorts |>
      select("rowId", "treatment")

    cohortMethodData$tempCohortsAfterMatching <- tempCohortsAfterMatching |>
      select("rowId", "treatment", matches("stratumId"), "iptw")
  } else {
    cohortMethodData$tempCohorts <- cohortMethodData$cohorts |>
      select("rowId", "treatment") |>
      sampleCohortsAndromeda(maxCohortSize = maxCohortSize, label = "before PS adjustment")

    cohortMethodData$tempCohortsAfterMatching <- population |>
      select("rowId", "treatment", matches("stratumId"), matches("iptw")) |>
      sampleCohorts(maxCohortSize = maxCohortSize, label = "after PS adjustment")
  }
  on.exit(cohortMethodData$tempCohorts <- NULL)
  on.exit(cohortMethodData$tempCohortsAfterMatching <- NULL, add = TRUE)

  beforeMatching <- computeMeansPerGroup(cohortMethodData$tempCohorts, cohortMethodData, covariateFilter)
  afterMatching <- computeMeansPerGroup(cohorts = cohortMethodData$tempCohortsAfterMatching, cohortMethodData, covariateFilter)

  # Bonferroni:
  if (is.null(alpha)) {
    useAlpha <- FALSE
  } else {
    useAlpha <- TRUE
    correctedAlphaBefore <- alpha / nrow(beforeMatching)
    correctedAlphaAfter <- alpha / nrow(afterMatching)
  }

  beforeMatching <- beforeMatching |>
    mutate(sdmP = computeBalanceP(.data$stdDiff, .data$sdmVariance, threshold)) |>
    mutate(balanced = if (useAlpha) (.data$sdmP >= correctedAlphaBefore) else (abs(.data$stdDiff) <= threshold)) |>
    select("covariateId",
           beforeMatchingMeanTarget = "meanTarget",
           beforeMatchingMeanComparator = "meanComparator",
           beforeMatchingSumTarget = "sumTarget",
           beforeMatchingSumComparator = "sumComparator",
           beforeMatchingSdTarget = "sdTarget",
           beforeMatchingSdComparator = "sdComparator",
           beforeMatchingMean = "mean",
           beforeMatchingSd = "sd",
           beforeMatchingStdDiff = "stdDiff",
           beforeMatchingSdmVariance = "sdmVariance",
           beforeMatchingSdmP = "sdmP",
           beforeMatchingBalanced = "balanced")
  afterMatching <- afterMatching |>
    mutate(sdmP = computeBalanceP(.data$stdDiff, .data$sdmVariance, threshold)) |>
    mutate(balanced = if (useAlpha) (.data$sdmP >= correctedAlphaAfter) else (abs(.data$stdDiff) >= threshold)) |>
    select("covariateId",
           afterMatchingMeanTarget = "meanTarget",
           afterMatchingMeanComparator = "meanComparator",
           afterMatchingSumTarget = "sumTarget",
           afterMatchingSumComparator = "sumComparator",
           afterMatchingSdTarget = "sdTarget",
           afterMatchingSdComparator = "sdComparator",
           afterMatchingMean = "mean",
           afterMatchingSd = "sd",
           afterMatchingStdDiff = "stdDiff",
           afterMatchingSdmVariance = "sdmVariance",
           afterMatchingSdmP = "sdmP",
           afterMatchingBalanced = "balanced",
           matches("overallMean"))

  balance <- beforeMatching |>
    full_join(afterMatching, by = "covariateId") |>
    inner_join(collect(cohortMethodData$covariateRef), by = "covariateId") |>
    inner_join(cohortMethodData$analysisRef |>
                 select("analysisId", "domainId", "isBinary") |>
                 collect() |>
                 mutate(domainId = as.factor(.data$domainId)), by = "analysisId") |>
    mutate(
      targetStdDiff = (.data$beforeMatchingMeanTarget - .data$afterMatchingMeanTarget) / sqrt((.data$beforeMatchingSdTarget^2 + .data$afterMatchingSdTarget^2) / 2),
      comparatorStdDiff = (.data$beforeMatchingMeanComparator - .data$afterMatchingMeanComparator) / sqrt((.data$beforeMatchingSdComparator^2 + .data$afterMatchingSdComparator^2) / 2),
      targetComparatorStdDiff = (.data$beforeMatchingMean - .data$afterMatchingMean) / sqrt((.data$beforeMatchingSd^2 + .data$beforeMatchingSd^2) / 2)
    ) |>
    arrange(desc(abs(.data$beforeMatchingStdDiff)))

  metaData <- attr(population, "metaData")
  if (!is.null(metaData) && !is.null(metaData$targetEstimator)) {
    attr(balance, "targetEstimator") <- metaData$targetEstimator
  }
  delta <- Sys.time() - start
  message(paste("Computing covariate balance took", signif(delta, 3), attr(delta, "units")))
  return(balance)
}

sampleSingleCohort <- function(cohorts, treatment, maxCohortSize) {
  variableStrata <- FALSE
  if ("stratumId" %in% colnames(cohorts)) {
    strataSizes <- cohorts |>
      filter(.data$treatment == !!treatment) |>
      group_by(.data$stratumId) |>
      summarise(count = n())
    variableStrata <- nrow(strataSizes) > 20 && any(strataSizes$count > 1)
  }
  if (variableStrata) {
    # If there are many small and large strata (variable ratio matching), small strata are more likely
    # to be completely deleted by row-based sampling than larger strata, causing bias. Hence we're sampling
    # entire strata to achieve the desired number of rows:
    strataSizes <- strataSizes[sample.int(nrow(strataSizes), replace = F), ]
    stratumIdsToKeep <- strataSizes$stratumId[cumsum(strataSizes$count) <= maxCohortSize]
    idx <- which(cohorts$treatment == !!treatment & cohorts$stratumId %in% stratumIdsToKeep)
  } else {
    idx <- which(cohorts$treatment == !!treatment)
    idx <- sample(idx, maxCohortSize)
  }
  return(idx)
}

sampleCohorts <- function(cohorts, maxCohortSize, label) {
  if (maxCohortSize <= 0) {
    return(cohorts)
  }
  sampled <- FALSE
  targetIdx <- which(cohorts$treatment == 1)
  comparatorIdx <- which(cohorts$treatment == 0)
  targetCount <- length(targetIdx)
  comparatorCount <- length(comparatorIdx)
  if (targetCount > maxCohortSize) {
    targetIdx <- sampleSingleCohort(cohorts, 1, maxCohortSize)
    message(
      "Downsampling target cohort ",
      label,
      " from ",
      targetCount,
      " to ",
      length(targetIdx),
      " before computing covariate balance"
    )
    sampled <- TRUE
  }
  if (comparatorCount > maxCohortSize) {
    comparatorIdx <- sampleSingleCohort(cohorts, 0, maxCohortSize)
    message(
      "Downsampling comparator cohort ",
      label,
      " from ",
      comparatorCount,
      " to ",
      length(comparatorIdx),
      " before computing covariate balance"
    )
    sampled <- TRUE
  }
  if (sampled) {
    return(cohorts[c(targetIdx, comparatorIdx), ])
  } else {
    return(cohorts)
  }
}

sampleCohortsAndromeda <- function(cohorts, maxCohortSize, label) {
  if (maxCohortSize <= 0) {
    return(cohorts)
  }
  cohortCounts <- cohorts |>
    group_by(.data$treatment) |>
    count() |>
    collect()
  if (any(cohortCounts$n > maxCohortSize)) {
    return(sampleCohorts(collect(cohorts), maxCohortSize, label))
  } else {
    return(cohorts)
  }
}

#' Create a scatterplot of the covariate balance
#'
#' @description
#' Create a scatterplot of the covariate balance, showing all variables with balance before and after
#' matching on the x and y axis respectively. Requires running `computeCovariateBalance` first.
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave] function to save to file in a different
#' format.
#'
#' @param balance     A data frame created by the `computeCovariateBalance` function.
#' @param absolute    Should the absolute value of the difference be used?
#' @param threshold   Show a threshold value for after matching standardized difference.
#' @param title       The main title for the plot.
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function `ggsave` in the ggplot2 package for supported file formats.
#' @param beforeLabel Label for the x-axis.
#' @param afterLabel  Label for the y-axis.
#' @param showCovariateCountLabel  Show a label with the number of covariates included in the plot?
#' @param showMaxLabel Show a label with the maximum absolute standardized difference after matching/stratification?
#' @param showUnbalanced Show covariates that are considered unbalanced with a different color?
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
                                            showMaxLabel = FALSE,
                                            showUnbalanced = FALSE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(balance, add = errorMessages)
  checkmate::assertLogical(absolute, len = 1, add = errorMessages)
  checkmate::assertNumber(threshold, lower = 0, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(beforeLabel, len = 1, add = errorMessages)
  checkmate::assertCharacter(afterLabel, len = 1, add = errorMessages)
  checkmate::assertLogical(showCovariateCountLabel, len = 1, add = errorMessages)
  checkmate::assertLogical(showMaxLabel, len = 1, add = errorMessages)
  checkmate::assertLogical(showUnbalanced, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  balance <- balance |>
    filter(!is.na(.data$beforeMatchingStdDiff), !is.na(.data$afterMatchingStdDiff))
  if (absolute) {
    balance$beforeMatchingStdDiff <- abs(balance$beforeMatchingStdDiff)
    balance$afterMatchingStdDiff <- abs(balance$afterMatchingStdDiff)
  }
  limits <- c(
    min(c(balance$beforeMatchingStdDiff, balance$afterMatchingStdDiff), na.rm = TRUE),
    max(c(balance$beforeMatchingStdDiff, balance$afterMatchingStdDiff), na.rm = TRUE)
  )
  if (showUnbalanced) {
    balance <- balance |>
      mutate(balanced = if_else(.data$afterMatchingBalanced, "Balanced", "Unbalanced")) |>
      mutate(balanced = factor(.data$balanced, levels = c("Balanced", "Unbalanced"))) |>
      bind_rows(tibble(balanced =  c("Balanced", "Unbalanced")))
  }

  plot <- ggplot2::ggplot(
    balance,
    ggplot2::aes(x = .data$beforeMatchingStdDiff, y = .data$afterMatchingStdDiff)
  )
  if (showUnbalanced) {
    plot <- plot + ggplot2::geom_point(ggplot2::aes(color = .data$balanced), shape = 16) +
      ggplot2::scale_color_manual(afterLabel, values = c(rgb(0, 0, 0.8, alpha = 0.3), rgb(0.8, 0, 0, alpha = 0.8))) +
      ggplot2::theme(legend.position = "bottom")
  } else {
    plot <- plot + ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16)
  }
  plot <- plot + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits)
  if (threshold != 0) {
    plot <- plot + ggplot2::geom_hline(yintercept = c(
      threshold,
      -threshold
    ), alpha = 0.5, linetype = "dotted")
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
                           sep = ""
  )
  x
}

#' Plot variables with largest imbalance
#'
#' @description
#' Create a plot showing those variables having the largest imbalance before matching, and those
#' variables having the largest imbalance after matching. Requires running
#' `computeCovariateBalance` first.
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave] function to save to file in a different
#' format.
#'
#' @param balance        A data frame created by the `computeCovariateBalance` function.
#' @param n              (Maximum) count of covariates to plot.
#' @param maxNameWidth   Covariate names longer than this number of characters are truncated to create
#'                       a nicer plot.
#' @param title          Optional: the main title for the plot.
#' @param fileName       Name of the file where the plot should be saved, for example 'plot.png'. See
#'                       the function `ggsave` in the ggplot2 package for supported file formats.
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
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(balance, add = errorMessages)
  checkmate::assertInt(n, lower = 1, add = errorMessages)
  checkmate::assertInt(maxNameWidth, lower = 1, add = errorMessages)
  checkmate::assertCharacter(title, null.ok = TRUE, len = 1, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(beforeLabel, len = 1, add = errorMessages)
  checkmate::assertCharacter(afterLabel, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  n <- min(n, nrow(balance))
  topBefore <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  topBefore <- topBefore[1:n, ]
  topBefore$facet <- paste("Top", n, beforeLabel)
  topAfter <- balance[order(-abs(balance$afterMatchingStdDiff)), ]
  topAfter <- topAfter[1:n, ]
  topAfter$facet <- paste("Top", n, afterLabel)
  filtered <- rbind(topBefore, topAfter)

  data <- tibble(
    covariateId = rep(filtered$covariateId, 2),
    covariate = rep(filtered$covariateName, 2),
    difference = c(filtered$beforeMatchingStdDiff, filtered$afterMatchingStdDiff),
    group = rep(c(beforeLabel, afterLabel), each = nrow(filtered)),
    facet = rep(filtered$facet, 2),
    rowId = rep(nrow(filtered):1, 2)
  )
  filtered$covariateName <- .truncRight(as.character(filtered$covariateName), maxNameWidth)
  data$facet <- factor(data$facet, levels = c(paste("Top", n, beforeLabel), paste("Top", n, afterLabel)))
  data$group <- factor(data$group, levels = c(beforeLabel, afterLabel))
  plot <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data$difference,
    y = .data$rowId,
    color = .data$group,
    group = .data$group,
    fill = .data$group,
    shape = .data$group
  )) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_fill_manual(values = c(
      rgb(0.8, 0, 0, alpha = 0.5),
      rgb(0, 0, 0.8, alpha = 0.5)
    )) +
    ggplot2::scale_color_manual(values = c(
      rgb(0.8, 0, 0, alpha = 0.5),
      rgb(0, 0, 0.8, alpha = 0.5)
    )) +
    ggplot2::scale_x_continuous("Standardized difference of mean") +
    ggplot2::scale_y_continuous(breaks = nrow(filtered):1, labels = filtered$covariateName) +
    ggplot2::facet_grid(facet ~ ., scales = "free", space = "free") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 7),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "top",
      legend.direction = "vertical",
      legend.title = ggplot2::element_blank()
    )
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 10, height = max(2 + n * 0.2, 5), dpi = 400)
  }
  return(plot)
}

#' Plot covariate prevalence
#'
#' @description
#' Plot prevalence of binary covariates in the target and comparator cohorts, before and after matching.
#' Requires running `computeCovariateBalance` first.
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave] function to save to file in a different
#' format.
#'
#' @param balance     A data frame created by the `computeCovariateBalance` function.
#' @param threshold   A threshold value for standardized difference. When exceeding the threshold, covariates will be
#'                    marked in a different color. If `threshold = 0`, no color coding will be used.
#' @param title       The main title for the plot.
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function `ggsave` in the ggplot2 package for supported file formats.
#' @param beforeLabel Label for the before matching / stratification panel.
#' @param afterLabel  Label for the after matching / stratification panel.
#' @param targetLabel  Label for the x-axis.
#' @param comparatorLabel Label for the y-axis.
#'
#' @export
plotCovariatePrevalence <- function(balance,
                                    threshold = 0,
                                    title = "Covariate prevalence",
                                    fileName = NULL,
                                    beforeLabel = "Before matching",
                                    afterLabel = "After matching",
                                    targetLabel = "Target",
                                    comparatorLabel = "Comparator") {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(balance, add = errorMessages)
  checkmate::assertNumber(threshold, lower = 0, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(beforeLabel, len = 1, add = errorMessages)
  checkmate::assertCharacter(afterLabel, len = 1, add = errorMessages)
  checkmate::assertCharacter(targetLabel, len = 1, add = errorMessages)
  checkmate::assertCharacter(comparatorLabel, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  balance <- balance |>
    filter(.data$isBinary == "Y")

  prevalence <- bind_rows(
    balance |>
      select(
        target = "beforeMatchingMeanTarget",
        comparator = "beforeMatchingMeanComparator",
        stdDiff = "beforeMatchingStdDiff"
      ) |>
      mutate(panel = beforeLabel),
    balance |>
      select(
        target = "afterMatchingMeanTarget",
        comparator = "afterMatchingMeanComparator",
        stdDiff = "afterMatchingStdDiff"
      ) |>
      mutate(panel = afterLabel)
  ) |>
    mutate(
      target = .data$target * 100,
      comparator = .data$comparator * 100,
      stdDiff = if_else(!is.na(.data$stdDiff) & abs(.data$stdDiff) > threshold,
                        sprintf("> %0.2f", threshold),
                        sprintf("<= %0.2f", threshold)
      )
    )
  prevalence$panel <- factor(prevalence$panel, levels = c(beforeLabel, afterLabel))
  if (threshold > 0) {
    plot <- ggplot2::ggplot(prevalence, ggplot2::aes(x = .data$comparator, y = .data$target, color = .data$stdDiff)) +
      ggplot2::geom_point(alpha = 0.3, shape = 16) +
      ggplot2::scale_color_manual("Std. diff.", values = c(rgb(0, 0, 0.8), rgb(0.8, 0, 0)))
  } else {
    plot <- ggplot2::ggplot(prevalence, ggplot2::aes(x = .data$comparator, y = .data$target)) +
      ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16)
  }
  plot <- plot + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_x_continuous(sprintf("Prevalence in %s (%%)", targetLabel), limits = c(0, 100)) +
    ggplot2::scale_y_continuous(sprintf("Prevalence in %s (%%)", comparatorLabel), limits = c(0, 100)) +
    ggplot2::facet_grid(~ .data$panel)
  if (!is.null(fileName)) {
    ggplot2::ggsave(filename = fileName, plot = plot, width = 8, height = 4, dpi = 400)
  }
  return(plot)
}

#' Get information on generalizability
#'
#' @description
#' to assess generalizability we compare the distribution of covariates before and after
#' any (propensity score) adjustments. We compute the standardized difference of mean as
#' our metric of generalizability. (Lipton et al., 2017)
#'
#' Depending on our target estimand, we need to consider a different base population for
#' generalizability. For example, if we aim to estimate the average treatment effect in
#' thetreated (ATT), our base population should be the target population, meaning we
#' should consider the covariate distribution before and after PS adjustment in the target
#' population only. By default this function will attempt to select the right base
#' population based on what operations have been performed on the population. For example,
#' if PS matching has been performed we assume the target estimand is the ATT, and the
#' target population is selected as base.
#'
#' Requires running [computeCovariateBalance()]` first.
#'
#' @param balance       A data frame created by the `computeCovariateBalance` function.
#' @param baseSelection The selection of the population to consider for generalizability.
#'                      Options are "auto", "target", "comparator", and "both". The "auto"
#'                      option will attempt to use the balance meta-data to pick the most
#'                      appropriate population based on the target estimator.
#'
#' @return
#' A tibble with the following columns:
#'
#' - covariateId: The ID of the covariate. Can be linked to the `covariates` and `covariateRef`
#'   tables in the `CohortMethodData` object.
#' - covariateName: The name of the covariate.
#' - beforeMatchingMean: The mean covariate value before any (propensity score) adjustment.
#' - afterMatchingMean: The mean covariate value after any (propensity score) adjustment.
#' - stdDiff: The standardized difference of means between before and after adjustment.
#'
#' The tibble also has a 'baseSelection' attribute, documenting the base population used
#' to assess generalizability.
#'
#' @references Tipton E, Hallberg K, Hedges LV, Chan W (2017) Implications of Small Samples
#' for Generalization: Adjustments and Rules of Thumb, Eval Rev. Oct;41(5):472-505.
#'
#' @export
getGeneralizabilityTable <- function(balance, baseSelection = "auto") {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(balance, add = errorMessages)
  checkmate::assertCharacter(baseSelection, len = 1, add = errorMessages)
  checkmate::assertChoice(baseSelection, c("auto", "target", "comparator",  "both"), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (baseSelection == "auto") {
    targetEstimator <- attr(balance, "targetEstimator")
    if (is.null(targetEstimator)) {
      stop("The baseSelection is set to 'auto' but the balance object does not contain a target estimator attribute. ",
           "Please set the baseSelection manually.")
    }
    if (targetEstimator == "ate" | targetEstimator == "ato") {
      baseSelection <- "both"
      message("Selecting both target and comparator as base for generalizability")
    } else if (targetEstimator == "att") {
      baseSelection <- "target"
      message("Selecting target as base for generalizability")
    } else if (targetEstimator == "atu") {
      baseSelection <- "comparator"
      message("Selecting comparator as base for generalizability")
    } else {
      stop("Unkown target estimator: ", targetEstimator)
    }
  }
  if (baseSelection == "target") {
    generalizability <- balance |>
      mutate(absGeneralizabilityStdDiff = abs(.data$targetStdDiff)) |>
      arrange(desc(.data$absGeneralizabilityStdDiff)) |>
      select("covariateId",
             "covariateName",
             beforeMatchingMean = "beforeMatchingMeanTarget",
             afterMatchingMean = "afterMatchingMeanTarget",
             stdDiff = "targetStdDiff")
  } else if (baseSelection == "comparator") {
    generalizability <- balance |>
      mutate(absGeneralizabilityStdDiff = abs(.data$comparatorStdDiff)) |>
      arrange(desc(.data$absGeneralizabilityStdDiff)) |>
      select("covariateId",
             "covariateName",
             beforeMatchingMean = "beforeMatchingMeanComparator",
             afterMatchingMean = "afterMatchingMeanComparator",
             stdDiff = "comparatorStdDiff")
  } else {
    generalizability <- balance |>
      mutate(absGeneralizabilityStdDiff = abs(.data$targetComparatorStdDiff)) |>
      arrange(desc(.data$absGeneralizabilityStdDiff)) |>
      select("covariateId",
             "covariateName",
             "beforeMatchingMean",
             "afterMatchingMean",
             stdDiff = "targetComparatorStdDiff")
  }
  attr(generalizability, "baseSelection") <- baseSelection
  return(generalizability)
}

computeBalanceP <- function(sdm, sdmVariance, threshold) {
  p <-pnorm((abs(sdm) - threshold)/sqrt(sdmVariance), lower.tail = FALSE)
  return(p)
}

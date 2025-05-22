library(testthat)
library(CohortMethod)

set.seed(1234)
data(cohortMethodDataSimulationProfile)
sampleSize <- 1000
cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = sampleSize)
# Enforce weighed calculation for after adjustment, but use same weight for
# all so (unweighted) gold standard applies:
studyPop <- cohortMethodData$cohorts |>
  collect() |>
  mutate(iptw = 0.1)

results <- computeCovariateBalance(studyPop, cohortMethodData)

test_that("Test computation of covariate means and SDs", {
  # Too computationally expensive to test all, so randomly pick 5:
  covariateIds <- sample(results$covariateId, 5)
  covariates <- cohortMethodData$covariates |>
    filter(covariateId %in% covariateIds) |>
    compute()
  # covariateId = covariateIds[1]
  for (covariateId in covariateIds) {
    result <- results |>
      filter(covariateId == !!covariateId)
    denseData <- cohortMethodData$cohorts |>
      left_join(covariates |>
                  filter(covariateId == !!covariateId),
                by = join_by("rowId")) |>
      mutate(covariateValue = if_else(is.na(covariateValue), 0, covariateValue))

    # Overall
    gs <- denseData |>
      summarise(mean = mean(covariateValue, na.rm = TRUE),
                sd = sd(covariateValue, na.rm = TRUE)) |>
      collect()
    expect_equal(result$beforeMatchingMean, gs$mean, tolerance = 0.01)
    expect_equal(result$beforeMatchingSd, gs$sd, tolerance = 0.01)
    expect_equal(result$afterMatchingMean, gs$mean, tolerance = 0.01)
    expect_equal(result$afterMatchingSd, gs$sd, tolerance = 0.01)

    # Target
    gs <- denseData |>
      filter(treatment == 1) |>
      summarise(mean = mean(covariateValue, na.rm = TRUE),
                sd = sd(covariateValue, na.rm = TRUE)) |>
      collect()
    expect_equal(result$beforeMatchingMeanTarget, gs$mean, tolerance = 0.01)
    expect_equal(result$beforeMatchingSdTarget, gs$sd, tolerance = 0.01)
    expect_equal(result$afterMatchingMeanTarget, gs$mean, tolerance = 0.01)
    expect_equal(result$afterMatchingSdTarget, gs$sd, tolerance = 0.01)

    # Comparator
    gs <- denseData |>
      filter(treatment == 0) |>
      summarise(mean = mean(covariateValue, na.rm = TRUE),
                sd = sd(covariateValue, na.rm = TRUE)) |>
      collect()
    expect_equal(result$beforeMatchingMeanComparator, gs$mean, tolerance = 0.01)
    expect_equal(result$beforeMatchingSdComparator, gs$sd, tolerance = 0.01)
    expect_equal(result$afterMatchingMeanComparator, gs$mean, tolerance = 0.01)
    expect_equal(result$afterMatchingSdComparator, gs$sd, tolerance = 0.01)
  }
})

test_that("Test computation of SDMs", {
  sdm <- (results$beforeMatchingMeanTarget - results$beforeMatchingMeanComparator) / sqrt((results$beforeMatchingSdTarget^2 + results$beforeMatchingSdComparator^2) / 2)
  expect_equal(results$beforeMatchingStdDiff, sdm)

  sdm <- (results$afterMatchingMeanTarget - results$afterMatchingMeanComparator) / sqrt((results$afterMatchingSdTarget^2 + results$afterMatchingSdComparator^2) / 2)
  expect_equal(results$afterMatchingStdDiff, sdm)

  sdm <- (results$beforeMatchingMean - results$afterMatchingMean) / sqrt((results$beforeMatchingSd^2 + results$beforeMatchingSd^2) / 2)
  expect_equal(results$targetComparatorStdDiff, sdm)

  sdm <- (results$beforeMatchingMeanTarget - results$afterMatchingMeanTarget) / sqrt((results$beforeMatchingSdTarget^2 + results$beforeMatchingSdComparator^2) / 2)
  expect_equal(results$targetStdDiff, sdm)

  sdm <- (results$beforeMatchingMeanComparator - results$afterMatchingMeanComparator) / sqrt((results$beforeMatchingSdTarget^2 + results$beforeMatchingSdComparator^2) / 2)
  expect_equal(results$comparatorStdDiff, sdm)

})

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

  sdm <- (results$beforeMatchingMeanTarget - results$afterMatchingMeanTarget) / sqrt((results$beforeMatchingSdTarget^2 + results$afterMatchingSdTarget^2) / 2)
  expect_equal(results$targetStdDiff, sdm)

  sdm <- (results$beforeMatchingMeanComparator - results$afterMatchingMeanComparator) / sqrt((results$beforeMatchingSdComparator^2 + results$beforeMatchingSdComparator^2) / 2)
  expect_equal(results$comparatorStdDiff, sdm)
})

test_that("Test computation of variance of SDM", {
  # Using simple regression tests. Use code in extras/test-BalanceVariance.R to evaluate nominal
  # operating characteristics using simulations.

  # IPTW:
  set.seed(123)
  studyPop <- cohortMethodData$cohorts |>
    collect() |>
    mutate(iptw = runif(1000))
  balance <- computeCovariateBalance(studyPop, cohortMethodData)
  # paste(head(balance$afterMatchingStdDiff), collapse = ", ")
  gs <- c(-0.216372042798834, 0.196595432223223, 0.205312964768121, -0.15162679819385, 0.18095913605808, -0.142808698304327)
  expect_equal(gs, head(balance$afterMatchingStdDiff))
  # paste(head(balance$afterMatchingSdmVariance), collapse = ", ")
  gs <- c(0.00458034898863292, 0.00579415481209744, 0.0046377887285107, 0.00432054247428411, 0.00600400186186108, 0.00374724096313366)
  expect_equal(gs, head(balance$afterMatchingSdmVariance))

  # No IPTW, no stratifiation:
  # paste(head(balance$beforeMatchingStdDiff), collapse = ", ")
  gs <- c(-0.213387325333441, 0.196321704204979, 0.18566725724897, -0.174003233973345, 0.17107978455366, -0.166666666666667)
  expect_equal(gs, head(balance$beforeMatchingStdDiff))
  # paste(head(balance$beforeMatchingSdmVariance), collapse = ", ")
  gs <- c(0.00413898762167734, 0.00413548464619086, 0.00413344562758986, 0.00413134382148707, 0.00413083839425748, 0.004130091643303)
  expect_equal(gs, head(balance$beforeMatchingSdmVariance))

  # Stratification:
  studyPop <- cohortMethodData$cohorts |>
    collect() |>
    mutate(stratumId = sample.int(5, 1000, replace = TRUE))
  balance <- computeCovariateBalance(studyPop, cohortMethodData)
  # paste(head(balance$afterMatchingStdDiff), collapse = ", ")
  gs <- c(-0.214280130488383, 0.196220426722934, 0.189238870506053, -0.17426283067367, 0.173284579424495, -0.165341228426427)
  expect_equal(gs, head(balance$afterMatchingStdDiff))
  # paste(head(balance$afterMatchingSdmVariance), collapse = ", ")
  gs <- c(0.00348732725723069, 0.00467911528394592, 0.00425835949102957, 0.0040561877804768, 0.00474228169036079, 0.00357227887209997)
  expect_equal(gs, head(balance$afterMatchingSdmVariance))
})

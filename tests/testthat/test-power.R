library(testthat)
library(CohortMethod)

test_that("Logistic regression power calculations", {
  alpha <- 0.05            # Significance level
  power <- 0.80            # Desired power
  p_null <- 0.1            # Baseline probability of event
  n <- 400                 # Sample size

  mdrr <- CohortMethod:::computeMdrrFromAggregateStats(pTarget = NA,
                                                       totalEvents = n * p_null,
                                                       totalSubjects = n,
                                                       alpha = alpha,
                                                       power = power,
                                                       twoSided = TRUE,
                                                       modelType = "logistic")



  # Estimate the coefficient (log(odds ratio))
  beta <- log(mdrr)

  # Estimate the standard error (for a simple logistic model, it's based on p_null and sample size)
  se_beta <- sqrt((1 / (n * p_null * (1 - p_null))))

  # Wald Z statistic
  z_value <- beta / se_beta

  # Determine power using normal distribution
  z_critical <- qnorm(1 - alpha / 2)
  power_achieved <- pnorm(z_value - z_critical)

  expect_equal(power_achieved, power, tolerance = 1e-3)
})

test_that("Exported computeMdrrFromAggregateStats matches computeMdrr", {
  set.seed(123)
  population <- data.frame(
    personSeqId = 1:1000,
    treatment = rbinom(1000, 1, 0.3),
    outcomeCount = rbinom(1000, 1, 0.1),
    timeAtRisk = 365
  )
  for (modelType in c("cox", "logistic")) {
    mdrr <- CohortMethod::computeMdrrFromAggregateStats(
      pTarget = mean(population$treatment),
      totalEvents = sum(population$outcomeCount != 0),
      totalSubjects = nrow(population),
      modelType = modelType
    )
    expect_equal(mdrr, computeMdrr(population, modelType = modelType)$mdrr)
  }
  expect_equal(CohortMethod::computeMdrrFromAggregateStats(pTarget = 0.5, totalEvents = 0, totalSubjects = 100), Inf)
  expect_error(CohortMethod::computeMdrrFromAggregateStats(pTarget = 0.5, totalEvents = 10, totalSubjects = 100, modelType = "poisson"))
})

test_that("computeMdrrFromAggregateStats argument checks", {
  # Zero events give Inf for any model type, as before the function was exported (runCmAnalyses
  # passes the outcome model type, which can be "poisson"):
  expect_equal(CohortMethod::computeMdrrFromAggregateStats(pTarget = 0.5, totalEvents = 0, totalSubjects = 100, modelType = "poisson"), Inf)
  expect_error(CohortMethod::computeMdrrFromAggregateStats(pTarget = NA, totalEvents = 5, totalSubjects = 10), "pTarget")
  expect_error(CohortMethod::computeMdrrFromAggregateStats(pTarget = 0.5, totalEvents = 5, totalSubjects = NA, modelType = "logistic"), "totalSubjects")
  expect_error(CohortMethod::computeMdrrFromAggregateStats(pTarget = 0.5, totalEvents = 10, totalSubjects = 5, modelType = "logistic"), "at least totalEvents")
  expect_error(CohortMethod::computeMdrrFromAggregateStats(pTarget = 0.5, totalEvents = 5.5, totalSubjects = 10), "totalEvents")
})

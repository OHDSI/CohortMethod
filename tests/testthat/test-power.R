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

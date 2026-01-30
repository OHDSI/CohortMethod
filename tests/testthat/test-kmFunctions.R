library(CohortMethod)
library(testthat)
library(survival)


test_that("Unweighted Kaplan-Meier", {
  gold <- survfit(Surv(time, status) ~ 1, data = lung)

  weight <- rep(1, length(lung$status))
  km <- adjustedKm(weight = weight, time = lung$time, y = lung$status - 1)

  expect_equal(km$time, gold$time)
  expect_equal(km$s, gold$surv)
})

test_that("Weighted Kaplan-Meier", {
  set.seed(123)

  unweightedKm <- adjustedKm(weight = rep(1, length(lung$time)), time = lung$time, y = lung$status - 1)
  weight <- runif(n = length(lung$status))
  weightedKm <- adjustedKm(weight = weight, time = lung$time, y = lung$status - 1)

  expect_equal(sum(weightedKm$s == unweightedKm$s), 0) # None of the values are equal

  # if (require("RISCA")) {
  #   risca <- RISCA::ipw.survival(lung$time, lung$status - 1, variable = rep(1, length(lung$status)), weights = weight)
  #   risca <- risca$table.surv
  #
  #   matched <- inner_join(risca, data.frame(times = weightedKm$time, survival = weightedKm$s),
  #                         by = "times")
  #
  #   expect_equal(matched$survival.x, matched$survival.y)
  # }
})

test_that("No error when plotting KM curves and one cohort is empty", {
  population <- tibble(
    rowId = c(1, 2, 3, 4),
    treatment = c(1, 1, 1, 1),
    outcomeCount = c(1, 0, 1, 0),
    stratumId = c(1, 1, 1, 1),
    survivalTime = c(1, 2, 3, 4)
  )
  plot <- plotKaplanMeier(population)
  expect_s3_class(plot, "gtable")
})

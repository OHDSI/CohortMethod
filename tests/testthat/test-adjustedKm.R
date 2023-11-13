library(testthat)
library(CohortMethod)

weights <- seq(100)*10^-3
times <- seq(100)
ys0 <- rep(0, 100)
ys1 <- rep(1, 100)


test_that("Void", {
  expect_error(adjustedKm())
})

# Parameter Sweep
test_that("Minimal", {
  res <- adjustedKm(weight = weights, time = times, y = ys1)

  expect_equal(nrow(res), 100)
  expect_equal(ncol(res), 3)
  expect_equal(names(res), c("time", "s", "var"))
})

test_that("Numeric types", {
  res <- adjustedKm(1, 1, 1)
  expect_equal(res, data.frame(time = 1, s = 0, var = 0))

  res <- adjustedKm(0, 0, 0)
  expect_equal(res, data.frame(time = 0, s = NaN, var = NaN))

  res <- adjustedKm(-1, -1, -1)
  expect_equal(res, data.frame(time = -1, s = 1, var = 0))
})

test_that("weight", {
  expect_error(
    adjustedKm("1", 1, 1),
    "type=character; target=double"
  )

  # Upper bounds
  res <- adjustedKm(2147483648, 1, 1)
  expect_equal(res, data.frame(time = 1, s = 0, var = 0))

  res <- adjustedKm(9*10^300, 1, 1)
  expect_equal(res, data.frame(time = 1, s = 0, var = NaN))

  res <- adjustedKm(9*10^999, 1, 1)
  expect_equal(res, data.frame(time = 1, s = NaN, var = NaN))

  # Lower bounds
  res <- adjustedKm(-2147483648, 1, 1)
  expect_equal(res, data.frame(time = 1, s = 0, var = 0))

  res <- adjustedKm(-9*10^300, 1, 1)
  expect_equal(res, data.frame(time = 1, s = 0, var = NaN))

  res <- adjustedKm(-9*10^999, 1, 1)
  expect_equal(res, data.frame(time = 1, s = NaN, var = NaN))
})

test_that("time", {
  expect_error(
    adjustedKm(1, "1", 1),
    "type=character; target=integer"
  )

  # Upper bounds
  res <- adjustedKm(1, 2147483647, 1)
  expect_equal(res, data.frame(time = 2147483647, s = 0, var = 0))

  expect_warning(adjustedKm(1, 2147483648, 1))

  # Lower bounds
  res <- adjustedKm(1, -2147483647, 1)
  expect_equal(res, data.frame(time = -2147483647, s = 0, var = 0))

  expect_warning(adjustedKm(1, -2147483648, 1))
})

test_that("var", {
  expect_error(
    adjustedKm(1, 1, "1"),
    "type=character; target=integer"
  )

  # Upper bounds
  res <- adjustedKm(1, 1, 2147483647)
  expect_equal(res, data.frame(time = 1, s = 1, var = 0))

  expect_warning(adjustedKm(1, 1, 2147483648))

  # Lower bounds
  res <- adjustedKm(1, 1, -2147483647)
  expect_equal(res, data.frame(time = 1, s = 1, var = 0))

  expect_warning(adjustedKm(1, -2147483648, 1))
})

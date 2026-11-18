library(testthat)
library(CohortMethod)

test_that("enforceMinCellValue works as expected", {
  test_data <- data.frame(
    A = c(1, 2, 3, NA, 0)
  )
  result <- enforceMinCellValue(test_data, "A", 2)
  expect_equal(result$A, c(-2, 2, 3, NA, 0))
})


test_that("minCellValue handles bad inputs", {
  x <- data.frame(a = c(0.1, 0.002, 0))
  res <- enforceMinCellValue(x, "a", NaN)
  checkmate::expect_data_frame(res)
  expect_true(all(is.na(res$a)))
})

test_that("Exported enforceMinCellValue handles per-row minimum values", {
  x <- data.frame(a = c(0.01, 0.02, 0.5, 0))
  res <- CohortMethod::enforceMinCellValue(x, "a", c(0.05, 0.01, 0.6, 0.1), silent = TRUE)
  expect_equal(res$a, c(-0.05, 0.02, -0.6, 0))
  expect_error(CohortMethod::enforceMinCellValue(x, "b", 1), "fieldName")
})

test_that("enforceMinCellValue edge cases", {
  x <- data.frame(a = c(1, 2, 3, 4))
  expect_error(CohortMethod::enforceMinCellValue(x, "a", c(5, 0)), "length 1 or one value per row")

  # Unknown per-row minimum: non-zero values are set to NA, zeros and NAs are kept:
  x <- data.frame(a = c(0.01, 0.5, 0, NA, 0.3))
  res <- CohortMethod::enforceMinCellValue(x, "a", c(0.05, NA, NA, NA, 0.1), silent = TRUE)
  expect_equal(res$a, c(-0.05, NA, 0, NA, 0.3))

  x <- data.frame(a = numeric(0))
  expect_equal(CohortMethod::enforceMinCellValue(x, "a", 5, silent = TRUE), x)

  x <- dplyr::tibble(a = c(1L, 10L, 0L))
  res <- CohortMethod::enforceMinCellValue(x, "a", 2.5, silent = TRUE)
  expect_equal(res$a, c(-2.5, 10, 0))
  res <- CohortMethod::enforceMinCellValue(x, "a", 5, silent = TRUE)
  expect_equal(res$a, c(-5L, 10L, 0L))
})

library("testthat")


test_that("Simple 1-on-1 matching", {
  rowId = 1:5
  treatment = c(1,0,1,0,1)
  propensityScore = c(0,0.1,0.3,0.4,1)
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- psMatch(data, caliper = 0, maxRatio = 1)
  expect_equal(result$stratumId, c(0,0,1,1))
})

test_that("Simple 1-on-n matching", {
  rowId = 1:6
  treatment = c(0,1,0,0,1,0)
  propensityScore = c(0,0.1,0.12,0.85,0.9,1)
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- psMatch(data, caliper = 0, maxRatio = 100)
  expect_equal(result$stratumId, c(0,0,0,1,1,1))
})

test_that("Medium 1-on-n matching", {
  rowId = 1:10000
  treatment = rep(0:1, 5000)
  propensityScore = (1:10000)/10000
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- psMatch(data, caliper = 0, maxRatio = 100)
  expect_equal(max(result$stratumId),4999)
})

test_that("Large 1-on-n matching", {
  rowId = 1:100000
  treatment = rep(0:1, 50000)
  propensityScore = (1:100000)/100000
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- psMatch(data, caliper = 0, maxRatio = 100)
  expect_equal(max(result$stratumId),49999)
})

test_that("Trimming", {
  rowId = 1:200
  treatment = rep(0:1, each = 100)
  propensityScore = rep(1:100,2)/100
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- psTrim(data, 0.05)
  expect_equal(min(result$propensityScore[result$treatment == 0]), 0.06)
  expect_equal(max(result$propensityScore[result$treatment == 1]), 0.95)
  expect_equal(min(result$propensityScore[result$treatment == 1]), 0.01)
  expect_equal(max(result$propensityScore[result$treatment == 0]), 1)
})

test_that("Stratification", {
  rowId = 1:200
  treatment = rep(0:1, each = 100)
  propensityScore = rep(1:100,2)/100
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- psStratify(data, 10)
  
  paste(result$rowId[result$stratumId == 9],collapse=",")
  expect_equal(result$rowId[result$stratumId == 0],c(1,2,3,4,5,6,7,8,9,10,101,102,103,104,105,106,107,108,109,110))
  expect_equal(result$rowId[result$stratumId == 9],c(91,92,93,94,95,96,97,98,99,100,191,192,193,194,195,196,197,198,199,200))
})

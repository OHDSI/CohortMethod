library("testthat")


test_that("Simple 1-on-1 matching", {
  row_id = 1:5
  treatment = c(1,0,1,0,1)
  propensity_score = c(0,0.1,0.3,0.4,1)
  data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
  result <- psMatch(data, caliper = 0, maxRatio = 1)
  expect_equal(result$STRATUM_ID, c(0,0,1,1))
})

test_that("Simple 1-on-n matching", {
  row_id = 1:6
  treatment = c(0,1,0,0,1,0)
  propensity_score = c(0,0.1,0.12,0.85,0.9,1)
  data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
  result <- psMatch(data, caliper = 0, maxRatio = 100)
  expect_equal(result$STRATUM_ID, c(0,0,0,1,1,1))
})

test_that("Simple 1-on-n matching", {
  row_id = 1:5
  treatment = c(0,1,1,1,0)
  propensity_score = row_id/5
  data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
  result <- psMatch(data, caliper = 0, maxRatio = 100)
  expect_equal(result$STRATUM_ID, c(1,1,0,0))
})

test_that("Simple 1-on-n matching", {
  row_id = 1:8
  treatment = c(0,1,0,0,0,0,1,0)
  propensity_score = c(0,0.1,0.11,0.12,0.13,0.85,0.9,1)
  data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
  result <- psMatch(data, caliper = 0, maxRatio = 100)
  expect_equal(result$STRATUM_ID, c(1,0,0,0,0,1,1,1))
})

test_that("Medium 1-on-n matching", {
  row_id = 1:10000
  treatment = rep(0:1, 5000)
  propensity_score = (1:10000)/10000
  data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
  result <- psMatch(data, caliper = 0, maxRatio = 100)
  expect_equal(max(result$STRATUM_ID),4999)
})

test_that("Large 1-on-n matching", {
  row_id = 1:1000000
  treatment = rep(0:1, 500000)
  propensity_score = (1:1000000)/1000000
  data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
  result <- psMatch(data, caliper = 0, maxRatio = 100)
  expect_equal(max(result$STRATUM_ID),499999)
})

test_that("Trimming", {
  row_id = 1:200
  treatment = rep(0:1, each = 100)
  propensity_score = rep(1:100,2)/100
  data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
  result <- psTrim(data, 0.05)
  expect_equal(min(result$PROPENSITY_SCORE[result$TREATMENT == 0]), 0.06)
  expect_equal(max(result$PROPENSITY_SCORE[result$TREATMENT == 1]), 0.95)
  expect_equal(min(result$PROPENSITY_SCORE[result$TREATMENT == 1]), 0.01)
  expect_equal(max(result$PROPENSITY_SCORE[result$TREATMENT == 0]), 1)
})

test_that("Stratification", {
  row_id = 1:200
  treatment = rep(0:1, each = 100)
  propensity_score = rep(1:100,2)/100
  data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
  result <- psStratify(data, 10)
  
  paste(result$ROW_ID[result$STRATUM_ID == 9],collapse=",")
  expect_equal(result$ROW_ID[result$STRATUM_ID == 0],c(1,2,3,4,5,6,7,8,9,10,101,102,103,104,105,106,107,108,109,110))
  expect_equal(result$ROW_ID[result$STRATUM_ID == 9],c(91,92,93,94,95,96,97,98,99,100,191,192,193,194,195,196,197,198,199,200))
})

library("testthat")
library("pROC")


test_that("Simple 1-on-1 matching", {
  rowId <- 1:5
  treatment <- c(1, 0, 1, 0, 1)
  propensityScore <- c(0, 0.1, 0.3, 0.4, 1)
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- matchOnPs(data, caliper = 0, maxRatio = 1)
  expect_equal(result$stratumId, c(0, 0, 1, 1))
})

test_that("Simple 1-on-n matching", {
  rowId <- 1:6
  treatment <- c(0, 1, 0, 0, 1, 0)
  propensityScore <- c(0, 0.1, 0.12, 0.85, 0.9, 1)
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- matchOnPs(data, caliper = 0, maxRatio = 100)
  expect_equal(result$stratumId, c(0, 0, 0, 1, 1, 1))
})

test_that("AUC", {
  ps <- data.frame(propensityScore = runif(100), treatment = round(runif(100)))
  rocobj <- roc(ps$treatment, ps$propensityScore, algorithm = 3)
  goldStandard <- as.numeric(ci(rocobj, method = "delong"))
  auc <- computePsAuc(ps, confidenceIntervals = FALSE)
  aucWithCi <- computePsAuc(ps, confidenceIntervals = TRUE)
  if ((auc < 0.5) != (goldStandard[2] < 0.5)) {
    auc <- 1 - auc
    aucWithCi <- c(1 - aucWithCi[1], 1 - aucWithCi[3], 1 - aucWithCi[2])
  }
  tolerance <- 0.001
  expect_equal(goldStandard[2], auc, tolerance = tolerance)
  expect_equal(goldStandard[2], as.numeric(aucWithCi[1]), tolerance = tolerance)
  expect_equal(goldStandard[1], as.numeric(aucWithCi[2]), tolerance = tolerance)
  expect_equal(goldStandard[3], as.numeric(aucWithCi[3]), tolerance = tolerance)
})

test_that("Simple 1-on-n matching", {
  rowId <- 1:5
  treatment <- c(0, 1, 1, 1, 0)
  propensityScore <- rowId/5
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- matchOnPs(data, caliper = 0, maxRatio = 100)
  expect_equal(result$stratumId, c(1, 1, 0, 0))
})

test_that("Simple 1-on-n matching", {
  rowId <- 1:8
  treatment <- c(0, 1, 0, 0, 0, 0, 1, 0)
  propensityScore <- c(0, 0.1, 0.11, 0.12, 0.13, 0.85, 0.9, 1)
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- matchOnPs(data, caliper = 0, maxRatio = 100)
  expect_equal(result$stratumId, c(1, 0, 0, 0, 0, 1, 1, 1))
})

test_that("Medium 1-on-n matching", {
  rowId <- 1:10000
  treatment <- rep(0:1, 5000)
  propensityScore <- (1:10000)/10000
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- matchOnPs(data, caliper = 0, maxRatio = 100)
  expect_equal(max(result$stratumId), 4999)
})

test_that("Large 1-on-n matching", {
  rowId <- 1:1e+06
  treatment <- rep(0:1, 5e+05)
  propensityScore <- (1:1e+06)/1e+06
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- matchOnPs(data, caliper = 0, maxRatio = 100)
  expect_equal(max(result$stratumId), 499999)
})

test_that("Standardized caliper", {
  rowId <- 1:10000
  treatment <- c(rep(0, 9999), 1)
  propensityScore <- c(rnorm(9999, 0.5, 0.25), 0.8)
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- matchOnPs(data, caliper = 0.2, caliperScale = "standardized", maxRatio = 10000)
  maxDistance <- max(abs(result$propensityScore - 0.8))
  expect_lt(maxDistance, 0.2*sd(propensityScore))
})


test_that("Standardized logit caliper", {
  invLogit <- function(x) {
    exp(x)/(exp(x)+1)
  }
  rowId <- 1:10000
  treatment <- c(rep(0, 9999), 1)
  propensityScore <- invLogit(c(rnorm(9999, 0, 5), 8))
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- matchOnPs(data, caliper = 0.2, caliperScale = "standardized logit", maxRatio = 10000)
  logit <- function(p){
    log(p / (1 - p))
  }
  maxDistance <- max(abs(logit(result$propensityScore) - 8))
  expect_lt(maxDistance, 0.2*sd(logit(propensityScore)))
})

test_that("Trimming", {
  rowId <- 1:200
  treatment <- rep(0:1, each = 100)
  propensityScore <- rep(1:100, 2)/100
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- trimByPs(data, 0.05)
  expect_equal(min(result$propensityScore[result$treatment == 0]), 0.01)
  expect_equal(max(result$propensityScore[result$treatment == 1]), 1)
  expect_equal(min(result$propensityScore[result$treatment == 1]), 0.06)
  expect_equal(max(result$propensityScore[result$treatment == 0]), 0.95)
})

test_that("Stratification", {
  rowId <- 1:200
  treatment <- rep(0:1, each = 100)
  propensityScore <- rep(1:100, 2)/100
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  result <- stratifyByPs(data, 10)

  paste(result$rowId[result$stratumId == 1], collapse = ",")
  expect_equal(result$rowId[result$stratumId == 1],
               c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110))
  expect_equal(result$rowId[result$stratumId == 10], c(91,
                                                       92,
                                                       93,
                                                       94,
                                                       95,
                                                       96,
                                                       97,
                                                       98,
                                                       99,
                                                       100,
                                                       191,
                                                       192,
                                                       193,
                                                       194,
                                                       195,
                                                       196,
                                                       197,
                                                       198,
                                                       199,
                                                       200))
})

test_that("matching with extra variable", {
  rowId <- 1:100
  treatment <- rep(0:1, 50)
  propensityScore <- (1:100)/100
  data <- data.frame(rowId = rowId,
                     treatment = treatment,
                     propensityScore = propensityScore,
                     age = floor(99:0/10))
  result <- matchOnPs(data, caliper = 0, maxRatio = 1, stratificationColumns = c("age"))
  expect_equal(max(result$stratumId), 49)
  for (i in 0:max(result$stratumId)) {
    expect_equal(max(result$age[result$stratumId == i]), min(result$age[result$stratumId == i]))
  }
})

test_that("matching with extra two variables", {
  rowId <- 1:100
  treatment <- rep(0:1, 50)
  propensityScore <- (1:100)/100
  data <- data.frame(rowId = rowId,
                     treatment = treatment,
                     propensityScore = propensityScore,
                     age = floor(99:0/10),
                     gender = rep(c(0, 1), each = 5, times = 10))
  result <- matchOnPs(data, caliper = 0, maxRatio = 1, stratificationColumns = c("age", "gender"))
  expect_equal(max(result$stratumId), 39)
  for (i in 0:max(result$stratumId)) {
    expect_equal(max(result$age[result$stratumId == i]), min(result$age[result$stratumId == i]))
    expect_equal(max(result$gender[result$stratumId == i]), min(result$gender[result$stratumId ==
      i]))
  }
})


test_that("Error messages for wrong input", {
  rowId <- 1:5
  treatment <- c(1, 0, 1, 0, 1)
  propensityScore <- c(0, 0.1, 0.3, 0.4, 1)
  data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
  expect_error(matchOnPs(data, caliper = 0.1, maxRatio = 1, caliperScale = "qwerty"))
  strata <- matchOnPs(data, caliper = 0.25, maxRatio = 1, caliperScale = "propensity score")
  expect_error(plotPs(data, scale = "qwerty"))
  expect_error(plotPs(data, type = "qwerty"))
})



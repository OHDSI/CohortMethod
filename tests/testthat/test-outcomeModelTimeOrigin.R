library(CohortMethod)
library(testthat)
library(survival)

set.seed(123)
data(cohortMethodDataSimulationProfile)
cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = 5000)
createPsArgs <- createCreatePsArgs(prior = createPrior("laplace", variance = 1, exclude = c(0)))

studyPopulation <- function(startAnchor, riskWindowStart) {
  createStudyPopulation(cohortMethodData, outcomeId = 194133,
                        createStudyPopulationArgs = createCreateStudyPopulationArgs(
                          removeSubjectsWithPriorOutcome = TRUE,
                          minDaysAtRisk = 1,
                          riskWindowStart = riskWindowStart,
                          startAnchor = startAnchor,
                          riskWindowEnd = 365,
                          endAnchor = startAnchor
                        ))
}
populations <- function(startAnchor, riskWindowStart) {
  population <- studyPopulation(startAnchor, riskWindowStart)
  ps <- createPs(cohortMethodData, population, createPsArgs = createPsArgs)
  list(population = population,
       ps = ps,
       matched = matchOnPs(ps, matchOnPsArgs = createMatchOnPsArgs(caliper = 0.2, maxRatio = 1)))
}
fit <- function(population, ...) {
  fitOutcomeModel(population, cohortMethodData,
                  fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox", ...))
}
estimate <- function(outcomeModel) { # logRr carries the treatment covariate ID as its name
  e <- outcomeModel$outcomeModelTreatmentEstimate
  c(logRr = unname(e$logRr), logLb95 = unname(e$logLb95), logUb95 = unname(e$logUb95))
}

test_that("Cohort-start time origin matches the default when every risk window opens at the same time", {
  p <- populations("cohort start", 0)
  expect_equal(estimate(fit(p$population, timeOrigin = "cohort start")),
               estimate(fit(p$population)), tolerance = 1e-8)
  expect_equal(estimate(fit(p$matched, stratified = TRUE, timeOrigin = "cohort start")),
               estimate(fit(p$matched, stratified = TRUE)), tolerance = 1e-8)
  expect_equal(estimate(fit(p$ps, inversePtWeighting = TRUE, timeOrigin = "cohort start")),
               estimate(fit(p$ps, inversePtWeighting = TRUE)), tolerance = 1e-8)
})

test_that("Cohort-start time origin is delayed entry, as in coxph", {
  # Anchored to cohort end, risk windows open at different times after cohort start
  p <- populations("cohort end", 1)
  expect_gt(length(unique(p$population$riskStart)), 1)
  reference <- function(population, stratified = FALSE, weights = rep(1, nrow(population))) {
    data <- data.frame(start = population$riskStart - 1,
                       end = population$riskStart - 1 + population$survivalTime,
                       y = as.numeric(population$outcomeCount > 0),
                       treatment = population$treatment,
                       w = weights)
    formula <- Surv(start, end, y) ~ treatment
    if (stratified) {
      data$stratumId <- population$stratumId
      formula <- Surv(start, end, y) ~ treatment + strata(stratumId)
    }
    unname(coef(coxph(formula, data, weights = w, ties = "breslow")))
  }

  delayed <- fit(p$population, timeOrigin = "cohort start")
  expect_equal(unname(estimate(delayed)["logRr"]), reference(p$population), tolerance = 1e-5)
  expect_false(isTRUE(all.equal(estimate(delayed), estimate(fit(p$population)))))

  expect_equal(unname(estimate(fit(p$matched, stratified = TRUE, timeOrigin = "cohort start"))["logRr"]),
               reference(p$matched, stratified = TRUE), tolerance = 1e-5)
  expect_equal(unname(estimate(fit(p$ps, inversePtWeighting = TRUE, timeOrigin = "cohort start"))["logRr"]),
               reference(p$ps, weights = p$ps$iptw), tolerance = 1e-5)
})

test_that("Legacy and efficient Cox fitters agree", {
  p <- populations("cohort start", 0)
  expectSameFits <- function(population, ...) {
    efficient <- fit(population, ...)
    legacy <- fit(population, coxFitter = "legacy", ...)
    expect_equal(estimate(legacy), estimate(efficient), tolerance = 1e-6)
    expect_equal(legacy$logLikelihoodProfile, efficient$logLikelihoodProfile, tolerance = 1e-6)
    expect_equal(legacy$outcomeModelCoxFitter, "legacy")
  }
  expectSameFits(p$population)
  expectSameFits(p$matched, stratified = TRUE)
  expectSameFits(p$ps, inversePtWeighting = TRUE)
  expectSameFits(p$matched, stratified = TRUE, useCovariates = TRUE,
                 prior = createPrior("laplace", variance = 0.1))
})

test_that("Time origin and Cox fitter settings are checked", {
  expect_error(createFitOutcomeModelArgs(modelType = "logistic", timeOrigin = "cohort start"), "Cox")
  expect_error(createFitOutcomeModelArgs(modelType = "poisson", coxFitter = "legacy"), "Cox")
  expect_error(createFitOutcomeModelArgs(timeOrigin = "cohort start", coxFitter = "legacy"), "efficient")
  expect_error(createFitOutcomeModelArgs(timeOrigin = "index"))
  expect_error(createFitOutcomeModelArgs(coxFitter = "fast"))

  population <- studyPopulation("cohort start", 0)
  population$riskStart <- NULL
  expect_error(fit(population, timeOrigin = "cohort start"), "riskStart")
})

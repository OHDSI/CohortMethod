library(CohortMethod)
library(testthat)
library(survival)

# A weighted population where some persons have several rows, so clustering on personSeqId matters
set.seed(123)
nPersons <- 300
personSeqId <- rep(seq_len(nPersons), sample(1:3, nPersons, replace = TRUE))
n <- length(personSeqId)
frailty <- rnorm(nPersons, sd = 0.7)[personSeqId]
treatment <- rbinom(n, 1, 0.4)
eventTime <- rexp(n, exp(-5 + 0.3 * treatment + frailty))
censoringTime <- runif(n, 50, 400)
population <- data.frame(
  rowId = seq_len(n),
  personSeqId = personSeqId,
  treatment = treatment,
  outcomeCount = as.integer(eventTime <= censoringTime),
  survivalTime = pmax(1, round(pmin(eventTime, censoringTime))),
  iptw = runif(n, 0.5, 3),
  stratumId = rep(1:30, length.out = n)
)
population$timeAtRisk <- population$survivalTime

robustSupported <- "method" %in% names(formals(utils::getS3method("confint", "cyclopsFit")))

fitEstimate <- function(population, ...) {
  outcomeModel <- fitOutcomeModel(
    population = population,
    fitOutcomeModelArgs = createFitOutcomeModelArgs(...)
  )
  expect_equal(outcomeModel$outcomeModelStatus, "OK")
  estimate <- outcomeModel$outcomeModelTreatmentEstimate
  estimate$logRr <- unname(estimate$logRr)
  return(estimate)
}

test_that("Robust variance of the Cox model matches coxph(cluster = )", {
  skip_if_not(robustSupported, "Cyclops without vcov(type = \"robust\")")
  estimate <- fitEstimate(population, modelType = "cox", inversePtWeighting = TRUE, varianceEstimator = "robust")
  reference <- coxph(Surv(survivalTime, outcomeCount) ~ treatment, data = population, weights = iptw,
                     cluster = personSeqId, ties = "breslow")
  expect_equal(estimate$logRr, unname(coef(reference)), tolerance = 1e-6)
  expect_equal(estimate$seLogRr, sqrt(reference$var[1, 1]), tolerance = 1e-6)
  # Wald interval
  expect_equal(estimate$logLb95, estimate$logRr - qnorm(0.975) * estimate$seLogRr, tolerance = 1e-10)
  expect_equal(estimate$logUb95, estimate$logRr + qnorm(0.975) * estimate$seLogRr, tolerance = 1e-10)
  # The model-based variance treats the weights as frequencies
  profile <- fitEstimate(population, modelType = "cox", inversePtWeighting = TRUE)
  expect_equal(profile$logRr, estimate$logRr)
  expect_lt(profile$seLogRr, estimate$seLogRr)
})

test_that("Robust variance of the stratified Cox model matches coxph(strata, cluster = )", {
  skip_if_not(robustSupported, "Cyclops without vcov(type = \"robust\")")
  estimate <- fitEstimate(population, modelType = "cox", stratified = TRUE, inversePtWeighting = TRUE,
                          varianceEstimator = "robust")
  reference <- coxph(Surv(survivalTime, outcomeCount) ~ treatment + strata(stratumId), data = population,
                     weights = iptw, cluster = personSeqId, ties = "breslow")
  expect_equal(estimate$logRr, unname(coef(reference)), tolerance = 1e-6)
  expect_equal(estimate$seLogRr, sqrt(reference$var[1, 1]), tolerance = 1e-6)
})

test_that("Robust variance of the logistic and Poisson models matches sandwich::vcovCL()", {
  skip_if_not(robustSupported, "Cyclops without vcov(type = \"robust\")")
  skip_if_not_installed("sandwich")
  estimate <- fitEstimate(population, modelType = "logistic", inversePtWeighting = TRUE, varianceEstimator = "robust")
  reference <- suppressWarnings(glm(outcomeCount ~ treatment, data = population, weights = iptw, family = binomial))
  referenceVcov <- sandwich::vcovCL(reference, cluster = ~personSeqId, type = "HC0", cadjust = FALSE)
  expect_equal(estimate$logRr, unname(coef(reference)[2]), tolerance = 1e-6)
  expect_equal(estimate$seLogRr, sqrt(referenceVcov[2, 2]), tolerance = 1e-6)

  estimate <- fitEstimate(population, modelType = "poisson", inversePtWeighting = TRUE, varianceEstimator = "robust")
  reference <- glm(outcomeCount ~ treatment + offset(log(timeAtRisk)), data = population, weights = iptw,
                   family = poisson)
  referenceVcov <- sandwich::vcovCL(reference, cluster = ~personSeqId, type = "HC0", cadjust = FALSE)
  expect_equal(estimate$logRr, unname(coef(reference)[2]), tolerance = 1e-5)
  expect_equal(estimate$seLogRr, sqrt(referenceVcov[2, 2]), tolerance = 1e-5)
})

test_that("Robust variance of main effects and interactions matches coxph(cluster = )", {
  skip_if_not(robustSupported, "Cyclops without vcov(type = \"robust\")")
  set.seed(1234)
  data(cohortMethodDataSimulationProfile)
  cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = 1000)
  on.exit(close(cohortMethodData))
  studyPop <- createStudyPopulation(
    cohortMethodData = cohortMethodData,
    outcomeId = 194133,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(minDaysAtRisk = 1)
  )
  # Pairs of rows share a person, so the clustering matters
  studyPop$personSeqId <- (studyPop$personSeqId + 1) %/% 2
  expect_gt(anyDuplicated(studyPop$personSeqId), 0)
  covariateId <- 8532001 # Female
  outcomeModel <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = cohortMethodData,
    fitOutcomeModelArgs = createFitOutcomeModelArgs(
      modelType = "cox",
      interactionCovariateIds = covariateId,
      varianceEstimator = "robust"
    )
  )
  expect_equal(outcomeModel$outcomeModelStatus, "OK")
  x <- cohortMethodData$covariates |>
    dplyr::filter(.data$covariateId == !!covariateId) |>
    dplyr::collect()
  studyPop$x <- 0
  studyPop$x[studyPop$rowId %in% x$rowId] <- 1
  studyPop$y <- as.integer(studyPop$outcomeCount > 0)
  reference <- coxph(Surv(survivalTime, y) ~ treatment + x + treatment:x, data = studyPop,
                     cluster = personSeqId, ties = "breslow")
  referenceSe <- sqrt(diag(reference$var))
  expect_equal(outcomeModel$outcomeModelTreatmentEstimate$seLogRr, unname(referenceSe[1]), tolerance = 1e-6)
  expect_equal(outcomeModel$outcomeModelmainEffectEstimates$seLogRr, unname(referenceSe[2]), tolerance = 1e-6)
  expect_equal(outcomeModel$outcomeModelInteractionEstimates$logRr, unname(coef(reference)[3]), tolerance = 1e-6)
  expect_equal(outcomeModel$outcomeModelInteractionEstimates$seLogRr, unname(referenceSe[3]), tolerance = 1e-6)

  # Unregularized covariates in the outcome model
  studyPop$y <- NULL
  outcomeModel <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = cohortMethodData,
    fitOutcomeModelArgs = createFitOutcomeModelArgs(
      modelType = "cox",
      useCovariates = TRUE,
      includeCovariateIds = covariateId,
      prior = createPrior("none"),
      varianceEstimator = "robust"
    )
  )
  expect_equal(outcomeModel$outcomeModelStatus, "OK")
  reference <- coxph(Surv(survivalTime, outcomeCount > 0) ~ treatment + x, data = studyPop, cluster = personSeqId,
                     ties = "breslow")
  expect_equal(unname(outcomeModel$outcomeModelTreatmentEstimate$logRr), unname(coef(reference)[1]), tolerance = 1e-6)
  expect_equal(outcomeModel$outcomeModelTreatmentEstimate$seLogRr, sqrt(reference$var[1, 1]), tolerance = 1e-6)
})

test_that("The bootstrap resamples persons", {
  getCyclopsBootstrap <- function(population, subjects) {
    informativePopulation <- CohortMethod:::getInformativePopulation(
      population = population,
      stratified = FALSE,
      inversePtWeighting = TRUE,
      modelType = "cox",
      addSubjectId = subjects
    )
    cyclopsData <- Cyclops::convertToCyclopsData(
      outcomes = informativePopulation,
      covariates = data.frame(rowId = informativePopulation$rowId,
                              covariateId = 1,
                              covariateValue = informativePopulation$treatment),
      modelType = "cox",
      quiet = TRUE
    )
    fit <- Cyclops::fitCyclopsModel(cyclopsData, control = createFitOutcomeModelArgs()$control)
    return(Cyclops::runBootstrap(fit, 50)$summary)
  }
  estimate <- fitEstimate(population, modelType = "cox", inversePtWeighting = TRUE, varianceEstimator = "bootstrap",
                          bootstrapReplicates = 50)
  reference <- getCyclopsBootstrap(population, subjects = TRUE)
  expect_equal(estimate$seLogRr, reference$std_err)
  expect_equal(c(estimate$logLb95, estimate$logUb95), c(reference$bpi_lower, reference$bpi_upper))
  # Persons have several rows, so resampling rows gives a different answer
  rowReference <- getCyclopsBootstrap(population, subjects = FALSE)
  expect_gt(abs(estimate$seLogRr - rowReference$std_err), 1e-3)

  # A Cyclops that cannot resample persons resamples rows, with a warning
  local({
    local_mocked_bindings(cyclopsResamplesSubjects = function() FALSE, .package = "CohortMethod")
    expect_warning(
      estimate <- fitEstimate(population, modelType = "cox", inversePtWeighting = TRUE,
                              varianceEstimator = "bootstrap", bootstrapReplicates = 50),
      "resamples rows"
    )
    expect_equal(estimate$seLogRr, rowReference$std_err)
  })

  # With one row per person, the same as the bootstrap by row of earlier versions
  onePerPerson <- population[!duplicated(population$personSeqId), ]
  estimate <- fitEstimate(onePerPerson, modelType = "cox", inversePtWeighting = TRUE, bootstrapCi = TRUE,
                          bootstrapReplicates = 50)
  reference <- getCyclopsBootstrap(onePerPerson, subjects = FALSE)
  expect_equal(estimate$seLogRr, reference$std_err)
  expect_equal(c(estimate$logLb95, estimate$logUb95), c(reference$bpi_lower, reference$bpi_upper))
})

test_that("The bootstrap of stratified models", {
  for (modelType in c("cox", "logistic")) {
    estimate <- fitEstimate(population, modelType = modelType, stratified = TRUE, varianceEstimator = "bootstrap",
                            bootstrapReplicates = 50)
    expect_true(is.finite(estimate$seLogRr))
    expect_lt(estimate$logLb95, estimate$logRr)
    expect_gt(estimate$logUb95, estimate$logRr)
  }
})

test_that("Outcome models record their variance estimator", {
  skip_if_not(robustSupported, "Cyclops without vcov(type = \"robust\")")
  outcomeModel <- fitOutcomeModel(population, fitOutcomeModelArgs = createFitOutcomeModelArgs(varianceEstimator = "robust"))
  expect_equal(outcomeModel$outcomeModelVarianceEstimator, "robust")
  expect_message(print(outcomeModel), "Variance estimator: robust")

  # A settings object saved before varianceEstimator was added
  for (bootstrapCi in c(FALSE, TRUE)) {
    oldSettings <- createFitOutcomeModelArgs(bootstrapCi = bootstrapCi, bootstrapReplicates = 50)
    oldSettings$varianceEstimator <- NULL
    outcomeModel <- fitOutcomeModel(population, fitOutcomeModelArgs = oldSettings)
    expect_equal(outcomeModel$outcomeModelStatus, "OK")
    expect_equal(outcomeModel$outcomeModelVarianceEstimator, if (bootstrapCi) "bootstrap" else "profile")
  }
})

test_that("The robust variance stops on a Cyclops without it", {
  local_mocked_bindings(cyclopsHasRobustVariance = function() FALSE, .package = "CohortMethod")
  expect_error(
    fitOutcomeModel(population, fitOutcomeModelArgs = createFitOutcomeModelArgs(varianceEstimator = "robust")),
    "requires a version of Cyclops"
  )
})

test_that("varianceEstimator settings", {
  expect_equal(createFitOutcomeModelArgs()$varianceEstimator, "profile")
  expect_false(createFitOutcomeModelArgs()$bootstrapCi)

  # bootstrapCi is kept as the old name of varianceEstimator = "bootstrap"
  settings <- createFitOutcomeModelArgs(bootstrapCi = TRUE)
  expect_equal(settings$varianceEstimator, "bootstrap")
  settings <- createFitOutcomeModelArgs(varianceEstimator = "bootstrap")
  expect_true(settings$bootstrapCi)
  expect_error(createFitOutcomeModelArgs(bootstrapCi = TRUE, varianceEstimator = "robust"), "bootstrapCi")
  expect_error(createFitOutcomeModelArgs(varianceEstimator = "sandwich"), "varianceEstimator")
  # Positional arguments keep their meaning: the 5th is bootstrapCi
  expect_equal(createFitOutcomeModelArgs("cox", FALSE, FALSE, TRUE, TRUE)$varianceEstimator, "bootstrap")

  expect_error(createFitOutcomeModelArgs(modelType = "logistic", stratified = TRUE, varianceEstimator = "robust"),
               "stratified")
  expect_error(createFitOutcomeModelArgs(modelType = "poisson", stratified = TRUE, varianceEstimator = "robust"),
               "stratified")
  expect_error(createFitOutcomeModelArgs(useCovariates = TRUE, varianceEstimator = "robust"), "regularized")
  settings <- createFitOutcomeModelArgs(useCovariates = TRUE, varianceEstimator = "robust",
                                        prior = createPrior("none"))
  expect_equal(settings$varianceEstimator, "robust")

  settings <- createFitOutcomeModelArgs(modelType = "cox", stratified = TRUE, varianceEstimator = "robust")
  settings2 <- FitOutcomeModelArgs$new(json = settings$toJson())
  expect_equal(settings, settings2, tolerance = 0.0001)

  # Settings saved before varianceEstimator was added
  for (bootstrapCi in c(FALSE, TRUE)) {
    oldSettings <- createFitOutcomeModelArgs(bootstrapCi = bootstrapCi)$toList()
    oldSettings$varianceEstimator <- NULL
    settings <- FitOutcomeModelArgs$new(json = jsonlite::toJSON(oldSettings, auto_unbox = TRUE, null = "null"))
    expect_equal(settings$varianceEstimator, if (bootstrapCi) "bootstrap" else "profile")
  }
})

library(CohortMethod)
library(testthat)
library(dplyr)

test_that("population", {
  ## Defaults ----
  mod <- fitOutcomeModel(
    population = studyPop
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Wrong data ----
  expect_error(
    fitOutcomeModel(
      population = iris
    ),
    "Names must include the elements"
  )

  ## 0 rows ----
  emptyPop <- studyPop %>%
    filter(row_number() == 0)

  expect_output(
    fitOutcomeModel(
      population = emptyPop
    ),
    "NO SUBJECTS IN POPULATION, CANNOT FIT"
  )

  ## 1 rows ----
  expect_output(
    fitOutcomeModel(
      population = studyPop[sample(nrow(studyPop), 1), ]
    ),
    "NO OUTCOMES FOUND FOR POPULATION, CANNOT FIT"
  )
  # n >= 2 rows will randomly fit, likeliness increases with greater n rows.

  ## Not data.frame ----
  expect_error(
    fitOutcomeModel(
      population = "studyPop"
    ),
    "not 'character'"
  )

  ## Minimal columns ----
  mod <- fitOutcomeModel(
    population = studyPop[, c(
      "rowId", "outcomeCount", "treatment", "timeAtRisk", "personSeqId")]
  )

  expect_s3_class(mod, "OutcomeModel")

  ## random columns ----
  expect_error(
    fitOutcomeModel(
      population = studyPop[, c(2:7)]
    ),
    "Names must include the elements"
  )
})

test_that("cohortMethodData", {
  ## Defaults ----
  mod <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Wrong data ----
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = iris
    ),
    "Must inherit from class 'CohortMethodData'"
  )

  ## Empty CohortMethodData ----
  empty <- list()
  class(empty) <- "CohortMethodData"

  expect_error(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = empty
    ),
    "Must have names"
  )

  ## Malformed CohortMethodData ----
  malformed <- list(
    analysisRef = sCohortMethodData$analysisRef,
    cohorts = sCohortMethodData$cohorts
  )

  class(malformed) <- "CohortMethodData"

  expect_error(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = malformed
    ),
    "'analysisRef','cohorts','covariateRef','covariates','outcomes'"
  )
})

test_that("modelType", {
  ## logistic ----
  logistic <- fitOutcomeModel(
    population = studyPop,
    modelType = "logistic"
  )

  expect_identical(logistic$outcomeModelType, "logistic")

  ## poisson ----
  poisson <- fitOutcomeModel(
    population = studyPop,
    modelType = "poisson"
  )

  expect_identical(poisson$outcomeModelType, "poisson")

  ## cox ----
  cox <- fitOutcomeModel(
    population = studyPop,
    modelType = "cox"
  )

  expect_identical(cox$outcomeModelType, "cox")

  ## linear model ----
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      modelType = "linear"
    ),
    "'logistic','poisson','cox'"
  )

  ## NULL ----
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      modelType = NULL
    ),
    "'logistic','poisson','cox'"
  )

  ## NA ----
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      modelType = NA
    ),
    "'logistic','poisson','cox'"
  )
})

test_that("stratified", {
  ## FALSE ----
  modFalse1 <- fitOutcomeModel(
    population = studyPop,
    stratified = FALSE
  )

  modFalse2 <- fitOutcomeModel(
    population = studyPopStratisfied,
    stratified = FALSE
  )

  modFalse3 <- fitOutcomeModel(
    population = studyPopMatched,
    stratified = FALSE
  )

  expect_s3_class(modFalse1, "OutcomeModel")
  expect_s3_class(modFalse2, "OutcomeModel")
  expect_s3_class(modFalse3, "OutcomeModel")

  # Estimates
  expect_identical(
    modFalse1$outcomeModelTreatmentEstimate,
    modFalse2$outcomeModelTreatmentEstimate
  )

  expect_false(identical(
    modFalse2$outcomeModelTreatmentEstimate,
    modFalse3$outcomeModelTreatmentEstimate
  ))

  # LogLikelihoodProfile
  expect_identical(
    modFalse1$logLikelihoodProfile,
    modFalse2$logLikelihoodProfile
  )

  expect_false(identical(
    modFalse2$logLikelihoodProfile,
    modFalse3$logLikelihoodProfile
  ))

  # outcomeCounts
  expect_identical(
    modFalse1$outcomeCounts,
    modFalse2$outcomeCounts
  )

  expect_false(identical(
    modFalse2$outcomeCounts,
    modFalse3$outcomeCounts
  ))

  ## TRUE ----
  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = studyPop,
      stratified = TRUE
    )),
    "matchOnPs or stratifyByPs"
  )

  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = ps,
      stratified = TRUE
    )),
    "matchOnPs or stratifyByPs"
  )

  modTrue1 <- fitOutcomeModel(
    population = studyPopStratisfied,
    stratified = TRUE
  )

  modTrue2 <- fitOutcomeModel(
    population = studyPopMatched,
    stratified = TRUE
  )

  expect_s3_class(modTrue1, "OutcomeModel")
  expect_s3_class(modTrue2, "OutcomeModel")

  expect_false(identical(
    modTrue1$outcomeModelTreatmentEstimate,
    modTrue2$outcomeModelTreatmentEstimate
  ))

  expect_false(identical(
    modTrue1$logLikelihoodProfile,
    modTrue2$logLikelihoodProfile
  ))

  expect_false(identical(
    modTrue1$outcomeCounts,
    modTrue2$outcomeCounts
  ))
})

test_that("useCovariates", {
  ## FALSE ----
  modFalse <- fitOutcomeModel(
    population = studyPopStratisfied,
    useCovariates = FALSE
  )

  expect_s3_class(modFalse, "OutcomeModel")

  ## TRUE ----
  expect_error(
    fitOutcomeModel(
      population = studyPopStratisfied,
      useCovariates = TRUE
    ),
    "cohortMethodData"
  )

  modTrue <- fitOutcomeModel(
    population = studyPopStratisfied,
    cohortMethodData = sCohortMethodData,
    useCovariates = TRUE
  )

  expect_s3_class(modTrue, "OutcomeModel")
  expect_false(identical(modTrue, modFalse))

  expect_false(identical(
    modTrue$logLikelihoodProfile,
    modFalse$logLikelihoodProfile
  ))

  expect_false(identical(
    modTrue$outcomeModelTreatmentEstimate,
    modFalse$outcomeModelTreatmentEstimate
  ))

  expect_identical(
    modTrue$outcomeCounts,
    modFalse$outcomeCounts
  )
})

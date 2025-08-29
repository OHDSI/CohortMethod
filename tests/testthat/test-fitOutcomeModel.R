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
  emptyPop <- studyPop |>
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
    "not 'character'|Must have names"
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

  ## sum(outcomeCount) == 0 ----
  pop <- studyPop |>
    mutate(outcomeCount = 0)

  expect_output(
    fitOutcomeModel(
      population = pop
    ),
    "NO OUTCOMES FOUND FOR POPULATION"
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
    "('analysisRef','cohorts','covariateRef','covariates','outcomes'|Must inherit from class 'CohortMethodData')"
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

  ## sum(treatment) == 0 ----
  pop0 <- studyPopStratisfied |>
    mutate(treatment = 0)

  pop1 <- studyPopStratisfied |>
    mutate(treatment = 1)

  expect_output(
    fitOutcomeModel(
      population = pop0,
      stratified = TRUE
    ),
    "NO STRATA WITH BOTH TARGET, COMPARATOR, .+ OUTCOME"
  )

  expect_output(
    fitOutcomeModel(
      population = pop1,
      stratified = TRUE
    ),
    "NO STRATA WITH BOTH TARGET, COMPARATOR, .+ OUTCOME"
  )
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

test_that("inversePtWeighting", {
  ## FALSE ----
  modFalse1 <- fitOutcomeModel(
    population = studyPop,
    inversePtWeighting = FALSE
  )

  modFalse2 <- fitOutcomeModel(
    population = ps,
    inversePtWeighting = FALSE
  )

  modFalse3 <- fitOutcomeModel(
    population = studyPopStratisfied,
    inversePtWeighting = FALSE
  )

  modFalse4 <- fitOutcomeModel(
    population = studyPopMatched,
    inversePtWeighting = FALSE
  )

  expect_s3_class(modFalse1, "OutcomeModel")
  expect_s3_class(modFalse2, "OutcomeModel")
  expect_s3_class(modFalse3, "OutcomeModel")
  expect_s3_class(modFalse4, "OutcomeModel")

  # ps vs stratisfied
  expect_identical(modFalse2, modFalse3)

  # studyPop vs ps
  expect_false(identical(
    modFalse1, modFalse2
  ))

  # ps vs matched
  expect_false(identical(
    modFalse2,
    modFalse4
  ))

  ## TRUE ----
  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = studyPop,
      inversePtWeighting = TRUE
    )),
    "IPTW"
  )

  modTrue2 <- fitOutcomeModel(
    population = ps,
    inversePtWeighting = TRUE
  )

  modTrue3 <- fitOutcomeModel(
    population = studyPopStratisfied,
    inversePtWeighting = TRUE
  )

  modTrue4 <- fitOutcomeModel(
    population = studyPopMatched,
    inversePtWeighting = TRUE
  )

  expect_s3_class(modTrue2, "OutcomeModel")
  expect_s3_class(modTrue3, "OutcomeModel")
  expect_s3_class(modTrue4, "OutcomeModel")

  # ps vs stratisfied
  expect_identical(modTrue2, modTrue3)

  # ps vs matched
  expect_false(identical(
    modTrue2,
    modTrue4
  ))
})

test_that("interactionCovariateIds", {
  # Hard check on 64bit?
  # Warning on empty?
  # Allow negatives?
  femaleCovId <- 8532001

  ## Defaults ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    interactionCovariateIds = femaleCovId
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Empty ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    interactionCovariateIds = c()
  )

  expect_s3_class(mod, "OutcomeModel")

  ## 64-bit int ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    interactionCovariateIds = bit64::as.integer64(99999999999)
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Negative
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    interactionCovariateIds = -femaleCovId
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Negative 64-bit ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    interactionCovariateIds = -bit64::as.integer64(99999999999)
  )

  expect_s3_class(mod, "OutcomeModel")

  ## NA ----
  expect_error(
    fitOutcomeModel(
      population = studyPopMatched,
      cohortMethodData = sCohortMethodData,
      interactionCovariateIds = NA
    )
  )

  ## length(interactionCovariateIds) > 1 & useCovariates ----
  mod <- suppressWarnings(fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    useCovariates = TRUE,
    interactionCovariateIds = c(femaleCovId)
  ))

  expect_s3_class(mod, "OutcomeModel")
})

test_that("excludeCovariateIds", {
  # Hard check on 64bit?
  # Warning on empty?
  # Allow negatives?
  femaleCovId <- 8532001

  ## Defaults ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    excludeCovariateIds = femaleCovId
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Empty ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    excludeCovariateIds = c()
  )

  expect_s3_class(mod, "OutcomeModel")

  ## 64-bit int ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    excludeCovariateIds = bit64::as.integer64(99999999999)
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Negative
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    excludeCovariateIds = -femaleCovId
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Negative 64-bit ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    excludeCovariateIds = -bit64::as.integer64(99999999999)
  )

  expect_s3_class(mod, "OutcomeModel")

  ## NA ----
  expect_error(
    fitOutcomeModel(
      population = studyPopMatched,
      cohortMethodData = sCohortMethodData,
      excludeCovariateIds = NA
    )
  )
})

test_that("includeCovariateIds", {
  # Hard check on 64bit?
  # Warning on empty?
  # Allow negatives?
  femaleCovId <- 8532001

  ## Defaults ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    includeCovariateIds = femaleCovId
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Empty ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    includeCovariateIds = c()
  )

  expect_s3_class(mod, "OutcomeModel")

  ## 64-bit int ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    includeCovariateIds = bit64::as.integer64(99999999999)
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Negative
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    includeCovariateIds = -femaleCovId
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Negative 64-bit ----
  mod <- fitOutcomeModel(
    population = studyPopMatched,
    cohortMethodData = sCohortMethodData,
    includeCovariateIds = -bit64::as.integer64(99999999999)
  )

  expect_s3_class(mod, "OutcomeModel")

  ## NA ----
  expect_error(
    fitOutcomeModel(
      population = studyPopMatched,
      cohortMethodData = sCohortMethodData,
      includeCovariateIds = NA
    )
  )
})

test_that("profileGrid", {
  # Grid and Bounds allowed to be NULL?
  # Grid allowed to be NA while Bounds is NULL?
  ## Defaults ----
  mod <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData,
    profileGrid = NULL
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Log scale ----
  mod <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData,
    profileGrid = log(seq(1000)),
    profileBounds = NULL
  )

  expect_s3_class(mod, "OutcomeModel")

  ## Neg log scale ----
  mod <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData,
    profileGrid = -log(seq(1000)),
    profileBounds = NULL
  )

  expect_s3_class(mod, "OutcomeModel")

  ## 1 - 1000 ----
  expect_output(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      profileGrid = seq(1000),
      profileBounds = NULL
    ),
    "Warning"
  )

  ## 1 - 100 ----
  expect_output(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      profileGrid = seq(100),
      profileBounds = NULL
    ),
    "Warning"
  )

  ## 1 - 10 ----
  mod <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData,
    profileGrid = seq(10),
    profileBounds = NULL
  )

  expect_s3_class(mod, "OutcomeModel")

  ## NULL ----
  mod <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData,
    profileGrid = NULL,
    profileBounds = NULL
  )

  expect_s3_class(mod, "OutcomeModel")

  ## NA ----
  mod <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData,
    profileGrid = NA,
    profileBounds = NULL
  )

  expect_s3_class(mod, "OutcomeModel")
})

test_that("profileBounds", {
  # Set lower bounds to >0?
  ## Defaults ----
  mod <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData,
    profileBounds = c(log(0.1), log(10))
  )

  expect_s3_class(mod, "OutcomeModel")

  ## 0.1, 1000 ----
  expect_warning(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      profileBounds = c(0.1, 1000)
    )
  )

  ## 0.1, 100 ----
  expect_warning(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      profileBounds = c(0.1, 100)
    )
  )

  ## 0.1, 10 ----
  mod <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData,
    profileBounds = c(0.1, 10)
  )

  expect_s3_class(mod, "OutcomeModel")

  ## character ----
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      profileBounds = c("0.1", "10")
    )
  )

  ## 64bit int ----
  expect_warning(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      profileBounds = bit64::as.integer64(c(log(0.1), log(10)))
    )
  )

  ## Logical ----
  ### 0: FALSE, 1: TRUE ----
  mod <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData,
    profileBounds = c(0, 1)
  )

  ### FALSE, TRUE ----
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      profileBounds = c(FALSE, TRUE)
    )
  )

  ### log(0): inf, log(1) ----
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      profileBounds = c(log(0), log(1))
    )
  )

  ### log(FALSE): inf, log(TRUE) ----
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      profileBounds = c(log(FALSE), log(TRUE))
    )
  )
})

test_that("prior", {
  # No feedback to user when prior is specified, but useCovariates == FALSE
  ## laplace ----
  modLaplace <- fitOutcomeModel(
    population = studyPop,
    useCovariates = TRUE,
    cohortMethodData = sCohortMethodData,
    prior = createPrior("laplace", useCrossValidation = TRUE)
  )

  expect_s3_class(modLaplace, "OutcomeModel")

  ## none ----
  modNone <- suppressWarnings(fitOutcomeModel(
    population = studyPop,
    useCovariates = TRUE,
    cohortMethodData = sCohortMethodData,
    prior = createPrior("none")
  ))

  expect_s3_class(modNone, "OutcomeModel")

  ## normal ----
  modNormal <- fitOutcomeModel(
    population = studyPop,
    useCovariates = TRUE,
    cohortMethodData = sCohortMethodData,
    prior = createPrior("normal", useCrossValidation = TRUE)
  )

  expect_s3_class(modNormal, "OutcomeModel")

  ## barupdate ----
  modBarupdate <- fitOutcomeModel(
    population = studyPop,
    cohortMethodData = sCohortMethodData,
    prior = createPrior("barupdate")
  )

  expect_s3_class(modBarupdate, "OutcomeModel")

  expect_false(identical(modLaplace, modNormal))
  expect_false(identical(modLaplace, modNone))
  expect_false(identical(modLaplace, modBarupdate))

  ## NULL ----
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      prior = NULL
    )
  )

  ## Empty prior ----
  emptyPrior <- list()
  class(emptyPrior) <- "cyclopsPrior"

  expect_error(
    fitOutcomeModel(
      population = studyPop,
      useCovariates = TRUE,
      cohortMethodData = sCohortMethodData,
      prior = emptyPrior
    )
  )

  ## Malformed prior ----
  priorLaplace <- createPrior("laplace", useCrossValidation = TRUE)
  malformedLaplace <- list(
    priorType = priorLaplace$priorType,
    variance = priorLaplace$variance
  )
  class(malformedLaplace) <- "cyclopsPrior"

  expect_error(
    fitOutcomeModel(
      population = studyPop,
      useCovariates = TRUE,
      cohortMethodData = sCohortMethodData,
      prior = malformedLaplace
    )
  )
})

test_that("control", {
  # No feedback to user when control is specified, but useCovariates == FALSE
  ## auto ----
  modAuto <- fitOutcomeModel(
    population = studyPop,
    useCovariates = TRUE,
    cohortMethodData = sCohortMethodData,
    control = createControl(
      cvType = "auto",
      seed = 1,
      resetCoefficients = TRUE,
      startingVariance = 0.01,
      tolerance = 2e-07,
      cvRepetitions = 10,
      noiseLevel = "quiet"
    )
  )

  ## grid ----
  modGrid <- fitOutcomeModel(
    population = studyPop,
    useCovariates = TRUE,
    cohortMethodData = sCohortMethodData,
    control = createControl(
      cvType = "grid",
      seed = 1,
      resetCoefficients = TRUE,
      startingVariance = 0.01,
      tolerance = 2e-07,
      cvRepetitions = 10,
      noiseLevel = "quiet"
    )
  )

  expect_false(identical(modAuto, modGrid))

  ## NULL ----
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      useCovariates = TRUE,
      cohortMethodData = sCohortMethodData,
      control = NULL
    )
  )

  ## Empty control ----
  controlEmpty <- list()
  class(controlEmpty) <- "cyclopsControl"

  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = studyPop,
      useCovariates = TRUE,
      cohortMethodData = sCohortMethodData,
      control = controlEmpty
    ))
  )

  ## Malfored control ----
  controlGrid <- createControl(
    cvType = "grid",
    seed = 1,
    resetCoefficients = TRUE,
    startingVariance = 0.01,
    tolerance = 2e-07,
    cvRepetitions = 10,
    noiseLevel = "quiet"
  )

  controlMalformed <- list(
    maxIterations = controlGrid$maxIterations,
    autoSearch = controlGrid$autoSearch,
    convergenceType = controlGrid$convergenceType
  )
  class(controlMalformed) <- "cyclopsControl"

  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = studyPop,
      useCovariates = TRUE,
      cohortMethodData = sCohortMethodData,
      control = controlMalformed
    ))
  )
})

test_that("Combinations", {
  ## stratified && nrow(population) > 0 && is.null(population$stratumId)
  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = studyPop,
      stratified = TRUE
    )),
    "matchOnPs or stratifyByPs"
  )
  ## is.null(cohortMethodData) && useCovariates
  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = studyPop,
      useCovariates = TRUE
    )),
    "covariates .+ no cohortMethodData object specified"
  )

  ## is.null(cohortMethodData) && length(interactionCovariateIds) != 0
  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = studyPop,
      interactionCovariateIds = c(8532001)
    )),
    "interaction terms .+ no cohortMethodData object specified"
  )

  ## any(excludeCovariateIds %in% interactionCovariateIds)
  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      interactionCovariateIds = c(8532001),
      excludeCovariateIds = c(8532001)
    )),
    "exclude covariates .+ used for interaction"
  )

  # any(includeCovariateIds %in% excludeCovariateIds)
  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      includeCovariateIds = c(8532001),
      excludeCovariateIds = c(8532001)
    )),
    "exclude covariates .+ included"
  )

  ## inversePtWeighting && is.null(population$iptw)
  expect_error(
    suppressWarnings(fitOutcomeModel(
      population = studyPop,
      inversePtWeighting = TRUE
    )),
    "no IPTW are provided"
  )

  ## !is.null(profileGrid) && !is.null(profileBounds)
  expect_error(
    fitOutcomeModel(
      population = studyPop,
      cohortMethodData = sCohortMethodData,
      profileGrid = log(seq(10)),
      profileBounds = c(log(0.1), log(10))
    ),
    "grid and bounds"
  )
})

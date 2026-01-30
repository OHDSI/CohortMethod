library(CohortMethod)
library(testthat)

test_that("GetDbCohortMethodDataArgs serialization and deserialization", {
  settings <- createGetDbCohortMethodDataArgs(covariateSettings = FeatureExtraction::createDefaultCovariateSettings())
  settings2 <- GetDbCohortMethodDataArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createGetDbCohortMethodDataArgs(
    studyStartDate = "20000101",
    studyEndDate = "20101231",
    covariateSettings = FeatureExtraction::createDefaultCovariateSettings(
      excludedCovariateConceptIds = c(1,2),
      addDescendantsToExclude = TRUE)
  )
  settings2 <- GetDbCohortMethodDataArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  complexCovariateSettings <- list(
    FeatureExtraction::createDefaultCovariateSettings(
      excludedCovariateConceptIds = c(1,2),
      addDescendantsToExclude = TRUE
    ),
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 999,
      covariateCohorts = data.frame(
        cohortId = 3,
        cohortName = "Feature cohort"
      )
    )
  )
  settings <- createGetDbCohortMethodDataArgs(covariateSettings = complexCovariateSettings)
  settings2 <- GetDbCohortMethodDataArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)
})

test_that("CreateStudyPopulationArgs serialization and deserialization", {
  settings <- createCreateStudyPopulationArgs()
  settings2 <- CreateStudyPopulationArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)
})

test_that("CreatePsArgs serialization and deserialization", {
  settings <- createCreatePsArgs()
  settings2 <- CreatePsArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createCreatePsArgs(includeCovariateIds = 1:3,
                                 excludeCovariateIds = 2)
  settings2 <- CreatePsArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)
})

test_that("TrimByPsArgs serialization and deserialization", {
  settings <- createTrimByPsArgs(trimFraction = 0.05)
  settings2 <- TrimByPsArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createTrimByPsArgs(equipoiseBounds = c(0.3, 0.7))
  settings2 <- TrimByPsArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createTrimByPsArgs(maxWeight = 10)
  settings2 <- TrimByPsArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  expect_error(createTrimByPsArgs(), "Must specify at least one")
})

test_that("TruncateIptwArgs serialization and deserialization", {
  settings <- createTruncateIptwArgs()
  settings2 <- TruncateIptwArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)
})

test_that("MatchOnPsArgs serialization and deserialization", {
  settings <- createMatchOnPsArgs()
  settings2 <- MatchOnPsArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createMatchOnPsArgs(matchCovariateIds = 1234)
  settings2 <- MatchOnPsArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)
})

test_that("StratifyByPsArgs serialization and deserialization", {
  settings <- createStratifyByPsArgs()
  settings2 <- StratifyByPsArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createStratifyByPsArgs(stratificationCovariateIds = 1234)
  settings2 <- StratifyByPsArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)
})

test_that("ComputeCovariateBalanceArg serialization and deserialization", {
  settings <- createComputeCovariateBalanceArgs()
  settings2 <- ComputeCovariateBalanceArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createComputeCovariateBalanceArgs(
    covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
  )
  settings2 <- ComputeCovariateBalanceArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createComputeCovariateBalanceArgs(
    covariateFilter = c(1, 2, 3)
  )
  settings2 <- ComputeCovariateBalanceArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)
})

test_that("FitOutcomeModelArgs serialization and deserialization", {
  settings <- createFitOutcomeModelArgs()
  settings2 <- FitOutcomeModelArgs$new(json = settings$toJson())
  expect_equal(settings, settings2, tolerance = 0.0001)
})

test_that("CmAnalysis serialization and deserialization", {
  settings <- createCmAnalysis(
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = FeatureExtraction::createDefaultCovariateSettings()
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs()
  )
  settings2 <- CmAnalysis$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createCmAnalysis(
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = FeatureExtraction::createDefaultCovariateSettings(
        excludedCovariateConceptIds = c(1, 2),
        addDescendantsToExclude = TRUE
      )
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(),
    matchOnPsArgs = createMatchOnPsArgs(),
    computeSharedCovariateBalanceArgs = createComputeCovariateBalanceArgs(),
    computeCovariateBalanceArgs = createComputeCovariateBalanceArgs(
      covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
    ),
    fitOutcomeModelArgs = createFitOutcomeModelArgs()
  )
  settings2 <- CmAnalysis$new(json = settings$toJson())
  expect_equal(settings, settings2, tolerance = 0.0001)

  # Save and load CmAnalysisList
  cmAnalysisList <- list(
    createCmAnalysis(
      analysisId = 1,
      description = "Cm",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = FeatureExtraction::createDefaultCovariateSettings(
          addDescendantsToExclude = TRUE
        )
      ),
      createStudyPopulationArgs = createCreateStudyPopulationArgs(),
      createPsArgs = createCreatePsArgs(),
      matchOnPsArgs = createMatchOnPsArgs(),
      computeSharedCovariateBalanceArgs = createComputeCovariateBalanceArgs(),
      computeCovariateBalanceArgs = createComputeCovariateBalanceArgs(
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      ),
      fitOutcomeModelArgs = createFitOutcomeModelArgs()
    ),
    createCmAnalysis(
      analysisId = 2,
      description = "Cm 2",
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        covariateSettings = FeatureExtraction::createDefaultCovariateSettings(
          addDescendantsToExclude = TRUE
        )
      ),
      createStudyPopulationArgs = createCreateStudyPopulationArgs(),
      createPsArgs = createCreatePsArgs(),
      stratifyByPsArgs = createStratifyByPsArgs(),
      computeSharedCovariateBalanceArgs = createComputeCovariateBalanceArgs(),
      computeCovariateBalanceArgs = createComputeCovariateBalanceArgs(
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      ),
      fitOutcomeModelArgs = createFitOutcomeModelArgs()
    )
  )

  tempFile <- tempfile(fileext = ".json")
  saveCmAnalysisList(cmAnalysisList, tempFile)
  cmAnalysisList2 <- loadCmAnalysisList(tempFile)
  expect_equal(cmAnalysisList, cmAnalysisList2, tolerance = 0.0001)
  unlink(tempFile)
})

test_that("Outcome serialization and deserialization", {
  settings <- createOutcome(outcomeId = 10)
  settings2 <- Outcome$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createOutcome(outcomeId = 10, trueEffectSize = 1)
  settings2 <- Outcome$new(json = settings$toJson())
  expect_equal(settings, settings2)
})

test_that("TargetComparatorOutcomes serialization and deserialization", {
  settings <- createTargetComparatorOutcomes(
    targetId = 1,
    comparatorId = 2,
    outcomes = list(createOutcome(outcomeId = 10),
                    createOutcome(outcomeId = 11, trueEffectSize = 1))
  )
  settings2 <- TargetComparatorOutcomes$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createTargetComparatorOutcomes(
    targetId = 1,
    comparatorId = 2,
    nestingCohortId = 3,
    outcomes = list(createOutcome(outcomeId = 10),
                    createOutcome(outcomeId = 11, trueEffectSize = 1))
  )
  settings2 <- TargetComparatorOutcomes$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # Save and load TargetComparatorOutcomesList
  targetComparatorOutcomesList = list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 10),
                      createOutcome(outcomeId = 11, trueEffectSize = 1))
    ),
    createTargetComparatorOutcomes(
      targetId = 3,
      comparatorId = 4,
      outcomes = list(createOutcome(outcomeId = 10),
                      createOutcome(outcomeId = 11, trueEffectSize = 1))
    )
  )

  tempFile <- tempfile(fileext = ".json")
  saveTargetComparatorOutcomesList(targetComparatorOutcomesList, tempFile)
  targetComparatorOutcomesList2 <- loadTargetComparatorOutcomesList(tempFile)
  expect_equal(targetComparatorOutcomesList, targetComparatorOutcomesList2, tolerance = 0.0001)
  unlink(tempFile)
})

test_that("CmDiagnosticThresholds serialization and deserialization", {
  settings <- createCmDiagnosticThresholds()
  settings2 <- CmDiagnosticThresholds$new(json = settings$toJson())
  expect_equal(settings, settings2)
})

test_that("CmAnalysesSpecifications serialization and deserialization", {
  settings <- createCmAnalysesSpecifications(
    CmAnalysisList <- list(
      createCmAnalysis(
        analysisId = 1,
        description = "Cm",
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = FeatureExtraction::createDefaultCovariateSettings(
            addDescendantsToExclude = TRUE
          )
        ),
        createStudyPopulationArgs = createCreateStudyPopulationArgs(),
        createPsArgs = createCreatePsArgs(),
        matchOnPsArgs = createMatchOnPsArgs(),
        computeSharedCovariateBalanceArgs = createComputeCovariateBalanceArgs(),
        computeCovariateBalanceArgs = createComputeCovariateBalanceArgs(
          covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
        ),
        fitOutcomeModelArgs = createFitOutcomeModelArgs()
      ),
      createCmAnalysis(
        analysisId = 2,
        description = "Cm 2",
        getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
          covariateSettings = FeatureExtraction::createDefaultCovariateSettings(
            addDescendantsToExclude = TRUE
          )
        ),
        createStudyPopulationArgs = createCreateStudyPopulationArgs(),
        createPsArgs = createCreatePsArgs(),
        stratifyByPsArgs = createStratifyByPsArgs(),
        computeSharedCovariateBalanceArgs = createComputeCovariateBalanceArgs(),
        computeCovariateBalanceArgs = createComputeCovariateBalanceArgs(
          covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
        ),
        fitOutcomeModelArgs = createFitOutcomeModelArgs()
      )
    ),
    targetComparatorOutcomesList = list(
      createTargetComparatorOutcomes(
        targetId = 1,
        comparatorId = 2,
        outcomes = list(createOutcome(outcomeId = 10),
                        createOutcome(outcomeId = 11, trueEffectSize = 1))
      ),
      createTargetComparatorOutcomes(
        targetId = 3,
        comparatorId = 4,
        outcomes = list(createOutcome(outcomeId = 10),
                        createOutcome(outcomeId = 11, trueEffectSize = 1))
      )
    ),
    analysesToExclude = data.frame(targetId = c(1, 1), outcomeId = c(10)),
    refitPsForEveryOutcome = FALSE,
    cmDiagnosticThresholds = createCmDiagnosticThresholds()
  )
  settings2 <- CmAnalysesSpecifications$new(json = settings$toJson())
  expect_equal(settings, settings2, tolerance = 1e-4)

  settings2 <- convertUntypedListToCmAnalysesSpecifications(settings$toList())
  expect_equal(settings, settings2, tolerance = 1e-4)

  json <- settings$toJson()
  untypedList <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  settings2 <- convertUntypedListToCmAnalysesSpecifications(untypedList)
  expect_equal(settings, settings2, tolerance = 1e-4)

  settings$analysesToExclude <- NULL
  settings2 <- CmAnalysesSpecifications$new(json = settings$toJson())
  expect_equal(settings, settings2, tolerance = 1e-4)

  settings$analysesToExclude <- data.frame(targetId = 1)
  settings2 <- CmAnalysesSpecifications$new(json = settings$toJson())
  expect_equal(settings, settings2, tolerance = 1e-4)
})

context("Content-addressable caching scenarios")

# These tests verify that content-addressable filenames correctly handle
# real-world scenarios: reusing artifacts when possible, generating new
# filenames when settings change, and cascading hash changes appropriately.

# Helper to create a basic analysis for testing
makeBasicAnalysis <- function(analysisId = 1, withPs = TRUE, withStrata = TRUE) {
  args <- list(
    analysisId = analysisId,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = FeatureExtraction::createCovariateSettings(
        useDemographicsGender = TRUE,
        useDemographicsAge = TRUE
      )
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(
      minDaysAtRisk = 1,
      riskWindowStart = 0,
      startAnchor = "cohort start",
      riskWindowEnd = 30,
      endAnchor = "cohort start"
    )
  )
  if (withPs) {
    args$createPsArgs <- createCreatePsArgs()
  }
  if (withStrata && withPs) {
    args$matchOnPsArgs <- createMatchOnPsArgs()
  }
  args$fitOutcomeModelArgs <- createFitOutcomeModelArgs(modelType = "cox")
  do.call(createCmAnalysis, args)
}

# Helper to build a reference table without needing a database
buildRef <- function(cmAnalysisList,
                     targetComparatorOutcomesList,
                     databaseId = "testDb",
                     refitPsForEveryOutcome = FALSE,
                     refitPsForEveryStudyPopulation = TRUE) {
  outputFolder <- tempfile(pattern = "cmRef")
  dir.create(outputFolder)
  on.exit(unlink(outputFolder, recursive = TRUE))

  CohortMethod:::createReferenceTable(
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = targetComparatorOutcomesList,
    analysesToExclude = NULL,
    outputFolder = outputFolder,
    refitPsForEveryOutcome = refitPsForEveryOutcome,
    refitPsForEveryStudyPopulation = refitPsForEveryStudyPopulation,
    databaseId = databaseId
  )
}


# ===========================================================================
# Scenario 1: Adding a new negative control outcome
# Negative controls need new outcome models but should NOT change PS models
# or CohortMethodData files
# ===========================================================================

test_that("Adding a negative control does NOT change CmData or shared PS filenames", {
  analysis <- makeBasicAnalysis()

  # Original: one outcome of interest, one negative control
  tcos1 <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 100, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 200, outcomeOfInterest = FALSE)  # negative control
      )
    )
  )

  # New: add a second negative control
  tcos2 <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 100, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 200, outcomeOfInterest = FALSE),
        createOutcome(outcomeId = 300, outcomeOfInterest = FALSE)  # NEW negative control
      )
    )
  )

  ref1 <- buildRef(list(analysis), tcos1)
  ref2 <- buildRef(list(analysis), tcos2)

  # CohortMethodData files should be identical (same load args, same target/comparator)
  expect_equal(
    unique(ref1$cohortMethodDataFile),
    unique(ref2$cohortMethodDataFile)
  )

  # Shared PS files should be identical (PS doesn't depend on outcome)
  sharedPs1 <- unique(ref1$sharedPsFile[ref1$sharedPsFile != ""])
  sharedPs2 <- unique(ref2$sharedPsFile[ref2$sharedPsFile != ""])
  expect_equal(sharedPs1, sharedPs2)

  # Original outcome (100) should have the same outcome model file
  om1_100 <- ref1$outcomeModelFile[ref1$outcomeId == 100]
  om2_100 <- ref2$outcomeModelFile[ref2$outcomeId == 100]
  expect_equal(om1_100, om2_100)

  # New negative control (300) should have a NEW outcome model file
  om2_300 <- ref2$outcomeModelFile[ref2$outcomeId == 300]
  expect_true(om2_300 != "")
  expect_true(!(om2_300 %in% ref1$outcomeModelFile))
})

test_that("Adding a negative control produces distinct outcome model per outcome", {
  analysis <- makeBasicAnalysis()

  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 100, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 200, outcomeOfInterest = FALSE),
        createOutcome(outcomeId = 300, outcomeOfInterest = FALSE)
      )
    )
  )

  ref <- buildRef(list(analysis), tcos)

  # Each outcome should have a unique outcome model filename
  omFiles <- ref$outcomeModelFile[ref$outcomeModelFile != ""]
  expect_equal(length(omFiles), length(unique(omFiles)))
})


# ===========================================================================
# Scenario 2: Adding an outcome of interest
# Should produce new StudyPop + StratPop + OutcomeModel for the new outcome
# while reusing CmData and shared PS
# ===========================================================================

test_that("Adding an outcome of interest reuses CmData and shared PS", {
  analysis <- makeBasicAnalysis()

  tcos1 <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 100, outcomeOfInterest = TRUE)
      )
    )
  )

  tcos2 <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 100, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 101, outcomeOfInterest = TRUE)  # NEW
      )
    )
  )

  ref1 <- buildRef(list(analysis), tcos1)
  ref2 <- buildRef(list(analysis), tcos2)

  # CohortMethodData should be reused
  expect_equal(unique(ref1$cohortMethodDataFile), unique(ref2$cohortMethodDataFile))

  # Shared PS should be reused (same model, not outcome-specific)
  sharedPs1 <- unique(ref1$sharedPsFile[ref1$sharedPsFile != ""])
  sharedPs2 <- unique(ref2$sharedPsFile[ref2$sharedPsFile != ""])
  expect_equal(sharedPs1, sharedPs2)

  # Original outcome 100 filenames should be identical
  expect_equal(
    ref1$studyPopFile[ref1$outcomeId == 100],
    ref2$studyPopFile[ref2$outcomeId == 100]
  )
  expect_equal(
    ref1$strataFile[ref1$outcomeId == 100],
    ref2$strataFile[ref2$outcomeId == 100]
  )
  expect_equal(
    ref1$outcomeModelFile[ref1$outcomeId == 100],
    ref2$outcomeModelFile[ref2$outcomeId == 100]
  )

  # New outcome 101 should have new study pop and outcome model
  newStudyPop <- ref2$studyPopFile[ref2$outcomeId == 101]
  expect_true(newStudyPop != "")
  expect_true(!(newStudyPop %in% ref1$studyPopFile))

  newOm <- ref2$outcomeModelFile[ref2$outcomeId == 101]
  expect_true(newOm != "")
  expect_true(!(newOm %in% ref1$outcomeModelFile))
})

test_that("Adding an outcome does NOT affect strata file of existing outcomes", {
  analysis <- makeBasicAnalysis()

  tcos1 <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 100, outcomeOfInterest = TRUE)
      )
    )
  )

  tcos2 <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 100, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 101, outcomeOfInterest = TRUE)
      )
    )
  )

  ref1 <- buildRef(list(analysis), tcos1)
  ref2 <- buildRef(list(analysis), tcos2)

  # Strata file for outcome 100 should be unchanged
  expect_equal(
    ref1$strataFile[ref1$outcomeId == 100],
    ref2$strataFile[ref2$outcomeId == 100]
  )
})


# ===========================================================================
# Scenario 3: Changing PS covariates cascades to all downstream artifacts
# CmData changes → new loadHash → new StudyPop, PS, Strata, OutcomeModel
# ===========================================================================

test_that("Changing covariates cascades: all filenames change", {
  analysis1 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = FeatureExtraction::createCovariateSettings(
        useDemographicsGender = TRUE,
        useDemographicsAge = TRUE
      )
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(),
    matchOnPsArgs = createMatchOnPsArgs(),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  # Same analysis but with additional covariate
  analysis2 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = FeatureExtraction::createCovariateSettings(
        useDemographicsGender = TRUE,
        useDemographicsAge = TRUE,
        useDemographicsRace = TRUE  # CHANGED: added covariate
      )
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(),
    matchOnPsArgs = createMatchOnPsArgs(),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 100, outcomeOfInterest = TRUE))
    )
  )

  ref1 <- buildRef(list(analysis1), tcos)
  ref2 <- buildRef(list(analysis2), tcos)

  # CohortMethodData files should differ (covariates changed)
  expect_true(ref1$cohortMethodDataFile[1] != ref2$cohortMethodDataFile[1])

  # Study population files should differ (cascaded via loadHash)
  expect_true(ref1$studyPopFile[1] != ref2$studyPopFile[1])

  # Shared PS files should differ (cascaded via loadHash)
  expect_true(ref1$sharedPsFile[1] != ref2$sharedPsFile[1])

  # Strata files should differ (cascaded)
  expect_true(ref1$strataFile[1] != ref2$strataFile[1])

  # Outcome model files should differ (cascaded)
  expect_true(ref1$outcomeModelFile[1] != ref2$outcomeModelFile[1])
})

test_that("Changing PS args cascades to PS and downstream but NOT CmData", {
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAge = TRUE
  )

  analysis1 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = covSettings
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(maxCohortSizeForFitting = 100000),
    matchOnPsArgs = createMatchOnPsArgs(),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  analysis2 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = covSettings
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(maxCohortSizeForFitting = 50000),  # CHANGED
    matchOnPsArgs = createMatchOnPsArgs(),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 100, outcomeOfInterest = TRUE))
    )
  )

  ref1 <- buildRef(list(analysis1), tcos)
  ref2 <- buildRef(list(analysis2), tcos)

  # CohortMethodData should be REUSED (load args unchanged)
  expect_equal(ref1$cohortMethodDataFile[1], ref2$cohortMethodDataFile[1])

  # StudyPop should be REUSED (studyPop args unchanged, same loadHash)
  expect_equal(ref1$studyPopFile[1], ref2$studyPopFile[1])

  # Shared PS should DIFFER (PS args changed)
  expect_true(ref1$sharedPsFile[1] != ref2$sharedPsFile[1])

  # Strata and outcome model should DIFFER (cascaded from PS change)
  expect_true(ref1$strataFile[1] != ref2$strataFile[1])
  expect_true(ref1$outcomeModelFile[1] != ref2$outcomeModelFile[1])
})

test_that("Changing study pop args cascades to StudyPop and downstream but NOT CmData", {
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE
  )

  analysis1 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = covSettings
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(minDaysAtRisk = 1),
    createPsArgs = createCreatePsArgs(),
    matchOnPsArgs = createMatchOnPsArgs(),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  analysis2 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = covSettings
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(minDaysAtRisk = 30),  # CHANGED
    createPsArgs = createCreatePsArgs(),
    matchOnPsArgs = createMatchOnPsArgs(),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 100, outcomeOfInterest = TRUE))
    )
  )

  ref1 <- buildRef(list(analysis1), tcos)
  ref2 <- buildRef(list(analysis2), tcos)

  # CohortMethodData should be REUSED
  expect_equal(ref1$cohortMethodDataFile[1], ref2$cohortMethodDataFile[1])

  # StudyPop should DIFFER
  expect_true(ref1$studyPopFile[1] != ref2$studyPopFile[1])

  # PS should DIFFER (depends on studyPop args in hash)
  expect_true(ref1$sharedPsFile[1] != ref2$sharedPsFile[1])

  # Strata should DIFFER
  expect_true(ref1$strataFile[1] != ref2$strataFile[1])

  # Outcome model should DIFFER
  expect_true(ref1$outcomeModelFile[1] != ref2$outcomeModelFile[1])
})


# ===========================================================================
# Scenario 4: databaseId differentiation
# Same settings on different databases MUST produce different filenames
# ===========================================================================

test_that("Different databaseId produces entirely different filenames", {
  analysis <- makeBasicAnalysis()

  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 100, outcomeOfInterest = TRUE))
    )
  )

  refA <- buildRef(list(analysis), tcos, databaseId = "database_A")
  refB <- buildRef(list(analysis), tcos, databaseId = "database_B")

  # ALL filenames should differ

  expect_true(refA$cohortMethodDataFile[1] != refB$cohortMethodDataFile[1])
  expect_true(refA$studyPopFile[1] != refB$studyPopFile[1])
  expect_true(refA$sharedPsFile[1] != refB$sharedPsFile[1])
  expect_true(refA$strataFile[1] != refB$strataFile[1])
  expect_true(refA$outcomeModelFile[1] != refB$outcomeModelFile[1])
})


# ===========================================================================
# Scenario 5: Changing matching/stratification args
# Should change strata + outcome models but NOT CmData, StudyPop, or PS
# ===========================================================================

test_that("Changing matching args changes strata but not CmData/StudyPop/PS", {
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE
  )

  analysis1 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = covSettings
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(),
    matchOnPsArgs = createMatchOnPsArgs(caliper = 0.2),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  analysis2 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = covSettings
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(),
    matchOnPsArgs = createMatchOnPsArgs(caliper = 0.1),  # CHANGED caliper
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 100, outcomeOfInterest = TRUE))
    )
  )

  ref1 <- buildRef(list(analysis1), tcos)
  ref2 <- buildRef(list(analysis2), tcos)

  # CmData should be REUSED
  expect_equal(ref1$cohortMethodDataFile[1], ref2$cohortMethodDataFile[1])

  # StudyPop should be REUSED
  expect_equal(ref1$studyPopFile[1], ref2$studyPopFile[1])

  # Shared PS should be REUSED (matching doesn't affect PS fitting)
  expect_equal(ref1$sharedPsFile[1], ref2$sharedPsFile[1])

  # Strata should DIFFER (matching caliper changed)
  expect_true(ref1$strataFile[1] != ref2$strataFile[1])

  # Outcome model should DIFFER (depends on strata)
  expect_true(ref1$outcomeModelFile[1] != ref2$outcomeModelFile[1])
})


# ===========================================================================
# Scenario 6: Changing outcome model args only
# Should change ONLY outcome models
# ===========================================================================

test_that("Changing outcome model args changes only outcome model files", {
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE
  )

  analysis1 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = covSettings
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(),
    matchOnPsArgs = createMatchOnPsArgs(),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  analysis2 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = covSettings
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(),
    matchOnPsArgs = createMatchOnPsArgs(),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "logistic")  # CHANGED
  )

  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 100, outcomeOfInterest = TRUE))
    )
  )

  ref1 <- buildRef(list(analysis1), tcos)
  ref2 <- buildRef(list(analysis2), tcos)

  # CmData, StudyPop, PS, Strata should all be REUSED
  expect_equal(ref1$cohortMethodDataFile[1], ref2$cohortMethodDataFile[1])
  expect_equal(ref1$studyPopFile[1], ref2$studyPopFile[1])
  expect_equal(ref1$sharedPsFile[1], ref2$sharedPsFile[1])
  expect_equal(ref1$strataFile[1], ref2$strataFile[1])

  # Only outcome model should DIFFER
  expect_true(ref1$outcomeModelFile[1] != ref2$outcomeModelFile[1])
})


# ===========================================================================
# Scenario 7: Multiple analyses sharing CmData
# ===========================================================================

test_that("Analyses with same load args share CmData file", {
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE
  )

  # Two analyses with same getDbCohortMethodDataArgs but different PS args
  analysis1 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = covSettings
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(maxCohortSizeForFitting = 100000),
    matchOnPsArgs = createMatchOnPsArgs(),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  analysis2 <- createCmAnalysis(
    analysisId = 2,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = covSettings
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs(maxCohortSizeForFitting = 50000),
    matchOnPsArgs = createMatchOnPsArgs(),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )

  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 100, outcomeOfInterest = TRUE))
    )
  )

  ref <- buildRef(list(analysis1, analysis2), tcos)

  # Both rows should share the same CmData file
  cmFiles <- unique(ref$cohortMethodDataFile)
  expect_equal(length(cmFiles), 1)

  # But have different PS files
  psFiles <- unique(ref$sharedPsFile[ref$sharedPsFile != ""])
  expect_equal(length(psFiles), 2)
})


# ===========================================================================
# Scenario 8: Hash stability across invocations
# ===========================================================================

test_that("Reference table is deterministic across repeated calls", {
  analysis <- makeBasicAnalysis()

  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 100, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 200, outcomeOfInterest = FALSE)
      )
    )
  )

  ref1 <- buildRef(list(analysis), tcos, databaseId = "stable_test")
  ref2 <- buildRef(list(analysis), tcos, databaseId = "stable_test")

  # All filenames should be identical
  expect_equal(ref1$cohortMethodDataFile, ref2$cohortMethodDataFile)
  expect_equal(ref1$studyPopFile, ref2$studyPopFile)
  expect_equal(ref1$sharedPsFile, ref2$sharedPsFile)
  expect_equal(ref1$strataFile, ref2$strataFile)
  expect_equal(ref1$outcomeModelFile, ref2$outcomeModelFile)
})


# ===========================================================================
# Base Population Sharing Tests
# Verify that the two-phase population architecture correctly shares
# base populations across outcomes while keeping study populations distinct.
# ===========================================================================

test_that("Two outcomes with same risk windows share the same basePopFile", {
  analysis <- makeBasicAnalysis()
  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 10, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 20, outcomeOfInterest = TRUE)
      )
    )
  )
  ref <- buildRef(list(analysis), tcos)

  # Same base population (same risk windows for both outcomes)
  expect_equal(ref$basePopFile[ref$outcomeId == 10],
               ref$basePopFile[ref$outcomeId == 20])
  # Different study populations (different outcomeId)
  expect_true(ref$studyPopFile[ref$outcomeId == 10] !=
                ref$studyPopFile[ref$outcomeId == 20])
})

test_that("Per-outcome risk window override creates different basePopFile", {
  analysis <- makeBasicAnalysis()
  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 10, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 20, outcomeOfInterest = TRUE,
                      riskWindowEnd = 60, endAnchor = "cohort start")  # Override default of 30
      )
    )
  )
  ref <- buildRef(list(analysis), tcos)

  # Different base populations (different risk window end)
  expect_true(ref$basePopFile[ref$outcomeId == 10] !=
                ref$basePopFile[ref$outcomeId == 20])
  # Different study populations too
  expect_true(ref$studyPopFile[ref$outcomeId == 10] !=
                ref$studyPopFile[ref$outcomeId == 20])
})

test_that("Changing priorOutcomeLookback changes studyPopFile but NOT basePopFile", {
  analysis1 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = FeatureExtraction::createCovariateSettings(
        useDemographicsGender = TRUE
      )
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(
      removeSubjectsWithPriorOutcome = TRUE,
      priorOutcomeLookback = 99999,
      minDaysAtRisk = 1,
      riskWindowStart = 0,
      startAnchor = "cohort start",
      riskWindowEnd = 30,
      endAnchor = "cohort start"
    ),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )
  analysis2 <- createCmAnalysis(
    analysisId = 2,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = FeatureExtraction::createCovariateSettings(
        useDemographicsGender = TRUE
      )
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(
      removeSubjectsWithPriorOutcome = TRUE,
      priorOutcomeLookback = 365,
      minDaysAtRisk = 1,
      riskWindowStart = 0,
      startAnchor = "cohort start",
      riskWindowEnd = 30,
      endAnchor = "cohort start"
    ),
    fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
  )
  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 10, outcomeOfInterest = TRUE))
    )
  )
  ref <- buildRef(list(analysis1, analysis2), tcos)

  row1 <- ref[ref$analysisId == 1, ]
  row2 <- ref[ref$analysisId == 2, ]

  # Same base population (same risk windows, minDaysAtRisk, etc.)
  expect_equal(row1$basePopFile, row2$basePopFile)
  # Different study populations (different priorOutcomeLookback)
  expect_true(row1$studyPopFile != row2$studyPopFile)
})

test_that("Shared PS file uses basePopHash (outcome-independent)", {
  analysis <- makeBasicAnalysis()
  tcos <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 10, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 20, outcomeOfInterest = TRUE)
      )
    )
  )
  ref <- buildRef(list(analysis), tcos)

  # Shared PS should be the same for both outcomes (based on basePopHash)
  expect_equal(ref$sharedPsFile[ref$outcomeId == 10],
               ref$sharedPsFile[ref$outcomeId == 20])
  # And both should be non-empty
  expect_true(ref$sharedPsFile[1] != "")
})

test_that("Adding an outcome preserves all existing filenames", {
  analysis <- makeBasicAnalysis()
  tcos1 <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 10, outcomeOfInterest = TRUE))
    )
  )
  tcos2 <- list(
    createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 10, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 20, outcomeOfInterest = TRUE)
      )
    )
  )

  ref1 <- buildRef(list(analysis), tcos1)
  ref2 <- buildRef(list(analysis), tcos2)
  ref2_o10 <- ref2[ref2$outcomeId == 10, ]

  # All filenames for outcome 10 should be identical
  expect_equal(ref1$basePopFile, ref2_o10$basePopFile)
  expect_equal(ref1$cohortMethodDataFile, ref2_o10$cohortMethodDataFile)
  expect_equal(ref1$studyPopFile, ref2_o10$studyPopFile)
  expect_equal(ref1$sharedPsFile, ref2_o10$sharedPsFile)
  expect_equal(ref1$strataFile, ref2_o10$strataFile)
  expect_equal(ref1$outcomeModelFile, ref2_o10$outcomeModelFile)
})

if (!isFALSE(tryCatch(find.package("Eunomia"), error = function(e) FALSE))) {

  # Minimal analysis used across all E2E tests: no PS, no matching, simple cox
  makeEunomiaAnalysis <- function(analysisId = 1) {
    createCmAnalysis(
      analysisId = analysisId,
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        firstExposureOnly = TRUE,
        washoutPeriod = 183,
        covariateSettings = FeatureExtraction::createCovariateSettings(
          useDemographicsGender = TRUE,
          useDemographicsAge = TRUE
        )
      ),
      createStudyPopulationArgs = createCreateStudyPopulationArgs(
        removeSubjectsWithPriorOutcome = TRUE,
        minDaysAtRisk = 1,
        riskWindowStart = 0,
        startAnchor = "cohort start",
        riskWindowEnd = 30,
        endAnchor = "cohort end"
      ),
      fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
    )
  }

  test_that("E2E: files are actually written to disk", {
    outputFolder <- tempfile(pattern = "cmCache")
    on.exit(unlink(outputFolder, recursive = TRUE))

    result <- runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      databaseId = "Eunomia",
      cmAnalysesSpecifications = createCmAnalysesSpecifications(
        cmAnalysisList = list(makeEunomiaAnalysis()),
        targetComparatorOutcomesList = list(
          createTargetComparatorOutcomes(
            targetId = 1,
            comparatorId = 2,
            outcomes = list(createOutcome(outcomeId = 3, outcomeOfInterest = TRUE))
          )
        )
      )
    )

    # Every non-empty filename in the reference table must exist on disk
    checkFiles <- function(col) {
      paths <- result[[col]]
      paths <- paths[paths != ""]
      paths <- unique(paths)
      missing <- paths[!file.exists(file.path(outputFolder, paths))]
      expect_equal(length(missing), 0,
                   info = sprintf("Missing %s files: %s", col, paste(missing, collapse = ", ")))
    }

    checkFiles("cohortMethodDataFile")
    checkFiles("basePopFile")
    checkFiles("studyPopFile")
    checkFiles("outcomeModelFile")
  })

  test_that("E2E: adding a new outcome reuses CmData and computes only new outcome model", {
    outputFolder <- tempfile(pattern = "cmCacheReuse")
    on.exit(unlink(outputFolder, recursive = TRUE))

    analysis <- makeEunomiaAnalysis()
    tcos_base <- createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 3, outcomeOfInterest = TRUE)
      )
    )
    tcos_extended <- createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(outcomeId = 3, outcomeOfInterest = TRUE),
        createOutcome(outcomeId = 4, outcomeOfInterest = TRUE)  # NEW outcome
      )
    )

    # First run: outcomes [3]
    result1 <- runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      databaseId = "Eunomia",
      cmAnalysesSpecifications = createCmAnalysesSpecifications(
        cmAnalysisList = list(analysis),
        targetComparatorOutcomesList = list(tcos_base)
      )
    )

    # Record modification times of existing artifacts after first run
    cmDataPath <- file.path(outputFolder, result1$cohortMethodDataFile[1])
    basePopPath <- file.path(outputFolder, result1$basePopFile[1])
    studyPopPath <- file.path(outputFolder, result1$studyPopFile[1])
    om3Path <- file.path(outputFolder, result1$outcomeModelFile[1])

    expect_true(file.exists(cmDataPath))
    expect_true(file.exists(basePopPath))
    expect_true(file.exists(studyPopPath))
    expect_true(file.exists(om3Path))

    mtime_cmData <- file.info(cmDataPath)$mtime
    mtime_basePop <- file.info(basePopPath)$mtime
    mtime_studyPop <- file.info(studyPopPath)$mtime
    mtime_om3 <- file.info(om3Path)$mtime

    # Brief sleep to ensure mtime would differ if files were rewritten
    Sys.sleep(1)

    # Second run: add outcome 4
    result2 <- runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      databaseId = "Eunomia",
      cmAnalysesSpecifications = createCmAnalysesSpecifications(
        cmAnalysisList = list(analysis),
        targetComparatorOutcomesList = list(tcos_extended)
      )
    )

    # CmData file: same filename, NOT re-written (mtime unchanged)
    expect_equal(
      result1$cohortMethodDataFile[1],
      result2$cohortMethodDataFile[result2$outcomeId == 3]
    )
    expect_equal(file.info(cmDataPath)$mtime, mtime_cmData)

    # Base population: same filename, NOT re-written (shared across outcomes)
    expect_equal(
      result1$basePopFile[1],
      result2$basePopFile[result2$outcomeId == 3]
    )
    expect_equal(
      result1$basePopFile[1],
      result2$basePopFile[result2$outcomeId == 4]
    )
    expect_equal(file.info(basePopPath)$mtime, mtime_basePop)

    # StudyPop for outcome 3: same filename, NOT re-written
    expect_equal(
      result1$studyPopFile[1],
      result2$studyPopFile[result2$outcomeId == 3]
    )
    expect_equal(file.info(studyPopPath)$mtime, mtime_studyPop)

    # Outcome model for outcome 3: same filename, NOT re-written
    expect_equal(
      result1$outcomeModelFile[1],
      result2$outcomeModelFile[result2$outcomeId == 3]
    )
    expect_equal(file.info(om3Path)$mtime, mtime_om3)

    # Outcome 4: new outcome model created
    om4Path <- file.path(outputFolder, result2$outcomeModelFile[result2$outcomeId == 4])
    expect_true(file.exists(om4Path))

    # Outcome 4's study pop: newly created
    sp4Path <- file.path(outputFolder, result2$studyPopFile[result2$outcomeId == 4])
    expect_true(file.exists(sp4Path))

    # New outcome 4 has a DIFFERENT filename than outcome 3
    expect_false(
      result2$outcomeModelFile[result2$outcomeId == 3] ==
        result2$outcomeModelFile[result2$outcomeId == 4]
    )
  })

  test_that("E2E: changing settings forces recomputation with new filename", {
    outputFolder <- tempfile(pattern = "cmCacheInvalidate")
    on.exit(unlink(outputFolder, recursive = TRUE))

    tcos <- createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(createOutcome(outcomeId = 3, outcomeOfInterest = TRUE))
    )

    analysis_v1 <- createCmAnalysis(
      analysisId = 1,
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        firstExposureOnly = TRUE,
        washoutPeriod = 183,
        covariateSettings = FeatureExtraction::createCovariateSettings(
          useDemographicsGender = TRUE,
          useDemographicsAge = TRUE
        )
      ),
      createStudyPopulationArgs = createCreateStudyPopulationArgs(
        minDaysAtRisk = 1,
        riskWindowStart = 0,
        startAnchor = "cohort start",
        riskWindowEnd = 30,
        endAnchor = "cohort end"
      ),
      fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
    )

    analysis_v2 <- createCmAnalysis(
      analysisId = 1,
      getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
        firstExposureOnly = TRUE,
        washoutPeriod = 183,
        covariateSettings = FeatureExtraction::createCovariateSettings(
          useDemographicsGender = TRUE,
          useDemographicsAge = TRUE
        )
      ),
      createStudyPopulationArgs = createCreateStudyPopulationArgs(
        minDaysAtRisk = 30,  # CHANGED: was 1, now 30
        riskWindowStart = 0,
        startAnchor = "cohort start",
        riskWindowEnd = 30,
        endAnchor = "cohort end"
      ),
      fitOutcomeModelArgs = createFitOutcomeModelArgs(modelType = "cox")
    )

    # First run
    result_v1 <- runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      databaseId = "Eunomia",
      cmAnalysesSpecifications = createCmAnalysesSpecifications(
        cmAnalysisList = list(analysis_v1),
        targetComparatorOutcomesList = list(tcos)
      )
    )

    # Record what was created
    cmDataFile_v1 <- result_v1$cohortMethodDataFile[1]
    studyPopFile_v1 <- result_v1$studyPopFile[1]
    omFile_v1 <- result_v1$outcomeModelFile[1]

    expect_true(file.exists(file.path(outputFolder, cmDataFile_v1)))
    expect_true(file.exists(file.path(outputFolder, studyPopFile_v1)))
    expect_true(file.exists(file.path(outputFolder, omFile_v1)))

    # Second run with changed study pop args
    result_v2 <- runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      databaseId = "Eunomia",
      cmAnalysesSpecifications = createCmAnalysesSpecifications(
        cmAnalysisList = list(analysis_v2),
        targetComparatorOutcomesList = list(tcos)
      )
    )

    cmDataFile_v2 <- result_v2$cohortMethodDataFile[1]
    studyPopFile_v2 <- result_v2$studyPopFile[1]
    omFile_v2 <- result_v2$outcomeModelFile[1]

    # CmData: same (load args unchanged), study pop and outcome model: new files
    expect_equal(cmDataFile_v1, cmDataFile_v2)
    expect_false(studyPopFile_v1 == studyPopFile_v2)
    expect_false(omFile_v1 == omFile_v2)

    # New files actually exist on disk
    expect_true(file.exists(file.path(outputFolder, studyPopFile_v2)))
    expect_true(file.exists(file.path(outputFolder, omFile_v2)))

    # OLD study pop and outcome model files still on disk (not deleted by new run)
    expect_true(file.exists(file.path(outputFolder, studyPopFile_v1)))
    expect_true(file.exists(file.path(outputFolder, omFile_v1)))
  })

}

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

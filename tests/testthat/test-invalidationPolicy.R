context("Artifact caching system")

# Helper to create covariate settings
getDefaultCovariateSettings <- function() {
  return(FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAge = TRUE
  ))
}

test_that("SettingsHasher hashes settings consistently", {
  # Create two identical settings objects
  args1 <- createGetDbCohortMethodDataArgs(
    covariateSettings = getDefaultCovariateSettings()
  )
  args2 <- createGetDbCohortMethodDataArgs(
    covariateSettings = getDefaultCovariateSettings()
  )

  hasher <- SettingsHasher$new()

  # Hash the same settings twice
  hash1 <- hasher$hashLoadArgs(args1)
  hash2 <- hasher$hashLoadArgs(args2)

  # Should produce identical hashes
  expect_equal(hash1, hash2)
  expect_true(is.character(hash1))
  expect_equal(nchar(hash1), 32)  # MD5 is 32 hex characters
})

test_that("SettingsHasher detects changes in load args", {
  hasher <- SettingsHasher$new()

  args1 <- createGetDbCohortMethodDataArgs(
    covariateSettings = getDefaultCovariateSettings()
  )
  args2 <- createGetDbCohortMethodDataArgs(
    covariateSettings = getDefaultCovariateSettings(),
    maxCohortSize = 50000  # Different
  )

  hash1 <- hasher$hashLoadArgs(args1)
  hash2 <- hasher$hashLoadArgs(args2)

  expect_true(hash1 != hash2)
})

test_that("SettingsHasher detects changes in study population args", {
  hasher <- SettingsHasher$new()

  args1 <- createCreateStudyPopulationArgs()
  args2 <- createCreateStudyPopulationArgs(minDaysAtRisk = 30)  # Different

  hash1 <- hasher$hashStudyPopArgs(args1)
  hash2 <- hasher$hashStudyPopArgs(args2)

  expect_true(hash1 != hash2)
})

test_that("SettingsHasher handles NULL arguments", {
  hasher <- SettingsHasher$new()

  hash <- hasher$hashLoadArgs(NULL)
  expect_equal(hash, "")

  hash <- hasher$hashPsArgs(NULL)
  expect_equal(hash, "")
})

test_that("SettingsHasher compareSettingsComponents works with identical specs", {
  hasher <- SettingsHasher$new()

  # Create identical specifications
  analysis <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = getDefaultCovariateSettings()
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs()
  )

  specs1 <- createCmAnalysesSpecifications(
    cmAnalysisList = list(analysis),
    targetComparatorOutcomesList = list(
      createTargetComparatorOutcomes(
        targetId = 1,
        comparatorId = 2,
        outcomes = list(createOutcome(outcomeId = 3))
      )
    )
  )

  specs2 <- createCmAnalysesSpecifications(
    cmAnalysisList = list(analysis),
    targetComparatorOutcomesList = list(
      createTargetComparatorOutcomes(
        targetId = 1,
        comparatorId = 2,
        outcomes = list(createOutcome(outcomeId = 3))
      )
    )
  )

  changes <- hasher$compareSettingsComponents(specs1, specs2)

  expect_false(any(as.logical(changes)))
})

test_that("SettingsHasher compareSettingsComponents detects load args change", {
  hasher <- SettingsHasher$new()

  analysis1 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = getDefaultCovariateSettings()
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs()
  )

  analysis2 <- createCmAnalysis(
    analysisId = 1,
    getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
      covariateSettings = getDefaultCovariateSettings(),
      maxCohortSize = 50000
    ),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createPsArgs = createCreatePsArgs()
  )

  specs1 <- createCmAnalysesSpecifications(
    cmAnalysisList = list(analysis1),
    targetComparatorOutcomesList = list(
      createTargetComparatorOutcomes(
        targetId = 1,
        comparatorId = 2,
        outcomes = list(createOutcome(outcomeId = 3))
      )
    )
  )

  specs2 <- createCmAnalysesSpecifications(
    cmAnalysisList = list(analysis2),
    targetComparatorOutcomesList = list(
      createTargetComparatorOutcomes(
        targetId = 1,
        comparatorId = 2,
        outcomes = list(createOutcome(outcomeId = 3))
      )
    )
  )

  changes <- hasher$compareSettingsComponents(specs1, specs2)

  expect_true(changes$loadArgsChanged)
})

test_that("InvalidationPolicy computes correct deletion scope for load args change", {
  policy <- InvalidationPolicy$new()

  changedComponents <- list(
    loadArgsChanged = TRUE,
    studyPopArgsChanged = FALSE,
    psArgsChanged = FALSE,
    strataArgsChanged = FALSE,
    outcomeModelArgsChanged = FALSE,
    balanceArgsChanged = FALSE,
    analyticsChanged = FALSE
  )

  patterns <- policy$computeInvalidationScope(changedComponents)

  # Should include all major artifact patterns
  expect_true(any(grepl("CmData", patterns)))
  expect_true(any(grepl("StudyPop", patterns)))
  expect_true(any(grepl("Ps", patterns)))
  expect_true(any(grepl("Analysis", patterns)))
})

test_that("InvalidationPolicy computes correct deletion scope for study pop args change", {
  policy <- InvalidationPolicy$new()

  changedComponents <- list(
    loadArgsChanged = FALSE,
    studyPopArgsChanged = TRUE,
    psArgsChanged = FALSE,
    strataArgsChanged = FALSE,
    outcomeModelArgsChanged = FALSE,
    balanceArgsChanged = FALSE,
    analyticsChanged = FALSE
  )

  patterns <- policy$computeInvalidationScope(changedComponents)

  # Should include study pop and downstream
  expect_true(any(grepl("StudyPop", patterns)))
  expect_true(any(grepl("Ps", patterns)))
  expect_true(any(grepl("Analysis", patterns)))

  # But not CmData
  expect_false(any(grepl("CmData_", patterns)))
})

test_that("InvalidationPolicy computes empty scope for outcome addition only", {
  policy <- InvalidationPolicy$new()

  changedComponents <- list(
    loadArgsChanged = FALSE,
    studyPopArgsChanged = FALSE,
    psArgsChanged = FALSE,
    strataArgsChanged = FALSE,
    outcomeModelArgsChanged = FALSE,
    balanceArgsChanged = FALSE,
    analyticsChanged = TRUE  # But only because new outcomes added
  )

  patterns <- policy$computeInvalidationScope(changedComponents)

  # Should be empty - new outcomes don't require deletion
  expect_equal(length(patterns), 0)
})

test_that("InvalidationPolicy provides meaningful messages", {
  policy <- InvalidationPolicy$new()

  changedComponents <- list(
    loadArgsChanged = TRUE,
    studyPopArgsChanged = FALSE,
    psArgsChanged = FALSE,
    strataArgsChanged = FALSE,
    outcomeModelArgsChanged = FALSE,
    balanceArgsChanged = FALSE,
    analyticsChanged = FALSE
  )

  message <- policy$getInvalidationMessage(changedComponents)
  expect_true(grepl("Data loading", message, ignore.case = TRUE))
  expect_true(is.character(message))
})

test_that("ValidateArtifact can list artifacts by type", {
  outputFolder <- tempfile(pattern = "cmArtifacts")
  dir.create(outputFolder)

  on.exit(unlink(outputFolder, recursive = TRUE))

  # Create some test files
  file.create(file.path(outputFolder, "CmData_l1_t1_c2.zip"))
  file.create(file.path(outputFolder, "StudyPop_l1_s1_t1_c2_o3.rds"))
  file.create(file.path(outputFolder, "Ps_l1_p1_t1_c2.rds"))

  validator <- ValidateArtifact$new()

  cmdata <- validator$listArtifactsByType(outputFolder, "cmdata")
  expect_equal(length(cmdata), 1)
  expect_true(grepl("CmData", cmdata))

  studypop <- validator$listArtifactsByType(outputFolder, "studypop")
  expect_equal(length(studypop), 1)
  expect_true(grepl("StudyPop", studypop))

  ps <- validator$listArtifactsByType(outputFolder, "ps")
  expect_equal(length(ps), 1)
  expect_true(grepl("Ps", ps))
})

test_that("ValidateArtifact can validate artifact file existence", {
  outputFolder <- tempfile(pattern = "cmArtifacts")
  dir.create(outputFolder)

  on.exit(unlink(outputFolder, recursive = TRUE))

  validator <- ValidateArtifact$new()

  # Non-existent file
  result <- validator$validateArtifactFile(file.path(outputFolder, "nonexistent.zip"))
  expect_false(result)

  # Create a file
  testFile <- file.path(outputFolder, "test.zip")
  file.create(testFile)

  # Existing file
  result <- validator$validateArtifactFile(testFile)
  expect_true(result)
})

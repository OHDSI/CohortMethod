library(CohortMethod)
library(testthat)

test_that("Check installation", {
  # logFile <- tempfile()
  # ParallelLogger::addDefaultFileLogger(logFile)
  checkCmInstallation(connectionDetails)
  # lines <- readLines(logFile) # Doesn't work inside R check because message() not caught
  # Log file should write response code
  # expect_true(as.logical(grep("Response code: 387848", lines)))
})

test_that("Multiple analyses", {
  outputFolder <- tempfile(pattern = "cmData")
  withr::defer({
    unlink(outputFolder, recursive = TRUE)
  }, testthat::teardown_env())

  tcos1 <- createTargetComparatorOutcomes(targetId = 1,
                                          comparatorId = 2,
                                          outcomes = list(createOutcome(outcomeId = 3,
                                                                        priorOutcomeLookback = 30),
                                                          createOutcome(outcomeId = 4,
                                                                        outcomeOfInterest = FALSE,
                                                                        trueEffectSize = 1)),
                                          excludedCovariateConceptIds = c(1118084, 1124300))
  # Empty cohorts:
  tcos2 <- createTargetComparatorOutcomes(targetId = 998,
                                          comparatorId = 999,
                                          outcomes = list(createOutcome(outcomeId = 3,
                                                                        priorOutcomeLookback = 30),
                                                          createOutcome(outcomeId = 4,
                                                                        outcomeOfInterest = FALSE,
                                                                        trueEffectSize = 1)))

  targetComparatorOutcomesList <- list(tcos1, tcos2)

  covarSettings <- createDefaultCovariateSettings(addDescendantsToExclude = TRUE)

  getDbCmDataArgs <- createGetDbCohortMethodDataArgs(washoutPeriod = 183,
                                                     firstExposureOnly = TRUE,
                                                     removeDuplicateSubjects = "remove all",
                                                     covariateSettings = covarSettings)

  # Duplicating some operations from createGetDbCohortMethodDataArgs just so we test them:
  createStudyPopArgs1 <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                         firstExposureOnly = TRUE,
                                                         restrictToCommonPeriod = TRUE,
                                                         removeDuplicateSubjects = "remove all",
                                                         washoutPeriod = 183,
                                                         censorAtNewRiskWindow = TRUE,
                                                         minDaysAtRisk = 1,
                                                         riskWindowStart = 0,
                                                         startAnchor = "cohort start",
                                                         riskWindowEnd = 30,
                                                         endAnchor = "cohort end")

  createStudyPopArgs2 <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                         firstExposureOnly = TRUE,
                                                         restrictToCommonPeriod = TRUE,
                                                         removeDuplicateSubjects = "keep first",
                                                         washoutPeriod = 183,
                                                         censorAtNewRiskWindow = TRUE,
                                                         minDaysAtRisk = 1,
                                                         riskWindowStart = 0,
                                                         startAnchor = "cohort start",
                                                         riskWindowEnd = 30,
                                                         endAnchor = "cohort end")

  fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(modelType = "cox")

  cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                  description = "No matching, simple outcome model",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createStudyPopArgs = createStudyPopArgs1,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

  createPsArgs <- createCreatePsArgs(prior = createPrior("laplace", variance = 0.01),
                                     estimator = "att")

  matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

  computeSharedCovBalArgs <- createComputeCovariateBalanceArgs()

  # computeCovBalArgs <- createComputeCovariateBalanceArgs(covariateFilter = 0:20 * 1000 + 3)
  computeCovBalArgs <- createComputeCovariateBalanceArgs(covariateFilter = FeatureExtraction::getDefaultTable1Specifications())

  fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(modelType = "cox",
                                                    stratified = TRUE)

  cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                  description = "Matching",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createStudyPopArgs = createStudyPopArgs2,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  matchOnPs = TRUE,
                                  matchOnPsArgs = matchOnPsArgs,
                                  computeSharedCovariateBalance = TRUE,
                                  computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
                                  computeCovariateBalance = TRUE,
                                  computeCovariateBalanceArgs = computeCovBalArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs2)

  truncateIptwArgs <- createTruncateIptwArgs(maxWeight = 10)

  fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(modelType = "cox",
                                                    inversePtWeighting = TRUE)
  cmAnalysis3 <- createCmAnalysis(analysisId = 3,
                                  description = "IPTW",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createStudyPopArgs = createStudyPopArgs2,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  truncateIptw = TRUE,
                                  truncateIptwArgs = truncateIptwArgs,
                                  computeSharedCovariateBalance = TRUE,
                                  computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs3)

  fitOutcomeModelArgs4 <- createFitOutcomeModelArgs(modelType = "cox",
                                                    stratified = TRUE,
                                                    interactionCovariateIds = 8532001)

  cmAnalysis4 <- createCmAnalysis(analysisId = 4,
                                  description = "Matching with gender interaction",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createStudyPopArgs = createStudyPopArgs2,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  matchOnPs = TRUE,
                                  matchOnPsArgs = matchOnPsArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs4)

  cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4)

  analysesToExclude <- data.frame(targetId = c(998, 998),
                                  analysisId = c(3,4))

  # cmAnalysis4 includes interaction terms which should throw a warning
  expect_warning({
    result <- runCmAnalyses(connectionDetails = connectionDetails,
                            cdmDatabaseSchema = "main",
                            exposureTable = "cohort",
                            outcomeTable = "cohort",
                            outputFolder = outputFolder,
                            cmAnalysisList = cmAnalysisList,
                            targetComparatorOutcomesList = targetComparatorOutcomesList,
                            analysesToExclude = analysesToExclude)

  }, "Separable interaction terms found and removed")

  referenceTable <- file.path(outputFolder, "outcomeModelReference.rds")
  expect_true(file.exists(referenceTable))
  ref <- readRDS(referenceTable)
  expect_equal(nrow(ref), 12)

  # analysesToExclude was enforced:
  expect_false(any(ref$targetId == 998 & ref$analysisId == 3))
  expect_false(any(ref$targetId == 998 & ref$analysisId == 4))

  analysisSum <- summarizeAnalyses(result, outputFolder = outputFolder)

  expect_equal(nrow(analysisSum), 12)

  # Make all people one gender for cmAnalysis4 so that interaction terms don't throw a warning
  connection <- DatabaseConnector::connect(connectionDetails)

  withr::defer({
    DatabaseConnector::disconnect(connection)
  }, testthat::teardown_env())

  person <- DatabaseConnector::querySql(connection, "SELECT * FROM person;")
  personNew <- person
  personNew$GENDER_CONCEPT_ID <- rep(8507, nrow(personNew))
  personNew$GENDER_SOURCE_VALUE <- "F"
  DatabaseConnector::insertTable(connection, tableName = "person",
                                 data = personNew,
                                 dropTableIfExists = TRUE, createTable = TRUE)

  warningList <- capture_warnings({
    runRes2 <- runCmAnalyses(connectionDetails = connectionDetails,
                             cdmDatabaseSchema = "main",
                             exposureTable = "cohort",
                             outcomeTable = "cohort",
                             outputFolder = outputFolder,
                             cmAnalysisList = list(cmAnalysis4),
                             targetComparatorOutcomesList = targetComparatorOutcomesList)
  })
  # Should not throw same warning as previous analysis
  expect_false("Separable interaction terms found and removed" %in% warningList)

  analysisSum <- summarizeAnalyses(runRes2, outputFolder = outputFolder)
  refernceTable <- file.path(outputFolder, "outcomeModelReference.rds")
  expect_true(file.exists(refernceTable))
  ref <- readRDS(refernceTable)
  expect_equal(nrow(ref), 4)

  # Reset person table
  DatabaseConnector::insertTable(connection, tableName = "person",
                                 data = person,
                                 dropTableIfExists = TRUE, createTable = TRUE)

  expect_error({
    runCmAnalyses(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = "main",
                  exposureTable = "cohort",
                  outcomeTable = "cohort",
                  outputFolder = outputFolder,
                  cmAnalysisList = list(cmAnalysis4),
                  targetComparatorOutcomesList = targetComparatorOutcomesList,
                  refitPsForEveryOutcome = TRUE,
                  refitPsForEveryStudyPopulation = FALSE)
  }, "Cannot have refitPsForEveryStudyPopulation = FALSE and refitPsForEveryOutcome = TRUE")

  expect_error({
    runCmAnalyses(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = "main",
                  exposureTable = "cohort",
                  outcomeTable = "cohort",
                  outputFolder = outputFolder,
                  cmAnalysisList = list(cmAnalysis4),
                  targetComparatorOutcomesList = targetComparatorOutcomesList <- list(tcos1, tcos2, "brokenObject"))
  })
})


test_that("Warnings for createPs", {
  nsaids <- c(1118084, 1124300)
  covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                                addDescendantsToExclude = TRUE)
  sCohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                             cdmDatabaseSchema = "main",
                                             targetId = 1,
                                             comparatorId = 2,
                                             outcomeIds = c(3, 4),
                                             exposureDatabaseSchema = "main",
                                             outcomeDatabaseSchema = "main",
                                             exposureTable = "cohort",
                                             outcomeTable = "cohort",
                                             covariateSettings = covSettings)

  studyPop <- createStudyPopulation(cohortMethodData = sCohortMethodData,
                                    outcomeId = 3,
                                    riskWindowEnd = 99999)

  studyPop1 <- studyPop %>% subset(select = -c(rowId))
  expect_error(
    createPs(cohortMethodData = sCohortMethodData,
             population = studyPop1),
    regexp = "Missing column rowId in population"
  )

  studyPop2 <- studyPop %>% subset(select = -c(treatment))
  expect_error(
    createPs(cohortMethodData = sCohortMethodData,
             population = studyPop2),
    regexp = "Missing column treatment in population"
  )

  studyPop3 <- sCohortMethodData$cohorts %>% collect()
  ps3a <- createPs(cohortMethodData = sCohortMethodData)
  ps3b <- createPs(cohortMethodData = sCohortMethodData,
                   population = studyPop3)
  expect_identical(ps3a, ps3b)

  covSettings2 <- createDefaultCovariateSettings()
  sCohortMethodData2 <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                              cdmDatabaseSchema = "main",
                                              targetId = 1,
                                              comparatorId = 2,
                                              outcomeIds = c(3, 4),
                                              exposureDatabaseSchema = "main",
                                              outcomeDatabaseSchema = "main",
                                              exposureTable = "cohort",
                                              outcomeTable = "cohort",
                                              covariateSettings = covSettings2)

  studyPop4 <- createStudyPopulation(cohortMethodData = sCohortMethodData2,
                                     outcomeId = 3,
                                     riskWindowEnd = 99999)
  expect_error(
    createPs(cohortMethodData = sCohortMethodData2,
             population = studyPop4),
    regexp = "High correlation between covariate(s) and treatment detected. Perhaps you forgot to exclude part of the exposure definition from the covariates?",
    fixed = TRUE
  )
})

test_that("Warnings for stratifyByPs", {
  nsaids <- c(1118084, 1124300)
  covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                                addDescendantsToExclude = TRUE)
  sCohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                             cdmDatabaseSchema = "main",
                                             targetId = 1,
                                             comparatorId = 2,
                                             outcomeIds = c(3, 4),
                                             exposureDatabaseSchema = "main",
                                             outcomeDatabaseSchema = "main",
                                             exposureTable = "cohort",
                                             outcomeTable = "cohort",
                                             covariateSettings = covSettings)
  studyPop <- createStudyPopulation(cohortMethodData = sCohortMethodData,
                                    outcomeId = 3,
                                    riskWindowEnd = 99999)

  ps <- createPs(cohortMethodData = sCohortMethodData,
                 population = studyPop)

  ps1 <- ps %>% subset(select = -c(rowId))
  expect_error(
    stratifyByPs(population = ps1),
    regexp = "Missing column rowId in population"
  )

  ps2 <- ps %>% subset(select = -c(treatment))
  expect_error(
    stratifyByPs(population = ps2),
    regexp = "Missing column treatment in population"
  )

  ps3 <- ps %>% subset(select = -c(propensityScore))
  expect_error(
    stratifyByPs(population = ps3),
    regexp = "Missing column propensityScore in population"
  )

  expect_warning(stratifyByPs(population = ps,
                              numberOfStrata = 99999),
                 regexp = "Specified 99999 strata, but only")

  vec1 <- rep(1, nrow(ps)/2)
  vec0 <- rep(0, nrow(ps)/2)
  stratCol <- c(vec0, vec1)
  ps4 <- ps %>% mutate(stratCol = stratCol)
  expect_warning({stratifyByPs(population = ps4,
                               numberOfStrata = 4,
                               stratificationColumns = "stratCol")},
                 NA)

})



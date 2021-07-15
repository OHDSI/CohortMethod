library(CohortMethod)
library(testthat)
library(Eunomia)

connectionDetails <- getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

test_that("Check installation", {
  logFile <- tempfile()
  ParallelLogger::addDefaultFileLogger(logFile)
  checkCmInstallation(connectionDetails)
  lines <- readLines(logFile)
  # Log file should write response code
  expect_true(as.logical(grep("Response code: 387848", lines)))
})

test_that("Multiple analyses", {

  tcos1 <- createTargetComparatorOutcomes(targetId = 1,
                                         comparatorId = 2,
                                         outcomeIds = c(3, 4),
                                         excludedCovariateConceptIds = c(1118084, 1124300))
  # Empty cohorts:
  tcos2 <- createTargetComparatorOutcomes(targetId = 998,
                                          comparatorId = 999,
                                          outcomeIds = c(3, 4))

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

  createPsArgs <- createCreatePsArgs(prior = createPrior("laplace", variance = 0.01))

  matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)


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
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs2)

  trimByIptwArgs <- createTrimByIptwArgs(maxWeight = 10, estimator = "att")

  fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(modelType = "cox",
                                                    inversePtWeighting = TRUE,
                                                    estimator = "att",
                                                    maxWeight = 9)
  cmAnalysis3 <- createCmAnalysis(analysisId = 3,
                                  description = "IPTW",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createStudyPopArgs = createStudyPopArgs2,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  trimByIptw = TRUE,
                                  trimByIptwArgs = trimByIptwArgs,
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
  cmAnalysisList <- list(cmAnalysis3)

  outputFolder <- tempfile(pattern = "cmData")

  analysisSum <- summarizeAnalyses(result, outputFolder = outputFolder)

  expect_equal(nrow(analysisSum), 16)

  # cmAnalysis4 includes interaction terms which should throw a warning
  expect_warning({
    result <- runCmAnalyses(connectionDetails = connectionDetails,
                            cdmDatabaseSchema = "main",
                            exposureTable = "cohort",
                            outcomeTable = "cohort",
                            outputFolder = outputFolder,
                            cmAnalysisList = cmAnalysisList,
                            targetComparatorOutcomesList = targetComparatorOutcomesList,
                            outcomeIdsOfInterest = 3,
                            prefilterCovariates = TRUE)
  }, "Separable interaction terms found and removed")

  # Make all people one gender for cmAnalysis4 so that interaction terms don't throw a warning
  connect <- DatabaseConnector::connect(connectionDetails)
  person <- querySql(connect, "SELECT * FROM person;")
  personNew <- person
  personNew$GENDER_CONCEPT_ID <- rep(8507, nrow(personNew))
  insertTable(connect, tableName = "person",
              data = personNew,
              dropTableIfExists = TRUE, createTable = TRUE)
  dbDisconnect(connect)

  runCmAnalyses(connectionDetails = connectionDetails,
                cdmDatabaseSchema = "main",
                exposureTable = "cohort",
                outcomeTable = "cohort",
                outputFolder = outputFolder,
                cmAnalysisList = list(cmAnalysis4),
                targetComparatorOutcomesList = targetComparatorOutcomesList,
                outcomeIdsOfInterest = 3,
                prefilterCovariates = TRUE)

  connect <- DatabaseConnector::connect(connectionDetails)
  personNew$GENDER_CONCEPT_ID <- rep(8507, nrow(personNew))
  insertTable(connect, tableName = "person",
              data = person,
              dropTableIfExists = TRUE, createTable = TRUE)
  dbDisconnect(connect)



test_that("PsFunctions Warnings", {
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
})

unlink(outputFolder, recursive = TRUE)
unlink(connectionDetails$server())


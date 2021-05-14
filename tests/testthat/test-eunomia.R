library(CohortMethod)
library(Eunomia)
library(testthat)

connectionDetails <- getEunomiaConnectionDetails()

Eunomia::createCohorts(connectionDetails)

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

  outputFolder <- tempfile(pattern = "cmData")
  result <- runCmAnalyses(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = "main",
                          exposureTable = "cohort",
                          outcomeTable = "cohort",
                          outputFolder = outputFolder,
                          cmAnalysisList = cmAnalysisList,
                          targetComparatorOutcomesList = targetComparatorOutcomesList,
                          outcomeIdsOfInterest = 3,
                          prefilterCovariates = TRUE)

  analysisSum <- summarizeAnalyses(result, outputFolder = outputFolder)

  expect_equal(nrow(analysisSum), 16)

  unlink(outputFolder, recursive = TRUE)
})

# Remove the Eunomia database:
unlink(connectionDetails$server)

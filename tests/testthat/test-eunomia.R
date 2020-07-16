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

  createStudyPopArgs <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                        minDaysAtRisk = 1,
                                                        riskWindowStart = 0,
                                                        startAnchor = "cohort start",
                                                        riskWindowEnd = 30,
                                                        endAnchor = "cohort end")

  fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(modelType = "cox")

  cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                  description = "No matching, simple outcome model",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createStudyPopArgs = createStudyPopArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

  createPsArgs <- createCreatePsArgs(prior = createPrior("laplace", variance = 0.01))

  matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)


  fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(modelType = "cox",
                                                    stratified = TRUE)

  cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                  description = "Matching",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createStudyPopArgs = createStudyPopArgs,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  matchOnPs = TRUE,
                                  matchOnPsArgs = matchOnPsArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs2)


  cmAnalysisList <- list(cmAnalysis1, cmAnalysis2)

  outputFolder <- tempfile(pattern = "cmData")
  result <- runCmAnalyses(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = "main",
                          exposureTable = "cohort",
                          outcomeTable = "cohort",
                          outputFolder = outputFolder,
                          cmAnalysisList = cmAnalysisList,
                          targetComparatorOutcomesList = targetComparatorOutcomesList)

  analysisSum <- summarizeAnalyses(result, outputFolder = outputFolder)

  expect_equal(nrow(analysisSum), 8)

  unlink(outputFolder, recursive = TRUE)
})

# Remove the Eunomia database:
unlink(connectionDetails$server)

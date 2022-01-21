library(CohortMethod)
library(testthat)

test_that("Multiple analyses", {
  tcos1 <- createTargetComparatorOutcomes(targetId = 1,
                                          comparatorId = 2,
                                          outcomeIds = c(3, 4),
                                          excludedCovariateConceptIds = c(1118084, 1124300))
  targetComparatorOutcomesList <- list(tcos1)

  covarSettings <- createCovariateSettings(useDemographicsGender = TRUE,
                                           useDemographicsAge = TRUE)

  getDbCmDataArgs <- createGetDbCohortMethodDataArgs(washoutPeriod = 183,
                                                     firstExposureOnly = TRUE,
                                                     removeDuplicateSubjects = "remove all",
                                                     covariateSettings = covarSettings,
                                                     maxCohortSize = 1500)

  createStudyPopArgs <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
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

  createPsArgs <- createCreatePsArgs(prior = createPrior("laplace", variance = 0.01))

  matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)


  fitOutcomeModelArgs <- createFitOutcomeModelArgs(modelType = "cox",
                                                   stratified = TRUE)

  cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                  description = "Matching",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createStudyPopArgs = createStudyPopArgs,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  matchOnPs = TRUE,
                                  matchOnPsArgs = matchOnPsArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs)

  cmAnalysisList <- list(cmAnalysis1)

  outputFolder <- tempfile(pattern = "cmData")
  result <- runCmAnalyses(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = "main",
                          exposureTable = "cohort",
                          outcomeTable = "cohort",
                          outputFolder = outputFolder,
                          cmAnalysisList = cmAnalysisList,
                          targetComparatorOutcomesList = targetComparatorOutcomesList,
                          refitPsForEveryOutcome = TRUE,
                          prefilterCovariates = TRUE)
  expect_equal(result$sharedPsFile, c("", ""))

  analysisSum <- summarizeAnalyses(result, outputFolder = outputFolder)

  expect_equal(nrow(analysisSum), 2)

  unlink(outputFolder, recursive = TRUE)
})

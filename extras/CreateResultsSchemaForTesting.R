# Code to create the inst/Results_Eunomia.zip file used in testing

library(CohortMethod)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

outputFolder <- tempfile(pattern = "cmOutput")

tcos1 <- createTargetComparatorOutcomes(
  targetId = 1,
  comparatorId = 2,
  outcomes = list(
    createOutcome(
      outcomeId = 3,
      priorOutcomeLookback = 30
    ),
    createOutcome(
      outcomeId = 4,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  ),
  excludedCovariateConceptIds = c(1118084, 1124300)
)
# Empty cohorts:
tcos2 <- createTargetComparatorOutcomes(
  targetId = 998,
  comparatorId = 999,
  outcomes = list(
    createOutcome(
      outcomeId = 3,
      priorOutcomeLookback = 30
    ),
    createOutcome(
      outcomeId = 4,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  )
)

targetComparatorOutcomesList <- list(tcos1, tcos2)

covarSettings <- createDefaultCovariateSettings(addDescendantsToExclude = TRUE)

getDbCmDataArgs <- createGetDbCohortMethodDataArgs(
  washoutPeriod = 183,
  firstExposureOnly = TRUE,
  removeDuplicateSubjects = "remove all",
  covariateSettings = covarSettings
)

# Duplicating some operations from createGetDbCohortMethodDataArgs just so we test them:
createStudyPopArgs1 <- createCreateStudyPopulationArgs(
  removeSubjectsWithPriorOutcome = TRUE,
  firstExposureOnly = TRUE,
  restrictToCommonPeriod = TRUE,
  removeDuplicateSubjects = "remove all",
  washoutPeriod = 183,
  censorAtNewRiskWindow = TRUE,
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 30,
  endAnchor = "cohort end"
)

createStudyPopArgs2 <- createCreateStudyPopulationArgs(
  removeSubjectsWithPriorOutcome = TRUE,
  firstExposureOnly = TRUE,
  restrictToCommonPeriod = TRUE,
  removeDuplicateSubjects = "keep first",
  washoutPeriod = 183,
  censorAtNewRiskWindow = TRUE,
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 30,
  endAnchor = "cohort end"
)

fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(modelType = "cox")

cmAnalysis1 <- createCmAnalysis(
  analysisId = 1,
  description = "No matching, simple outcome model",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs1,
  fitOutcomeModelArgs = fitOutcomeModelArgs1
)

createPsArgs <- createCreatePsArgs(
  prior = createPrior("laplace", variance = 0.01),
  estimator = "att"
)

matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

computeSharedCovBalArgs <- createComputeCovariateBalanceArgs()

# computeCovBalArgs <- createComputeCovariateBalanceArgs(covariateFilter = 0:20 * 1000 + 3)
computeCovBalArgs <- createComputeCovariateBalanceArgs(covariateFilter = FeatureExtraction::getDefaultTable1Specifications())

fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = TRUE
)

cmAnalysis2 <- createCmAnalysis(
  analysisId = 2,
  description = "Matching",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  matchOnPsArgs = matchOnPsArgs,
  computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
  computeCovariateBalanceArgs = computeCovBalArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs2
)

truncateIptwArgs <- createTruncateIptwArgs(maxWeight = 10)

fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(
  modelType = "cox",
  inversePtWeighting = TRUE
)
cmAnalysis3 <- createCmAnalysis(
  analysisId = 3,
  description = "IPTW",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  truncateIptwArgs = truncateIptwArgs,
  computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs3
)

fitOutcomeModelArgs4 <- createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = TRUE,
  interactionCovariateIds = 8532001
)

cmAnalysis4 <- createCmAnalysis(
  analysisId = 4,
  description = "Matching with gender interaction",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs4
)

cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4)

analysesToExclude <- data.frame(
  targetId = c(998, 998),
  analysisId = c(3, 4)
)

result <- runCmAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  exposureTable = "cohort",
  outcomeTable = "cohort",
  outputFolder = outputFolder,
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = analysesToExclude
)


exportToCsv(outputFolder, databaseId = "Eunomia")

file.rename(file.path(outputFolder, "export", "Results_Eunomia.zip"),
            file.path("inst", "Results_Eunomia.zip"))

unlink(connectionDetails$server())
unlink(outputFolder, recursive = TRUE)

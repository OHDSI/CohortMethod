library(CohortMethod)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

outputFolder <- tempfile(pattern = "cmData")

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

# Empty comparator cohort only:
tcos3 <- createTargetComparatorOutcomes(
  targetId = 1,
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

targetComparatorOutcomesList <- list(tcos1, tcos2, tcos3)

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
  createStudyPopulationArgs = createStudyPopArgs1,
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
  createStudyPopulationArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  matchOnPsArgs = matchOnPsArgs,
  computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
  computeCovariateBalanceArgs = computeCovBalArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs2
)

stratifyByPsArgs <- createStratifyByPsArgs()

cmAnalysis3 <- createCmAnalysis(
  analysisId = 3,
  description = "Stratification",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopulationArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  stratifyByPsArgs = stratifyByPsArgs,
  computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
  computeCovariateBalanceArgs = computeCovBalArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs2
)

truncateIptwArgs <- createTruncateIptwArgs(maxWeight = 10)

fitOutcomeModelArgs4 <- createFitOutcomeModelArgs(
  modelType = "cox",
  inversePtWeighting = TRUE,
  bootstrapCi = TRUE,
  bootstrapReplicates = 200
)
cmAnalysis4 <- createCmAnalysis(
  analysisId = 4,
  description = "IPTW",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopulationArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  truncateIptwArgs = truncateIptwArgs,
  computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs4
)

fitOutcomeModelArgs5 <- createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = TRUE,
  interactionCovariateIds = 8532001
)

cmAnalysis5 <- createCmAnalysis(
  analysisId = 5,
  description = "Matching with gender interaction",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopulationArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs5
)

cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5)

analysesToExclude <- data.frame(
  targetId = c(998, 998),
  analysisId = c(3, 4)
)

# cmAnalysis5 includes interaction terms which should throw a warning

result <- runCmAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  exposureTable = "cohort",
  outcomeTable = "cohort",
  outputFolder = outputFolder,
  cmAnalysesSpecifications = createCmAnalysesSpecifications(
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = targetComparatorOutcomesList,
    analysesToExclude = analysesToExclude
  )
)
# referenceTable <- getFileReference(outputFolder)
# cmDiagnosticThresholds <- createCmDiagnosticThresholds()
CohortMethod::exportToCsv(outputFolder, databaseId = "Test")

profiles <- readr::read_csv(file.path(outputFolder, "export", "cm_likelihood_profile.csv"))

unlink(outputFolder)

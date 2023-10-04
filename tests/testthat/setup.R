library(Eunomia)
library(CohortMethod)

connectionDetails <- getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

# fitOutcomeModel ----
## Study Population ----
nsaids <- c(1118084, 1124300)

covSettings <- createDefaultCovariateSettings(
  excludedCovariateConceptIds = nsaids,
  addDescendantsToExclude = TRUE
)

sCohortMethodData <- getDbCohortMethodData(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  targetId = 1,
  comparatorId = 2,
  outcomeIds = c(3, 4),
  exposureDatabaseSchema = "main",
  outcomeDatabaseSchema = "main",
  exposureTable = "cohort",
  outcomeTable = "cohort",
  covariateSettings = covSettings
)

studyPop <- createStudyPopulation(
  cohortMethodData = sCohortMethodData,
  outcomeId = 3,
  riskWindowEnd = 99999
)

## CohortMethod Data ----
sCohortMethodData <- getDbCohortMethodData(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  targetId = 1,
  comparatorId = 2,
  outcomeIds = c(3, 4),
  exposureDatabaseSchema = "main",
  outcomeDatabaseSchema = "main",
  exposureTable = "cohort",
  outcomeTable = "cohort",
  covariateSettings = covSettings
)

ps <- createPs(cohortMethodData = sCohortMethodData, population = studyPop)
studyPopStratisfied <- stratifyByPs(ps, 5)
studyPopMatched <- matchOnPs(population = ps)

## runCmAnalyses ----
outputFolder <- tempfile(pattern = "cmData")

covarSettings <- createDefaultCovariateSettings(addDescendantsToExclude = TRUE)

getDbCmDataArgs <- createGetDbCohortMethodDataArgs(
  washoutPeriod = 183,
  firstExposureOnly = TRUE,
  removeDuplicateSubjects = "remove all",
  covariateSettings = covarSettings
)

createPsArgs <- createCreatePsArgs(
  prior = createPrior("laplace", variance = 0.01),
  estimator = "att"
)

matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

computeSharedCovBalArgs <- createComputeCovariateBalanceArgs()

computeCovBalArgs <- createComputeCovariateBalanceArgs(
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)

truncateIptwArgs <- createTruncateIptwArgs(maxWeight = 10)

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

analysesToExclude <- data.frame(
  targetId = c(998, 998),
  analysisId = c(3, 4)
)

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

### Analysis 1 ----
fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(
  modelType = "cox"
)

cmAnalysis1 <- createCmAnalysis(
  analysisId = 1,
  description = "No matching, simple outcome model",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs1,
  fitOutcomeModelArgs = fitOutcomeModelArgs1
)

### Analysis 2 ----
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

### Analysis 3 ----
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

### Analysis 4 ----
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

## Clean-up ----
withr::defer(
  {
    unlink(outputFolder)

    # Remove the Eunomia database:
    unlink(connectionDetails$server())
    if (getOption("use.devtools.sql_shim", FALSE)) {
      # Remove symbolic link to sql folder created when devtools::test loads helpers
      packageRoot <- normalizePath(system.file("..", package = "CohortMethod"))
      unlink(file.path(packageRoot, "sql"), recursive = FALSE)
    }
  },
  testthat::teardown_env()
)

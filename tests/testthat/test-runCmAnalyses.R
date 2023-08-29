# Setup ----
library(testthat)
library(CohortMethod)

## Analysis 1 ----
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

## Analysis 2 ----
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

## Analysis 3 ----
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

## Analysis 4 ----
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

# Tests ----
test_that("Warnings set 1/2", {
  unlink(outputFolder, recursive = TRUE)
  warn1 <- capture_warnings({
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      analysesToExclude = analysesToExclude
    )
  })

  warn2 <- capture_warnings({
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = list(cmAnalysis4),
      targetComparatorOutcomesList = targetComparatorOutcomesList
    )
  })

  expect_true(!identical(warn1, warn2))
})

test_that("targetComparatorOutcomeList", {
  unlink(outputFolder, recursive = TRUE)
  res <- suppressWarnings(runCmAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    outputFolder = outputFolder,
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = targetComparatorOutcomesList
  ))

  # Dimensions
  expect_identical(dim(res), c(16L, 17L))

  unlink(outputFolder, recursive = TRUE)
  ### list() ----
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = list()
    )),
    "Must have length >= 1"
  )

  ### NULL ----
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = NULL
    )),
    "Must be of type 'list'"
  )

  ### list(list(), list()) ----
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = list(list(), list())
    )),
    "Must inherit from.+'targetComparatorOutcomes'"
  )

  ### list(NULL, NULL) ----
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = list(NULL, NULL)
    )),
    "Must inherit from.+'targetComparatorOutcomes'"
  )
})
#
# test_that("tempEmulationSchema", {
#   unlink(outputFolder, recursive = TRUE)
#   ### "main"
#   expect_no_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       # Eunomia
#       tempEmulationSchema = "main"
#     )
#   ))
#
#   ### 3 ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       tempEmulationSchema = 3
#     )),
#     "Must be of type 'character'"
#   )
#
#   ### c("main", "main") ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       tempEmulationSchema = c("main", "main")
#     )),
#     "Must have length 1"
#   )
# })
#
# test_that("exposureDatabaseSchema", {
#   ### "main" ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_no_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       exposureDatabaseSchema = "main"
#     ))
#   )
#
#   ### "SchemaThatDoesNotExist" ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       exposureDatabaseSchema = "SchemaThatDoesNotExist"
#     )),
#     "no such table: SchemaThatDoesNotExist.cohort"
#   )
#
#   ### 3 ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       exposureDatabaseSchema = 3
#     )),
#     "Must be of type 'character'"
#   )
#
#   ### c("main", "main") ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       exposureDatabaseSchema = c("main", "main")
#     )),
#     "Must have length 1"
#   )
# })
#
# test_that("outcomeDatabaseSchema", {
#   ### "main" ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_no_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       outcomeDatabaseSchema = "main"
#     ))
#   )
#
#   ### 3 ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       outcomeDatabaseSchema = 3
#     )),
#     "Must be of type 'character'"
#   )
#
#   ### c("main", "main") ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       outcomeDatabaseSchema = c("main", "main")
#     )),
#     "Must have length 1"
#   )
# })
#
# test_that("cdmVersion", {
#   ### "5" ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_no_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       cdmVersion = "5"
#     ))
#   )
#
#   ### 5 ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       cdmVersion = 5
#     )),
#     "Must be of type 'character'"
#   )
#
#   ### "Five" ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       cdmVersion = "Five"
#     )),
#     "All elements must have exactly 1 characters"
#   )
#
#   ### c("4", "5", "6") ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       cdmVersion = c("4", "5", "6")
#     )),
#     "Must have length 1"
#   )
# })
#
# test_that("analysesToExclude", {
#   unlink(outputFolder, recursive = TRUE)
#
#   analysesToExclude <- data.frame(
#     targetId = c(998, 998),
#     analysisId = c(3, 4)
#   )
#
#   ### analysesToExclude ----
#   expect_no_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       analysesToExclude = analysesToExclude
#     ))
#   )
#
#   ### NULL ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_no_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       analysesToExclude = NULL
#     ))
#   )
#
#   ### data.frame() ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       analysesToExclude = data.frame()
#     )),
#     "should contain columns 'targetId', 'comparatorId', 'outcomeId', or 'analysisId'"
#   )
#
#   ### data.frame(numeric()) ----
#   expect_warning(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       analysesToExclude = data.frame(
#         targetId = numeric(),
#         comparatorId = numeric(),
#         outcomeId = numeric(),
#         analysisId = numeric()
#       ))
#     ),
#     "Passed `data.frame` with 0 rows to parameter: `analysesToExclude`, no analyses excluded."
#   )
# })
#
# test_that("refitPsForEveryOutcome", {
#   ### FALSE ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_no_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       refitPsForEveryOutcome = FALSE
#     ))
#   )
#
#   ### TRUE ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_no_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       refitPsForEveryOutcome = TRUE
#     ))
#   )
#
#   ### 0 ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       refitPsForEveryOutcome = 0
#     )),
#     "Must be of type 'logical'"
#   )
# })
#
# test_that("refitPsForEveryStudyPopulation", {
#   ### FALSE ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_no_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       refitPsForEveryStudyPopulation = FALSE
#     ))
#   )
#
#   ### TRUE ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_no_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       refitPsForEveryStudyPopulation = TRUE
#     ))
#   )
#
#   ### 0 ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     suppressWarnings(runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = cmAnalysisList,
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       refitPsForEveryStudyPopulation = 0
#     )),
#     "Must be of type 'logical'"
#   )
# })
#
# test_that("refitPsForEveryX", {
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(suppressWarnings(
#     runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = list(cmAnalysis4),
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       refitPsForEveryOutcome = TRUE,
#       refitPsForEveryStudyPopulation = FALSE
#     )
#   ),
#     "Cannot have refitPsForEveryStudyPopulation = FALSE and refitPsForEveryOutcome = TRUE"
#   )
# })
#
# test_that("multiThreadingSettings", {
#   ### createDefaultMultiThreadingSettings() ----
#   # unlink(outputFolder, recursive = TRUE)
#   # expect_no_error(
#   #   suppressWarnings(runCmAnalyses(
#   #     connectionDetails = connectionDetails,
#   #     cdmDatabaseSchema = "main",
#   #     exposureTable = "cohort",
#   #     outcomeTable = "cohort",
#   #     outputFolder = outputFolder,
#   #     cmAnalysisList = list(cmAnalysis4),
#   #     targetComparatorOutcomesList = targetComparatorOutcomesList,
#   #     multiThreadingSettings = createDefaultMultiThreadingSettings(4)
#   #   ))
#   # )
#
#   ### NULL ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = list(cmAnalysis4),
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       multiThreadingSettings = NULL
#     )
#   )
#
#   ### list() ----
#   unlink(outputFolder, recursive = TRUE)
#   expect_error(
#     runCmAnalyses(
#       connectionDetails = connectionDetails,
#       cdmDatabaseSchema = "main",
#       exposureTable = "cohort",
#       outcomeTable = "cohort",
#       outputFolder = outputFolder,
#       cmAnalysisList = list(cmAnalysis4),
#       targetComparatorOutcomesList = targetComparatorOutcomesList,
#       multiThreadingSettings = list()
#     )
#   )
# })

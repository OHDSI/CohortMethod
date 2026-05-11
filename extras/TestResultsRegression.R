# TestResultsRegression.R
#
# Verifies that the current code produces identical results to the released version.
# Compares exported CSV results against the reference zip committed on main.
#
# Usage (from package root):
#   RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript --vanilla extras/TestResultsRegression.R

library(CohortMethod)

# ===========================================================================
# 1. Extract reference results from git main branch
# ===========================================================================
referenceDir <- tempfile(pattern = "cmReference")
dir.create(referenceDir)

refZipPath <- file.path(referenceDir, "Results_Eunomia.zip")
# Use the script's directory to find the package root (extras/ is one level below)
packageRoot <- normalizePath(file.path(getwd()))
if (!file.exists(file.path(packageRoot, ".git"))) {
  # Fallback: try installed package location

  packageRoot <- normalizePath(system.file("..", package = "CohortMethod"))
}
# Try local main first, then origin/main
exitCode <- system2("git", c("-C", packageRoot, "show", "main:inst/Results_Eunomia.zip"),
                    stdout = refZipPath, stderr = FALSE)
if (exitCode != 0) {
  exitCode <- system2("git", c("-C", packageRoot, "show", "origin/main:inst/Results_Eunomia.zip"),
                      stdout = refZipPath, stderr = FALSE)
}
if (exitCode != 0) {
  stop("Failed to extract inst/Results_Eunomia.zip from main branch. ",
       "Make sure you are in the CohortMethod git repo and 'main' branch exists.")
}
unzip(refZipPath, exdir = referenceDir)
message("Reference results extracted from main branch")

# ===========================================================================
# 2. Run full analysis with current code (mirrors CreateResultsSchemaForTesting.R)
# ===========================================================================
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

outputFolder <- tempfile(pattern = "cmRegression")

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

createStudyPopArgs1 <- createCreateStudyPopulationArgs(
  removeSubjectsWithPriorOutcome = TRUE,
  censorAtNewRiskWindow = TRUE,
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 30,
  endAnchor = "cohort end"
)

createStudyPopArgs2 <- createCreateStudyPopulationArgs(
  removeSubjectsWithPriorOutcome = TRUE,
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

computeCovBalArgs <- createComputeCovariateBalanceArgs(
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)

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

truncateIptwArgs <- createTruncateIptwArgs(maxWeight = 10)

fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(
  modelType = "cox",
  inversePtWeighting = TRUE
)
cmAnalysis3 <- createCmAnalysis(
  analysisId = 3,
  description = "IPTW",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopulationArgs = createStudyPopArgs2,
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
  createStudyPopulationArgs = createStudyPopArgs2,
  createPsArgs = createPsArgs,
  matchOnPsArgs = matchOnPsArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs4
)

cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4)

analysesToExclude <- data.frame(
  targetId = c(998, 998),
  analysisId = c(3, 4)
)

message("Running analyses with current code...")
result <- runCmAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  exposureTable = "cohort",
  outcomeTable = "cohort",
  outputFolder = outputFolder,
  databaseId = "Eunomia",
  cmAnalysesSpecifications = createCmAnalysesSpecifications(
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = targetComparatorOutcomesList,
    analysesToExclude = analysesToExclude
  )
)

exportToCsv(outputFolder, databaseId = "Eunomia")
message("Analysis complete, results exported")

# ===========================================================================
# 3. Compare key CSV files
# ===========================================================================
newDir <- file.path(outputFolder, "export")

compareNumericColumns <- function(file, columns, tolerance = 1e-6) {
  refPath <- file.path(referenceDir, file)
  newPath <- file.path(newDir, file)

  if (!file.exists(refPath)) {
    message(sprintf("  SKIP: %s (not in reference)", file))
    return(invisible(NULL))
  }
  if (!file.exists(newPath)) {
    stop(sprintf("Missing output file: %s", file))
  }

  ref <- readr::read_csv(refPath, show_col_types = FALSE)
  new <- readr::read_csv(newPath, show_col_types = FALSE)

  # Sort both by common key columns for stable comparison
  keyColumns <- intersect(c("analysis_id", "target_id", "comparator_id",
                            "outcome_id", "interaction_covariate_id"), names(ref))
  ref <- ref[do.call(order, ref[keyColumns]), ]
  new <- new[do.call(order, new[keyColumns]), ]

  if (nrow(ref) != nrow(new)) {
    stop(sprintf("Row count mismatch in %s: reference=%d, new=%d",
                 file, nrow(ref), nrow(new)))
  }

  columnsChecked <- 0
  for (col in columns) {
    if (col %in% names(ref) && col %in% names(new)) {
      refVals <- as.numeric(ref[[col]])
      newVals <- as.numeric(new[[col]])
      diffs <- abs(refVals - newVals)
      diffs <- diffs[!is.na(diffs)]
      if (length(diffs) > 0 && any(diffs > tolerance)) {
        stop(sprintf("Column '%s' in %s differs: max diff = %g (tolerance = %g)",
                     col, file, max(diffs), tolerance))
      }
      columnsChecked <- columnsChecked + 1
    }
  }
  message(sprintf("  PASS: %s (%d numeric columns within tolerance %g)",
                  file, columnsChecked, tolerance))
}

compareExactColumns <- function(file, columns) {
  refPath <- file.path(referenceDir, file)
  newPath <- file.path(newDir, file)

  if (!file.exists(refPath)) {
    message(sprintf("  SKIP: %s (not in reference)", file))
    return(invisible(NULL))
  }
  if (!file.exists(newPath)) {
    stop(sprintf("Missing output file: %s", file))
  }

  ref <- readr::read_csv(refPath, show_col_types = FALSE)
  new <- readr::read_csv(newPath, show_col_types = FALSE)

  keyColumns <- intersect(c("analysis_id", "target_id", "comparator_id",
                            "outcome_id"), names(ref))
  ref <- ref[do.call(order, ref[keyColumns]), ]
  new <- new[do.call(order, new[keyColumns]), ]

  if (nrow(ref) != nrow(new)) {
    stop(sprintf("Row count mismatch in %s: reference=%d, new=%d",
                 file, nrow(ref), nrow(new)))
  }

  columnsChecked <- 0
  for (col in columns) {
    if (col %in% names(ref) && col %in% names(new)) {
      if (!identical(ref[[col]], new[[col]])) {
        # Show first difference for debugging
        idx <- which(ref[[col]] != new[[col]])[1]
        stop(sprintf("Column '%s' in %s differs at row %d: reference='%s', new='%s'",
                     col, file, idx, ref[[col]][idx], new[[col]][idx]))
      }
      columnsChecked <- columnsChecked + 1
    }
  }
  message(sprintf("  PASS: %s (%d columns match exactly)", file, columnsChecked))
}

message("")
message("=== Results Regression Test ===")
message("")

compareNumericColumns("cm_result.csv",
  c("log_rr", "se_log_rr", "ci_95_lb", "ci_95_ub", "p",
    "calibrated_log_rr", "calibrated_se_log_rr"))

# Attrition: compare final population sizes per analysis/target/comparator/outcome.
# Intermediate attrition steps may differ in order due to two-phase population creation,
# but the final population must match.
compareFinalAttrition <- function() {
  file <- "cm_attrition.csv"
  refPath <- file.path(referenceDir, file)
  newPath <- file.path(newDir, file)

  if (!file.exists(refPath)) {
    message(sprintf("  SKIP: %s (not in reference)", file))
    return(invisible(NULL))
  }

  ref <- readr::read_csv(refPath, show_col_types = FALSE)
  new <- readr::read_csv(newPath, show_col_types = FALSE)

  # Get last (final) attrition row per group
  keyColumns <- intersect(c("target_comparator_id", "analysis_id", "outcome_id",
                            "exposure_id", "database_id"), names(ref))
  refFinal <- ref |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keyColumns))) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keyColumns)))
  newFinal <- new |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keyColumns))) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keyColumns)))

  if (nrow(refFinal) != nrow(newFinal)) {
    stop(sprintf("Final attrition group count mismatch: reference=%d, new=%d",
                 nrow(refFinal), nrow(newFinal)))
  }
  for (col in c("subjects", "exposures", "outcomes")) {
    if (!identical(refFinal[[col]], newFinal[[col]])) {
      stop(sprintf("Final attrition column '%s' differs", col))
    }
  }
  message(sprintf("  PASS: %s (final population sizes match across %d groups)",
                  file, nrow(refFinal)))
}
compareFinalAttrition()

compareExactColumns("cm_diagnostics_summary.csv",
  c("balance_diagnostic", "shared_balance_diagnostic",
    "equipoise_diagnostic", "mdrr_diagnostic"))

compareNumericColumns("cm_diagnostics_summary.csv",
  c("max_sdm", "equipoise", "mdrr"), tolerance = 1e-6)

compareNumericColumns("cm_covariate_balance.csv",
  c("std_diff_before", "std_diff_after"), tolerance = 1e-4)

compareNumericColumns("cm_interaction_result.csv",
  c("log_rr", "se_log_rr"), tolerance = 1e-6)

message("")
message("=== ALL CHECKS PASSED ===")
message("")

# ===========================================================================
# Cleanup
# ===========================================================================
unlink(referenceDir, recursive = TRUE)
unlink(connectionDetails$server())
unlink(outputFolder, recursive = TRUE)

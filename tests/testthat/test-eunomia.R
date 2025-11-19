library(CohortMethod)
library(testthat)

if (!isFALSE(tryCatch(find.package("Eunomia"), error = function(e) FALSE))) {

  # Eunomia connection details set in setup.R

  outputFolder1 <- tempfile(pattern = "cmData")
  outputFolder2 <- tempfile(pattern = "cmData")
  outputFolder3 <- tempfile(pattern = "cmData")
  outputFolder4 <- tempfile(pattern = "cmData")
  outputFolder5 <- tempfile(pattern = "cmData")
  if (is_checking()) {
    withr::defer(
      {
        unlink(outputFolder1, recursive = TRUE)
        unlink(outputFolder2, recursive = TRUE)
        unlink(outputFolder3, recursive = TRUE)
        unlink(outputFolder4, recursive = TRUE)
        unlink(outputFolder5, recursive = TRUE)
      },
      testthat::teardown_env()
    )
  }

  test_that("Check installation", {
    expect_no_error(
      checkCmInstallation(connectionDetails)
    )
  })

  test_that("Multiple analyses", {

    # A test for a general analyses with mutliple TCOs and analyses, with analyses to exclude

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
      firstExposureOnly = TRUE,
      restrictToCommonPeriod = TRUE,
      removeDuplicateSubjects = "remove all",
      washoutPeriod = 183,
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
      prior = createPrior("laplace", variance = 0.01, exclude = c(0)),
      estimator = "att"
    )

    matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

    computeSharedCovBalArgs <- createComputeCovariateBalanceArgs()

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
    expect_warning(
      {
        result <- runCmAnalyses(
          connectionDetails = connectionDetails,
          cdmDatabaseSchema = "main",
          exposureTable = "cohort",
          outcomeTable = "cohort",
          outputFolder = outputFolder1,
          cmAnalysesSpecifications = createCmAnalysesSpecifications(
            cmAnalysisList = cmAnalysisList,
            targetComparatorOutcomesList = targetComparatorOutcomesList,
            analysesToExclude = analysesToExclude
          )
        )
      },
      "Separable interaction terms found and removed"
    )

    ref <- getFileReference(outputFolder1)
    expect_equal(nrow(ref), 26)

    # analysesToExclude was enforced:
    expect_false(any(ref$targetId == 998 & ref$analysisId == 3))
    expect_false(any(ref$targetId == 998 & ref$analysisId == 4))

    analysisSum <- getResultsSummary(outputFolder1)

    expect_equal(nrow(analysisSum), 26)

    exportToCsv(outputFolder1, databaseId = "Test")
    cohortMethodResultFile <- file.path(outputFolder1, "export", "cm_result.csv")
    expect_true(file.exists(cohortMethodResultFile))

    diagnosticsSummary <- readr::read_csv(file.path(outputFolder1, "export", "cm_diagnostics_summary.csv"), show_col_types = FALSE)
    expect_true(all(diagnosticsSummary$ease_diagnostic == "NOT EVALUATED"))

    targetComparatorOutcome <- readr::read_csv(file.path(outputFolder1, "export", "cm_target_comparator_outcome.csv"), show_col_types = FALSE)
    expect_true(is.numeric(targetComparatorOutcome$outcome_of_interest))

    # Verify negative controls have diagnostics:
    ncDiagnostics <- diagnosticsSummary |>
      inner_join(targetComparatorOutcome, by = join_by(target_id, comparator_id, outcome_id)) |>
      filter(.data$outcome_of_interest == 0)
    expect_gt(nrow(ncDiagnostics), 0)

    # Check if there is data for the Kaplan Meier curves:
    km <- readr::read_csv(file.path(outputFolder1, "export", "cm_kaplan_meier_dist.csv"), show_col_types = FALSE)
    expect_true(nrow(km) > 0)
  })


  test_that("Multiple analyses with refit PS", {

    # A test for refitting the propensity model for every outcome

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

    targetComparatorOutcomesList <- list(tcos1)

    covarSettings <- createCovariateSettings(
      useDemographicsGender = TRUE,
      useDemographicsAge = TRUE
    )

    getDbCmDataArgs <- createGetDbCohortMethodDataArgs(
      firstExposureOnly = TRUE,
      restrictToCommonPeriod = TRUE,
      removeDuplicateSubjects = "remove all",
      washoutPeriod = 183,
      covariateSettings = covarSettings
    )

    createStudyPopArgs <- createCreateStudyPopulationArgs(
      removeSubjectsWithPriorOutcome = TRUE,
      censorAtNewRiskWindow = TRUE,
      minDaysAtRisk = 1,
      riskWindowStart = 0,
      startAnchor = "cohort start",
      riskWindowEnd = 30,
      endAnchor = "cohort end"
    )

    createPsArgs <- createCreatePsArgs(
      prior = createPrior("laplace", variance = 0.01, exclude = c(0)),
      estimator = "att"
    )

    matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

    fitOutcomeModelArgs <- createFitOutcomeModelArgs(
      modelType = "cox",
      stratified = TRUE
    )

    expect_message({
      cmAnalysis <- createCmAnalysis(
        analysisId = 1,
        description = "Matching",
        getDbCohortMethodDataArgs = getDbCmDataArgs,
        createStudyPopulationArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )},
      "not computing covariate balance"
    )

    cmAnalysisList <- list(cmAnalysis)

    # Warning due to poor convergence (because data too small for outcome model)
    expect_warning(
      {
        result <- runCmAnalyses(
          connectionDetails = connectionDetails,
          cdmDatabaseSchema = "main",
          exposureTable = "cohort",
          outcomeTable = "cohort",
          outputFolder = outputFolder2,
          cmAnalysesSpecifications = createCmAnalysesSpecifications(
            cmAnalysisList = cmAnalysisList,
            targetComparatorOutcomesList = targetComparatorOutcomesList,
            refitPsForEveryOutcome = TRUE
          )
        )
      }, "BLR convergence"
    )

    expect_equal(result$sharedPsFile, c("", ""))
    expect_equal(result$psFile, c("Ps_l1_s1_p1_t1_c2_o3.rds", "Ps_l1_s1_p1_t1_c2_o4.rds"))
  })

  test_that("High correlation covariates", {

    # A test to see if high-correlation covariates are handled correctly

    # Not excluding drug concepts:
    tcos1 <- createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(
          outcomeId = 3
        )
      )
    )

    targetComparatorOutcomesList <- list(tcos1)

    covarSettings <- createCovariateSettings(
      useDrugEraShortTerm = TRUE,
      addDescendantsToExclude = TRUE
    )

    getDbCmDataArgs <- createGetDbCohortMethodDataArgs(
      firstExposureOnly = TRUE,
      restrictToCommonPeriod = FALSE,
      removeDuplicateSubjects = "remove all",
      washoutPeriod = 183,
      covariateSettings = covarSettings
    )

    createStudyPopArgs <- createCreateStudyPopulationArgs(
      removeSubjectsWithPriorOutcome = TRUE,
      censorAtNewRiskWindow = TRUE,
      minDaysAtRisk = 1,
      riskWindowStart = 0,
      startAnchor = "cohort start",
      riskWindowEnd = 30,
      endAnchor = "cohort end"
    )

    createPsArgs <- createCreatePsArgs(
      prior = createPrior("laplace", variance = 0.01, exclude = c(0)),
      estimator = "att",
      stopOnError = FALSE
    )

    matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 1)

    fitOutcomeModelArgs <- createFitOutcomeModelArgs(
      modelType = "cox",
      stratified = TRUE
    )

    cmAnalysis <- createCmAnalysis(
      analysisId = 1,
      description = "Matching",
      getDbCohortMethodDataArgs = getDbCmDataArgs,
      createStudyPopulationArgs = createStudyPopArgs,
      createPsArgs = createPsArgs,
      matchOnPsArgs = matchOnPsArgs,
      fitOutcomeModelArgs = fitOutcomeModelArgs
    )

    cmAnalysisList <- list(cmAnalysis)

    result <- runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder3,
      cmAnalysesSpecifications = createCmAnalysesSpecifications(
        cmAnalysisList = cmAnalysisList,
        targetComparatorOutcomesList = targetComparatorOutcomesList
      )
    )

    exportToCsv(outputFolder3, databaseId = "Test")
    propensityModelFile <- file.path(outputFolder3, "export", "cm_propensity_model.csv")
    model <- readr::read_csv(propensityModelFile, show_col_types = FALSE)
    model <- model |>
      arrange(covariate_id)
    expect_equal(model$covariate_id, c(1118084404, 1124300404))
    expect_equal(model$coefficient, c(1000000, -1000000))
  })

  test_that("Match and stratify by additional covariates", {

    # A test to make sure matching and stratifying by additional covariates works

    tcos1 <- createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(
          outcomeId = 3
        )
      )
    )

    targetComparatorOutcomesList <- list(tcos1)

    covarSettings <- createCovariateSettings(
      useDemographicsGender = TRUE,
      useDemographicsAge = TRUE
    )

    getDbCmDataArgs <- createGetDbCohortMethodDataArgs(
      firstExposureOnly = TRUE,
      restrictToCommonPeriod = FALSE,
      removeDuplicateSubjects = "remove all",
      washoutPeriod = 183,
      covariateSettings = covarSettings
    )

    createStudyPopArgs <- createCreateStudyPopulationArgs(
      removeSubjectsWithPriorOutcome = TRUE,
      censorAtNewRiskWindow = TRUE,
      minDaysAtRisk = 1,
      riskWindowStart = 0,
      startAnchor = "cohort start",
      riskWindowEnd = 30,
      endAnchor = "cohort end"
    )

    createPsArgs <- createCreatePsArgs(
      prior = createPrior("laplace", variance = 0.01, exclude = c(0)),
      estimator = "att"
    )

    # Additionally match on gender:
    matchOnPsArgs <- createMatchOnPsArgs(
      maxRatio = 1,
      matchCovariateIds = 8532001
    )

    fitOutcomeModelArgs <- createFitOutcomeModelArgs(
      modelType = "cox",
      stratified = TRUE
    )

    cmAnalysis1 <- createCmAnalysis(
      analysisId = 1,
      description = "Matching",
      getDbCohortMethodDataArgs = getDbCmDataArgs,
      createStudyPopulationArgs = createStudyPopArgs,
      createPsArgs = createPsArgs,
      matchOnPsArgs = matchOnPsArgs,
      fitOutcomeModelArgs = fitOutcomeModelArgs
    )

    # Additionally stratify on gender:
    stratifyByPsArgs <- createStratifyByPsArgs(
      numberOfStrata = 5,
      stratificationCovariateIds = 8532001
    )

    cmAnalysis2 <- createCmAnalysis(
      analysisId = 2,
      description = "Stratification",
      getDbCohortMethodDataArgs = getDbCmDataArgs,
      createStudyPopulationArgs = createStudyPopArgs,
      createPsArgs = createPsArgs,
      stratifyByPsArgs = stratifyByPsArgs,
      fitOutcomeModelArgs = fitOutcomeModelArgs
    )

    cmAnalysisList <- list(cmAnalysis1, cmAnalysis2)

    expect_warning(
      {
        result <- runCmAnalyses(
          connectionDetails = connectionDetails,
          cdmDatabaseSchema = "main",
          exposureTable = "cohort",
          outcomeTable = "cohort",
          outputFolder = outputFolder4,
          cmAnalysesSpecifications = createCmAnalysesSpecifications(
            cmAnalysisList = cmAnalysisList,
            targetComparatorOutcomesList = targetComparatorOutcomesList
          )
        )
      }, "zero"
    )
    strataPop <- readRDS(file.path(outputFolder4, result$strataFile[1]))
    expect_true("covariateId_8532001" %in% colnames(strataPop))

    strataPop <- readRDS(file.path(outputFolder4, result$strataFile[2]))
    expect_true("covariateId_8532001" %in% colnames(strataPop))
  })


  test_that("Combine nesting and non-nesting", {

    # A test to make sure we can have the same TC with and without nesting at the same time

    tcos1 <- createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      outcomes = list(
        createOutcome(
          outcomeId = 3
        )
      )
    )

    tcos2 <- createTargetComparatorOutcomes(
      targetId = 1,
      comparatorId = 2,
      nestingCohortId = 4,
      outcomes = list(
        createOutcome(
          outcomeId = 3
        )
      )
    )

    targetComparatorOutcomesList <- list(tcos1, tcos2)

    covarSettings <- createCovariateSettings(
      useDemographicsGender = TRUE,
      useDemographicsAge = TRUE
    )

    getDbCmDataArgs <- createGetDbCohortMethodDataArgs(
      firstExposureOnly = TRUE,
      restrictToCommonPeriod = FALSE,
      removeDuplicateSubjects = "remove all",
      washoutPeriod = 183,
      covariateSettings = covarSettings
    )

    createStudyPopArgs <- createCreateStudyPopulationArgs(
      removeSubjectsWithPriorOutcome = TRUE,
      censorAtNewRiskWindow = TRUE,
      minDaysAtRisk = 1,
      riskWindowStart = 0,
      startAnchor = "cohort start",
      riskWindowEnd = 30,
      endAnchor = "cohort end"
    )

    createPsArgs <- createCreatePsArgs(
      prior = createPrior("laplace", variance = 0.01, exclude = c(0)),
      estimator = "att"
    )

    matchOnPsArgs <- createMatchOnPsArgs(
      maxRatio = 1
    )

    fitOutcomeModelArgs <- createFitOutcomeModelArgs(
      modelType = "cox",
      stratified = TRUE
    )

    computeSharedCovBalArgs <- createComputeCovariateBalanceArgs()

    computeCovBalArgs <- createComputeCovariateBalanceArgs(covariateFilter = FeatureExtraction::getDefaultTable1Specifications())

    cmAnalysis1 <- createCmAnalysis(
      analysisId = 1,
      description = "Matching",
      getDbCohortMethodDataArgs = getDbCmDataArgs,
      createStudyPopulationArgs = createStudyPopArgs,
      createPsArgs = createPsArgs,
      matchOnPsArgs = matchOnPsArgs,
      fitOutcomeModelArgs = fitOutcomeModelArgs,
      computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
      computeCovariateBalanceArgs = computeCovBalArgs
    )

    stratifyByPsArgs <- createStratifyByPsArgs(
      numberOfStrata = 5
    )

    cmAnalysis2 <- createCmAnalysis(
      analysisId = 2,
      description = "Stratification",
      getDbCohortMethodDataArgs = getDbCmDataArgs,
      createStudyPopulationArgs = createStudyPopArgs,
      createPsArgs = createPsArgs,
      stratifyByPsArgs = stratifyByPsArgs,
      fitOutcomeModelArgs = fitOutcomeModelArgs,
      computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
      computeCovariateBalanceArgs = computeCovBalArgs
    )

    cmAnalysisList <- list(cmAnalysis1, cmAnalysis2)

    expect_warning(
      {
        result <- runCmAnalyses(
          connectionDetails = connectionDetails,
          cdmDatabaseSchema = "main",
          exposureTable = "cohort",
          outcomeTable = "cohort",
          outputFolder = outputFolder5,
          cmAnalysesSpecifications = createCmAnalysesSpecifications(
            cmAnalysisList = cmAnalysisList,
            targetComparatorOutcomesList = targetComparatorOutcomesList
          )
        )
      }, "zero"
    )
    expect_equal(nrow(result), 4)
    expect_equal(sum(result$nestingCohortId == 4, na.rm = TRUE), 2)

    expect_equal(length(grep("_t1_c2_n4", result$cohortMethodDataFile)), 2)
    expect_equal(length(grep("_t1_c2_n4", result$studyPopFile)), 2)
    expect_equal(length(grep("_t1_c2_n4", result$sharedPsFile)), 2)
    expect_equal(length(grep("_t1_c2_n4", result$psFile)), 2)
    expect_equal(length(grep("_t1_c2_n4", result$strataFile)), 2)
    expect_equal(length(grep("_t1_c2_n4", result$sharedBalanceFile)), 2)
    expect_equal(length(grep("_t1_c2_n4", result$filteredForbalanceFile)), 2)
    expect_equal(length(grep("_t1_c2_n4", result$balanceFile)), 2)
    expect_equal(length(grep("_t1_c2_n4", result$outcomeModelFile)), 2)

    uniqueCmdFiles <- result[!duplicated(result$cohortMethodDataFile), ]
    for (i in seq_len(nrow(uniqueCmdFiles))) {
      cmd <- loadCohortMethodData(file.path(outputFolder5, result$cohortMethodDataFile[i]))
      nestingCohortId <- attr(cmd,"metaData")$nestingCohortId
      if (is.null(nestingCohortId)) {
        nestingCohortId <- as.numeric(NA)
      }
      expect_equal(nestingCohortId, uniqueCmdFiles$nestingCohortId[i])
    }

    for (i in seq_len(nrow(result))) {
      strataPop <- readRDS(file.path(outputFolder5,  result$strataFile[i]))
      nestingCohortId <- attr(strataPop,"metaData")$nestingCohortId
      if (is.null(nestingCohortId)) {
        nestingCohortId <- as.numeric(NA)
      }
      expect_equal(nestingCohortId, result$nestingCohortId[i])
    }

    resultsSummary <- getResultsSummary(outputFolder5)
    expect_equal(nrow(resultsSummary), 4)
    expect_equal(sum(resultsSummary$nestingCohortId == 4, na.rm = TRUE), 2)

    exportToCsv(outputFolder5, databaseId = "Test")
    cmResult <- readr::read_csv(file.path(outputFolder5, "export", "cm_result.csv"), show_col_types = FALSE)
    expect_equal(nrow(cmResult), 4)
    expect_equal(sum(cmResult$nesting_cohort_id == 4, na.rm = TRUE), 2)
    expect_equal(sum(cmResult$nesting_cohort_id == 0, na.rm = TRUE), 2)
  })
}

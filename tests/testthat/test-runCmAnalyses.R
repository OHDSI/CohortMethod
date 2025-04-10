library(testthat)
library(CohortMethod)

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
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = list()
    ),
    "Must have length >= 1"
  )

  ### NULL ----
  expect_error(
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = NULL
    ),
    "Must be of type 'list'"
  )

  ### list(list(), list()) ----
  expect_error(
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = list(list(), list())
    ),
    "targetComparatorOutcomesList.+types:.+targetComparatorOutcomes"
  )

  ### list(NULL, NULL) ----
  expect_error(
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = list(NULL, NULL)
    ),
    "targetComparatorOutcomesList.+types:.+targetComparatorOutcomes"
  )
})

test_that("tempEmulationSchema", {
  unlink(outputFolder, recursive = TRUE)
  ### "main"
  expect_no_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      # Eunomia
      tempEmulationSchema = "main"
    )
  ))

  ### 3 ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      tempEmulationSchema = 3
    ),
    "Must be of type 'character'"
  )

  ### c("main", "main") ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      tempEmulationSchema = c("main", "main")
    )),
    "Must have length 1"
  )
})

test_that("exposureDatabaseSchema", {
  ### "main" ----
  unlink(outputFolder, recursive = TRUE)
  expect_no_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      exposureDatabaseSchema = "main"
    ))
  )

  ### "SchemaThatDoesNotExist" ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      exposureDatabaseSchema = "SchemaThatDoesNotExist"
    )),
    "no such table: SchemaThatDoesNotExist.cohort"
  )

  ### 3 ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      exposureDatabaseSchema = 3
    )),
    "Must be of type 'character'"
  )

  ### c("main", "main") ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      exposureDatabaseSchema = c("main", "main")
    )),
    "Must have length 1"
  )
})

test_that("outcomeDatabaseSchema", {
  ### "main" ----
  unlink(outputFolder, recursive = TRUE)
  expect_no_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      outcomeDatabaseSchema = "main"
    ))
  )

  ### 3 ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      outcomeDatabaseSchema = 3
    )),
    "Must be of type 'character'"
  )

  ### c("main", "main") ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      outcomeDatabaseSchema = c("main", "main")
    )),
    "Must have length 1"
  )
})

test_that("analysesToExclude", {
  unlink(outputFolder, recursive = TRUE)

  analysesToExclude <- data.frame(
    targetId = c(998, 998),
    analysisId = c(3, 4)
  )

  ### analysesToExclude ----
  expect_no_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      analysesToExclude = analysesToExclude
    ))
  )

  ### NULL ----
  unlink(outputFolder, recursive = TRUE)
  expect_no_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      analysesToExclude = NULL
    ))
  )

  ### data.frame() ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      analysesToExclude = data.frame()
    )),
    "should contain columns 'targetId', 'comparatorId', 'outcomeId', or 'analysisId'"
  )

  ### data.frame(numeric()) ----
  unlink(outputFolder, recursive = TRUE)
  expect_warning(
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      analysesToExclude = data.frame(
        targetId = numeric(),
        comparatorId = numeric(),
        outcomeId = numeric(),
        analysisId = numeric()
      )
    ),
    "Passed `data.frame` with 0 rows to parameter: `analysesToExclude`, no analyses excluded."
  )
})

test_that("refitPsForEveryOutcome", {
  ### FALSE ----
  unlink(outputFolder, recursive = TRUE)
  expect_no_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      refitPsForEveryOutcome = FALSE
    ))
  )

  ### TRUE ----
  unlink(outputFolder, recursive = TRUE)
  expect_no_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      refitPsForEveryOutcome = TRUE
    ))
  )
  # Note:
  # Throws Error:
  # cannot open file '.\Temp\RtmpwLKCGK\cmData6dbc562227db': it is a directory

  ### Check files ----
  refitTrue <- suppressWarnings(runCmAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    outputFolder = outputFolder,
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = targetComparatorOutcomesList,
    refitPsForEveryOutcome = TRUE
  ))

  refitFalse <- suppressWarnings(runCmAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    outputFolder = outputFolder,
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = targetComparatorOutcomesList,
    refitPsForEveryOutcome = FALSE
  ))

  expect_false(identical(refitTrue, refitFalse))
  expect_true(all(grepl(
    pattern = "(^StudyPop_l1_s\\d+_t\\d+_c\\d+_o\\d+\\.rds$|^$)",
    x = c(refitTrue$studyPopFile, refitFalse$studyPopFile)
  )))

  expect_true(all(grepl(
    pattern = "(^Ps_l1_s\\d+_p\\d+_t\\d+_c\\d+\\.rds$|^$)",
    x = c(refitTrue$sharedPsFile, refitFalse$sharedPsFile)
  )))

  expect_true(all(grepl(
    pattern = "(^Ps_l1_s\\d+_p\\d+_t\\d+_c\\d+_o\\d+\\.rds$|^$)",
    x = c(refitTrue$psFile, refitFalse$psFile)
  )))

  expect_true(all(grepl(
    pattern = "(^Balance_l1_s\\d+_p\\d+_t\\d+_c\\d+_s\\d+_b\\d+\\.rds$|^$)",
    x = c(refitTrue$sharedBalanceFile, refitFalse$sharedBalanceFile)
  )))

  ### 0 ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      refitPsForEveryOutcome = 0
    )),
    "Must be of type 'logical'"
  )
})

test_that("refitPsForEveryStudyPopulation", {
  ### FALSE ----
  unlink(outputFolder, recursive = TRUE)
  expect_no_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      refitPsForEveryStudyPopulation = FALSE
    ))
  )

  ### TRUE ----
  unlink(outputFolder, recursive = TRUE)
  expect_no_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      refitPsForEveryStudyPopulation = TRUE
    ))
  )

  ## output check ----
  refitFalse <- suppressWarnings(runCmAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    outputFolder = outputFolder,
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = targetComparatorOutcomesList,
    refitPsForEveryStudyPopulation = FALSE
  ))

  refitTrue <- suppressWarnings(runCmAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeTable = "cohort",
    outputFolder = outputFolder,
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = targetComparatorOutcomesList,
    refitPsForEveryStudyPopulation = TRUE
  ))

  # Check refitTrue != refitFalse
  expect_false(identical(refitTrue, refitFalse))

  modelsTrue <- refitTrue$sharedPsFile[
    !refitTrue$sharedPsFile %in% refitFalse$sharedPsFile]

  modelsFalse <- refitFalse$sharedPsFile[
    !refitFalse$sharedPsFile %in% refitTrue$sharedPsFile]

  expectedDif <- c(7L, 7L, 0L, 0L, 7L, 7L, 0L, 0L, 7L, 7L, 0L, 0L)

  actualDif <- lapply(seq_len(length(modelsTrue)), function(i) {
    fileFalse <- readRDS(file.path(outputFolder, modelsFalse[i]))
    fileTrue <- readRDS(file.path(outputFolder, modelsTrue[i]))
    nrow(fileFalse) - nrow(fileTrue)
  }) |>
    unlist()

  expect_identical(expectedDif, actualDif)

  ### 0 ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = cmAnalysisList,
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      refitPsForEveryStudyPopulation = 0
    )),
    "Must be.+'logical'"
  )
})

test_that("refitPsForEveryX", {
  unlink(outputFolder, recursive = TRUE)
  expect_error(suppressWarnings(
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = list(cmAnalysis4),
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      refitPsForEveryOutcome = TRUE,
      refitPsForEveryStudyPopulation = FALSE
    )
  ),
    "Cannot have refitPsForEveryStudyPopulation = FALSE and refitPsForEveryOutcome = TRUE"
  )
})

test_that("multiThreadingSettings", {
  ### createDefaultMultiThreadingSettings() ----
  unlink(outputFolder, recursive = TRUE)
  expect_no_error(
    suppressWarnings(runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = list(cmAnalysis4),
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      multiThreadingSettings = createDefaultMultiThreadingSettings(4)
    ))
  )

  ### NULL ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = list(cmAnalysis4),
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      multiThreadingSettings = NULL
    ),
    "Must.+class.+CmMultiThreadingSettings"
  )

  ### list() ----
  unlink(outputFolder, recursive = TRUE)
  expect_error(
    runCmAnalyses(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      exposureTable = "cohort",
      outcomeTable = "cohort",
      outputFolder = outputFolder,
      cmAnalysisList = list(cmAnalysis4),
      targetComparatorOutcomesList = targetComparatorOutcomesList,
      multiThreadingSettings = list()
    ),
    "Must.+class.+CmMultiThreadingSettings"
  )
})

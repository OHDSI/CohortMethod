# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# options(andromedaTempFolder = "s:/andromedaTemp")
# outputFolder <- "s:/temp/cohortMethodVignette2"
# maxCores <- 8
# library(dplyr)


#' Create CohortMethod diagnostics thresholds
#'
#' @description
#' Threshold used when calling [exportToCsv()] to determine if we pass or fail diagnostics.
#'
#' @param mdrrThreshold         What is the maximum allowed minimum detectable relative risk
#'                              (MDRR)?
#' @param easeThreshold         What is the maximum allowed expected absolute systematic error
#'                              (EASE).
#' @param sdmThreshold          What is the maximum allowed standardized difference of mean (SDM)? If
#'                              any covariate has an SDM exceeding this threshold, the diagnostic will
#'                              fail.
#' @param equipoiseThreshold    What is the minimum required equipoise?
#' @param attritionFractionThreshold DEPRECATED. See `generalizabilitySdmThreshold` instead.
#' @param generalizabilitySdmThreshold What is the maximum allowed standardized difference of mean
#'                                     (SDM)when comparing the population before and after PS
#'                                     adjustments? If the SDM is greater than this value, the diagnostic
#'                                     will fail.
#'
#' @return
#' An object of type `CmDiagnosticThresholds`.
#'
#' @export
createCmDiagnosticThresholds <- function(mdrrThreshold = 10,
                                         easeThreshold = 0.25,
                                         sdmThreshold = 0.1,
                                         equipoiseThreshold = 0.2,
                                         attritionFractionThreshold = NULL,
                                         generalizabilitySdmThreshold = 1) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(mdrrThreshold, len = 1, lower = 0, add = errorMessages)
  checkmate::assertNumeric(easeThreshold, len = 1, lower = 0, add = errorMessages)
  checkmate::assertNumeric(sdmThreshold, len = 1, lower = 0, add = errorMessages)
  checkmate::assertNumeric(equipoiseThreshold, len = 1, lower = 0, add = errorMessages)
  checkmate::assertNumeric(generalizabilitySdmThreshold, len = 1, lower = 0, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (!is.null(attritionFractionThreshold)) {
    warning("The attritionFractionThreshold argument is deprecated and will be ignored. ",
            "See generalizabilitySdmThreshold instead.")
  }
  thresholds <- list()
  for (name in names(formals(createCmDiagnosticThresholds))) {
    thresholds[[name]] <- get(name)
  }
  class(thresholds) <- "CmDiagnosticThresholds"
  return(thresholds)
}

#' Export cohort method results to CSV files
#'
#' @details
#' This requires that [runCmAnalyses()] has been executed first. It exports
#' all the results in the `outputFolder` to CSV files for sharing with other
#' sites.
#'
#' @param outputFolder  The folder where runCmAnalyses() generated all results.
#' @param exportFolder  The folder where the CSV files will written.
#' @param databaseId    A unique ID for the database. This will be appended to
#'                      most tables.
#' @param minCellCount  To preserve privacy: the minimum number of subjects contributing
#'                      to a count before it can be included in the results. If the
#'                      count is below this threshold, it will be set to `-minCellCount`.
#' @param maxCores      How many parallel cores should be used?
#' @param cmDiagnosticThresholds An object of type `CmDiagnosticThresholds` as created using
#'                                 [createCmDiagnosticThresholds()].
#'
#' @return
#' Does not return anything. Is called for the side-effect of populating the `exportFolder`
#' with CSV files.
#'
#' @export
exportToCsv <- function(outputFolder,
                        exportFolder = file.path(outputFolder, "export"),
                        databaseId,
                        minCellCount = 5,
                        maxCores = 1,
                        cmDiagnosticThresholds = createCmDiagnosticThresholds()) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::assertDirectoryExists(outputFolder, add = errorMessages)
  checkmate::assertFileExists(file.path(outputFolder, "cmAnalysisList.rds"), add = errorMessages)
  checkmate::assertFileExists(file.path(outputFolder, "targetComparatorOutcomesList.rds"), add = errorMessages)
  checkmate::assertFileExists(file.path(outputFolder, "resultsSummary.rds"), add = errorMessages)
  checkmate::assertCharacter(exportFolder, len = 1, add = errorMessages)
  checkmate::assertAtomic(databaseId, len = 1, add = errorMessages)
  checkmate::assertInt(minCellCount, lower = 0, add = errorMessages)
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }
  start <- Sys.time()
  message("Exporting results to CSV")
  exportCohortMethodAnalyses(
    outputFolder = outputFolder,
    exportFolder = exportFolder
  )

  exportFromCohortMethodData(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId
  )

  exportTargetComparatorOutcomes(
    outputFolder = outputFolder,
    exportFolder = exportFolder
  )

  exportAttrition(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId,
    minCellCount = minCellCount
  )

  exportCmFollowUpDist(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId,
    minCellCount = minCellCount
  )

  exportCohortMethodResults(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId,
    minCellCount = minCellCount
  )

  exportCmInteractionResults(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId,
    minCellCount = minCellCount
  )

  exportLikelihoodProfiles(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId
  )

  exportCovariateBalance(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId,
    minCellCount = minCellCount
  )

  exportSharedCovariateBalance(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId,
    minCellCount = minCellCount
  )

  exportPreferenceScoreDistribution(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId
  )

  exportPropensityModel(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId
  )

  exportKaplanMeier(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId,
    minCellCount = minCellCount,
    maxCores = maxCores
  )

  exportDiagnosticsSummary(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId,
    cmDiagnosticThresholds = cmDiagnosticThresholds
  )

  # Add all to zip file -------------------------------------------------------------------------------
  message("Adding results to zip file")
  zipName <- file.path(exportFolder, sprintf("Results_%s.zip", databaseId))
  files <- list.files(exportFolder, pattern = ".*\\.csv$")
  oldWd <- setwd(exportFolder)
  on.exit(setwd(oldWd))
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)

  delta <- Sys.time() - start
  message("Exporting to CSV took ", signif(delta, 3), " ", attr(delta, "units"))
  message("Results are ready for sharing at:", zipName)
}

writeToCsv <- function(data, fileName, append = FALSE) {
  colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  # Workaround for issue https://github.com/tidyverse/vroom/issues/519:
  readr::local_edition(1)
  readr::write_csv(x = data, file = fileName, append = append)
}

enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
  values <- pull(data, fieldName)
  toCensor <- !is.na(values) & (values < minValues) & (values != 0)
  if (!silent) {
    percent <- round(100 * sum(toCensor) / nrow(data), 1)
    message(
      "   censoring ",
      sum(toCensor),
      " values (",
      percent,
      "%) from ",
      fieldName,
      " because value below minimum"
    )
  }

  if (all(is.na(toCensor)) || all(is.na(minValues))) {
    data[, fieldName] <- NA
  } else if (length(minValues) == 1) {
    data[toCensor, fieldName] <- -minValues
  } else {
    data[toCensor, fieldName] <- -minValues[toCensor]
  }
  return(data)
}

createEmptyResult <- function(tableName) {
  # Workaround for issue https://github.com/tidyverse/vroom/issues/519:
  readr::local_edition(1)
  columns <- readr::read_csv(
    file = system.file("csv", "resultsDataModelSpecification.csv", package = "CohortMethod"),
    show_col_types = FALSE) |>
    SqlRender::snakeCaseToCamelCaseNames() |>
    filter(.data$tableName == !!tableName) |>
    pull(.data$columnName) |>
    SqlRender::snakeCaseToCamelCase()
  result <- vector(length = length(columns))
  names(result) <- columns
  result <- as_tibble(t(result), name_repair = "check_unique")
  result <- result[FALSE, ]
  return(result)
}

exportCohortMethodAnalyses <- function(outputFolder, exportFolder) {
  message("- cm__analysis table")

  tempFileName <- tempfile()

  cmAnalysisListFile <- file.path(outputFolder, "cmAnalysisList.rds")
  cmAnalysisList <- readRDS(cmAnalysisListFile)
  cmAnalysisToRow <- function(cmAnalysis) {
    ParallelLogger::saveSettingsToJson(cmAnalysis, tempFileName)
    row <- tibble(
      analysisId = cmAnalysis$analysisId,
      description = cmAnalysis$description,
      definition = readChar(tempFileName, file.info(tempFileName)$size)
    )
    return(row)
  }
  cohortMethodAnalysis <- lapply(cmAnalysisList, cmAnalysisToRow)
  cohortMethodAnalysis <- bind_rows(cohortMethodAnalysis) |>
    distinct()
  unlink(tempFileName)

  fileName <- file.path(exportFolder, "cm_analysis.csv")
  writeToCsv(cohortMethodAnalysis, fileName)
}

exportFromCohortMethodData <- function(outputFolder, exportFolder, databaseId) {
  # Combining processing of tables so we only have to load cmData objects once
  message("- covariate_analysis and covariate tables")
  reference <- getFileReference(outputFolder)
  cmDataFiles <- reference |>
    distinct(.data$cohortMethodDataFile) |>
    filter(.data$cohortMethodDataFile != "") |>
    pull()

  covariateAnalysis <- list()
  covariates <- list()

  for (cmDataFile in cmDataFiles) {
    cmData <- CohortMethod::loadCohortMethodData(file.path(outputFolder, cmDataFile))
    rows <- reference |>
      filter(.data$cohortMethodDataFile == !!cmDataFile)
    analysisIds <- rows |>
      distinct(.data$analysisId)

    covariateAnalysis[[length(covariateAnalysis) + 1]] <- cmData$analysisRef |>
      select(
        covariateAnalysisId = "analysisId",
        covariateAnalysisName = "analysisName"
      ) |>
      collect() |>
      cross_join(analysisIds)

    covariates[[length(covariates) + 1]] <- cmData$covariateRef |>
      select(
        "covariateId",
        "covariateName",
        covariateAnalysisId = "analysisId"
      ) |>
      collect() |>
      cross_join(analysisIds) |>
      mutate(databaseId = !!databaseId)
  }

  covariateAnalysis <- bind_rows(covariateAnalysis) |>
    distinct(.data$covariateAnalysisId, .data$analysisId, .keep_all = TRUE)
  if (nrow(covariateAnalysis) == 0) {
    covariateAnalysis <- createEmptyResult("cm_covariate_analysis")
  }
  fileName <- file.path(exportFolder, "cm_covariate_analysis.csv")
  writeToCsv(covariateAnalysis, fileName)

  covariates <- bind_rows(covariates) |>
    distinct(.data$covariateId, .data$analysisId, .keep_all = TRUE)
  if (nrow(covariates) == 0) {
    covariates <- createEmptyResult("cm_covariate")
  }
  fileName <- file.path(exportFolder, "cm_covariate.csv")
  writeToCsv(covariates, fileName)
}

exportTargetComparatorOutcomes <- function(outputFolder, exportFolder) {
  message("- target_comparator_outcome table")

  tcosList <- readRDS(file.path(outputFolder, "targetComparatorOutcomesList.rds"))
  convertOutcomeToTable <- function(outcome) {
    table <- tibble(
      outcomeId = outcome$outcomeId,
      outcomeOfInterest = as.integer(outcome$outcomeOfInterest),
      trueEffectSize = if (is.null(outcome$trueEffectSize)) as.numeric(NA) else outcome$trueEffectSize
    )
    return(table)
  }
  # tcos <- tcosList[[1]]
  convertToTable <- function(tcos) {
    table <- lapply(tcos$outcomes, convertOutcomeToTable) |>
      bind_rows() |>
      mutate(
        targetId = tcos$targetId,
        comparatorId = tcos$comparatorId
      )
    return(table)

  }
  table <- lapply(tcosList, convertToTable)
  table <- bind_rows(table)

  fileName <- file.path(exportFolder, "cm_target_comparator_outcome.csv")
  writeToCsv(table, fileName)
}

exportAttrition <- function(outputFolder,
                            exportFolder,
                            databaseId,
                            minCellCount) {
  message("- attrition table")
  fileName <- file.path(exportFolder, "cm_attrition.csv")
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  reference <- getFileReference(outputFolder) |>
    filter(.data$outcomeOfInterest)
  first <- !file.exists(fileName)
  pb <- txtProgressBar(style = 3)
  for (i in seq_len(nrow(reference))) {
    outcomeModel <- readRDS(file.path(outputFolder, reference$outcomeModelFile[i]))
    attrition <- outcomeModel$attrition |>
      select("description", "targetPersons", "comparatorPersons") |>
      mutate(sequenceNumber = row_number())
    attritionTarget <- attrition |>
      select("sequenceNumber", "description", subjects = "targetPersons") |>
      mutate(exposureId = reference$targetId[i])
    attritionComparator <- attrition |>
      select("sequenceNumber", "description", subjects = "comparatorPersons") |>
      mutate(exposureId = reference$comparatorId[i])
    attrition <- bind_rows(attritionTarget, attritionComparator) |>
      mutate(
        targetId = reference$targetId[i],
        comparatorId = reference$comparatorId[i],
        analysisId = reference$analysisId[i],
        outcomeId = reference$outcomeId[i],
        databaseId = databaseId
      ) |>
      enforceMinCellValue("subjects", minCellCount, silent = TRUE)

    writeToCsv(attrition, fileName, append = !first)
    first <- FALSE
    if (i %% 100 == 10) {
      setTxtProgressBar(pb, i / nrow(reference))
    }
  }
  if (first) {
    results <- createEmptyResult("cm_attrition")
    writeToCsv(results, fileName)
  }
  setTxtProgressBar(pb, 1)
  close(pb)
}

exportCmFollowUpDist <- function(outputFolder,
                                 exportFolder,
                                 databaseId,
                                 minCellCount) {
  message("- cm_follow_up_dist table")
  # row = rows[1, ]
  getFollowUpDist <- function(row) {
    if (row$strataFile == "") {
      strataPop <- readRDS(file.path(outputFolder, row$studyPopFile))
    } else {
      strataPop <- readRDS(file.path(outputFolder, row$strataFile))
    }
    targetDist <- quantile(
      strataPop$survivalTime[strataPop$treatment == 1],
      c(0, 0.1, 0.25, 0.5, 0.85, 0.9, 1)
    )
    comparatorDist <- quantile(
      strataPop$survivalTime[strataPop$treatment == 0],
      c(0, 0.1, 0.25, 0.5, 0.85, 0.9, 1)
    )
    if (nrow(strataPop) == 0) {
      targetMinMaxDates <- tibble(
        minDate = as.Date(NA),
        maxDate = as.Date(NA)
      )
      comparatorMinMaxDates <- targetMinMaxDates
    } else {
      targetMinMaxDates <- strataPop |>
        filter(.data$treatment == 1) |>
        summarise(
          minDate = min(.data$cohortStartDate),
          maxDate = max(.data$cohortStartDate)
        )

      comparatorMinMaxDates <- strataPop |>
        filter(.data$treatment == 0) |>
        summarise(
          minDate = min(.data$cohortStartDate),
          maxDate = max(.data$cohortStartDate)
        )
    }

    table <- tibble(
      target_id = row$targetId,
      comparator_id = row$comparatorId,
      outcome_id = row$outcomeId,
      analysis_id = row$analysisId,
      target_min_days = targetDist[1],
      target_p10_days = targetDist[2],
      target_p25_days = targetDist[3],
      target_median_days = targetDist[4],
      target_p75_days = targetDist[5],
      target_p90_days = targetDist[6],
      target_max_days = targetDist[7],
      comparator_min_days = comparatorDist[1],
      comparator_p10_days = comparatorDist[2],
      comparator_p25_days = comparatorDist[3],
      comparator_median_days = comparatorDist[4],
      comparator_p75_days = comparatorDist[5],
      comparator_p90_days = comparatorDist[6],
      comparator_max_days = comparatorDist[7],
      targetMinDate = targetMinMaxDates$minDate,
      targetMaxDate = targetMinMaxDates$maxDate,
      comparatorMinDate = comparatorMinMaxDates$minDate,
      comparatorMaxDate = comparatorMinMaxDates$maxDate
    )
    return(table)
  }
  reference <- getFileReference(outputFolder)
  rows <- reference |>
    filter(.data$outcomeOfInterest)
  results <- lapply(split(rows, 1:nrow(rows)), getFollowUpDist)
  results <- bind_rows(results)
  results$database_id <- databaseId
  if (nrow(results) == 0) {
    results <- createEmptyResult("cm_follow_up_dist")
  }

  fileName <- file.path(exportFolder, "cm_follow_up_dist.csv")
  writeToCsv(results, fileName)
}

exportCohortMethodResults <- function(outputFolder,
                                      exportFolder,
                                      databaseId,
                                      minCellCount) {
  message("- cm__result table")
  results <- getResultsSummary(outputFolder) |>
    select(
      "analysisId",
      "targetId",
      "comparatorId",
      "outcomeId",
      "rr",
      "ci95Lb",
      "ci95Ub",
      "p",
      "oneSidedP",
      "targetSubjects",
      "comparatorSubjects",
      "targetDays",
      "comparatorDays",
      "targetOutcomes",
      "comparatorOutcomes",
      "logRr",
      "seLogRr",
      "llr",
      "calibratedRr",
      "calibratedCi95Lb",
      "calibratedCi95Ub",
      "calibratedP",
      "calibratedOneSidedP",
      "calibratedLogRr",
      "calibratedSeLogRr",
      "targetEstimator"
    ) |>
    mutate(databaseId = !!databaseId) |>
    enforceMinCellValue("targetSubjects", minCellCount) |>
    enforceMinCellValue("comparatorSubjects", minCellCount) |>
    enforceMinCellValue("targetOutcomes", minCellCount) |>
    enforceMinCellValue("comparatorOutcomes", minCellCount)
  fileName <- file.path(exportFolder, "cm_result.csv")
  writeToCsv(results, fileName)
}

exportCmInteractionResults <- function(outputFolder,
                                       exportFolder,
                                       databaseId,
                                       minCellCount) {
  message("- cm_interaction_result table")
  results <- getInteractionResultsSummary(outputFolder)
  if (nrow(results) == 0) {
    results <- createEmptyResult("cm_interaction_result")
  } else {
    results <- results |>
      select(
        "analysisId",
        "targetId",
        "comparatorId",
        "outcomeId",
        "interactionCovariateId",
        "rr",
        "ci95Lb",
        "ci95Ub",
        "p",
        "targetSubjects",
        "comparatorSubjects",
        "targetDays",
        "comparatorDays",
        "targetOutcomes",
        "comparatorOutcomes",
        "logRr",
        "seLogRr",
        "calibratedRr",
        "calibratedCi95Lb",
        "calibratedCi95Ub",
        "calibratedP",
        "calibratedLogRr",
        "calibratedSeLogRr",
        "targetEstimator"
      ) |>
      mutate(databaseId = !!databaseId) |>
      enforceMinCellValue("targetSubjects", minCellCount) |>
      enforceMinCellValue("comparatorSubjects", minCellCount) |>
      enforceMinCellValue("targetOutcomes", minCellCount) |>
      enforceMinCellValue("comparatorOutcomes", minCellCount)
  }
  fileName <- file.path(exportFolder, "cm_interaction_result.csv")
  writeToCsv(results, fileName)
}

exportLikelihoodProfiles <- function(outputFolder,
                                     exportFolder,
                                     databaseId) {
  message("- likelihood_profile table")
  reference <- getFileReference(outputFolder)
  fileName <- file.path(exportFolder, "cm_likelihood_profile.csv")
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  first <- TRUE
  pb <- txtProgressBar(style = 3)
  for (i in seq_len(nrow(reference))) {
    if (reference$outcomeModelFile[i] != "") {
      outcomeModel <- readRDS(file.path(outputFolder, reference$outcomeModelFile[i]))
      profile <- outcomeModel$logLikelihoodProfile
      if (!is.null(profile)) {
        profile <- profile |>
          transmute(
            logRr = .data$point,
            logLikelihood = .data$value - max(.data$value)
          ) |>
          mutate(
            targetId = reference$targetId[i],
            comparatorId = reference$comparatorId[i],
            outcomeId = reference$outcomeId[i],
            analysisId = reference$analysisId[i],
            databaseId = !!databaseId
          )
        writeToCsv(profile, fileName, append = !first)
        first <- FALSE
      }
    }
    if (first) {
      results <- createEmptyResult("cm_likelihood_profile")
      writeToCsv(results, fileName)
    }
    setTxtProgressBar(pb, i / nrow(reference))
  }
  setTxtProgressBar(pb, 1)
  close(pb)
}

exportCovariateBalance <- function(outputFolder,
                                   exportFolder,
                                   databaseId,
                                   minCellCount) {
  message("- covariate_balance table")
  reference <- getFileReference(outputFolder) |>
    filter(.data$balanceFile != "")
  balanceFiles <- reference |>
    distinct(.data$balanceFile) |>
    pull()

  fileName <- file.path(exportFolder, "cm_covariate_balance.csv")
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  first <- TRUE
  pb <- txtProgressBar(style = 3)

  for (i in seq_along(balanceFiles)) {
    rows <- reference |>
      filter(.data$balanceFile == !!balanceFiles[i])
    balance <- readRDS(file.path(outputFolder, balanceFiles[i]))
    balance <- tibble(
      databaseId = !!databaseId,
      targetId = rows$targetId[1],
      comparatorId = rows$comparatorId[1],
      outcomeId = rows$outcomeId,
      analysisId = unique(rows$analysisId)
    ) |>
      cross_join(tidyBalance(balance, minCellCount))
    writeToCsv(balance, fileName, append = !first)
    first <- FALSE
    setTxtProgressBar(pb, i / length(balanceFiles))

  }
  if (first) {
    results <- createEmptyResult("cm_covariate_balance")
    writeToCsv(results, fileName)
  }
  setTxtProgressBar(pb, 1)
  close(pb)
}

exportSharedCovariateBalance <- function(outputFolder,
                                         exportFolder,
                                         databaseId,
                                         minCellCount) {
  message("- shared_covariate_balance table")
  reference <- getFileReference(outputFolder) |>
    filter(.data$sharedBalanceFile != "") |>
    distinct(.data$sharedBalanceFile, .data$analysisId, .data$targetId, .data$comparatorId)
  sharedBalanceFiles <- reference |>
    distinct(.data$sharedBalanceFile) |>
    pull()

  fileName <- file.path(exportFolder, "cm_shared_covariate_balance.csv")
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  first <- TRUE
  pb <- txtProgressBar(style = 3)
  for (i in seq_along(sharedBalanceFiles)) {
    rows <- reference |>
      filter(.data$sharedBalanceFile == sharedBalanceFiles[i])
    balance <- readRDS(file.path(outputFolder, sharedBalanceFiles[i]))
    balance <- tibble(
      databaseId = !!databaseId,
      targetId = rows$targetId[1],
      comparatorId = rows$comparatorId[1],
      analysisId = unique(rows$analysisId)
    ) |>
      cross_join(tidyBalance(balance, minCellCount))
    writeToCsv(balance, fileName, append = !first)
    first <- FALSE
    setTxtProgressBar(pb, i / length(sharedBalanceFiles))
  }
  if (first) {
    results <- createEmptyResult("cm_shared_covariate_balance")
    writeToCsv(results, fileName)
  }
  setTxtProgressBar(pb, 1)
  close(pb)
}

tidyBalance <- function(balance, minCellCount) {
  inferredTargetBeforeSize <- mean(balance$beforeMatchingSumTarget / balance$beforeMatchingMeanTarget, na.rm = TRUE)
  inferredComparatorBeforeSize <- mean(balance$beforeMatchingSumComparator / balance$beforeMatchingMeanComparator, na.rm = TRUE)
  inferredTargetAfterSize <- mean(balance$afterMatchingSumTarget / balance$afterMatchingMeanTarget, na.rm = TRUE)
  inferredComparatorAfterSize <- mean(balance$afterMatchingSumComparator / balance$afterMatchingMeanComparator, na.rm = TRUE)

  balance <- balance |>
    select("covariateId",
           targetMeanBefore = "beforeMatchingMeanTarget",
           comparatorMeanBefore = "beforeMatchingMeanComparator",
           stdDiffBefore = "beforeMatchingStdDiff",
           targetMeanAfter = "afterMatchingMeanTarget",
           comparatorMeanAfter = "afterMatchingMeanComparator",
           stdDiffAfter = "afterMatchingStdDiff",
           meanBefore = "beforeMatchingMean",
           meanAfter = "afterMatchingMean",
           "targetStdDiff",
           "comparatorStdDiff",
           "targetComparatorStdDiff",

    ) |>
    mutate(
      targetMeanBefore = ifelse(is.na(.data$targetMeanBefore), 0, .data$targetMeanBefore),
      comparatorMeanBefore = ifelse(is.na(.data$comparatorMeanBefore), 0, .data$comparatorMeanBefore),
      stdDiffBefore = ifelse(is.na(.data$stdDiffBefore), 0, .data$stdDiffBefore),
      targetMeanAfter = ifelse(is.na(.data$targetMeanAfter), 0, .data$targetMeanAfter),
      comparatorMeanAfter = ifelse(is.na(.data$comparatorMeanAfter), 0, .data$comparatorMeanAfter),
      stdDiffAfter = ifelse(is.na(.data$stdDiffAfter), 0, .data$stdDiffAfter),
      meanBefore = ifelse(is.na(.data$meanBefore), 0, .data$meanBefore),
      meanAfter = ifelse(is.na(.data$stdDiffAfter), 0, .data$meanAfter),
      targetStdDiff = ifelse(is.na(.data$targetStdDiff), 0, .data$targetStdDiff),
      comparatorStdDiff = ifelse(is.na(.data$comparatorStdDiff), 0, .data$comparatorStdDiff),
      targetComparatorStdDiff = ifelse(is.na(.data$targetComparatorStdDiff), 0, .data$targetComparatorStdDiff)
    ) |>
    filter(!(round(.data$targetMeanBefore) == 0 &
               round(.data$comparatorMeanBefore, 3) == 0 &
               round(.data$stdDiffBefore, 3) == 0 &
               round(.data$targetMeanAfter, 3) == 0 &
               round(.data$comparatorMeanAfter, 3) == 0 &
               round(.data$stdDiffAfter, 3) == 0 &
               round(.data$meanBefore, 3) == 0 &
               round(.data$meanAfter, 3) == 0 &
               round(.data$targetStdDiff, 3) == 0 &
               round(.data$comparatorStdDiff, 3) == 0 &
               round(.data$targetComparatorStdDiff, 3) == 0)
    ) |>
    enforceMinCellValue("targetMeanBefore",
                        minCellCount / inferredTargetBeforeSize,
                        silent = TRUE
    ) |>
    enforceMinCellValue("comparatorMeanBefore",
                        minCellCount / inferredComparatorBeforeSize,
                        silent = TRUE
    ) |>
    enforceMinCellValue("targetMeanAfter",
                        minCellCount / inferredTargetAfterSize,
                        silent = TRUE
    ) |>
    enforceMinCellValue("comparatorMeanAfter",
                        minCellCount / inferredComparatorAfterSize,
                        silent = TRUE
    ) |>
    enforceMinCellValue("meanBefore",
                        minCellCount / inferredComparatorAfterSize,
                        silent = TRUE
    ) |>
    enforceMinCellValue("meanAfter",
                        minCellCount / inferredComparatorAfterSize,
                        silent = TRUE
    ) |>
    mutate(
      targetMeanBefore = round(.data$targetMeanBefore, 3),
      comparatorMeanBefore = round(.data$comparatorMeanBefore, 3),
      stdDiffBefore = round(.data$stdDiffBefore, 3),
      targetMeanAfter = round(.data$targetMeanAfter, 3),
      comparatorMeanAfter = round(.data$comparatorMeanAfter, 3),
      stdDiffAfter = round(.data$stdDiffAfter, 3),
      meanBefore = round(.data$meanBefore, 3),
      meanAfter = round(.data$meanAfter, 3),
      targetStdDiff = round(.data$targetStdDiff, 3),
      comparatorStdDiff = round(.data$comparatorStdDiff, 3),
      targetComparatorStdDiff = round(.data$targetComparatorStdDiff, 3)
    )
  return(balance)
}

exportPreferenceScoreDistribution <- function(outputFolder,
                                              exportFolder,
                                              databaseId) {
  message("- preference_score_dist table")

  reference <- getFileReference(outputFolder) |>
    filter(.data$sharedPsFile != "") |>
    distinct(.data$sharedPsFile, .data$analysisId, .data$targetId, .data$comparatorId)

  # rows <- split(reference, reference$sharedPsFile)[[2]]
  preparePlot <- function(rows) {
    ps <- readRDS(file.path(outputFolder, rows$sharedPsFile[1]))
    if (nrow(ps) > 0 &&
        min(ps$propensityScore) < max(ps$propensityScore) &&
        sum(ps$treatment == 1) > 1 &&
        sum(ps$treatment == 0) > 1 ) {
      ps <- computePreferenceScore(ps)
      d1 <- density(ps$preferenceScore[ps$treatment == 1], from = 0, to = 1, n = 100)
      d0 <- density(ps$preferenceScore[ps$treatment == 0], from = 0, to = 1, n = 100)
      result <- rows |>
        select(
          "analysisId",
          "targetId",
          "comparatorId"
        ) |>
        mutate(databaseId = !!databaseId) |>
        cross_join(
          tibble(
            preferenceScore = d1$x,
            targetDensity = d1$y,
            comparatorDensity = d0$y
          )
        )
      return(result)
    } else {
      return(NULL)
    }
  }
  data <- lapply(split(reference, reference$sharedPsFile), preparePlot)
  data <- bind_rows(data)
  if (nrow(data) == 0) {
    data <- createEmptyResult("cm_preference_score_dist")
  }
  fileName <- file.path(exportFolder, "cm_preference_score_dist.csv")
  writeToCsv(data, fileName)
}

exportPropensityModel <- function(outputFolder,
                                  exportFolder,
                                  databaseId) {
  message("- propensity_model table")
  reference <- getFileReference(outputFolder) |>
    filter(.data$sharedPsFile != "") |>
    distinct(.data$sharedPsFile, .data$analysisId, .data$targetId, .data$comparatorId)

  # rows <- split(reference, reference$sharedPsFile)[[1]]
  prepareData <- function(rows) {
    ps <- readRDS(file.path(outputFolder, rows$sharedPsFile[1]))
    metaData <- attr(ps, "metaData")
    if (is.null(metaData$psError)) {
      model <- metaData$psModelCoef
      model <- tibble(
        covariateId = names(metaData$psModelCoef),
        coefficient = as.vector(metaData$psModelCoef)
      ) |>
        filter(.data$coefficient != 0) |>
        mutate(covariateId = ifelse(.data$covariateId == "(Intercept)", 0, .data$covariateId)) |>
        mutate(covariateId = as.numeric(.data$covariateId))
      rows <- rows |>
        select("targetId", "comparatorId", "analysisId") |>
        mutate(databaseId = !!databaseId) |>
        cross_join(model)
      return(rows)
    } else {
      return(NULL)
    }
  }
  data <- lapply(split(reference, reference$sharedPsFile), prepareData)
  data <- bind_rows(data)
  if (nrow(data) == 0) {
    data <- createEmptyResult("cm_propensity_model")
  }
  fileName <- file.path(exportFolder, "cm_propensity_model.csv")
  writeToCsv(data, fileName)
}

exportKaplanMeier <- function(outputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount,
                              maxCores) {
  message("- kaplan_meier_dist table")
  message("  Computing KM curves")
  reference <- getFileReference(outputFolder) |>
    filter(.data$outcomeOfInterest) |>
    select(
      "strataFile",
      "studyPopFile",
      "targetId",
      "comparatorId",
      "outcomeId",
      "analysisId"
    )

  tempFolder <- file.path(exportFolder, "temp")
  if (!file.exists(tempFolder)) {
    dir.create(tempFolder)
  }

  if (nrow(reference) > 0) {
    tasks <- split(reference, seq_len(nrow(reference)))
    cluster <- ParallelLogger::makeCluster(min(length(tasks), 4, maxCores))
    ParallelLogger::clusterApply(cluster,
                                 tasks,
                                 prepareKm,
                                 outputFolder = outputFolder,
                                 tempFolder = tempFolder,
                                 databaseId = databaseId,
                                 minCellCount = minCellCount
    )
    ParallelLogger::stopCluster(cluster)
  }

  message("  Writing to single csv file")
  outputFile <- file.path(exportFolder, "cm_kaplan_meier_dist.csv")
  files <- list.files(tempFolder, "km_.*.rds", full.names = TRUE)
  first <- TRUE
  pb <- txtProgressBar(style = 3)
  for (i in seq_along(files)) {
    data <- readRDS(files[i])
    writeToCsv(data, outputFile, append = !first)
    first <- FALSE
    if (i %% 100 == 10) {
      setTxtProgressBar(pb, i / length(files))
    }
  }
  if (first) {
    data <- createEmptyResult("cm_kaplan_meier_dist")
    writeToCsv(data, outputFile)
  }
  setTxtProgressBar(pb, 1)
  close(pb)
  unlink(tempFolder, recursive = TRUE)
}

# task = tasks[[1]]
prepareKm <- function(task,
                      outputFolder,
                      tempFolder,
                      databaseId,
                      minCellCount) {
  ParallelLogger::logTrace(
    "Preparing KM plot for target ",
    task$targetId,
    ", comparator ",
    task$comparatorId,
    ", outcome ",
    task$outcomeId,
    ", analysis ",
    task$analysisId
  )
  outputFileName <- file.path(tempFolder, sprintf(
    "km_t%s_c%s_o%s_a%s.rds",
    task$targetId,
    task$comparatorId,
    task$outcomeId,
    task$analysisId
  ))
  if (file.exists(outputFileName)) {
    return(NULL)
  }
  popFile <- task$strataFile
  if (popFile == "") {
    popFile <- task$studyPopFile
  }
  population <- readRDS(file.path(
    outputFolder,
    popFile
  ))
  if (nrow(population) == 0) {
    # Can happen when matching and treatment is predictable
    return(NULL)
  }
  data <- prepareKaplanMeier(population)
  if (is.null(data)) {
    # No shared strata
    return(NULL)
  }
  data$targetId <- task$targetId
  data$comparatorId <- task$comparatorId
  data$outcomeId <- task$outcomeId
  data$analysisId <- task$analysisId
  data$databaseId <- databaseId
  data <- enforceMinCellValue(data, "targetAtRisk", minCellCount)
  data <- enforceMinCellValue(data, "comparatorAtRisk", minCellCount)
  # Avoid SQL reserved word 'time':
  data <- data |>
    rename(timeDay = "time")
  saveRDS(data, outputFileName)
}

prepareKaplanMeier <- function(population) {
  dataCutoff <- 0.9
  population$y <- 0
  population$y[population$outcomeCount != 0] <- 1
  if (!"stratumId" %in% colnames(population) || length(unique(population$stratumId)) == nrow(population) / 2) {
    sv <- survival::survfit(survival::Surv(survivalTime, y) ~ treatment, population, conf.int = TRUE)
    idx <- summary(sv, censored = T)$strata == "treatment=1"
    survTarget <- tibble(
      time = sv$time[idx],
      targetSurvival = sv$surv[idx],
      targetSurvivalLb = sv$lower[idx],
      targetSurvivalUb = sv$upper[idx]
    )
    idx <- summary(sv, censored = T)$strata == "treatment=0"
    survComparator <- tibble(
      time = sv$time[idx],
      comparatorSurvival = sv$surv[idx],
      comparatorSurvivalLb = sv$lower[idx],
      comparatorSurvivalUb = sv$upper[idx]
    )
    data <- merge(survTarget, survComparator, all = TRUE)
  } else {
    population$stratumSizeT <- 1
    strataSizesT <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 1, ], sum)
    if (max(strataSizesT$stratumSizeT) == 1) {
      # variable ratio matching: use propensity score to compute IPTW
      if (is.null(population$propensityScore)) {
        stop("Variable ratio matching detected, but no propensity score found")
      }
      weights <- aggregate(propensityScore ~ stratumId, population, mean)
      if (max(weights$propensityScore) > 0.99999) {
        return(NULL)
      }
      weights$weight <- weights$propensityScore / (1 - weights$propensityScore)
    } else {
      # stratification: infer probability of treatment from subject counts
      strataSizesC <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 0, ], sum)
      colnames(strataSizesC)[2] <- "stratumSizeC"
      weights <- merge(strataSizesT, strataSizesC)
      if (nrow(weights) == 0) {
        warning("No shared strata between target and comparator")
        return(NULL)
      }
      weights$weight <- weights$stratumSizeT / weights$stratumSizeC
    }
    population <- merge(population, weights[, c("stratumId", "weight")])
    population$weight[population$treatment == 1] <- 1
    idx <- population$treatment == 1
    survTarget <- adjustedKm(
      weight = population$weight[idx],
      time = population$survivalTime[idx],
      y = population$y[idx]
    )
    survTarget$targetSurvivalUb <- survTarget$s^exp(qnorm(0.975) / log(survTarget$s) * sqrt(survTarget$var) / survTarget$s)
    survTarget$targetSurvivalLb <- survTarget$s^exp(qnorm(0.025) / log(survTarget$s) * sqrt(survTarget$var) / survTarget$s)
    survTarget$targetSurvivalLb[survTarget$s > 0.9999] <- survTarget$s[survTarget$s > 0.9999]
    survTarget$targetSurvival <- survTarget$s
    survTarget$s <- NULL
    survTarget$var <- NULL
    idx <- population$treatment == 0
    survComparator <- adjustedKm(
      weight = population$weight[idx],
      time = population$survivalTime[idx],
      y = population$y[idx]
    )
    survComparator$comparatorSurvivalUb <- survComparator$s^exp(qnorm(0.975) / log(survComparator$s) *
                                                                  sqrt(survComparator$var) / survComparator$s)
    survComparator$comparatorSurvivalLb <- survComparator$s^exp(qnorm(0.025) / log(survComparator$s) *
                                                                  sqrt(survComparator$var) / survComparator$s)
    survComparator$comparatorSurvivalLb[survComparator$s > 0.9999] <- survComparator$s[survComparator$s >
                                                                                         0.9999]
    survComparator$comparatorSurvival <- survComparator$s
    survComparator$s <- NULL
    survComparator$var <- NULL
    data <- merge(survTarget, survComparator, all = TRUE)
  }
  data <- data[, c("time", "targetSurvival", "targetSurvivalLb", "targetSurvivalUb", "comparatorSurvival", "comparatorSurvivalLb", "comparatorSurvivalUb")]
  cutoff <- quantile(population$survivalTime, dataCutoff)
  data <- data[data$time <= cutoff, ]
  if (cutoff <= 300) {
    xBreaks <- seq(0, cutoff, by = 50)
  } else if (cutoff <= 600) {
    xBreaks <- seq(0, cutoff, by = 100)
  } else {
    xBreaks <- seq(0, cutoff, by = 250)
  }

  targetAtRisk <- c()
  comparatorAtRisk <- c()
  for (xBreak in xBreaks) {
    targetAtRisk <- c(
      targetAtRisk,
      sum(population$treatment == 1 & population$survivalTime >= xBreak)
    )
    comparatorAtRisk <- c(
      comparatorAtRisk,
      sum(population$treatment == 0 & population$survivalTime >=
            xBreak)
    )
  }
  data <- merge(data, tibble(
    time = xBreaks,
    targetAtRisk = targetAtRisk,
    comparatorAtRisk = comparatorAtRisk
  ), all = TRUE)
  if (is.na(data$targetSurvival[1])) {
    data$targetSurvival[1] <- 1
    data$targetSurvivalUb[1] <- 1
    data$targetSurvivalLb[1] <- 1
  }
  if (is.na(data$comparatorSurvival[1])) {
    data$comparatorSurvival[1] <- 1
    data$comparatorSurvivalUb[1] <- 1
    data$comparatorSurvivalLb[1] <- 1
  }
  idx <- which(is.na(data$targetSurvival))
  while (length(idx) > 0) {
    data$targetSurvival[idx] <- data$targetSurvival[idx - 1]
    data$targetSurvivalLb[idx] <- data$targetSurvivalLb[idx - 1]
    data$targetSurvivalUb[idx] <- data$targetSurvivalUb[idx - 1]
    idx <- which(is.na(data$targetSurvival))
  }
  idx <- which(is.na(data$comparatorSurvival))
  while (length(idx) > 0) {
    data$comparatorSurvival[idx] <- data$comparatorSurvival[idx - 1]
    data$comparatorSurvivalLb[idx] <- data$comparatorSurvivalLb[idx - 1]
    data$comparatorSurvivalUb[idx] <- data$comparatorSurvivalUb[idx - 1]
    idx <- which(is.na(data$comparatorSurvival))
  }
  data$targetSurvival <- round(data$targetSurvival, 4)
  data$targetSurvivalLb <- round(data$targetSurvivalLb, 4)
  data$targetSurvivalUb <- round(data$targetSurvivalUb, 4)
  data$comparatorSurvival <- round(data$comparatorSurvival, 4)
  data$comparatorSurvivalLb <- round(data$comparatorSurvivalLb, 4)
  data$comparatorSurvivalUb <- round(data$comparatorSurvivalUb, 4)

  # Remove duplicate (except time) entries:
  data <- data[order(data$time), ]
  data <- data[!duplicated(data[, -1]), ]
  return(data)
}

exportDiagnosticsSummary <- function(outputFolder,
                                     exportFolder,
                                     databaseId,
                                     cmDiagnosticThresholds) {
  message("- diagnostics_summary table")
  reference <- getFileReference(outputFolder)
  resultsSummary <- getResultsSummary(outputFolder)

  getMaxSdms <- function(balanceFile) {
    balance <- readRDS(file.path(outputFolder, balanceFile))
    if (nrow(balance) == 0) {
      row <- tibble(balanceFile = !!balanceFile,
                    maxSdm = as.numeric(NA),
                    maxTargetSdm = as.numeric(NA),
                    maxComparatorSdm = as.numeric(NA),
                    maxTargetComparatorSdm = as.numeric(NA))
      return(row)
    } else {
      row <- tibble(balanceFile = !!balanceFile,
                    maxSdm = as.numeric(max(abs(balance$afterMatchingStdDiff), na.rm = TRUE)),
                    maxTargetSdm = as.numeric(max(abs(balance$targetStdDiff), na.rm = TRUE)),
                    maxComparatorSdm = as.numeric(max(abs(balance$comparatorStdDiff), na.rm = TRUE)),
                    maxTargetComparatorSdm = as.numeric(max(abs(balance$targetComparatorStdDiff), na.rm = TRUE)))
      return(row)
    }
  }
  getEquipoise <- function(sharedPsFile) {
    ps <- readRDS(file.path(outputFolder, sharedPsFile))
    row <- tibble(sharedPsFile = !!sharedPsFile,
           equipoise = computeEquipoise(ps))
    return(row)
  }

  balanceFiles <- reference |>
    filter(.data$balanceFile != "") |>
    distinct(.data$balanceFile) |>
    pull()
  maxSdm <- bind_rows(lapply(balanceFiles, getMaxSdms)) |>
    select("balanceFile", "maxSdm")
  sharedBalanceFiles <- reference |>
    filter(.data$sharedBalanceFile != "") |>
    distinct(.data$sharedBalanceFile) |>
    pull()
  sharedMaxSdm <- bind_rows(lapply(sharedBalanceFiles, getMaxSdms)) |>
    rename(sharedBalanceFile = "balanceFile",
           sharedMaxSdm = "maxSdm")
  sharedPsFiles <- reference |>
    filter(.data$sharedPsFile != "") |>
    distinct(.data$sharedPsFile) |>
    pull()
  equipoise <- bind_rows(lapply(sharedPsFiles, getEquipoise))
  results <- reference |>
    inner_join(
      resultsSummary,
      by = join_by("analysisId", "targetId", "comparatorId", "outcomeId")) |>
    left_join(maxSdm, by = "balanceFile") |>
    left_join(sharedMaxSdm, by = "sharedBalanceFile") |>
    mutate(generalizabilityMaxSdm = if_else(.data$targetEstimator == "att",
                                            .data$maxTargetSdm,
                                            if_else(.data$targetEstimator == "atu",
                                                    .data$maxComparatorSdm,
                                                    .data$maxTargetComparatorSdm))) |>
    left_join(equipoise, by = "sharedPsFile") |>
    select(
      "analysisId",
      "targetId",
      "comparatorId",
      "outcomeId",
      "maxSdm",
      "sharedMaxSdm",
      "equipoise",
      "mdrr",
      "generalizabilityMaxSdm",
      "ease"
    )

  # Apply diagnostics thresholds:
  results <- results |>
    mutate(databaseId = !!databaseId) |>
    mutate(balanceDiagnostic = case_when(
      is.na(.data$maxSdm) ~ "NOT EVALUATED",
      .data$maxSdm < cmDiagnosticThresholds$sdmThreshold ~ "PASS",
      TRUE ~ "FAIL"
    )) |>
    mutate(sharedBalanceDiagnostic = case_when(
      is.na(.data$sharedMaxSdm) ~ "NOT EVALUATED",
      .data$sharedMaxSdm < cmDiagnosticThresholds$sdmThreshold ~ "PASS",
      TRUE ~ "FAIL"
    )) |>
    mutate(equipoiseDiagnostic = case_when(
      is.na(.data$equipoise) ~ "NOT EVALUATED",
      .data$equipoise >= cmDiagnosticThresholds$equipoiseThreshold ~ "PASS",
      TRUE ~ "FAIL"
    )) |>
    mutate(mdrrDiagnostic = case_when(
      is.na(.data$mdrr) ~ "NOT EVALUATED",
      .data$mdrr < cmDiagnosticThresholds$mdrrThreshold ~ "PASS",
      TRUE ~ "FAIL"
    )) |>
    mutate(generalizabilityDiagnostic = case_when(
      is.na(.data$generalizabilityMaxSdm) ~ "NOT EVALUATED",
      .data$generalizabilityMaxSdm < cmDiagnosticThresholds$generalizabilitySdmThreshold ~ "PASS",
      TRUE ~ "FAIL"
    )) |>
    mutate(easeDiagnostic = case_when(
      is.na(.data$ease) ~ "NOT EVALUATED",
      abs(.data$ease) < cmDiagnosticThresholds$easeThreshold ~ "PASS",
      TRUE ~ "FAIL"
    )) |>
    mutate(unblind = ifelse(.data$mdrrDiagnostic != "FAIL" &
                              .data$generalizabilityDiagnostic != "FAIL" &
                              .data$easeDiagnostic != "FAIL" &
                              .data$equipoiseDiagnostic != "FAIL" &
                              .data$balanceDiagnostic != "FAIL" &
                              .data$sharedBalanceDiagnostic != "FAIL", 1, 0)) |>
    mutate(unblindForEvidenceSynthesis = ifelse(.data$generalizabilityDiagnostic != "FAIL" &
                                                  .data$easeDiagnostic != "FAIL" &
                                                  .data$equipoiseDiagnostic != "FAIL" &
                                                  .data$balanceDiagnostic != "FAIL" &
                                                  .data$sharedBalanceDiagnostic != "FAIL", 1, 0))

  # Add deprecated fields:
  results <- results |>
    mutate(attritionFraction = as.numeric(NA),
           attritionDiagnostic = "NOT EVALUATED")

  if (nrow(results) == 0) {
    results <- createEmptyResult("cm_diagnostics_summary")
  }
  fileName <- file.path(exportFolder, "cm_diagnostics_summary.csv")
  writeToCsv(results, fileName)
}

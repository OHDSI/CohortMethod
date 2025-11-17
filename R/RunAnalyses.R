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

#' Create CohortMethod multi-threading settings
#'
#' @param getDbCohortMethodDataThreads   The number of parallel threads to use for building the
#'                                       cohortMethod data objects.
#' @param createPsThreads                The number of parallel threads to use for fitting the
#'                                       propensity models.
#' @param psCvThreads                    The number of parallel threads to use for the cross-
#'                                       validation when estimating the hyperparameter for the
#'                                       propensity model. Note that the total number of CV threads at
#'                                       one time could be `createPsThreads * psCvThreads`.
#' @param createStudyPopThreads          The number of parallel threads to use for creating the study
#'                                       population.
#' @param trimMatchStratifyThreads       The number of parallel threads to use for trimming, matching
#'                                       and stratifying.
#' @param computeSharedBalanceThreads    The number of parallel threads to use for computing shared covariate
#'                                       balance.
#' @param computeBalanceThreads          The number of parallel threads to use for computing covariate
#'                                       balance.
#' @param fitOutcomeModelThreads         The number of parallel threads to use for fitting the outcome
#'                                       models.
#' @param prefilterCovariatesThreads     The number of parallel threads to use for prefiltering covariates.
#' @param outcomeCvThreads               The number of parallel threads to use for the cross-
#'                                       validation when estimating the hyperparameter for the outcome
#'                                       model. Note that the total number of CV threads at one time
#'                                       could be `fitOutcomeModelThreads * outcomeCvThreads`.
#' @param calibrationThreads             The number of parallel threads to use for empirical calibration.
#'
#' @return
#' An object of type `CmMultiThreadingSettings`.
#'
#' @seealso [createDefaultMultiThreadingSettings()]
#'
#' @export
createMultiThreadingSettings <- function(getDbCohortMethodDataThreads = 1,
                                         createPsThreads = 1,
                                         psCvThreads = 1,
                                         createStudyPopThreads = 1,
                                         trimMatchStratifyThreads = 1,
                                         computeSharedBalanceThreads = 1,
                                         computeBalanceThreads = 1,
                                         prefilterCovariatesThreads = 1,
                                         fitOutcomeModelThreads = 1,
                                         outcomeCvThreads = 1,
                                         calibrationThreads = 1) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(getDbCohortMethodDataThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(createPsThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(psCvThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(createStudyPopThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(trimMatchStratifyThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(prefilterCovariatesThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(fitOutcomeModelThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(outcomeCvThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(calibrationThreads, lower = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  settings <- list()
  for (name in names(formals(createMultiThreadingSettings))) {
    settings[[name]] <- get(name)
  }
  class(settings) <- "CmMultiThreadingSettings"
  return(settings)
}


#' Create default CohortMethod multi-threading settings
#'
#' @description
#' Create CohortMethod multi-threading settings based on the maximum number of cores to be
#' used.
#'
#' @param maxCores  Maximum number of CPU cores to use.
#'
#' @return
#' An object of type `CmMultiThreadingSettings`.
#'
#' @seealso [createMultiThreadingSettings()]
#'
#' @examples
#' settings <- createDefaultMultiThreadingSettings(10)
#'
#' @export
createDefaultMultiThreadingSettings <- function(maxCores) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  settings <- createMultiThreadingSettings(
    getDbCohortMethodDataThreads = min(3, maxCores),
    createPsThreads = max(1, floor(maxCores / 10)),
    psCvThreads = min(10, maxCores),
    createStudyPopThreads = min(3, maxCores),
    trimMatchStratifyThreads = min(5, maxCores),
    computeSharedBalanceThreads = min(3, maxCores),
    computeBalanceThreads = min(5, maxCores),
    prefilterCovariatesThreads = min(3, maxCores),
    fitOutcomeModelThreads = max(1, floor(maxCores / 4)),
    outcomeCvThreads = min(4, maxCores),
    calibrationThreads = min(4, maxCores)
  )
  return(settings)
}

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses for the target-comparator-outcomes of interest. This function will run all
#' specified analyses against all hypotheses of interest, meaning that the total number of outcome
#' models is `length(cmAnalysisList) * length(targetComparatorOutcomesList)` (if all analyses specify an
#' outcome model should be fitted). When you provide several analyses it will determine whether any of
#' the analyses have anything in common, and will take advantage of this fact. For example, if we
#' specify several analyses that only differ in the way the outcome model is fitted, then this
#' function will extract the data and fit the propensity model only once, and re-use this in all the
#' analysis.
#'
#' After completion, a tibble containing references to all generated files can be obtained using the
#' [getFileReference()] function. A summary of the analysis results can be obtained using the
#' [getResultsSummary()] function.
#'
#' ## Analyses to Exclude
#'
#' Normally, `runCmAnalyses` will run all combinations of target-comparator-outcome-analyses settings.
#' However, sometimes we may not need all those combinations. Using the `analysesToExclude` argument,
#' we can remove certain items from the full matrix. This argument should be a data frame with at least
#' one of the following columns:
#'
#' - targetId
#' - comparatorId
#' - outcomeId
#' - analysisId
#'
#' This data frame will be joined to the outcome model reference table before executing, and matching rows
#' will be removed. For example, if one specifies only one target ID and analysis ID, then any analyses with
#' that target and that analysis ID will be skipped.
#'
#' @param connectionDetails              An R object of type `connectionDetails` created using the
#'                                       [DatabaseConnector::createConnectionDetails()] function.
#' @param cdmDatabaseSchema              The name of the database schema that contains the OMOP CDM
#'                                       instance. Requires read permissions to this database. On SQL
#'                                       Server, this should specify both the database and the schema,
#'                                       so for example 'cdm_instance.dbo'.
#' @param tempEmulationSchema Some database platforms like Oracle and Impala do not truly support temp tables. To
#'                            emulate temp tables, provide a schema with write privileges where temp tables
#'                            can be created.
#' @param exposureDatabaseSchema         The name of the database schema that is the location where the
#'                                       exposure data used to define the exposure cohorts is
#'                                       available. If exposureTable = DRUG_ERA, exposureDatabaseSchema
#'                                       is not used by assumed to be cdmSchema.  Requires read
#'                                       permissions to this database.
#' @param exposureTable                  The tablename that contains the exposure cohorts.  If
#'                                       exposureTable <> DRUG_ERA, then expectation is exposureTable
#'                                       has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                       COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema          The name of the database schema that is the location where the
#'                                       data used to define the outcome cohorts is available. If
#'                                       exposureTable = CONDITION_ERA, exposureDatabaseSchema is not
#'                                       used by assumed to be cdmSchema.  Requires read permissions to
#'                                       this database.
#' @param outcomeTable                   The tablename that contains the outcome cohorts.  If
#'                                       outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                       outcomeTable has format of COHORT table: COHORT_DEFINITION_ID,
#'                                       SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.
#' @param outputFolder                   Name of the folder where all the outputs will written to.
#' @param multiThreadingSettings         An object of type `CmMultiThreadingSettings` as created using
#'                                       the [createMultiThreadingSettings()] or
#'                                       [createDefaultMultiThreadingSettings()] functions.
#' @param cmAnalysesSpecifications       An object of type `CmAnalysesSpecifications` as created using
#'                                       the `createCmAnalysesSpecifications()`.
#'
#' @return
#' A tibble describing for each target-comparator-outcome-analysisId combination where the intermediary and
#' outcome model files can be found, relative to the `outputFolder`.
#'
#' @export
runCmAnalyses <- function(connectionDetails,
                          cdmDatabaseSchema,
                          tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "condition_occurrence",
                          outputFolder = "./CohortMethodOutput",
                          multiThreadingSettings = createMultiThreadingSettings(),
                          cmAnalysesSpecifications) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(exposureDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(exposureTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(outcomeDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(outcomeTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::assertClass(multiThreadingSettings, "CmMultiThreadingSettings", add = errorMessages)
  checkmate::assertR6(cmAnalysesSpecifications, "CmAnalysesSpecifications", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  outputFolder <- normalizePath(outputFolder, mustWork = FALSE)
  cmAnalysesSpecificationsFile <- file.path(outputFolder, "cmAnalysesSpecifications.rds")
  if (file.exists(cmAnalysesSpecificationsFile)) {
    oldCmAnalysesSpecifications <- readRDS(cmAnalysesSpecificationsFile)
    if (!isTRUE(all.equal(oldCmAnalysesSpecifications, cmAnalysesSpecifications))) {
      rm(list = ls(envir = cache), envir = cache)
      message(sprintf("Output files already exist in '%s', but the analysis settings have changed.", outputFolder))
      response <- utils::askYesNo("Do you want to delete the old files before proceeding?")
      if (is.na(response)) {
        # Cancel:
        return()
      } else if (response == TRUE) {
        unlink(outputFolder, recursive = TRUE)
      }
    }
  }
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }
  saveRDS(cmAnalysesSpecifications, cmAnalysesSpecificationsFile)
  referenceTable <- createReferenceTable(
    cmAnalysisList = cmAnalysesSpecifications$cmAnalysisList,
    targetComparatorOutcomesList = cmAnalysesSpecifications$targetComparatorOutcomesList,
    analysesToExclude = cmAnalysesSpecifications$analysesToExclude,
    outputFolder = outputFolder,
    refitPsForEveryOutcome = cmAnalysesSpecifications$refitPsForEveryOutcome,
    refitPsForEveryStudyPopulation = cmAnalysesSpecifications$refitPsForEveryStudyPopulation
  )
  referenceTable |>
    select(-"includedCovariateConceptIds", "excludedCovariateConceptIds") |>
    saveRDS(file.path(outputFolder, "outcomeModelReference.rds"))

  # Create cohortMethodData objects -----------------------------
  subset <- referenceTable[!duplicated(referenceTable$cohortMethodDataFile), ]
  subset <- subset[subset$cohortMethodDataFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$cohortMethodDataFile)), ]
  if (nrow(subset) != 0) {
    message("*** Creating cohortMethodData objects ***")
    createCmDataTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(
        cmAnalysisList,
        list(analysisId = refRow$analysisId)
      )[[1]]
      # Create a clone because we'll alter the covariateSettings in the args object:
      getDbCohortMethodDataArgs <- analysisRow$getDbCohortMethodDataArgs$clone()
      covariateSettings <- getDbCohortMethodDataArgs$covariateSettings
      if (is(covariateSettings, "covariateSettings")) {
        covariateSettings <- list(covariateSettings)
      }
      for (i in 1:length(covariateSettings)) {
        covariateSettings[[i]]$excludedCovariateConceptIds <- unique(c(
          as.numeric(unlist(strsplit(
            as.character(refRow$excludedCovariateConceptIds),
            ","
          ))),
          covariateSettings[[i]]$excludedCovariateConceptIds
        ))
        covariateSettings[[i]]$includedCovariateConceptIds <- unique(c(
          as.numeric(unlist(strsplit(
            as.character(refRow$includedCovariateConceptIds),
            ","
          ))),
          covariateSettings[[i]]$includedCovariateConceptIds
        ))
      }
      getDbCohortMethodDataArgs$covariateSettings <- covariateSettings
      outcomeIds <- unique(referenceTable$outcomeId[referenceTable$cohortMethodDataFile == refRow$cohortMethodDataFile])
      args <- list(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        tempEmulationSchema = tempEmulationSchema,
        exposureDatabaseSchema = exposureDatabaseSchema,
        exposureTable = exposureTable,
        outcomeDatabaseSchema = outcomeDatabaseSchema,
        outcomeTable = outcomeTable,
        outcomeIds = outcomeIds,
        targetId = refRow$targetId,
        comparatorId = refRow$comparatorId,
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs
      )
      task <- list(
        args = args,
        cohortMethodDataFile = file.path(outputFolder, refRow$cohortMethodDataFile)
      )
      return(task)
    }
    objectsToCreate <- lapply(1:nrow(subset), createCmDataTask)
    cluster <- ParallelLogger::makeCluster(min(length(objectsToCreate), multiThreadingSettings$getDbCohortMethodDataThreads))
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, objectsToCreate, doCreateCmDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  # Create study populations --------------------------------------
  subset <- referenceTable[!duplicated(referenceTable$studyPopFile), ]
  subset <- subset[subset$studyPopFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$studyPopFile)), ]
  if (nrow(subset) != 0) {
    message("*** Creating study populations ***")
    createStudyPopTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(
        cmAnalysisList,
        list(analysisId = refRow$analysisId)
      )[[1]]
      createStudyPopulationArgs <- analysisRow$createStudyPopulationArgs

      # Override defaults with outcome-specific settings if provided
      tco <- ParallelLogger::matchInList(
        targetComparatorOutcomesList,
        list(
          comparatorId = refRow$comparatorId,
          targetId = refRow$targetId
        )
      )[[1]]
      outcome <- ParallelLogger::matchInList(
        tco$outcomes,
        list(outcomeId = as.numeric(refRow$outcomeId))
      )
      if (!is.null(outcome$priorOutcomeLookback)) {
        createStudyPopulationArgs$priorOutcomeLookback <- outcome$priorOutcomeLookback
      }
      if (!is.null(outcome$riskWindowStart)) {
        createStudyPopulationArgs$riskWindowStart <- outcome$riskWindowStart
      }
      if (!is.null(outcome$startAnchor)) {
        createStudyPopulationArgs$startAnchor <- outcome$startAnchor
      }
      if (!is.null(outcome$riskWindowEnd)) {
        createStudyPopulationArgs$riskWindowEnd <- outcome$riskWindowEnd
      }
      if (!is.null(outcome$endAnchor)) {
        createStudyPopulationArgs$endAnchor <- outcome$endAnchor
      }
      task <- list(
        cohortMethodDataFile = file.path(
          outputFolder,
          refRow$cohortMethodDataFile
        ),
        outcomeId = refRow$outcomeId,
        createStudyPopulationArgs = createStudyPopulationArgs,
        minimizeFileSizes = getOption("minimizeFileSizes"),
        studyPopFile = file.path(outputFolder, refRow$studyPopFile)
      )
      return(task)
    }
    objectsToCreate <- lapply(1:nrow(subset), createStudyPopTask)
    cluster <- ParallelLogger::makeCluster(min(length(objectsToCreate), multiThreadingSettings$createStudyPopThreads))
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, objectsToCreate, doCreateStudyPopObject)
    ParallelLogger::stopCluster(cluster)
  }

  # Fit propensity models ---------------------------------------
  if (cmAnalysesSpecifications$refitPsForEveryOutcome) {
    subset <- referenceTable[!duplicated(referenceTable$psFile), ]
    subset <- subset[subset$psFile != "", ]
    subset <- subset[!file.exists(file.path(outputFolder, subset$psFile)), ]
    if (nrow(subset) != 0) {
      message("*** Fitting propensity models ***")
      createPsTask <- function(i) {
        refRow <- subset[i, ]
        analysisRow <- ParallelLogger::matchInList(
          cmAnalysisList,
          list(analysisId = refRow$analysisId)
        )[[1]]
        createPsArgs = analysisRow$createPsArgs
        createPsArgs$control$threads <- multiThreadingSettings$psCvThreads
        task <- list(
          cohortMethodDataFile = file.path(
            outputFolder,
            refRow$cohortMethodDataFile
          ),
          studyPopFile = file.path(outputFolder, refRow$studyPopFile),
          args = createPsArgs,
          psFile = file.path(outputFolder, refRow$psFile)
        )
        return(task)
      }

      modelsToFit <- lapply(1:nrow(subset), createPsTask)
      cluster <- ParallelLogger::makeCluster(min(length(modelsToFit), multiThreadingSettings$createPsThreads))
      ParallelLogger::clusterRequire(cluster, "CohortMethod")
      dummy <- ParallelLogger::clusterApply(cluster, modelsToFit, doFitPsModel)
      ParallelLogger::stopCluster(cluster)
    }
  } else {
    subset <- referenceTable[!duplicated(referenceTable$sharedPsFile), ]
    subset <- subset[subset$sharedPsFile != "", ]
    subset <- subset[!file.exists(file.path(outputFolder, subset$sharedPsFile)), ]
    if (nrow(subset) != 0) {
      message("*** Fitting shared propensity models ***")
      createSharedPsTask <- function(i) {
        refRow <- subset[i, ]
        analysisRow <- ParallelLogger::matchInList(
          cmAnalysisList,
          list(analysisId = refRow$analysisId)
        )[[1]]
        createPsArg <- analysisRow$createPsArg
        createPsArg$control$threads <- multiThreadingSettings$psCvThreads
        task <- list(
          cohortMethodDataFile = file.path(
            outputFolder,
            refRow$cohortMethodDataFile
          ),
          args = createPsArgs,
          createStudyPopulationArgs = analysisRow$createStudyPopulationArgs,
          sharedPsFile = file.path(outputFolder, refRow$sharedPsFile)
        )
        return(task)
      }
      modelsToFit <- lapply(1:nrow(subset), createSharedPsTask)
      cluster <- ParallelLogger::makeCluster(min(length(modelsToFit), multiThreadingSettings$createPsThreads))
      ParallelLogger::clusterRequire(cluster, "CohortMethod")
      dummy <- ParallelLogger::clusterApply(cluster, modelsToFit, doFitSharedPsModel, cmAnalysesSpecifications$refitPsForEveryStudyPopulation)
      ParallelLogger::stopCluster(cluster)
    }

    subset <- referenceTable[!duplicated(referenceTable$psFile), ]
    subset <- subset[subset$psFile != "", ]
    subset <- subset[!file.exists(file.path(outputFolder, subset$psFile)), ]
    if (nrow(subset) != 0) {
      message("*** Adding propensity scores to study population objects ***")
      tasks <- split(subset, subset$sharedPsFile)
      cluster <- ParallelLogger::makeCluster(min(length(tasks), multiThreadingSettings$trimMatchStratifyThreads))
      ParallelLogger::clusterRequire(cluster, "CohortMethod")
      dummy <- ParallelLogger::clusterApply(cluster, tasks, addPsToStudyPopForSubset, outputFolder = outputFolder)
      ParallelLogger::stopCluster(cluster)
    }
  }

  # Trimming/Matching/Stratifying -----------------------------------------
  subset <- referenceTable[!duplicated(referenceTable$strataFile), ]
  subset <- subset[subset$strataFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$strataFile)), ]
  if (nrow(subset) != 0) {
    message("*** Trimming/Matching/Stratifying ***")
    createTrimMatchStratTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(
        cmAnalysisList,
        list(analysisId = refRow$analysisId)
      )[[1]]
      task <- list(
        psFile = file.path(outputFolder, refRow$psFile),
        args = analysisRow,
        strataFile = file.path(outputFolder, refRow$strataFile)
      )
      return(task)
    }
    tasks <- lapply(1:nrow(subset), createTrimMatchStratTask)

    cluster <- ParallelLogger::makeCluster(min(length(tasks), multiThreadingSettings$trimMatchStratifyThreads))
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, tasks, doTrimMatchStratify)
    ParallelLogger::stopCluster(cluster)
  }

  # Computing shared covariate balance ----------------------------------
  subset <- referenceTable[!duplicated(referenceTable$sharedBalanceFile), ]
  subset <- subset[subset$sharedBalanceFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$sharedBalanceFile)), ]
  if (nrow(subset) != 0) {
    message("*** Computing shared covariate balance ***")
    createSharedBalanceTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(
        cmAnalysisList,
        list(analysisId = refRow$analysisId)
      )[[1]]

      task <- list(
        cohortMethodDataFile = file.path(
          outputFolder,
          refRow$cohortMethodDataFile
        ),
        sharedPsFile = file.path(outputFolder, refRow$sharedPsFile),
        args = analysisRow,
        sharedBalanceFile = file.path(outputFolder, refRow$sharedBalanceFile)
      )
      return(task)
    }
    tasks <- lapply(1:nrow(subset), createSharedBalanceTask)
    cluster <- ParallelLogger::makeCluster(min(length(tasks), multiThreadingSettings$computeSharedBalanceThreads))
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, tasks, doComputeSharedBalance)
    ParallelLogger::stopCluster(cluster)
  }

  # Filtering covariates for computing covariate balance ------------------------------
  subset <- referenceTable[!duplicated(referenceTable$filteredForbalanceFile), ]
  subset <- subset[subset$filteredForbalanceFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$filteredForbalanceFile)), ]
  if (nrow(subset) != 0) {
    message("*** Filtering covariates for computing covariate balance ***")
    createFilterForCovariateBalanceTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(
        cmAnalysisList,
        list(analysisId = refRow$analysisId)
      )[[1]]

      computeCovariateBalanceArgs <- analysisRow$computeCovariateBalanceArgs
      task <- list(
        cohortMethodDataFile = file.path(outputFolder, refRow$cohortMethodDataFile),
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        filteredForbalanceFile = file.path(outputFolder, refRow$filteredForbalanceFile)
      )
      return(task)
    }
    tasks <- lapply(1:nrow(subset), createFilterForCovariateBalanceTask)
    cluster <- ParallelLogger::makeCluster(min(length(tasks), multiThreadingSettings$prefilterCovariatesThreads))
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, tasks, doFilterForCovariateBalance)
    ParallelLogger::stopCluster(cluster)
  }

  # Computing covariate balance (per outcome) -----------------------
  subset <- referenceTable[!duplicated(referenceTable$balanceFile), ]
  subset <- subset[subset$balanceFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$balanceFile)), ]
  if (nrow(subset) != 0) {
    message("*** Computing covariate balance (per outcome) ***")
    createBalanceTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(
        cmAnalysisList,
        list(analysisId = refRow$analysisId)
      )[[1]]
      computeCovariateBalanceArgs <- analysisRow$computeCovariateBalanceArgs
      if (refRow$strataFile == "") {
        strataFile = file.path(outputFolder, refRow$studyPopFile)
      } else {
        strataFile = file.path(outputFolder, refRow$strataFile)
      }
      task <- list(
        filteredForbalanceFile = ifelse(refRow$filteredForbalanceFile == "", "", file.path(outputFolder, refRow$filteredForbalanceFile)),
        cohortMethodDataFile = file.path(outputFolder, refRow$cohortMethodDataFile),
        strataFile = strataFile,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        balanceFile = file.path(outputFolder, refRow$balanceFile)
      )
      return(task)
    }
    tasks <- lapply(1:nrow(subset), createBalanceTask)
    cluster <- ParallelLogger::makeCluster(min(length(tasks), multiThreadingSettings$computeBalanceThreads))
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, tasks, doComputeBalance)
    ParallelLogger::stopCluster(cluster)
  }

  # Prefiltering covariates for outcome models  -------------------------
  subset <- referenceTable[!duplicated(referenceTable$prefilteredCovariatesFile), ]
  subset <- subset[subset$prefilteredCovariatesFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$prefilteredCovariatesFile)), ]
  if (nrow(subset) != 0) {
    message("*** Prefiltering covariates for outcome models ***")
    createPrefilterTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(
        cmAnalysisList,
        list(analysisId = refRow$analysisId)
      )[[1]]
      task <- list(
        cohortMethodDataFile = file.path(
          outputFolder,
          refRow$cohortMethodDataFile
        ),
        args = analysisRow$fitOutcomeModelArgs,
        prefilteredCovariatesFile = file.path(
          outputFolder,
          refRow$prefilteredCovariatesFile
        )
      )
      return(task)
    }
    tasks <- lapply(1:nrow(subset), createPrefilterTask)
    cluster <- ParallelLogger::makeCluster(min(length(tasks), multiThreadingSettings$prefilterCovariatesThreads))
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, tasks, doPrefilterCovariates)
    ParallelLogger::stopCluster(cluster)
  }

  # Fitting outcome models --------------------------
  subset <- referenceTable[referenceTable$outcomeOfInterest & referenceTable$outcomeModelFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$outcomeModelFile)), ]
  if (nrow(subset) != 0) {
    message("*** Fitting outcome models for outcomes of interest ***")

    createOutcomeModelTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(
        cmAnalysisList,
        list(analysisId = refRow$analysisId)
      )[[1]]
      args <- analysisRow$fitOutcomeModelArgs
      args$control$threads <- multiThreadingSettings$outcomeCvThreads
      if (refRow$strataFile != "") {
        studyPopFile <- refRow$strataFile
      } else if (refRow$psFile != "") {
        studyPopFile <- refRow$psFile
      } else {
        studyPopFile <- refRow$studyPopFile
      }
      prefilteredCovariatesFile <- refRow$prefilteredCovariatesFile
      if (prefilteredCovariatesFile != "") {
        prefilteredCovariatesFile <- file.path(outputFolder, refRow$prefilteredCovariatesFile)
      }
      return(list(
        cohortMethodDataFile = file.path(outputFolder, refRow$cohortMethodDataFile),
        prefilteredCovariatesFile = prefilteredCovariatesFile,
        args = args,
        studyPopFile = file.path(outputFolder, studyPopFile),
        outcomeModelFile = file.path(outputFolder, refRow$outcomeModelFile)
      ))
    }
    modelsToFit <- lapply(1:nrow(subset), createOutcomeModelTask)
    cluster <- ParallelLogger::makeCluster(min(length(modelsToFit), multiThreadingSettings$fitOutcomeModelThreads))
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, modelsToFit, doFitOutcomeModel)
    ParallelLogger::stopCluster(cluster)
  }

  subset <- referenceTable[!referenceTable$outcomeOfInterest & referenceTable$outcomeModelFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$outcomeModelFile)), ]
  if (nrow(subset) != 0) {
    message("*** Fitting outcome models for other outcomes ***")

    createArgs <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(
        cmAnalysisList,
        list(analysisId = refRow$analysisId)
      )[[1]]
      analysisRow$fitOutcomeModelArgs$control$threads <- multiThreadingSettings$outcomeCvThreads
      prefilteredCovariatesFile <- refRow$prefilteredCovariatesFile
      if (prefilteredCovariatesFile != "") {
        prefilteredCovariatesFile <- file.path(outputFolder, refRow$prefilteredCovariatesFile)
      }
      params <- list(
        cohortMethodDataFile = file.path(outputFolder, refRow$cohortMethodDataFile),
        prefilteredCovariatesFile = prefilteredCovariatesFile,
        psFile =  file.path(outputFolder, refRow$psFile),
        sharedPsFile = file.path(outputFolder, refRow$sharedPsFile),
        refitPsForEveryOutcome = cmAnalysesSpecifications$refitPsForEveryOutcome,
        outcomeId = refRow$outcomeId,
        args = analysisRow,
        outcomeModelFile = file.path(outputFolder, refRow$outcomeModelFile)
      )
      return(params)
    }
    modelsToFit <- lapply(1:nrow(subset), createArgs)
    cluster <- ParallelLogger::makeCluster(min(length(modelsToFit), multiThreadingSettings$fitOutcomeModelThreads))
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, modelsToFit, doFitOutcomeModelPlus)
    ParallelLogger::stopCluster(cluster)
  }
  # Summarize results ------------------------------------------------------------------------------
  mainFileName <- file.path(outputFolder, "resultsSummary.rds")
  interactionsFileName <- file.path(outputFolder, "interactionResultsSummary.rds")
  diagnosticsSummaryFileName <- file.path(outputFolder, "diagnosticsSummary.rds")
  if (!file.exists(mainFileName)) {
    message("*** Summarizing results ***")
    summarizeResults(
      referenceTable = referenceTable,
      outputFolder = outputFolder,
      mainFileName = mainFileName,
      interactionsFileName = interactionsFileName,
      diagnosticsSummaryFileName = diagnosticsSummaryFileName,
      calibrationThreads = multiThreadingSettings$calibrationThreads,
      cmDiagnosticThresholds = cmAnalysesSpecifications$cmDiagnosticThresholds
    )
  }
  referenceTable <- referenceTable |>
    select(-"includedCovariateConceptIds", "excludedCovariateConceptIds")
  invisible(referenceTable)
}

getCohortMethodData <- function(cohortMethodDataFile) {
  if (exists("cohortMethodData", envir = cache)) {
    cohortMethodData <- get("cohortMethodData", envir = cache)
  }
  if (!mget("cohortMethodDataFile", envir = cache, ifnotfound = "") == cohortMethodDataFile) {
    if (exists("cohortMethodData", envir = cache)) {
      Andromeda::close(cohortMethodData)
    }
    cohortMethodData <- loadCohortMethodData(cohortMethodDataFile)
    assign("cohortMethodData", cohortMethodData, envir = cache)
    assign("cohortMethodDataFile", cohortMethodDataFile, envir = cache)
  }
  return(cohortMethodData)
}

getPs <- function(psFile) {
  if (mget("psFile", envir = cache, ifnotfound = "") == psFile) {
    ps <- get("ps", envir = cache)
  } else {
    ps <- readRDS(psFile)
    columnsToKeep <- c("rowId", "treatment", "personSeqId", "cohortStartDate", "propensityScore", "preferenceScore", "iptw")
    if ("outcomeCount" %in% colnames(ps)) {
      columnsToKeep <- c(columnsToKeep, "outcomeCount", "timeAtRisk", "survivalTime")
    }
    ps <- ps[, columnsToKeep]
    assign("ps", ps, envir = cache)
    assign("psFile", psFile, envir = cache)
  }
  return(ps)
}

doCreateCmDataObject <- function(params) {
  ParallelLogger::logDebug(sprintf("Calling getDbCohortMethodData() for targetId %s, comparatorId %s",
                                   params$args$targetId,
                                   params$args$comparatorId))
  cohortMethodData <- do.call("getDbCohortMethodData", params$args)
  saveCohortMethodData(cohortMethodData, params$cohortMethodDataFile)
  return(NULL)
}

doCreateStudyPopObject <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFile)
  args <- list(
    cohortMethodData = cohortMethodData,
    outcomeId = params$outcomeId,
    createStudyPopulationArgs = params$createStudyPopulationArgs
  )
  ParallelLogger::logDebug(sprintf("Calling createStudyPopulation() using %s for outcomeId %s",
                                   params$cohortMethodDataFile,
                                   args$outcomeId))
  studyPop <- do.call("createStudyPopulation", args)
  if (!is.null(params$minimizeFileSizes) && params$minimizeFileSizes) {
    metaData <- attr(studyPop, "metaData")
    studyPop <- studyPop[, c("rowId", "treatment", "personSeqId", "outcomeCount", "timeAtRisk", "survivalTime")]
    attr(studyPop, "metaData") <- metaData
  }
  saveRDS(studyPop, params$studyPopFile)
  return(NULL)
}

doFitPsModel <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFile)
  studyPop <- readRDS(params$studyPopFile)
  args <- list(
    cohortMethodData = cohortMethodData,
    population = studyPop,
    createPsArgs = params$args
  )
  ParallelLogger::logDebug(sprintf("Calling createPs() using %s and %s",
                                   params$cohortMethodDataFile,
                                   params$studyPopFile))
  ps <- do.call("createPs", args)
  saveRDS(ps, params$psFile)
  return(NULL)
}

doFitSharedPsModel <- function(params, refitPsForEveryStudyPopulation) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFile)
  if (refitPsForEveryStudyPopulation) {
    args <- list(
      cohortMethodData = cohortMethodData,
      createStudyPopulationArgs = params$createStudyPopulationArgs
    )
    message("Fitting propensity model across all outcomes (ignore messages about 'no outcome specified')")
    ParallelLogger::logDebug(sprintf("Calling createPs() for shared PS using %s",
                                     params$cohortMethodDataFile))
    studyPop <- do.call("createStudyPopulation", args)
  } else {
    studyPop <- NULL
  }
  args <- list(
    cohortMethodData = cohortMethodData,
    population = studyPop,
    createPsArgs = params$args
  )
  ps <- do.call("createPs", args)
  saveRDS(ps, params$sharedPsFile)
  return(NULL)
}

addPsToStudyPopForSubset <- function(subset, outputFolder) {
  ps <- readRDS(file.path(outputFolder, subset$sharedPsFile[1]))

  addToStudyPop <- function(i) {
    refRow <- subset[i, ]
    studyPop <- readRDS(file.path(outputFolder, refRow$studyPopFile))
    studyPop <- addPsToStudyPopulation(studyPop, ps)
    saveRDS(studyPop, file.path(outputFolder, refRow$psFile))
    return(NULL)
  }
  plyr::l_ply(1:nrow(subset), addToStudyPop)
}

addPsToStudyPopulation <- function(studyPopulation, ps) {
  # Merge meta-data
  newMetaData <- attr(studyPopulation, "metaData")
  psMetaData <-  attr(ps, "metaData")
  missingColumns <- setdiff(names(psMetaData), names(newMetaData))
  newMetaData <- append(newMetaData, psMetaData[missingColumns])
  attr(studyPopulation, "metaData") <- newMetaData

  # Merge data
  missingColumns <- setdiff(names(ps), names(studyPopulation))
  idx <- match(studyPopulation$rowId, ps$rowId)
  studyPopulation <- bind_cols(
    studyPopulation,
    ps[idx, missingColumns]
  )
  return(studyPopulation)
}

applyTrimMatchStratify <- function(ps, arguments) {
  if (!is.null(arguments$trimByPsArgs)) {
    functionArgs <- list(
      population = ps,
      trimByPsArgs = arguments$trimByPsArgs
    )
    ps <- do.call("trimByPs", functionArgs)
  }
  if (!is.null(arguments$truncateIptwArgs)) {
    functionArgs <- list(
      population = ps,
      truncateIptwArgs = arguments$truncateIptwArgs
    )
    ps <- do.call("truncateIptw", functionArgs)
  }
  if (!is.null(arguments$matchOnPsArgs)) {
    functionArgs <- list(
      population = ps,
      matchOnPsArgs = arguments$matchOnPsArgs
    )
    ps <- do.call("matchOnPs", functionArgs)
  } else if (!is.null(arguments$stratifyByPsArgs)) {
    functionArgs <- list(
      population = ps,
      stratifyByPsArgs = arguments$stratifyByPsArgs
    )
    ps <- do.call("stratifyByPs", functionArgs)
  }
  return(ps)
}

doTrimMatchStratify <- function(params) {
  ps <- getPs(params$psFile)
  ParallelLogger::logDebug(sprintf("Performing matching etc., using %s",
                                   params$psFile))
  ps <- applyTrimMatchStratify(ps, params$args)
  saveRDS(ps, params$strataFile)
  return(NULL)
}

doPrefilterCovariates <- function(params) {
  cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFile)
  covariates <- cohortMethodData$covariates
  if (nrow_temp(covariates) > 0) {
    if (params$args$useCovariates) {
      covariatesToInclude <- params$args$includeCovariateIds
      covariatesToExclude <- params$args$excludeCovariateIds
    } else {
      covariatesToInclude <- c()
      covariatesToExclude <- c()
    }
    covariatesToInclude <- unique(c(covariatesToInclude, params$args$interactionCovariateIds))
    if (length(covariatesToInclude) != 0) {
      covariates <- covariates |>
        filter(.data$covariateId %in% covariatesToInclude)
    }
    if (length(covariatesToExclude) != 0) {
      covariates <- covariates |>
        filter(!.data$covariateId %in% covariatesToExclude)
    }
  }
  filteredCohortMethodData <- Andromeda::andromeda(
    cohorts = cohortMethodData$cohorts,
    outcomes = cohortMethodData$outcomes,
    covariates = covariates,
    covariateRef = cohortMethodData$covariateRef,
    analysisRef = cohortMethodData$analysisRef
  )
  attr(filteredCohortMethodData, "metaData") <- attr(cohortMethodData, "metaData")
  class(filteredCohortMethodData) <- "CohortMethodData"
  attr(class(filteredCohortMethodData), "package") <- "CohortMethod"
  saveCohortMethodData(filteredCohortMethodData, params$prefilteredCovariatesFile)
  return(NULL)
}

doFitOutcomeModel <- function(params) {
  if (params$prefilteredCovariatesFile == "") {
    cohortMethodDataFile <- params$cohortMethodDataFile
  } else {
    cohortMethodDataFile <- params$prefilteredCovariatesFile
  }
  cohortMethodData <- getCohortMethodData(cohortMethodDataFile)
  studyPop <- readRDS(params$studyPopFile)
  args <- list(
    cohortMethodData = cohortMethodData,
    population = studyPop,
    fitOutcomeModelArgs = params$args
  )
  ParallelLogger::logDebug(sprintf("Calling fitOutcomeModel() using %s and %s",
                                   cohortMethodDataFile,
                                   params$studyPopFile))
  outcomeModel <- do.call("fitOutcomeModel", args)
  saveRDS(outcomeModel, params$outcomeModelFile)
  return(NULL)
}

doFitOutcomeModelPlus <- function(params) {
  if (params$prefilteredCovariatesFile == "") {
    cohortMethodDataFile <- params$cohortMethodDataFile
  } else {
    cohortMethodDataFile <- params$prefilteredCovariatesFile
  }
  cohortMethodData <- getCohortMethodData(cohortMethodDataFile)

  ParallelLogger::logDebug(sprintf("Calling createStudyPopulation(), performing matching etc., and calling fitOutcomeModel() using %s for outcomeID %s",
                                   cohortMethodDataFile,
                                   params$outcomeId))

  # Create study pop
  args <- list(
    cohortMethodData = cohortMethodData,
    outcomeId = params$outcomeId,
    createStudyPopulationArgs = params$args$createStudyPopulationArgs
  )
  studyPop <- do.call("createStudyPopulation", args)

  if (!is.null(params$args$createPsArgs)) {
    if (params$refitPsForEveryOutcome) {
      ps <- getPs(params$psFile)
    } else {
      ps <- getPs(params$sharedPsFile)
      ps <- addPsToStudyPopulation(studyPop, ps)
    }
  } else {
    ps <- studyPop
  }
  rm(studyPop)
  ps <- applyTrimMatchStratify(ps, params$args)
  args <- list(
    population = ps,
    cohortMethodData = cohortMethodData,
    fitOutcomeModelArgs = params$args$fitOutcomeModelArgs
  )
  outcomeModel <- do.call("fitOutcomeModel", args)
  saveRDS(outcomeModel, params$outcomeModelFile)
  return(NULL)
}

doComputeSharedBalance <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFile)

  ParallelLogger::logDebug(sprintf("Computing shared balance using %s",
                                   params$cohortMethodDataFile))

  # Create study pop
  message("Computing covariate balance across all outcomes (ignore messages about 'no outcome specified')")
  args <- list(
    cohortMethodData = cohortMethodData,
    createStudyPopulationArgs = params$args$createStudyPopulationArgs
  )
  studyPop <- do.call("createStudyPopulation", args)

  if (!is.null(params$args$createPsArgs)) {
    # Add PS
    ps <- getPs(params$sharedPsFile)
    idx <- match(studyPop$rowId, ps$rowId)
    studyPop$propensityScore <- ps$propensityScore[idx]
    studyPop$iptw <- ps$iptw[idx]
    ps <- studyPop
  } else {
    ps <- studyPop
  }
  ps <- applyTrimMatchStratify(ps, params$args)
  args <- list(
    cohortMethodData = cohortMethodData,
    population = ps,
    computeCovariateBalanceArgs = params$args$computeSharedCovariateBalanceArgs
  )
  balance <- do.call("computeCovariateBalance", args)
  saveRDS(balance, params$sharedBalanceFile)
  return(NULL)
}

doFilterForCovariateBalance <- function(params) {
  cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFile)
  covariateFilter <- params$computeCovariateBalanceArgs$covariateFilter
  covariates <- filterCovariates(cohortMethodData$covariates, cohortMethodData$covariateRef, covariateFilter)
  filteredCohortMethodData <- Andromeda::andromeda(
    cohorts = cohortMethodData$cohorts,
    outcomes = cohortMethodData$outcomes,
    covariates = covariates,
    covariateRef = cohortMethodData$covariateRef,
    analysisRef = cohortMethodData$analysisRef
  )
  attr(filteredCohortMethodData, "metaData") <- attr(cohortMethodData, "metaData")
  class(filteredCohortMethodData) <- "CohortMethodData"
  attr(class(filteredCohortMethodData), "package") <- "CohortMethod"
  saveCohortMethodData(filteredCohortMethodData, params$filteredForbalanceFile)
  return(NULL)
}

doComputeBalance <- function(params) {
  if (params$filteredForbalanceFile == "") {
    cohortMethodData <- getCohortMethodData(params$cohortMethodDataFile)
  } else {
    cohortMethodData <- getCohortMethodData(params$filteredForbalanceFile)
  }
  strataPop <- readRDS(params$strataFile)

  args <- list(
    cohortMethodData = cohortMethodData,
    population = strataPop,
    computeCovariateBalanceArgs = params$computeCovariateBalanceArgs
  )
  ParallelLogger::logDebug(sprintf("Computing balance balance using %s and %s",
                                   params$cohortMethodDataFile,
                                   params$strataFile))
  balance <- do.call("computeCovariateBalance", args)
  saveRDS(balance, params$balanceFile)
  return(NULL)
}

createReferenceTable <- function(cmAnalysisList,
                                 targetComparatorOutcomesList,
                                 analysesToExclude,
                                 outputFolder,
                                 refitPsForEveryOutcome,
                                 refitPsForEveryStudyPopulation) {
  # Create all rows per target-comparator-outcome-analysis combination:
  convertAnalysisToTable <- function(analysis) {
    tibble(
      analysisId = analysis$analysisId,
      fitOutcomeModel = !is.null(analysis$fitOutcomeModelArgs),
      analysisFolder = sprintf("Analysis_%d", analysis$analysisId)
    )
  }
  analyses <- bind_rows(lapply(cmAnalysisList, convertAnalysisToTable))
  foldersToCreate <- file.path(outputFolder, analyses$analysisFolder)
  foldersToCreate <- foldersToCreate[!dir.exists(foldersToCreate)]
  sapply(foldersToCreate, dir.create)
  convertOutcomeToTable <- function(outcome) {
    table <- tibble(
      outcomeId = outcome$outcomeId,
      outcomeOfInterest = outcome$outcomeOfInterest,
      trueEffectSize = if (is.null(outcome$trueEffectSize)) as.numeric(NA) else outcome$trueEffectSize
    )
    return(table)
  }
  convertTcosToTable <- function(tcos) {
    table <- lapply(tcos$outcomes, convertOutcomeToTable) |>
      bind_rows() |>
      mutate(
        targetId = tcos$targetId,
        comparatorId = tcos$comparatorId,
        includedCovariateConceptIds = paste(tcos$includedCovariateConceptIds, collapse = ","),
        excludedCovariateConceptIds = paste(tcos$excludedCovariateConceptIds, collapse = ",")
      )
    return(table)
  }
  tcos <- bind_rows(lapply(targetComparatorOutcomesList, convertTcosToTable))
  referenceTable <- analyses |>
    cross_join(tcos)

  # Add cohort method data file names
  which.list <- function(list, object) {
    return(do.call("c", lapply(1:length(list), function(i, list, object) {
      if (identical(list[[i]], object)) {
        return(i)
      } else {
        return(c())
      }
    }, list, object)))
  }
  loadingArgsList <- unique(ParallelLogger::selectFromList(
    cmAnalysisList,
    "getDbCohortMethodDataArgs"
  ))
  loadingArgsList <- lapply(loadingArgsList, function(x) {
    return(x[[1]])
  })
  loadArgsId <- sapply(
    cmAnalysisList,
    function(cmAnalysis, loadingArgsList) {
      return(which.list(
        loadingArgsList,
        cmAnalysis$getDbCohortMethodDataArgs
      ))
    },
    loadingArgsList
  )
  analysisIdToLoadArgsId <- tibble(analysisId = analyses$analysisId, loadArgsId = loadArgsId)
  referenceTable <- inner_join(referenceTable, analysisIdToLoadArgsId, by = "analysisId")
  referenceTable$cohortMethodDataFile <- .createCohortMethodDataFileName(
    loadId = referenceTable$loadArgsId,
    targetId = referenceTable$targetId,
    comparatorId = referenceTable$comparatorId
  )

  # Add studypop filenames
  studyPopArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, "createStudyPopulationArgs"))
  studyPopArgsList <- lapply(studyPopArgsList, function(x) {
    return(x[[1]])
  })
  studyPopArgsId <- sapply(
    cmAnalysisList,
    function(cmAnalysis, studyPopArgsList) {
      return(which.list(
        studyPopArgsList,
        cmAnalysis$createStudyPopulationArgs
      ))
    },
    studyPopArgsList
  )
  analysisIdToStudyPopArgsId <- tibble(
    analysisId = analyses$analysisId,
    studyPopArgsId = studyPopArgsId
  )
  referenceTable <- inner_join(referenceTable, analysisIdToStudyPopArgsId, by = "analysisId")
  referenceTable$studyPopFile <- .createStudyPopulationFileName(
    loadId = referenceTable$loadArgsId,
    studyPopId = referenceTable$studyPopArgsId,
    targetId = referenceTable$targetId,
    comparatorId = referenceTable$comparatorId,
    outcomeId = referenceTable$outcomeId
  )

  # Add PS filenames
  psArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, "createPsArgs"))
  psArgsList <- lapply(
    psArgsList,
    function(x) {
      return(if (length(x) > 0) {
        return(x[[1]])
      } else {
        return(NULL)
      })
    }
  )
  noPsIds <- which(sapply(psArgsList, is.null))
  psArgsId <- sapply(
    cmAnalysisList,
    function(cmAnalysis,
             psArgsList) {
      return(which.list(psArgsList, cmAnalysis$createPsArgs))
    },
    psArgsList
  )
  analysisIdToPsArgsId <- tibble(analysisId = analyses$analysisId, psArgsId = psArgsId)
  referenceTable <- inner_join(referenceTable, analysisIdToPsArgsId, by = "analysisId")
  idx <- !(referenceTable$psArgsId %in% noPsIds)
  referenceTable$psFile <- ""
  referenceTable$psFile[idx] <- .createPsOutcomeFileName(
    loadId = referenceTable$loadArgsId[idx],
    studyPopId = referenceTable$studyPopArgsId[idx],
    psId = referenceTable$psArgsId[idx],
    targetId = referenceTable$targetId[idx],
    comparatorId = referenceTable$comparatorId[idx],
    outcomeId = referenceTable$outcomeId[idx]
  )
  referenceTable$sharedPsFile <- ""
  if (!refitPsForEveryOutcome) {
    if (refitPsForEveryStudyPopulation) {
      # Find equivalent studyPopArgs, so we can reuse PS over those as well:
      studyPopArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, "createStudyPopulationArgs"))
      studyPopArgsList <- lapply(studyPopArgsList, function(x) {
        return(x[[1]])
      })
      equivalent <- function(studyPopArgs1, studyPopArgs2) {
        if (identical(studyPopArgs1, studyPopArgs2)) {
          return(TRUE)
        }
        if (studyPopArgs1$firstExposureOnly != studyPopArgs2$firstExposureOnly ||
            studyPopArgs1$restrictToCommonPeriod != studyPopArgs2$restrictToCommonPeriod ||
            studyPopArgs1$washoutPeriod != studyPopArgs2$washoutPeriod ||
            studyPopArgs1$removeDuplicateSubjects != studyPopArgs2$removeDuplicateSubjects ||
            studyPopArgs1$minDaysAtRisk != studyPopArgs2$minDaysAtRisk ||
            studyPopArgs1$minDaysAtRisk != 0) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      }
      findFirstEquivalent <- function(studyPopArgsList, studyPopArgs) {
        for (i in 1:length(studyPopArgsList)) {
          if (equivalent(studyPopArgsList[[i]], studyPopArgs)) {
            return(i)
          }
        }
      }
      studyPopArgsEquivalentId <- sapply(
        cmAnalysisList,
        function(cmAnalysis, studyPopArgsList) {
          return(findFirstEquivalent(
            studyPopArgsList,
            cmAnalysis$createStudyPopulationArgs
          ))
        },
        studyPopArgsList
      )
      analysisIdToStudyPopArgsEquivalentId <- tibble(
        analysisId = analyses$analysisId,
        studyPopArgsEquivalentId = studyPopArgsEquivalentId
      )
      referenceTable <- inner_join(referenceTable, analysisIdToStudyPopArgsEquivalentId, by = "analysisId")
      referenceTable$sharedPsFile[idx] <- .createPsFileName(
        loadId = referenceTable$loadArgsId[idx],
        studyPopId = referenceTable$studyPopArgsEquivalentId[idx],
        psId = referenceTable$psArgsId[idx],
        targetId = referenceTable$targetId[idx],
        comparatorId = referenceTable$comparatorId[idx]
      )
    } else {
      # One propensity model across all study population settings:
      referenceTable$sharedPsFile[idx] <- .createPsFileName(
        loadId = referenceTable$loadArgsId[idx],
        studyPopId = NULL,
        psId = referenceTable$psArgsId[idx],
        targetId = referenceTable$targetId[idx],
        comparatorId = referenceTable$comparatorId[idx]
      )
    }
  }

  # Add strata filenames
  strataArgNames <- c(
    "trimByPsArgs",
    "truncateIptwArgs",
    "matchOnPsArgs",
    "stratifyByPsArgs"
  )
  strataArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, strataArgNames))
  strataArgsList <- strataArgsList[sapply(
    strataArgsList,
    function(strataArgs) {
      return(!is.null(strataArgs$trimByPsArgs) |
               !is.null(strataArgs$truncateIptwArgs) |
               !is.null(strataArgs$matchOnPsArgs) |
               !is.null(strataArgs$stratifyByPsArgs))
    }
  )]
  if (length(strataArgsList) == 0) {
    referenceTable$strataArgsId <- 0
  } else {
    strataArgsId <- sapply(ParallelLogger::selectFromList(cmAnalysisList, strataArgNames),
                           function(cmAnalysis) {
                             i <- which.list(strataArgsList, cmAnalysis)
                             if (is.null(i)) {
                               i <- 0
                             }
                             return(i)
                           })
    analysisIdToStrataArgsId <- tibble(analysisId = analyses$analysisId, strataArgsId = strataArgsId)
    referenceTable <- inner_join(referenceTable, analysisIdToStrataArgsId, by = "analysisId")
  }
  idx <- referenceTable$strataArgsId != 0
  referenceTable$strataFile <- ""
  referenceTable$strataFile[idx] <- .createStratifiedPopFileName(
    loadId = referenceTable$loadArgsId[idx],
    studyPopId = referenceTable$studyPopArgsId[idx],
    psId = referenceTable$psArgsId[idx],
    strataId = referenceTable$strataArgsId[idx],
    targetId = referenceTable$targetId[idx],
    comparatorId = referenceTable$comparatorId[idx],
    outcomeId = referenceTable$outcomeId[idx]
  )

  # Add shared covariate balance files (per target-comparator-analysis)
  balanceArgName <- "computeSharedCovariateBalanceArgs"
  if (refitPsForEveryOutcome) {
    referenceTable$sharedBalanceFile <- ""
  } else {
    balanceArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, balanceArgName))
    balanceArgsList <- balanceArgsList[sapply(
      balanceArgsList,
      function(balanceArgs) {
        return(!is.null(balanceArgs$computeSharedCovariateBalanceArgs))
      }
    )]
    if (length(balanceArgsList) == 0) {
      referenceTable$sharedBalanceId <- 0
    } else {
      sharedBalanceId <- sapply(ParallelLogger::selectFromList(cmAnalysisList, balanceArgName),
                                function(cmAnalysis) {
                                  i <- which.list(balanceArgsList, cmAnalysis)
                                  if (is.null(i)) {
                                    i <- 0
                                  }
                                  return(i)
                                })
      analysisIdToBalanceArgsId <- tibble(analysisId = analyses$analysisId, sharedBalanceId = sharedBalanceId)
      referenceTable <- inner_join(referenceTable, analysisIdToBalanceArgsId, by = "analysisId")
    }
    idx <- referenceTable$sharedBalanceId != 0
    referenceTable$sharedBalanceFile <- ""
    referenceTable$sharedBalanceFile[idx] <- .createsharedBalanceFileName(
      loadId = referenceTable$loadArgsId[idx],
      studyPopId = referenceTable$studyPopArgsId[idx],
      psId = referenceTable$psArgsId[idx],
      strataId = referenceTable$strataArgsId[idx],
      sharedBalanceId = referenceTable$sharedBalanceId[idx],
      targetId = referenceTable$targetId[idx],
      comparatorId = referenceTable$comparatorId[idx]
    )
  }

  # Add covariate balance files (per target-comparator-analysis-outcome)
  balanceArgName <- "computeCovariateBalanceArgs"
  balanceArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, balanceArgName))
  balanceArgsList <- balanceArgsList[sapply(
    balanceArgsList,
    function(balanceArgs) {
      return(!is.null(balanceArgs$computeCovariateBalanceArgs))
    }
  )]
  if (length(balanceArgsList) == 0) {
    referenceTable$balanceId <- 0
    balanceIdsRequiringFiltering <- c()
  } else {
    balanceId <- sapply(ParallelLogger::selectFromList(cmAnalysisList, balanceArgName),
                        function(cmAnalysis) {
                          i <- which.list(balanceArgsList, cmAnalysis)
                          if (is.null(i)) {
                            i <- 0
                          }
                          return(i)
                        })
    analysisIdToBalanceArgsId <- tibble(analysisId = analyses$analysisId, balanceId = balanceId)
    referenceTable <- inner_join(referenceTable, analysisIdToBalanceArgsId, by = "analysisId")

    balanceIdsRequiringFiltering <- sapply(
      1:length(balanceArgsList),
      function(i) ifelse(is.null(balanceArgsList[[i]]$computeCovariateBalanceArgs$covariateFilter), NA, i)
    )
  }
  idx <- referenceTable$balanceId %in% balanceIdsRequiringFiltering
  referenceTable$filteredForbalanceFile <- ""
  referenceTable$filteredForbalanceFile[idx] <- .createFilterForBalanceFileName(
    loadId = referenceTable$loadArgsId[idx],
    studyPopId = referenceTable$studyPopArgsId[idx],
    psId = referenceTable$psArgsId[idx],
    strataId = referenceTable$strataArgsId[idx],
    balanceId = referenceTable$balanceId[idx],
    targetId = referenceTable$targetId[idx],
    comparatorId = referenceTable$comparatorId[idx]
  )
  idx <- referenceTable$balanceId != 0
  referenceTable$balanceFile <- ""
  referenceTable$balanceFile[idx] <- .createBalanceFileName(
    loadId = referenceTable$loadArgsId[idx],
    studyPopId = referenceTable$studyPopArgsId[idx],
    psId = referenceTable$psArgsId[idx],
    strataId = referenceTable$strataArgsId[idx],
    balanceId = referenceTable$balanceId[idx],
    targetId = referenceTable$targetId[idx],
    comparatorId = referenceTable$comparatorId[idx],
    outcomeId = referenceTable$outcomeId[idx]
  )

  # Add prefiltered covariate files
  loadingFittingArgsList <- unique(ParallelLogger::selectFromList(
    cmAnalysisList,
    c("getDbCohortMethodDataArgs", "fitOutcomeModelArgs")
  ))
  needsFilter <- function(loadingFittingArgs) {
    if (!"fitOutcomeModelArgs" %in% names(loadingFittingArgs)) {
      return(NULL)
    }
    keep <- (loadingFittingArgs$fitOutcomeModelArgs$useCovariates & (length(loadingFittingArgs$fitOutcomeModelArgs$excludeCovariateIds) != 0 |
                                                                       length(loadingFittingArgs$fitOutcomeModelArgs$includeCovariateIds) != 0)) |
      length(loadingFittingArgs$fitOutcomeModelArgs$interactionCovariateIds) != 0
    if (keep) {
      loadingFittingArgs$relevantFields <- list(
        useCovariates = loadingFittingArgs$fitOutcomeModelArgs$useCovariates,
        excludeCovariateIds = loadingFittingArgs$fitOutcomeModelArgs$excludeCovariateIds,
        includeCovariateIds = loadingFittingArgs$fitOutcomeModelArgs$includeCovariateIds,
        interactionCovariateIds = loadingFittingArgs$fitOutcomeModelArgs$interactionCovariateIds
      )
      return(loadingFittingArgs)
    } else {
      return(NULL)
    }
  }
  loadingFittingArgsList <- plyr::compact(lapply(loadingFittingArgsList, needsFilter))
  referenceTable$prefilteredCovariatesFile <- ""
  if (length(loadingFittingArgsList) != 0) {
    # Filtering needed
    relevantArgsList <- ParallelLogger::selectFromList(
      loadingFittingArgsList,
      c("getDbCohortMethodDataArgs", "relevantFields")
    )
    uniqueRelevantArgsList <- unique(relevantArgsList)
    prefilterIds <- sapply(
      relevantArgsList,
      function(relevantArgs, uniqueRelevantArgsList) {
        return(which.list(
          uniqueRelevantArgsList,
          relevantArgs
        ))
      },
      uniqueRelevantArgsList
    )
    matchableArgsList <- ParallelLogger::selectFromList(
      loadingFittingArgsList,
      c("getDbCohortMethodDataArgs", "fitOutcomeModelArgs")
    )

    matchingIds <- sapply(
      ParallelLogger::selectFromList(
        cmAnalysisList,
        c("getDbCohortMethodDataArgs", "fitOutcomeModelArgs")
      ),
      function(cmAnalysis, matchableArgs) {
        return(which.list(
          matchableArgs,
          cmAnalysis
        ))
      },
      matchableArgsList
    )
    analysisIdToPrefilterId <- tibble(
      analysisId = analyses$analysisId,
      prefilterId = sapply(matchingIds, function(matchingId, prefilterIds) if (is.null(matchingId)) -1 else prefilterIds[matchingId], prefilterIds)
    )
    referenceTable <- inner_join(referenceTable, analysisIdToPrefilterId, by = "analysisId")
    referenceTable$prefilteredCovariatesFile <- .createPrefilteredCovariatesFileName(
      loadId = referenceTable$loadArgsId,
      targetId = referenceTable$targetId,
      comparatorId = referenceTable$comparatorId,
      prefilterId = referenceTable$prefilterId
    )
  }

  # Add outcome model file names
  referenceTable <- referenceTable |>
    mutate(outcomeModelFile = ifelse(.data$fitOutcomeModel,
                                     .createOutcomeModelFileName(
                                       folder = .data$analysisFolder,
                                       targetId = .data$targetId,
                                       comparatorId = .data$comparatorId,
                                       outcomeId = .data$outcomeId
                                     ),
                                     ""
    ))

  # Some cleanup:
  referenceTable <- referenceTable[, c(
    "analysisId",
    "targetId",
    "comparatorId",
    "outcomeId",
    "includedCovariateConceptIds",
    "excludedCovariateConceptIds",
    "outcomeOfInterest",
    "trueEffectSize",
    "cohortMethodDataFile",
    "studyPopFile",
    "sharedPsFile",
    "psFile",
    "strataFile",
    "sharedBalanceFile",
    "filteredForbalanceFile",
    "balanceFile",
    "prefilteredCovariatesFile",
    "outcomeModelFile"
  )]
  referenceTable <- referenceTable[order(
    referenceTable$analysisId,
    referenceTable$targetId,
    referenceTable$comparatorId,
    referenceTable$outcomeId
  ), ]

  # Remove non-essential files for outcomes not of interest:
  idx <- !referenceTable$outcomeOfInterest
  referenceTable$strataFile[idx] <- ""
  referenceTable$filteredForbalanceFile[idx] <- ""
  referenceTable$balanceFile[idx] <- ""
  if (!refitPsForEveryOutcome) {
    # If we're computing a PS per outcome it probably is a good idea to save it.
    # For that it is more convenient to save the study population as well (so we can
    # use the regular PS-model fitting routine)
    referenceTable$studyPopFile[idx] <- ""
    referenceTable$psFile[idx] <- ""
  }

  # Remove rows that the user specified to exclude:
  if (!is.null(analysesToExclude)) {
    matchingColumns <- colnames(analysesToExclude)[colnames(analysesToExclude) %in% c("targetId", "comparatorId", "outcomeId", "analysisId")]
    if (length(matchingColumns) == 0) {
      stop("The 'analysesToExclude' argument should contain columns 'targetId', 'comparatorId', 'outcomeId', or 'analysisId'.")
    }
    analysesToExclude <- analysesToExclude[, matchingColumns]
    countBefore <- nrow(referenceTable)
    referenceTable <- referenceTable |>
      anti_join(analysesToExclude, by = matchingColumns)
    countAfter <- nrow(referenceTable)
    message(sprintf(
      "Removed %d of the %d target-comparator-outcome-analysis combinations as specified by the user.",
      countBefore - countAfter,
      countBefore
    ))
  }
  return(referenceTable)
}

.f <- function(x) {
  return(format(x, scientific = FALSE, trim = TRUE))
}

.createCohortMethodDataFileName <- function(loadId, targetId, comparatorId) {
  name <- sprintf("CmData_l%s_t%s_c%s.zip", loadId, .f(targetId), .f(comparatorId))
  return(name)
}


.createPrefilteredCovariatesFileName <- function(loadId, targetId, comparatorId, prefilterId) {
  name <- sprintf("Prefilter_l%s_t%s_c%s_p%s.zip", loadId, .f(targetId), .f(comparatorId), prefilterId)
  name[prefilterId == -1] <- rep("", sum(prefilterId == -1))
  return(name)
}

.createStudyPopulationFileName <- function(loadId,
                                           studyPopId,
                                           targetId,
                                           comparatorId,
                                           outcomeId) {
  name <- sprintf("StudyPop_l%s_s%s_t%s_c%s_o%s.rds", loadId, studyPopId, .f(targetId), .f(comparatorId), .f(outcomeId))
  return(name)
}

.createPsFileName <- function(loadId, studyPopId, psId, targetId, comparatorId) {
  if (is.null(studyPopId)) {
    name <- sprintf("Ps_l%s_p%s_t%s_c%s.rds", loadId, psId, .f(targetId), .f(comparatorId))
  } else {
    name <- sprintf("Ps_l%s_s%s_p%s_t%s_c%s.rds", loadId, studyPopId, psId, .f(targetId), .f(comparatorId))
  }
  return(name)
}

.createPsOutcomeFileName <- function(loadId,
                                     studyPopId,
                                     psId,
                                     targetId,
                                     comparatorId,
                                     outcomeId) {
  name <- sprintf("Ps_l%s_s%s_p%s_t%s_c%s_o%s.rds", loadId, studyPopId, psId, .f(targetId), .f(comparatorId), .f(outcomeId))
  return(name)
}

.createStratifiedPopFileName <- function(loadId,
                                         studyPopId,
                                         psId,
                                         strataId,
                                         targetId,
                                         comparatorId,
                                         outcomeId) {
  name <- sprintf("StratPop_l%s_s%s_p%s_t%s_c%s_s%s_o%s.rds", loadId, studyPopId, psId, .f(targetId), .f(comparatorId), strataId, .f(outcomeId))
  return(name)
}

.createsharedBalanceFileName <- function(loadId,
                                         studyPopId,
                                         psId,
                                         strataId,
                                         sharedBalanceId,
                                         targetId,
                                         comparatorId) {
  name <- sprintf("Balance_l%s_s%s_p%s_t%s_c%s_s%s_b%s.rds", loadId, studyPopId, psId, .f(targetId), .f(comparatorId), strataId, sharedBalanceId)
  return(name)
}

.createFilterForBalanceFileName <- function(loadId,
                                            studyPopId,
                                            psId,
                                            strataId,
                                            balanceId,
                                            targetId,
                                            comparatorId) {
  name <- sprintf("FilterForBalance_l%s_s%s_p%s_t%s_c%s_s%s_b%s.zip", loadId, studyPopId, psId, .f(targetId), .f(comparatorId), strataId, balanceId)
  return(name)
}

.createBalanceFileName <- function(loadId,
                                   studyPopId,
                                   psId,
                                   strataId,
                                   balanceId,
                                   targetId,
                                   comparatorId,
                                   outcomeId) {
  name <- sprintf("Balance_l%s_s%s_p%s_t%s_c%s_s%s_b%s_o%s.rds", loadId, studyPopId, psId, .f(targetId), .f(comparatorId), strataId, balanceId, .f(outcomeId))
  return(name)
}

.createOutcomeModelFileName <- function(folder, targetId, comparatorId, outcomeId) {
  name <- sprintf("om_t%s_c%s_o%s.rds", .f(targetId), .f(comparatorId), .f(outcomeId))
  return(file.path(folder, name))
}

#' Get file reference
#'
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @return
#' A tibble containing file names of artifacts generated for each target-comparator-outcome-analysis combination.
#'
#' @export
getFileReference <- function(outputFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  outputFolder <- normalizePath(outputFolder)
  omr <- readRDS(file.path(outputFolder, "outcomeModelReference.rds"))
  return(omr)
}

#' Get a summary report of the analyses results
#'
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @return
#' A tibble containing summary statistics for each target-comparator-outcome-analysis combination.
#'
#' @export
getResultsSummary <- function(outputFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  outputFolder <- normalizePath(outputFolder)
  results <- readRDS(file.path(outputFolder, "resultsSummary.rds"))
  return(results)
}

#' Get a summary report of the analyses diagnostics
#'
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @return
#' A tibble containing summary diagnostics for each outcome-covariate-analysis combination.
#'
#' @export
getDiagnosticsSummary <- function(outputFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  outputFolder <- normalizePath(outputFolder)
  results <- readRDS(file.path(outputFolder, "diagnosticsSummary.rds"))
  return(results)
}

#' Get a summary report of the analyses results
#'
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @return
#' A tibble containing summary statistics for each target-comparator-outcome-analysis combination.
#'
#' @export
getInteractionResultsSummary <- function(outputFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  outputFolder <- normalizePath(outputFolder)
  results <- readRDS(file.path(outputFolder, "interactionResultsSummary.rds"))
  return(results)
}

summarizeResults <- function(referenceTable,
                             outputFolder,
                             mainFileName,
                             interactionsFileName,
                             diagnosticsSummaryFileName,
                             calibrationThreads,
                             cmDiagnosticThresholds) {
  subset <- referenceTable |>
    filter(.data$outcomeModelFile != "")
  subset <- addMaxSdm(subset, outputFolder)
  subset <- addEquipoise(subset, outputFolder)
  results <- vector("list", nrow(subset))
  interActionResults <- list()
  pb <- txtProgressBar(style = 3)
  for (i in seq_len(nrow(subset))) {
    outcomeModel <- readRDS(file.path(outputFolder, subset$outcomeModelFile[i]))
    coefficient <- as.vector(coef(outcomeModel))
    ci <- confint(outcomeModel)
    if (is.null(coefficient)) {
      p <- NA
      oneSidedP <- NA
    } else {
      p <- EmpiricalCalibration::computeTraditionalP(logRr = coefficient,
                                                     seLogRr = outcomeModel$outcomeModelTreatmentEstimate$seLogRr)
      oneSidedP <- EmpiricalCalibration::computeTraditionalP(logRr = coefficient,
                                                             seLogRr = outcomeModel$outcomeModelTreatmentEstimate$seLogRr,
                                                             twoSided = FALSE,
                                                             upper = TRUE)
    }
    totalSubjects <- outcomeModel$populationCounts$targetExposures + outcomeModel$populationCounts$comparatorExposures
    totalEvents <- outcomeModel$outcomeCounts$targetOutcomes + outcomeModel$outcomeCounts$comparatorOutcomes
    pTarget <- outcomeModel$populationCounts$targetExposures / totalSubjects
    mdrr <- CohortMethod:::computeMdrrFromAggregateStats(
      pTarget = pTarget,
      totalEvents = totalEvents,
      totalSubjects = totalSubjects,
      modelType = outcomeModel$outcomeModelType
    )
    row <- subset[i, ] |>
      select(
        "analysisId",
        "targetId",
        "comparatorId",
        "outcomeId",
        "trueEffectSize",
        "maxSdm",
        "sharedMaxSdm",
        "maxTargetSdm",
        "maxComparatorSdm",
        "maxTargetComparatorSdm",
        "equipoise"
      ) |>
      bind_cols(outcomeModel$populationCounts |>
                  select(
                    targetSubjects = "targetPersons",
                    comparatorSubjects = "comparatorPersons"
                  )) |>
      bind_cols(outcomeModel$timeAtRisk |>
                  select(
                    "targetDays",
                    "comparatorDays"
                  )) |>
      bind_cols(outcomeModel$outcomeCounts |>
                  select("targetOutcomes", "comparatorOutcomes"))
    row <- row |>
      mutate(
        rr = if (is.null(coefficient)) NA else exp(coefficient),
        ci95Lb = if (is.null(coefficient)) NA else exp(ci[1]),
        ci95Ub = if (is.null(coefficient)) NA else exp(ci[2]),
        p = !!p,
        oneSidedP = !!oneSidedP,
        logRr = if (is.null(coefficient)) NA else coefficient,
        seLogRr = if (is.null(coefficient)) NA else outcomeModel$outcomeModelTreatmentEstimate$seLogRr,
        llr = if (is.null(coefficient)) NA else outcomeModel$outcomeModelTreatmentEstimate$llr,
        mdrr = !!mdrr,
        targetEstimator = outcomeModel$targetEstimator
      ) |>
      mutate(generalizabilityMaxSdm = if_else(.data$targetEstimator == "att",
                                              .data$maxTargetSdm,
                                              if_else(.data$targetEstimator == "atu",
                                                      .data$maxComparatorSdm,
                                                      .data$maxTargetComparatorSdm)))
    row <- row |>
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
      ))
    results[[i]] <- row

    if (!is.null(outcomeModel$outcomeModelInteractionEstimates)) {
      for (j in seq_len(nrow(outcomeModel$outcomeModelInteractionEstimates))) {
        z <- outcomeModel$outcomeModelInteractionEstimates$logRr[j] / outcomeModel$outcomeModelInteractionEstimates$seLogRr[j]
        p <- 2 * pmin(pnorm(z), 1 - pnorm(z))
        interActionResults[[length(interActionResults) + 1]] <- row |>
          mutate(
            interactionCovariateId = outcomeModel$outcomeModelInteractionEstimates$covariateId[j],
            rr = exp(outcomeModel$outcomeModelInteractionEstimates$logRr[j]),
            ci95Lb = exp(outcomeModel$outcomeModelInteractionEstimates$logLb95[j]),
            ci95Ub = exp(outcomeModel$outcomeModelInteractionEstimates$logUb95[j]),
            p = !!p,
            logRr = outcomeModel$outcomeModelInteractionEstimates$logRr[j],
            seLogRr = outcomeModel$outcomeModelInteractionEstimates$seLogRr[j],
            targetEstimator = outcomeModel$targetEstimator
          )
      }
    }
    setTxtProgressBar(pb, i / nrow(subset))
  }
  close(pb)
  results <- bind_rows(results)
  results <- calibrateEstimates(
    results = results,
    calibrationThreads = calibrationThreads
  )
  resultsSummary <- results |>
    select("analysisId",
           "targetId",
           "comparatorId",
           "outcomeId",
           "targetSubjects",
           "comparatorSubjects",
           "targetDays",
           "comparatorDays",
           "targetOutcomes",
           "comparatorOutcomes",
           "rr",
           "ci95Lb",
           "ci95Ub",
           "p",
           "oneSidedP",
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
           "targetEstimator")


  diagnosticsSummary <- results |>
    mutate(easeDiagnostic = case_when(
      is.na(.data$ease) ~ "NOT EVALUATED",
      abs(.data$ease) < cmDiagnosticThresholds$easeThreshold ~ "PASS",
      TRUE ~ "FAIL"
    )) |>
    mutate(unblindForEvidenceSynthesis = .data$unblindForCalibration & .data$easeDiagnostic != "FAIL") |>
    mutate(unblind = .data$unblindForEvidenceSynthesis & .data$mdrrDiagnostic != "FAIL") |>
    select("analysisId",
           "targetId",
           "comparatorId",
           "outcomeId",
           "maxSdm",
           "balanceDiagnostic",
           "sharedMaxSdm",
           "sharedBalanceDiagnostic",
           "equipoise",
           "equipoiseDiagnostic",
           "generalizabilityMaxSdm",
           "generalizabilityDiagnostic",
           "mdrr",
           "mdrrDiagnostic",
           "ease",
           "easeDiagnostic",
           "unblindForEvidenceSynthesis",
           "unblind")

  interActionResults <- bind_rows(interActionResults)
  if (nrow(interActionResults) > 0) {
    interActionResults <- calibrateEstimates(
      results = interActionResults,
      calibrationThreads = calibrationThreads,
      interactions = TRUE
    )
    interActionResults <- interActionResults |>
      select("analysisId",
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
             "calibratedOneSidedP",
             "calibratedLogRr",
             "calibratedSeLogRr",
             "targetEstimator")
  }
  saveRDS(interActionResults, interactionsFileName)
  saveRDS(diagnosticsSummary, diagnosticsSummaryFileName)
  saveRDS(resultsSummary, mainFileName)
}

addMaxSdm <- function(referenceTable, outputFolder) {
  maxOrNa <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(as.numeric(NA))
    } else {
      return(as.numeric(max(x)))
    }
  }

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
                    maxSdm = maxOrNa(abs(balance$afterMatchingStdDiff)),
                    maxTargetSdm = maxOrNa(abs(balance$targetStdDiff)),
                    maxComparatorSdm = maxOrNa(abs(balance$comparatorStdDiff)),
                    maxTargetComparatorSdm = maxOrNa(abs(balance$targetComparatorStdDiff)))
      return(row)
    }
  }

  balanceFiles <- referenceTable |>
    filter(.data$balanceFile != "") |>
    distinct(.data$balanceFile) |>
    pull()
  if (length(balanceFiles) == 0) {
    maxSdm <- tibble(balanceFile = "NA", maxSdm = NA)
  } else {
    maxSdm <- bind_rows(lapply(balanceFiles, getMaxSdms)) |>
      select("balanceFile", "maxSdm")
  }
  sharedBalanceFiles <- referenceTable |>
    filter(.data$sharedBalanceFile != "") |>
    distinct(.data$sharedBalanceFile) |>
    pull()
  if (length(sharedBalanceFiles) == 0) {
    sharedMaxSdm <- tibble(sharedBalanceFile = "NA",
                           sharedMaxSdm = NA,
                           maxTargetSdm = NA,
                           maxComparatorSdm = NA,
                           maxTargetComparatorSdm = NA)
  } else {
    sharedMaxSdm <- bind_rows(lapply(sharedBalanceFiles, getMaxSdms)) |>
      rename(sharedBalanceFile = "balanceFile",
             sharedMaxSdm = "maxSdm")
  }
  referenceTable <- referenceTable |>
    left_join(maxSdm,by = join_by(balanceFile)) |>
    left_join(sharedMaxSdm, by = join_by(sharedBalanceFile))
  return(referenceTable)
}

addEquipoise <- function(referenceTable, outputFolder) {
  getEquipoise <- function(psFile) {
    ps <- readRDS(file.path(outputFolder, psFile))
    row <- tibble(psFile = !!psFile,
                  equipoise = computeEquipoise(ps))
    return(row)
  }
  sharedPsFiles <- referenceTable |>
    filter(.data$sharedPsFile != "") |>
    distinct(.data$sharedPsFile) |>
    pull()
  if (length(sharedPsFiles) > 0) {
    sharedEquipoise <- bind_rows(lapply(sharedPsFiles, getEquipoise)) |>
      rename(sharedPsFile = "psFile")
    referenceTable <- referenceTable |>
      left_join(sharedEquipoise, by = join_by(sharedPsFile))
  } else {
    # Maybe fitted PS per outcome, so therefore no shared PS files?
    psFiles <- referenceTable |>
      filter(.data$sharedPsFile == "" & .data$psFile != "") |>
      distinct(.data$psFile) |>
      pull()
    equipoise <- bind_rows(lapply(psFiles, getEquipoise))
    referenceTable <- referenceTable |>
      left_join(equipoise, by = join_by(psFile))
  }
  return(referenceTable)
}


calibrateEstimates <- function(results, calibrationThreads, interactions = FALSE) {
  if (nrow(results) == 0) {
    return(results)
  }
  results <- results |>
    mutate(unblindForCalibration = .data$balanceDiagnostic != "FAIL" &
             .data$sharedBalanceDiagnostic != "FAIL" &
             .data$equipoiseDiagnostic != "FAIL" &
             .data$generalizabilityDiagnostic != "FAIL")
  if (interactions) {
    message("Calibrating estimates for interactions")
    groups <- split(results, paste(results$targetId, results$comparatorId, results$analysisId, results$interactionCovariateId))
  } else {
    message("Calibrating estimates")
    groups <- split(results, paste(results$targetId, results$comparatorId, results$analysisId))
  }
  cluster <- ParallelLogger::makeCluster(min(length(groups), calibrationThreads))
  results <- ParallelLogger::clusterApply(cluster, groups, calibrateGroup)
  ParallelLogger::stopCluster(cluster)
  results <- bind_rows(results)
  return(results)
}

# group = groups[[1]]
calibrateGroup <- function(group) {
  ncs <- group[!is.na(group$trueEffectSize) & group$trueEffectSize == 1 & !is.na(group$seLogRr) & group$unblindForCalibration, ]
  pcs <- group[!is.na(group$trueEffectSize) & group$trueEffectSize != 1 & !is.na(group$seLogRr) & group$unblindForCalibration, ]
  if (nrow(ncs) >= 5) {
    null <- EmpiricalCalibration::fitMcmcNull(logRr = ncs$logRr, seLogRr = ncs$seLogRr)
    ease <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(null)
    calibratedP <- EmpiricalCalibration::calibrateP(null = null,
                                                    logRr = group$logRr,
                                                    seLogRr = group$seLogRr)
    calibratedOneSidedP <- EmpiricalCalibration::calibrateP(null = null,
                                                            logRr = group$logRr,
                                                            seLogRr = group$seLogRr,
                                                            twoSided = FALSE,
                                                            upper = TRUE)
    if (nrow(pcs) >= 5) {
      model <- EmpiricalCalibration::fitSystematicErrorModel(
        logRr = c(ncs$logRr, pcs$logRr),
        seLogRr = c(ncs$seLogRr, pcs$seLogRr),
        trueLogRr = log(c(ncs$trueEffectSize, pcs$trueEffectSize)),
        estimateCovarianceMatrix = FALSE
      )
    } else {
      model <- EmpiricalCalibration::convertNullToErrorModel(null)
    }
    calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(model = model, logRr = group$logRr, seLogRr = group$seLogRr)
    group$calibratedRr <- exp(calibratedCi$logRr)
    group$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
    group$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
    group$calibratedP <- calibratedP$p
    group$calibratedOneSidedP <- calibratedOneSidedP$p
    group$calibratedLogRr <- calibratedCi$logRr
    group$calibratedSeLogRr <- calibratedCi$seLogRr
    group$ease <- ease$ease
  } else {
    group$calibratedRr <- NA
    group$calibratedCi95Lb <- NA
    group$calibratedCi95Ub <- NA
    group$calibratedP <- NA
    group$calibratedOneSidedP <- NA
    group$calibratedLogRr <- NA
    group$calibratedSeLogRr <- NA
    group$ease <- NA
  }
  return(group)
}

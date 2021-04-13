# Copyright 2021 Observational Health Data Sciences and Informatics
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
#' @param connectionDetails              An R object of type `connectionDetails` created using the
#'                                     [DatabaseConnector::createConnectionDetails()] function.
#' @param cdmDatabaseSchema              The name of the database schema that contains the OMOP CDM
#'                                       instance. Requires read permissions to this database. On SQL
#'                                       Server, this should specify both the database and the schema,
#'                                       so for example 'cdm_instance.dbo'.
#' @param cdmVersion                     Define the OMOP CDM version used: currently support "4" and
#'                                       "5".
#' @param oracleTempSchema    DEPRECATED: use `tempEmulationSchema` instead.
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
#' @param cmAnalysisList                 A list of objects of type `cmAnalysis` as created using
#'                                       the `[createCmAnalysis] function.
#' @param targetComparatorOutcomesList   A list of objects of type `targetComparatorOutcomes` as
#'                                       created using the [createTargetComparatorOutcomes]
#'                                       function.
#' @param refitPsForEveryOutcome         Should the propensity model be fitted for every outcome (i.e.
#'                                       after people who already had the outcome are removed)? If
#'                                       false, a single propensity model will be fitted, and people
#'                                       who had the outcome previously will be removed afterwards.
#' @param refitPsForEveryStudyPopulation Should the propensity model be fitted for every study population
#'                                       definition? If false, a single propensity model will be fitted,
#'                                       and the study population criteria will be applied afterwards.
#' @param prefilterCovariates            If TRUE, and some outcome models require filtering covariates
#'                                       by concept ID (e.g. because `includeCovariateIds` or
#'                                       `interactionCovariateIds` is specified), this filtering
#'                                       will be done once for all outcome models that need it. This
#'                                       can greatly speed up the analyses if multiple outcome models
#'                                       require the same filtering.
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
#' @param fitOutcomeModelThreads         The number of parallel threads to use for fitting the outcome
#'                                       models.
#' @param prefilterCovariatesThreads     The number of parallel threads to use for prefiltering covariates.
#' @param outcomeCvThreads               The number of parallel threads to use for the cross-
#'                                       validation when estimating the hyperparameter for the outcome
#'                                       model. Note that the total number of CV threads at one time
#'                                       could be `fitOutcomeModelThreads * outcomeCvThreads`.
#' @param outcomeIdsOfInterest           If provided, creation of non-essential files will be skipped
#'                                       for all other outcome IDs. This could be helpful to speed up
#'                                       analyses with many controls.
#'
#' @return
#' A tibble describing for each target-comparator-outcome-analysisId combination where the intermediary and
#' outcome model files can be found, relative to the `outputFolder`.
#'
#' @export
runCmAnalyses <- function(connectionDetails,
                          cdmDatabaseSchema,
                          oracleTempSchema = NULL,
                          tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "condition_occurrence",
                          cdmVersion = 5,
                          outputFolder = "./CohortMethodOutput",
                          cmAnalysisList,
                          targetComparatorOutcomesList,
                          refitPsForEveryOutcome = FALSE,
                          refitPsForEveryStudyPopulation = TRUE,
                          prefilterCovariates = TRUE,
                          getDbCohortMethodDataThreads = 1,
                          createPsThreads = 1,
                          psCvThreads = 1,
                          createStudyPopThreads = 1,
                          trimMatchStratifyThreads = 1,
                          prefilterCovariatesThreads = 1,
                          fitOutcomeModelThreads = 1,
                          outcomeCvThreads = 1,
                          outcomeIdsOfInterest) {
  if (!missing(outcomeIdsOfInterest) && !is.null(outcomeIdsOfInterest) && refitPsForEveryOutcome) {
    stop("Cannot have both outcomeIdsOfInterest and refitPsForEveryOutcome set to TRUE")
  }
  if (!refitPsForEveryStudyPopulation && refitPsForEveryOutcome) {
    stop("Cannot have refitPsForEveryStudyPopulation = FALSE and refitPsForEveryOutcome = TRUE")
  }
  for (targetComparatorOutcomes in targetComparatorOutcomesList) {
    stopifnot(class(targetComparatorOutcomes) == "targetComparatorOutcomes")
  }
  for (cmAnalysis in cmAnalysisList) {
    stopifnot(class(cmAnalysis) == "cmAnalysis")
  }
  uniquetargetComparatorOutcomesList <- unique(ParallelLogger::selectFromList(targetComparatorOutcomesList,
                                                                              c("targetId",
                                                                                "comparatorId",
                                                                                "outcomeIds")))
  if (length(uniquetargetComparatorOutcomesList) != length(targetComparatorOutcomesList)) {
    stop("Duplicate target-comparator-outcomes combinations are not allowed")
  }
  analysisIds <- unlist(ParallelLogger::selectFromList(cmAnalysisList, "analysisId"))
  uniqueAnalysisIds <- unique(analysisIds)
  if (length(uniqueAnalysisIds) != length(analysisIds)) {
    stop("Duplicate analysis IDs are not allowed")
  }
  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  referenceTable <- createReferenceTable(cmAnalysisList,
                                         targetComparatorOutcomesList,
                                         outputFolder,
                                         refitPsForEveryOutcome,
                                         refitPsForEveryStudyPopulation,
                                         prefilterCovariates,
                                         outcomeIdsOfInterest)

  saveRDS(referenceTable, file.path(outputFolder, "outcomeModelReference.rds"))

  ParallelLogger::logInfo("*** Creating cohortMethodData objects ***")
  subset <- referenceTable[!duplicated(referenceTable$cohortMethodDataFile), ]
  subset <- subset[subset$cohortMethodDataFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$cohortMethodDataFile)), ]
  if (nrow(subset) != 0) {
    createCmDataTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(cmAnalysisList,
                                                 list(analysisId = refRow$analysisId))[[1]]
      getDbCohortMethodDataArgs <- analysisRow$getDbCohortMethodDataArgs
      covariateSettings <- getDbCohortMethodDataArgs$covariateSettings
      if (is(covariateSettings, "covariateSettings"))
        covariateSettings <- list(covariateSettings)
      for (i in 1:length(covariateSettings)) {
        covariateSettings[[i]]$excludedCovariateConceptIds <- unique(c(as.numeric(unlist(strsplit(as.character(refRow$excludedCovariateConceptIds),
                                                                                                  ","))),
                                                                       covariateSettings[[i]]$excludedCovariateConceptIds))
        covariateSettings[[i]]$includedCovariateConceptIds <- unique(c(as.numeric(unlist(strsplit(as.character(refRow$includedCovariateConceptIds),
                                                                                                  ","))),
                                                                       covariateSettings[[i]]$includedCovariateConceptIds))
      }
      getDbCohortMethodDataArgs$covariateSettings <- covariateSettings
      outcomeIds <- unique(referenceTable$outcomeId[referenceTable$cohortMethodDataFile == refRow$cohortMethodDataFile])
      args <- list(connectionDetails = connectionDetails,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   tempEmulationSchema = tempEmulationSchema,
                   exposureDatabaseSchema = exposureDatabaseSchema,
                   exposureTable = exposureTable,
                   outcomeDatabaseSchema = outcomeDatabaseSchema,
                   outcomeTable = outcomeTable,
                   cdmVersion = cdmVersion,
                   outcomeIds = outcomeIds,
                   targetId = refRow$targetId,
                   comparatorId = refRow$comparatorId)
      args <- append(args, getDbCohortMethodDataArgs)
      task <- list(args = args,
                   cohortMethodDataFile = file.path(outputFolder, refRow$cohortMethodDataFile))
      return(task)
    }
    objectsToCreate <- lapply(1:nrow(subset), createCmDataTask)
    cluster <- ParallelLogger::makeCluster(getDbCohortMethodDataThreads)
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, objectsToCreate, createCmDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Creating study populations ***")
  subset <- referenceTable[!duplicated(referenceTable$studyPopFile), ]
  subset <- subset[subset$studyPopFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$studyPopFile)), ]
  if (nrow(subset) != 0) {
    createStudyPopTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(cmAnalysisList,
                                                 list(analysisId = refRow$analysisId))[[1]]
      args <- analysisRow$createStudyPopArgs
      args$outcomeId <- refRow$outcomeId
      task <- list(cohortMethodDataFile = file.path(outputFolder,
                                                    refRow$cohortMethodDataFile),
                   args = args,
                   minimizeFileSizes = getOption("minimizeFileSizes"),
                   studyPopFile = file.path(outputFolder, refRow$studyPopFile))
      return(task)
    }
    objectsToCreate <- lapply(1:nrow(subset), createStudyPopTask)
    cluster <- ParallelLogger::makeCluster(createStudyPopThreads)
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, objectsToCreate, createStudyPopObject)
    ParallelLogger::stopCluster(cluster)
  }

  if (refitPsForEveryOutcome) {
    ParallelLogger::logInfo("*** Fitting propensity score models ***")
    subset <- referenceTable[!duplicated(referenceTable$psFile), ]
    subset <- subset[subset$psFile != "", ]
    subset <- subset[!file.exists(file.path(outputFolder, subset$psFile)), ]
    if (nrow(subset) != 0) {
      createPsTask <- function(i) {
        refRow <- subset[i, ]
        analysisRow <- ParallelLogger::matchInList(cmAnalysisList,
                                                   list(analysisId = refRow$analysisId))[[1]]
        args <- analysisRow$createPsArgs
        args$control$threads <- psCvThreads
        task <- list(cohortMethodDataFile = file.path(outputFolder,
                                                      refRow$cohortMethodDataFile),
                     studyPopFile = file.path(outputFolder, refRow$studyPopFile),
                     args = args,
                     psFile = file.path(outputFolder, refRow$psFile))
        return(task)
      }

      modelsToFit <- lapply(1:nrow(subset), createPsTask)
      cluster <- ParallelLogger::makeCluster(createPsThreads)
      ParallelLogger::clusterRequire(cluster, "CohortMethod")
      dummy <- ParallelLogger::clusterApply(cluster, modelsToFit, fitPsModel)
      ParallelLogger::stopCluster(cluster)
    }
  } else {
    ParallelLogger::logInfo("*** Fitting shared propensity score models ***")
    subset <- referenceTable[!duplicated(referenceTable$sharedPsFile), ]
    subset <- subset[subset$sharedPsFile != "", ]
    subset <- subset[!file.exists(file.path(outputFolder, subset$sharedPsFile)), ]
    if (nrow(subset) != 0) {
      createSharedPsTask <- function(i) {
        refRow <- subset[i, ]
        analysisRow <- ParallelLogger::matchInList(cmAnalysisList,
                                                   list(analysisId = refRow$analysisId))[[1]]

        createPsArg <- analysisRow$createPsArg
        createPsArg$control$threads <- psCvThreads
        task <- list(cohortMethodDataFile = file.path(outputFolder,
                                                      refRow$cohortMethodDataFile),
                     createPsArg = createPsArg,
                     createStudyPopArgs = analysisRow$createStudyPopArgs,
                     sharedPsFile = file.path(outputFolder, refRow$sharedPsFile))
        return(task)
      }
      modelsToFit <- lapply(1:nrow(subset), createSharedPsTask)
      cluster <- ParallelLogger::makeCluster(createPsThreads)
      ParallelLogger::clusterRequire(cluster, "CohortMethod")
      dummy <- ParallelLogger::clusterApply(cluster, modelsToFit, fitSharedPsModel, refitPsForEveryStudyPopulation)
      ParallelLogger::stopCluster(cluster)
    }
    ParallelLogger::logInfo("*** Adding propensity scores to study population objects ***")
    subset <- referenceTable[!duplicated(referenceTable$psFile), ]
    subset <- subset[subset$psFile != "", ]
    subset <- subset[!file.exists(file.path(outputFolder, subset$psFile)), ]

    if (nrow(subset) != 0) {
      tasks <- split(subset, subset$sharedPsFile)
      cluster <- ParallelLogger::makeCluster(trimMatchStratifyThreads)
      ParallelLogger::clusterRequire(cluster, "CohortMethod")
      dummy <- ParallelLogger::clusterApply(cluster, tasks, addPsToStudyPop, outputFolder = outputFolder)
      ParallelLogger::stopCluster(cluster)
    }
  }

  ParallelLogger::logInfo("*** Trimming/Matching/Stratifying ***")
  subset <- referenceTable[!duplicated(referenceTable$strataFile), ]
  subset <- subset[subset$strataFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$strataFile)), ]
  if (nrow(subset) != 0) {
    createTrimMatchStratTask <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(cmAnalysisList,
                                                 list(analysisId = refRow$analysisId))[[1]]
      task <- list(psFile = file.path(outputFolder, refRow$psFile),
                   args = analysisRow,
                   strataFile = file.path(outputFolder, refRow$strataFile))
      return(task)
    }
    tasks <- lapply(1:nrow(subset), createTrimMatchStratTask)

    cluster <- ParallelLogger::makeCluster(trimMatchStratifyThreads)
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, tasks, trimMatchStratify)
    ParallelLogger::stopCluster(cluster)
  }

  if (prefilterCovariates) {
    ParallelLogger::logInfo("*** Prefiltering covariates for outcome models ***")
    subset <- referenceTable[!duplicated(referenceTable$prefilteredCovariatesFile), ]
    subset <- subset[subset$prefilteredCovariatesFile != "", ]
    subset <- subset[!file.exists(file.path(outputFolder, subset$prefilteredCovariatesFile)), ]
    if (nrow(subset) != 0) {
      createPrefilterTask <- function(i) {
        refRow <- subset[i, ]
        analysisRow <- ParallelLogger::matchInList(cmAnalysisList,
                                                   list(analysisId = refRow$analysisId))[[1]]
        task <- list(cohortMethodDataFile = file.path(outputFolder,
                                                      refRow$cohortMethodDataFile),
                     args = analysisRow$fitOutcomeModelArgs,
                     prefilteredCovariatesFile = file.path(outputFolder,
                                                           refRow$prefilteredCovariatesFile))
        return(task)
      }
      tasks <- lapply(1:nrow(subset), createPrefilterTask)
      cluster <- ParallelLogger::makeCluster(min(prefilterCovariatesThreads, length(tasks)))
      ParallelLogger::clusterRequire(cluster, "CohortMethod")
      dummy <- ParallelLogger::clusterApply(cluster, tasks, doPrefilterCovariates)
      ParallelLogger::stopCluster(cluster)
    }
  }

  if (missing(outcomeIdsOfInterest) || is.null(outcomeIdsOfInterest)) {
    ParallelLogger::logInfo("*** Fitting outcome models ***")
  } else {
    ParallelLogger::logInfo("*** Fitting outcome models for outcomes of interest ***")
  }
  subset <- referenceTable[referenceTable$outcomeOfInterest & referenceTable$outcomeModelFile != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$outcomeModelFile)) , ]
  createOutcomeModelTask <- function(i) {
    refRow <- subset[i, ]
    analysisRow <- ParallelLogger::matchInList(cmAnalysisList,
                                               list(analysisId = refRow$analysisId))[[1]]
    args <- analysisRow$fitOutcomeModelArgs
    args$control$threads <- outcomeCvThreads
    if (refRow$strataFile != "") {
      studyPopFile <- refRow$strataFile
    } else if (refRow$psFile != "") {
      studyPopFile <- refRow$psFile
    } else {
      studyPopFile <- refRow$studyPopFile
    }
    prefilteredCovariatesFile <- refRow$prefilteredCovariatesFile
    if (prefilteredCovariatesFile != "") {
      prefilteredCovariatesFile = file.path(outputFolder, refRow$prefilteredCovariatesFile)
    }
    return(list(cohortMethodDataFile = file.path(outputFolder, refRow$cohortMethodDataFile),
                prefilteredCovariatesFile = prefilteredCovariatesFile,
                args = args,
                studyPopFile = file.path(outputFolder, studyPopFile),
                outcomeModelFile = file.path(outputFolder, refRow$outcomeModelFile)))
  }
  if (nrow(subset) == 0) {
    modelsToFit <- list()
  } else {
    modelsToFit <- lapply(1:nrow(subset), createOutcomeModelTask)
  }

  if (length(modelsToFit) != 0) {
    cluster <- ParallelLogger::makeCluster(fitOutcomeModelThreads)
    ParallelLogger::clusterRequire(cluster, "CohortMethod")
    dummy <- ParallelLogger::clusterApply(cluster, modelsToFit, doFitOutcomeModel)
    ParallelLogger::stopCluster(cluster)
  }

  if (!missing(outcomeIdsOfInterest) && !is.null(outcomeIdsOfInterest)) {
    ParallelLogger::logInfo("*** Fitting outcome models for other outcomes ***")

    subset <- referenceTable[!referenceTable$outcomeOfInterest &
                               referenceTable$outcomeModelFile != "" &
                               !file.exists(file.path(outputFolder, referenceTable$outcomeModelFile)) , ]
    createArgs <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- ParallelLogger::matchInList(cmAnalysisList,
                                                 list(analysisId = refRow$analysisId))[[1]]
      analysisRow$fitOutcomeModelArgs$control$threads <- outcomeCvThreads
      analysisRow$createStudyPopArgs$outcomeId <- refRow$outcomeId
      prefilteredCovariatesFile <- refRow$prefilteredCovariatesFile
      if (prefilteredCovariatesFile != "") {
        prefilteredCovariatesFile = file.path(outputFolder, refRow$prefilteredCovariatesFile)
      }
      params <- list(cohortMethodDataFile = file.path(outputFolder, refRow$cohortMethodDataFile),
                     prefilteredCovariatesFile = prefilteredCovariatesFile,
                     sharedPsFile = file.path(outputFolder, refRow$sharedPsFile),
                     args = analysisRow,
                     outcomeModelFile = file.path(outputFolder, refRow$outcomeModelFile))
      return(params)
    }
    if (nrow(subset) == 0) {
      modelsToFit <- list()
    } else {
      modelsToFit <- lapply(1:nrow(subset), createArgs)
    }

    if (length(modelsToFit) != 0) {
      cluster <- ParallelLogger::makeCluster(fitOutcomeModelThreads)
      ParallelLogger::clusterRequire(cluster, "CohortMethod")
      dummy <- ParallelLogger::clusterApply(cluster, modelsToFit, doFitOutcomeModelPlus)
      ParallelLogger::stopCluster(cluster)
    }
  }
  invisible(referenceTable)
}

getCohortMethodData <- function(cohortMethodDataFile) {
  if (exists("cohortMethodData", envir = globalenv())) {
    cohortMethodData <- get("cohortMethodData", envir = globalenv())
  }
  if (!mget("cohortMethodDataFile", envir = globalenv(), ifnotfound = "") == cohortMethodDataFile) {
    if (exists("cohortMethodData", envir = globalenv())) {
      Andromeda::close(cohortMethodData)
    }
    cohortMethodData <- loadCohortMethodData(cohortMethodDataFile)
    assign("cohortMethodData", cohortMethodData, envir = globalenv())
    assign("cohortMethodDataFile", cohortMethodDataFile, envir = globalenv())
  }
  return(cohortMethodData)
}

getPs <- function(psFile) {
  if (mget("psFile", envir = globalenv(), ifnotfound = "") == psFile) {
    ps <- get("ps", envir = globalenv())
  } else {
    ps <- readRDS(psFile)
    assign("ps", ps, envir = globalenv())
    assign("psFile", psFile, envir = globalenv())
  }
  return(ps)
}

createCmDataObject <- function(params) {
  cohortMethodData <- do.call("getDbCohortMethodData", params$args)
  saveCohortMethodData(cohortMethodData, params$cohortMethodDataFile)
  return(NULL)
}

createStudyPopObject <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFile)
  args <- params$args
  args$cohortMethodData <- cohortMethodData
  studyPop <- do.call("createStudyPopulation", args)
  if (!is.null(params$minimizeFileSizes) && params$minimizeFileSizes) {
    metaData <- attr(studyPop, "metaData")
    studyPop <- studyPop[, c("rowId", "treatment", "personSeqId", "outcomeCount", "timeAtRisk", "survivalTime")]
    attr(studyPop, "metaData") <- metaData
  }
  saveRDS(studyPop, params$studyPopFile)
  return(NULL)
}

fitPsModel <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFile)
  studyPop <- readRDS(params$studyPopFile)
  args <- params$args
  args$cohortMethodData <- cohortMethodData
  args$population <- studyPop
  ps <- do.call("createPs", args)
  saveRDS(ps, params$psFile)
  return(NULL)
}

fitSharedPsModel <- function(params, refitPsForEveryStudyPopulation) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFile)
  if (refitPsForEveryStudyPopulation) {
    args <- params$createStudyPopArgs
    args$cohortMethodData <- cohortMethodData
    ParallelLogger::logInfo("Fitting propensity model across all outcomes (ignore messages about 'no outcome specified')")
    studyPop <- do.call("createStudyPopulation", args)
  } else {
    studyPop <- NULL
  }
  args <- params$createPsArg
  args$cohortMethodData <- cohortMethodData
  args$population <- studyPop
  ps <- do.call("createPs", args)
  saveRDS(ps, params$sharedPsFile)
  return(NULL)
}

addPsToStudyPop <- function(subset, outputFolder) {
  ps <- readRDS(file.path(outputFolder, subset$sharedPsFile[1]))

  addToStudyPop <- function(i) {
    refRow <- subset[i, ]
    studyPop <- readRDS(file.path(outputFolder, refRow$studyPopFile))
    newMetaData <- attr(studyPop, "metaData")
    newMetaData$psModelCoef <- attr(ps, "metaData")$psModelCoef
    newMetaData$psModelPriorVariance <- attr(ps, "metaData")$psModelPriorVariance
    idx <- match(studyPop$rowId, ps$rowId)
    studyPop$propensityScore <- ps$propensityScore[idx]
    attr(studyPop, "metaData") <- newMetaData
    saveRDS(studyPop, file.path(outputFolder, refRow$psFile))
    return(NULL)
  }
  plyr::l_ply(1:nrow(subset), addToStudyPop)
}

trimMatchStratify <- function(params) {
  ps <- getPs(params$psFile)
  if (params$args$trimByPs) {
    args <- list(population = ps)
    args <- append(args, params$args$trimByPsArgs)
    ps <- do.call("trimByPs", args)
  } else if (params$args$trimByPsToEquipoise) {
    args <- list(population = ps)
    args <- append(args, params$args$trimByPsToEquipoiseArgs)
    ps <- do.call("trimByPsToEquipoise", args)
  } else if (params$args$trimByIptw) {
    args <- list(population = ps)
    args <- append(args, params$args$trimByIptwArgs)
    ps <- do.call("trimByIptw", args)
  }
  if (params$args$matchOnPs) {
    args <- list(population = ps)
    args <- append(args, params$args$matchOnPsArgs)
    ps <- do.call("matchOnPs", args)
  } else if (params$args$matchOnPsAndCovariates) {
    args <- list(population = ps)
    args <- append(args, params$args$matchOnPsAndCovariatesArgs)
    ps <- do.call("matchOnPsAndCovariates", args)
  } else if (params$args$stratifyByPs) {
    args <- list(population = ps)
    args <- append(args, params$args$stratifyByPsArgs)
    ps <- do.call("stratifyByPs", args)
  } else if (params$args$stratifyByPsAndCovariates) {
    args <- list(population = ps)
    args <- append(args, params$args$stratifyByPsAndCovariatesArgs)
    ps <- do.call("stratifyByPsAndCovariates", args)
  }
  saveRDS(ps, params$strataFile)
  return(NULL)
}

doPrefilterCovariates <- function(params) {
  cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFile)
  covariates <- cohortMethodData$covariates
  if (covariates %>% count() %>% pull() > 0) {
    if (params$args$useCovariates) {
      covariatesToInclude <- params$args$includeCovariateIds
      covariatesToExclude <- params$args$excludeCovariateIds
    } else {
      covariatesToInclude <- c()
      covariatesToExclude <- c()
    }
    covariatesToInclude <- unique(c(covariatesToInclude, params$args$interactionCovariateIds))
    if (length(covariatesToInclude) != 0) {
      covariates <- covariates %>%
        filter(.data$covariateId %in% covariatesToInclude)
    }
    if (length(covariatesToExclude) != 0) {
      covariates <- covariates %>%
        filter(!.data$covariateId %in% covariatesToExclude)
    }
  }
  filteredCohortMethodData <- Andromeda::andromeda(cohorts = cohortMethodData$cohorts,
                                                   outcomes = cohortMethodData$outcomes,
                                                   covariates = covariates,
                                                   covariateRef = cohortMethodData$covariateRef,
                                                   analysisRef = cohortMethodData$analysisRef)
  attr(filteredCohortMethodData, "metaData") <- attr(cohortMethodData, "metaData")
  class(filteredCohortMethodData) <- "CohortMethodData"
  attr(class(filteredCohortMethodData), "package") <- "CohortMethod"
  saveCohortMethodData(filteredCohortMethodData, params$prefilteredCovariatesFile)
  return(NULL)
}

doFitOutcomeModel <- function(params) {
  if (params$prefilteredCovariatesFile == "") {
    cohortMethodData <- getCohortMethodData(params$cohortMethodDataFile)
  } else {
    cohortMethodData <- getCohortMethodData(params$prefilteredCovariatesFile)
  }
  studyPop <- readRDS(params$studyPopFile)
  args <- list(cohortMethodData = cohortMethodData, population = studyPop)
  args <- append(args, params$args)
  # outcomeModel <- do.call('fitOutcomeModel', args)
  outcomeModel <- fitOutcomeModel(population = args$population,
                                  cohortMethodData = args$cohortMethodData,
                                  modelType = args$modelType,
                                  stratified = args$stratified,
                                  useCovariates = args$useCovariates,
                                  inversePtWeighting = args$inversePtWeighting,
                                  estimator = args$estimator,
                                  includeCovariateIds = args$includeCovariateIds,
                                  excludeCovariateIds = args$excludeCovariateIds,
                                  interactionCovariateIds = args$interactionCovariateIds,
                                  prior = args$prior,
                                  control = args$control)
  saveRDS(outcomeModel, params$outcomeModelFile)
  return(NULL)
}

doFitOutcomeModelPlus <- function(params) {
  if (params$prefilteredCovariatesFile == "") {
    cohortMethodData <- getCohortMethodData(params$cohortMethodDataFile)
  } else {
    cohortMethodData <- getCohortMethodData(params$prefilteredCovariatesFile)
  }

  # Create study pop
  args <- params$args$createStudyPopArgs
  args$cohortMethodData <- cohortMethodData
  studyPop <- do.call("createStudyPopulation", args)

  if (params$args$createPs) {
    # Add PS
    ps <- getPs(params$sharedPsFile)
    idx <- match(studyPop$rowId, ps$rowId)
    studyPop$propensityScore <- ps$propensityScore[idx]
    ps <- studyPop
  } else {
    ps <- studyPop
  }
  # Trim, match. stratify
  if (params$args$trimByPs) {
    args <- list(population = ps)
    args <- append(args, params$args$trimByPsArgs)
    ps <- do.call("trimByPs", args)
  } else if (params$args$trimByPsToEquipoise) {
    args <- list(population = ps)
    args <- append(args, params$args$trimByPsToEquipoiseArgs)
    ps <- do.call("trimByPsToEquipoise", args)
  } else if (params$args$trimByIptw) {
    args <- list(population = ps)
    args <- append(args, params$args$trimByIptwArgs)
    ps <- do.call("trimByIptw", args)
  }
  if (params$args$matchOnPs) {
    args <- list(population = ps)
    args <- append(args, params$args$matchOnPsArgs)
    ps <- do.call("matchOnPs", args)
  } else if (params$args$matchOnPsAndCovariates) {
    args <- list(population = ps)
    args <- append(args, params$args$matchOnPsAndCovariatesArgs)
    ps <- do.call("matchOnPsAndCovariates", args)
  } else if (params$args$stratifyByPs) {
    args <- list(population = ps)
    args <- append(args, params$args$stratifyByPsArgs)
    ps <- do.call("stratifyByPs", args)
  } else if (params$args$stratifyByPsAndCovariates) {
    args <- list(population = ps)
    args <- append(args, params$args$stratifyByPsAndCovariatesArgs)
    ps <- do.call("stratifyByPsAndCovariates", args)
  }
  args <- params$args$fitOutcomeModelArgs
  args$population <- ps
  args$cohortMethodData <- cohortMethodData
  outcomeModel <- do.call('fitOutcomeModel', args)
  saveRDS(outcomeModel, params$outcomeModelFile)
  if (!is.null(outcomeModel)) {
    rm(outcomeModel)
  }
  if (!is.null(cohortMethodData)) {
    rm(cohortMethodData)
  }
  if (!is.null(ps)) {
    rm(ps)
  }
  if (!is.null(args)) {
    rm(args)
  }
  return(NULL)
}

createReferenceTable <- function(cmAnalysisList,
                                 targetComparatorOutcomesList,
                                 outputFolder,
                                 refitPsForEveryOutcome,
                                 refitPsForEveryStudyPopulation,
                                 prefilterCovariates,
                                 outcomeIdsOfInterest) {
  # Create all rows per target-comparator-outcome-analysis combination:
  analysisIds <- unlist(ParallelLogger::selectFromList(cmAnalysisList, "analysisId"))
  instantiateDco <- function(dco, cmAnalysis, folder) {
    rows <- dplyr::tibble(analysisId = cmAnalysis$analysisId,
                           targetId = .selectByType(cmAnalysis$targetType, dco$targetId, "target"),
                           comparatorId = .selectByType(cmAnalysis$comparatorType,
                                                        dco$comparatorId,
                                                        "comparator"),
                           includedCovariateConceptIds = paste(dco$includedCovariateConceptIds,
                                                               collapse = ","),
                           excludedCovariateConceptIds = paste(dco$excludedCovariateConceptIds,
                                                               collapse = ","),
                           outcomeId = dco$outcomeIds)

    if (cmAnalysis$fitOutcomeModel) {
      rows$outcomeModelFile <- .createOutcomeModelFileName(folder = folder,
                                                           targetId = rows$targetId,
                                                           comparatorId = rows$comparatorId,
                                                           outcomeId = rows$outcomeId)
    } else {
      rows$outcomeModelFile <- ""
    }
    return(rows)
  }
  instantiateDcos <- function(cmAnalysis, dcos, folder) {
    analysisFolder <- paste("Analysis_", cmAnalysis$analysisId, sep = "")
    if (!file.exists(file.path(folder, analysisFolder)))
      dir.create(file.path(folder, analysisFolder))

    return(do.call("rbind", lapply(dcos, instantiateDco, cmAnalysis, analysisFolder)))
  }

  referenceTable <- bind_rows(lapply(cmAnalysisList,
                                     instantiateDcos,
                                     dcos = targetComparatorOutcomesList,
                                     folder = outputFolder))

  # Find unique load operations
  which.list <- function(list, object) {
    return(do.call("c", lapply(1:length(list), function(i, list, object) {
      if (identical(list[[i]], object)) return(i) else return(c())
    }, list, object)))
  }

  loadingArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList,
                                                           "getDbCohortMethodDataArgs"))
  loadingArgsList <- lapply(loadingArgsList, function(x) return(x[[1]]))
  loadArgsId <- sapply(cmAnalysisList,
                       function(cmAnalysis, loadingArgsList) return(which.list(loadingArgsList,
                                                                               cmAnalysis$getDbCohortMethodDataArgs)),
                       loadingArgsList)
  analysisIdToLoadArgsId <- dplyr::tibble(analysisId = analysisIds, loadArgsId = loadArgsId)
  referenceTable <- inner_join(referenceTable, analysisIdToLoadArgsId, by = "analysisId")
  referenceTable$cohortMethodDataFile <- .createCohortMethodDataFileName(loadId = referenceTable$loadArgsId,
                                                                         targetId = referenceTable$targetId,
                                                                         comparatorId = referenceTable$comparatorId)

  # Add studypop filenames
  studyPopArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, "createStudyPopArgs"))
  studyPopArgsList <- lapply(studyPopArgsList, function(x) return(x[[1]]))
  studyPopArgsId <- sapply(cmAnalysisList,
                           function(cmAnalysis, studyPopArgsList) return(which.list(studyPopArgsList,
                                                                                    cmAnalysis$createStudyPopArgs)),
                           studyPopArgsList)
  analysisIdToStudyPopArgsId <- dplyr::tibble(analysisId = analysisIds,
                                               studyPopArgsId = studyPopArgsId)
  referenceTable <- inner_join(referenceTable, analysisIdToStudyPopArgsId, by = "analysisId")
  referenceTable$studyPopFile <- .createStudyPopulationFileName(loadId = referenceTable$loadArgsId,
                                                                studyPopId = referenceTable$studyPopArgsId,
                                                                targetId = referenceTable$targetId,
                                                                comparatorId = referenceTable$comparatorId,
                                                                outcomeId = referenceTable$outcomeId)

  # Add PS filenames
  psArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, "createPsArgs"))
  psArgsList <- lapply(psArgsList,
                       function(x) return(if (length(x) > 0) return(x[[1]]) else return(NULL)))
  noPsIds <- which(sapply(psArgsList, is.null))
  psArgsId <- sapply(cmAnalysisList,
                     function(cmAnalysis,
                              psArgsList) return(which.list(psArgsList, cmAnalysis$createPsArgs)),
                     psArgsList)
  analysisIdToPsArgsId <- dplyr::tibble(analysisId = analysisIds, psArgsId = psArgsId)
  referenceTable <- inner_join(referenceTable, analysisIdToPsArgsId, by = "analysisId")
  idx <- !(referenceTable$psArgsId %in% noPsIds)
  referenceTable$psFile <- ""
  referenceTable$psFile[idx] <- .createPsOutcomeFileName(loadId = referenceTable$loadArgsId[idx],
                                                         studyPopId = referenceTable$studyPopArgsId[idx],
                                                         psId = referenceTable$psArgsId[idx],
                                                         targetId = referenceTable$targetId[idx],
                                                         comparatorId = referenceTable$comparatorId[idx],
                                                         outcomeId = referenceTable$outcomeId[idx])
  referenceTable$sharedPsFile <- ""
  if (!refitPsForEveryOutcome) {
    if (refitPsForEveryStudyPopulation) {
      # Find equivalent studyPopArgs, so we can reuse PS over those as well:
      studyPopArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, "createStudyPopArgs"))
      studyPopArgsList <- lapply(studyPopArgsList, function(x) return(x[[1]]))
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
          if (equivalent(studyPopArgsList[[i]], studyPopArgs))
            return(i)
        }
      }
      studyPopArgsEquivalentId <- sapply(cmAnalysisList,
                                         function(cmAnalysis, studyPopArgsList) return(findFirstEquivalent(studyPopArgsList,
                                                                                                           cmAnalysis$createStudyPopArgs)),
                                         studyPopArgsList)
      analysisIdToStudyPopArgsEquivalentId <- dplyr::tibble(analysisId = analysisIds,
                                                             studyPopArgsEquivalentId = studyPopArgsEquivalentId)
      referenceTable <- inner_join(referenceTable, analysisIdToStudyPopArgsEquivalentId, by = "analysisId")
      referenceTable$sharedPsFile[idx] <- .createPsFileName(loadId = referenceTable$loadArgsId[idx],
                                                            studyPopId = referenceTable$studyPopArgsEquivalentId[idx],
                                                            psId = referenceTable$psArgsId[idx],
                                                            targetId = referenceTable$targetId[idx],
                                                            comparatorId = referenceTable$comparatorId[idx])
    } else {
      # One propensity model across all study population settings:
      referenceTable$sharedPsFile[idx] <- .createPsFileName(loadId = referenceTable$loadArgsId[idx],
                                                            studyPopId = NULL,
                                                            psId = referenceTable$psArgsId[idx],
                                                            targetId = referenceTable$targetId[idx],
                                                            comparatorId = referenceTable$comparatorId[idx])
    }
  }

  # Add strata filenames
  args <- c("trimByPs",
            "trimByPsArgs",
            "trimByPsToEquipoise",
            "trimByPsToEquipoiseArgs",
            "trimByIptw",
            "trimByIptwArgs",
            "matchOnPs",
            "matchOnPsArgs",
            "matchOnPsAndCovariates",
            "matchOnPsAndCovariatesArgs",
            "stratifyByPs",
            "stratifyByPsArgs",
            "stratifyByPsAndCovariates",
            "stratifyByPsAndCovariatesArgs")
  normStrataArgs <- function(strataArgs) {
    return(strataArgs[args][!is.na(names(strataArgs[args]))])
  }
  strataArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, args))
  strataArgsList <- strataArgsList[sapply(strataArgsList,
                                          function(strataArgs) return(strataArgs$trimByPs |
                                                                        strataArgs$trimByPsToEquipoise |
                                                                        strataArgs$trimByIptw |
                                                                        strataArgs$matchOnPs |
                                                                        strataArgs$matchOnPsAndCovariates |
                                                                        strataArgs$stratifyByPs |
                                                                        strataArgs$stratifyByPsAndCovariates))]
  strataArgsList <- lapply(strataArgsList, normStrataArgs)
  if (length(strataArgsList) == 0) {
    referenceTable$strataArgsId <- 0
  } else {
    strataArgsId <- sapply(cmAnalysisList, function(cmAnalysis) {
      i <- which.list(strataArgsList, normStrataArgs(cmAnalysis))
      if (is.null(i))
        i <- 0
      return(i)
    })
    analysisIdToStrataArgsId <- dplyr::tibble(analysisId = analysisIds, strataArgsId = strataArgsId)
    referenceTable <- inner_join(referenceTable, analysisIdToStrataArgsId, by = "analysisId")
  }
  idx <- referenceTable$strataArgsId != 0
  referenceTable$strataFile <- ""
  referenceTable$strataFile[idx] <- .createStratifiedPopFileName(loadId = referenceTable$loadArgsId[idx],
                                                                 studyPopId = referenceTable$studyPopArgsId[idx],
                                                                 psId = referenceTable$psArgsId[idx],
                                                                 strataId = referenceTable$strataArgsId[idx],
                                                                 targetId = referenceTable$targetId[idx],
                                                                 comparatorId = referenceTable$comparatorId[idx],
                                                                 outcomeId = referenceTable$outcomeId[idx])


  # Add interest flag
  if (missing(outcomeIdsOfInterest) || is.null(outcomeIdsOfInterest)) {
    referenceTable$outcomeOfInterest <- TRUE
  } else {
    referenceTable$outcomeOfInterest <- FALSE
    referenceTable$outcomeOfInterest[referenceTable$outcomeId %in% outcomeIdsOfInterest] <- TRUE
    referenceTable$studyPopFile[!referenceTable$outcomeOfInterest] <- ""
    referenceTable$psFile[!referenceTable$outcomeOfInterest] <- ""
    referenceTable$strataFile[!referenceTable$outcomeOfInterest] <- ""
  }

  # Add prefiltered covariate files
  if (!prefilterCovariates) {
    referenceTable$prefilteredCovariatesFile <- ""
  } else {
    loadingFittingArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList,
                                                                    c("getDbCohortMethodDataArgs", "fitOutcomeModelArgs")))
    needsFilter <- function(loadingFittingArgs) {
      keep <- (loadingFittingArgs$fitOutcomeModelArgs$useCovariates & (length(loadingFittingArgs$fitOutcomeModelArgs$excludeCovariateIds) != 0 |
                                                                         length(loadingFittingArgs$fitOutcomeModelArgs$includeCovariateIds) != 0)) |
        length(loadingFittingArgs$fitOutcomeModelArgs$interactionCovariateIds) != 0
      if (keep) {
        loadingFittingArgs$relevantFields <- list(useCovariates = loadingFittingArgs$fitOutcomeModelArgs$useCovariates,
                                                  excludeCovariateIds = loadingFittingArgs$fitOutcomeModelArgs$excludeCovariateIds,
                                                  includeCovariateIds = loadingFittingArgs$fitOutcomeModelArgs$includeCovariateIds,
                                                  interactionCovariateIds = loadingFittingArgs$fitOutcomeModelArgs$interactionCovariateIds)
        return(loadingFittingArgs)
      } else {
        return(NULL)
      }
    }
    loadingFittingArgsList <- plyr::compact(lapply(loadingFittingArgsList, needsFilter))
    referenceTable$prefilteredCovariatesFile <- ""
    if (length(loadingFittingArgsList) != 0) {
      # Filtering needed
      relevantArgsList <- ParallelLogger::selectFromList(loadingFittingArgsList,
                                                         c("getDbCohortMethodDataArgs", "relevantFields"))
      uniqueRelevantArgsList <- unique(relevantArgsList)
      prefilterIds <- sapply(relevantArgsList,
                             function(relevantArgs, uniqueRelevantArgsList) return(which.list(uniqueRelevantArgsList,
                                                                                              relevantArgs)),
                             uniqueRelevantArgsList)
      matchableArgsList <- ParallelLogger::selectFromList(loadingFittingArgsList,
                                                          c("getDbCohortMethodDataArgs", "fitOutcomeModelArgs"))

      matchingIds <- sapply(ParallelLogger::selectFromList(cmAnalysisList,
                                                           c("getDbCohortMethodDataArgs", "fitOutcomeModelArgs")),
                            function(cmAnalysis, matchableArgs) return(which.list(matchableArgs,
                                                                                  cmAnalysis)),
                            matchableArgsList)
      analysisIdToPrefilterId <- dplyr::tibble(analysisId = analysisIds,
                                                prefilterId = sapply(matchingIds, function(matchingId, prefilterIds) if (is.null(matchingId)) -1 else prefilterIds[matchingId], prefilterIds))
      referenceTable <- inner_join(referenceTable, analysisIdToPrefilterId, by = "analysisId")
      referenceTable$prefilteredCovariatesFile <- .createPrefilteredCovariatesFileName(loadId = referenceTable$loadArgsId,
                                                                                       targetId = referenceTable$targetId,
                                                                                       comparatorId = referenceTable$comparatorId,
                                                                                       prefilterId = referenceTable$prefilterId)

    }
  }

  # Some cleanup:
  referenceTable <- referenceTable[, c("analysisId",
                                       "targetId",
                                       "comparatorId",
                                       "outcomeId",
                                       "includedCovariateConceptIds",
                                       "excludedCovariateConceptIds",
                                       "outcomeOfInterest",
                                       "cohortMethodDataFile",
                                       "studyPopFile",
                                       "sharedPsFile",
                                       "psFile",
                                       "strataFile",
                                       "prefilteredCovariatesFile",
                                       "outcomeModelFile")]
  referenceTable <- referenceTable[order(referenceTable$analysisId,
                                         referenceTable$targetId,
                                         referenceTable$comparatorId,
                                         referenceTable$outcomeId), ]
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

.createOutcomeModelFileName <- function(folder, targetId, comparatorId, outcomeId) {
  name <- sprintf("om_t%s_c%s_o%s.rds", .f(targetId), .f(comparatorId), .f(outcomeId))
  return(file.path(folder, name))
}

.selectByType <- function(type, value, label) {
  if (is.null(type)) {
    if (is.list(value)) {
      stop(paste("Multiple ",
                 label,
                 "s specified, but none selected in analyses (comparatorType).",
                 sep = ""))
    }
    return(value)
  } else {
    if (!is.list(value) || is.null(value[type])) {
      stop(paste(label, "type not found:", type))
    }
    return(value[type])
  }
}

#' Create a summary report of the analyses
#'
#' @param referenceTable   A [dplyr::tibble] as created by the [runCmAnalyses] function.
#' @param outputFolder     Name of the folder where all the outputs have been written to.
#'
#' @return
#' A tibble containing summary statistics for each target-comparator-outcome-analysis combination.
#'
#' @export
summarizeAnalyses <- function(referenceTable, outputFolder) {

  summarizeOneAnalysis <- function(outcomeModelFile, outputFolder) {
    result <- dplyr::tibble(rr = 0,
                             ci95lb = 0,
                             ci95ub = 0,
                             p = 1,
                             target = 0,
                             comparator = 0,
                             targetDays = NA,
                             comparatorDays = NA,
                             eventsTarget = 0,
                             eventsComparator = 0,
                             logRr = 0,
                             seLogRr = 0)
    if (outcomeModelFile != "") {
      outcomeModel <- readRDS(file.path(outputFolder, outcomeModelFile))
      result$rr <- if (is.null(coef(outcomeModel)))
        NA else exp(coef(outcomeModel))
      result$ci95lb <- if (is.null(coef(outcomeModel)))
        NA else exp(confint(outcomeModel)[1])
      result$ci95ub <- if (is.null(coef(outcomeModel)))
        NA else exp(confint(outcomeModel)[2])
      if (is.null(coef(outcomeModel))) {
        result$p <- NA
      } else {
        z <- coef(outcomeModel)/outcomeModel$outcomeModelTreatmentEstimate$seLogRr
        result$p <- 2 * pmin(pnorm(z), 1 - pnorm(z))
      }
      result$target <- outcomeModel$populationCounts$targetPersons
      result$comparator <- outcomeModel$populationCounts$comparatorPersons
      result$targetDays <- outcomeModel$timeAtRisk$targetDays
      result$comparatorDays <- outcomeModel$timeAtRisk$comparatorDays
      result$eventsTarget <- outcomeModel$outcomeCounts$targetOutcomes
      result$eventsComparator <- outcomeModel$outcomeCounts$comparatorOutcomes
      result$logRr <- if (is.null(coef(outcomeModel)))
        NA else coef(outcomeModel)
      result$seLogRr <- if (is.null(coef(outcomeModel)))
        NA else outcomeModel$outcomeModelTreatmentEstimate$seLogRr
      result$llr <- if (is.null(coef(outcomeModel)))
        NA else outcomeModel$outcomeModelTreatmentEstimate$llr

      if (!is.null(outcomeModel$outcomeModelInteractionEstimates)) {
        for (i in 1:nrow(outcomeModel$outcomeModelInteractionEstimates)) {
          result[, paste("rr", outcomeModel$outcomeModelInteractionEstimates$covariateId[i], sep = "I")] <- exp(outcomeModel$outcomeModelInteractionEstimates$logRr[i])
          result[, paste("ci95lb", outcomeModel$outcomeModelInteractionEstimates$covariateId[i], sep = "I")] <- exp(outcomeModel$outcomeModelInteractionEstimates$logLb95[i])
          result[, paste("ci95ub", outcomeModel$outcomeModelInteractionEstimates$covariateId[i], sep = "I")] <- exp(outcomeModel$outcomeModelInteractionEstimates$logUb95[i])
          result[, paste("logRr", outcomeModel$outcomeModelInteractionEstimates$covariateId[i], sep = "I")] <- outcomeModel$outcomeModelInteractionEstimates$logRr[i]
          result[, paste("seLogRr", outcomeModel$outcomeModelInteractionEstimates$covariateId[i], sep = "I")] <- outcomeModel$outcomeModelInteractionEstimates$seLogRr[i]
        }
      }
    }
    return(result)
  }
  columns <- c("analysisId", "targetId", "comparatorId", "outcomeId")
  results <- plyr::llply(referenceTable$outcomeModelFile, summarizeOneAnalysis, outputFolder = outputFolder, .progress = "text")
  results <- bind_rows(results)
  results <- bind_cols(referenceTable[, columns], results)
  return(results)
}

# @file RunAnalyses.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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
#' @param connectionDetails              An R object of type \code{connectionDetails} created using the
#'                                       function \code{createConnectionDetails} in the
#'                                       \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema              The name of the database schema that contains the OMOP CDM
#'                                       instance. Requires read permissions to this database. On SQL
#'                                       Server, this should specifiy both the database and the schema,
#'                                       so for example 'cdm_instance.dbo'.
#' @param cdmVersion                     Define the OMOP CDM version used: currently support "4" and
#'                                       "5".
#' @param oracleTempSchema               For Oracle only: the name of the database schema where you
#'                                       want all temporary tables to be managed. Requires
#'                                       create/insert permissions to this database.
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
#' @param cmAnalysisList                 A list of objects of type \code{cmAnalysis} as created using
#'                                       the \code{\link{createCmAnalysis}} function.
#' @param targetComparatorOutcomesList   A list of objects of type \code{targetComparatorOutcomes} as
#'                                       created using the \code{\link{createTargetComparatorOutcomes}}
#'                                       function.
#' @param refitPsForEveryOutcome         Should the propensity model be fitted for every outcome (i.e.
#'                                       after people who already had the outcome are removed)? If
#'                                       false, a single propensity model will be fitted, and people
#'                                       who had the outcome previously will be removed afterwards.
#' @param refitPsForEveryStudyPopulation Should the propensity model be fitted for every study population
#'                                       definition? If false, a single propensity model will be fitted,
#'                                       and the study population criteria will be applied afterwards.
#' @param prefilterCovariates            If TRUE, and some outcome models require filtering covariates
#'                                       by concept ID (e.g. because \code{includeCovariateIds} or
#'                                       \code{interactionCovariateIds} is specified), this filtering
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
#' @param compressCohortMethodData       Compress CohortMethodData objects?
#'
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{targetId} \tab The ID of the target exposure.\cr
#' \verb{comparatorId} \tab The ID of the comparator group.\cr \verb{excludedCovariateConceptIds} \tab
#' The ID(s) of concepts that cannot be used to construct covariates. \cr
#' \verb{includedCovariateConceptIds} \tab The ID(s) of concepts that should be used to construct
#' covariates. \cr \verb{outcomeId} \tab The ID of the outcome \cr \verb{cohortMethodDataFolder} \tab
#' The ID of the outcome.\cr \verb{sharedPsFile} \tab The name of the file containing the propensity
#' scores of the shared \cr \tab propensity model. This model is used to create the outcome-specific
#' \cr \tab propensity scores by removing people with prior outcomes.\cr \verb{studyPopFile} \tab The
#' name of the file containing the study population (prior\cr \tab and trimming, matching, or
#' stratification on the PS.\cr \verb{psFile} \tab The name of file containing the propensity scores
#' for a specific \cr \tab outcomes (ie after people with prior outcomes have been removed).\cr
#' \verb{strataFile} \tab The name of the file containing the identifiers of the population \cr \tab
#' after any trimming, matching or stratifying, including their strata.\cr \verb{outcomeModelFile} \tab The name of the file
#' containing the outcome model.\cr }
#'
#' @export
runCmAnalyses <- function(connectionDetails,
                          cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
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
                          outcomeIdsOfInterest,
                          compressCohortMethodData = FALSE) {
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
  subset <- referenceTable[!duplicated(referenceTable$cohortMethodDataFolder), ]
  subset <- subset[subset$cohortMethodDataFolder != "", ]
  subset <- subset[!file.exists(file.path(outputFolder, subset$cohortMethodDataFolder)), ]
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
      outcomeIds <- unique(referenceTable$outcomeId[referenceTable$cohortMethodDataFolder == refRow$cohortMethodDataFolder])
      args <- list(connectionDetails = connectionDetails,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   oracleTempSchema = oracleTempSchema,
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
                   compressCohortMethodData = compressCohortMethodData,
                   cohortMethodDataFolder = file.path(outputFolder, refRow$cohortMethodDataFolder))
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
      task <- list(cohortMethodDataFolder = file.path(outputFolder,
                                                      refRow$cohortMethodDataFolder),
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
        task <- list(cohortMethodDataFolder = file.path(outputFolder,
                                                        refRow$cohortMethodDataFolder),
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
        task <- list(cohortMethodDataFolder = file.path(outputFolder,
                                                        refRow$cohortMethodDataFolder),
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
    subset <- referenceTable[!duplicated(referenceTable$prefilteredCovariatesFolder), ]
    subset <- subset[subset$prefilteredCovariatesFolder != "", ]
    subset <- subset[!file.exists(file.path(outputFolder, subset$prefilteredCovariatesFolder)), ]
    if (nrow(subset) != 0) {
      createPrefilterTask <- function(i) {
        refRow <- subset[i, ]
        analysisRow <- ParallelLogger::matchInList(cmAnalysisList,
                                                   list(analysisId = refRow$analysisId))[[1]]
        task <- list(cohortMethodDataFolder = file.path(outputFolder,
                                                        refRow$cohortMethodDataFolder),
                     args = analysisRow$fitOutcomeModelArgs,
                     prefilteredCovariatesFolder = file.path(outputFolder,
                                                             refRow$prefilteredCovariatesFolder))
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
    prefilteredCovariatesFolder <- refRow$prefilteredCovariatesFolder
    if (prefilteredCovariatesFolder != "") {
      prefilteredCovariatesFolder = file.path(outputFolder, refRow$prefilteredCovariatesFolder)
    }
    return(list(cohortMethodDataFolder = file.path(outputFolder, refRow$cohortMethodDataFolder),
                prefilteredCovariatesFolder = prefilteredCovariatesFolder,
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
      prefilteredCovariatesFolder <- refRow$prefilteredCovariatesFolder
      if (prefilteredCovariatesFolder != "") {
        prefilteredCovariatesFolder = file.path(outputFolder, refRow$prefilteredCovariatesFolder)
      }
      params <- list(cohortMethodDataFolder = file.path(outputFolder, refRow$cohortMethodDataFolder),
                     prefilteredCovariatesFolder = prefilteredCovariatesFolder,
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

getCohortMethodData <- function(cohortMethodDataFolder, requireCovariates = TRUE) {
  if (mget("cohortMethodDataFolder", envir = globalenv(), ifnotfound = "") == cohortMethodDataFolder &&
      (!requireCovariates || as.logical(mget("hasCovariates", envir = globalenv(), ifnotfound = FALSE)))) {
    cohortMethodData <- get("cohortMethodData", envir = globalenv())
  } else {
    cohortMethodData <- loadCohortMethodData(cohortMethodDataFolder, readOnly = TRUE, skipCovariates = !requireCovariates)
    assign("cohortMethodData", cohortMethodData, envir = globalenv())
    assign("cohortMethodDataFolder", cohortMethodDataFolder, envir = globalenv())
    assign("hasCovariates", requireCovariates, envir = globalenv())
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
  saveCohortMethodData(cohortMethodData, params$cohortMethodDataFolder, params$compressCohortMethodData)
  return(NULL)
}

createStudyPopObject <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder, requireCovariates = FALSE)
  args <- params$args
  args$cohortMethodData <- cohortMethodData
  studyPop <- do.call("createStudyPopulation", args)
  if (!is.null(params$minimizeFileSizes) && params$minimizeFileSizes) {
    metaData <- attr(studyPop, "metaData")
    studyPop <- studyPop[, c("rowId", "treatment", "subjectId", "outcomeCount", "timeAtRisk", "survivalTime")]
    attr(studyPop, "metaData") <- metaData
  }
  saveRDS(studyPop, params$studyPopFile)
  return(NULL)
}

fitPsModel <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder, requireCovariates = TRUE)
  studyPop <- readRDS(params$studyPopFile)
  args <- params$args
  args$cohortMethodData <- cohortMethodData
  args$population <- studyPop
  ps <- do.call("createPs", args)
  saveRDS(ps, params$psFile)
  return(NULL)
}

fitSharedPsModel <- function(params, refitPsForEveryStudyPopulation) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder, requireCovariates = TRUE)
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
  cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFolder, readOnly = TRUE)
  covariates <- cohortMethodData$covariates
  if (params$args$useCovariates) {
    if (length(params$args$includeCovariateIds) != 0) {
      idx <- ffbase::`%in%`(covariates$covariateId, ff::as.ff(params$args$includeCovariateIds))
    } else {
      idx <- ff::ff(TRUE, nrow(covariates))
    }
    if (length(params$args$excludeCovariateIds) != 0) {
      idx[idx] <- !ffbase::`%in%`(covariates$covariateId[idx], ff::as.ff(params$args$excludeCovariateIds))
    }
    if (length(params$args$interactionCovariateIds) != 0) {
      idx <- idx & ffbase::`%in%`(covariates$covariateId, ff::as.ff(params$args$interactionCovariateIds))
    }
  } else {
    idx <- ffbase::`%in%`(covariates$covariateId, ff::as.ff(params$args$interactionCovariateIds))
  }
  if (!ffbase::any.ff(idx)) {
    # ffdf cannot have zero rows. Just select first row of covariates.
    covariates <- covariates[ff::ff(as.integer(1)), ]
  } else if (ffbase::any.ff(!idx)) {
    covariates <- covariates[idx, ]
  } else {
    covariates <- ff::clone.ffdf(covariates)
  }
  covariateRef <- ff::clone.ffdf(cohortMethodData$covariateRef)
  analysisRef <- ff::clone.ffdf(cohortMethodData$analysisRef)
  ffbase::save.ffdf(covariates, covariateRef, analysisRef, dir = params$prefilteredCovariatesFolder)
  ff::close.ffdf(covariates)
  ff::close.ffdf(covariateRef)
  ff::close.ffdf(analysisRef)
  return(NULL)
}

doFitOutcomeModel <- function(params) {
  requireCovariates <- ((length(params$args$interactionCovariateIds) != 0 || params$args$useCovariates) && params$prefilteredCovariatesFolder == "")
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder, requireCovariates = requireCovariates)
  if (params$prefilteredCovariatesFolder != "") {
    covariates <- NULL
    covariateRef <- NULL
    analysisRef <- NULL
    ffbase::load.ffdf(params$prefilteredCovariatesFolder) # Loads covariates, covariateRef, and analysisRef ffdfs
    ff::open.ffdf(covariates, readonly = TRUE)
    ff::open.ffdf(covariateRef, readonly = TRUE)
    ff::open.ffdf(analysisRef, readonly = TRUE)
    cohortMethodData$covariates <- covariates
    cohortMethodData$covariateRef <- covariateRef
    cohortMethodData$analysisRef <- analysisRef
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
                                  includeCovariateIds = args$includeCovariateIds,
                                  excludeCovariateIds = args$excludeCovariateIds,
                                  interactionCovariateIds = args$interactionCovariateIds,
                                  prior = args$prior,
                                  control = args$control)
  saveRDS(outcomeModel, params$outcomeModelFile)
  return(NULL)
}

doFitOutcomeModelPlus <- function(params) {
  requireCovariates <- ((length(params$args$fitOutcomeModelArgs$interactionCovariateIds) != 0 || params$args$fitOutcomeModelArgs$useCovariates) && params$prefilteredCovariatesFolder == "")
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder, requireCovariates = requireCovariates)
  if (params$prefilteredCovariatesFolder != "") {
    covariates <- NULL
    covariateRef <- NULL
    analysisRef <- NULL
    ffbase::load.ffdf(params$prefilteredCovariatesFolder) # Loads covariates, covariateRef, and analysisRef ffdfs
    ff::open.ffdf(covariates, readonly = TRUE)
    ff::open.ffdf(covariateRef, readonly = TRUE)
    ff::open.ffdf(analysisRef, readonly = TRUE)
    cohortMethodData$covariates <- covariates
    cohortMethodData$covariateRef <- covariateRef
    cohortMethodData$analysisRef <- analysisRef
  }

  # Create study pop
  args <- params$args$createStudyPopArgs
  args$cohortMethodData <- cohortMethodData
  studyPop <- do.call("createStudyPopulation", args)
  # studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
  #                                   population = NULL,
  #                                   outcomeId = args$outcomeId,
  #                                   firstExposureOnly = args$firstExposureOnly,
  #                                   washoutPeriod = args$washoutPeriod,
  #                                   removeDuplicateSubjects = args$removeDuplicateSubjects,
  #                                   removeSubjectsWithPriorOutcome = args$removeSubjectsWithPriorOutcome,
  #                                   priorOutcomeLookback = args$priorOutcomeLookback,
  #                                   minDaysAtRisk = args$minDaysAtRisk,
  #                                   riskWindowStart = args$riskWindowStart,
  #                                   addExposureDaysToStart = args$addExposureDaysToStart,
  #                                   riskWindowEnd = args$riskWindowEnd,
  #                                   addExposureDaysToEnd = args$addExposureDaysToEnd)

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
  # outcomeModel <- fitOutcomeModel(population = args$population,
  #                                 cohortMethodData = args$cohortMethodData,
  #                                 modelType = args$modelType,
  #                                 stratified = args$stratified,
  #                                 useCovariates = args$useCovariates,
  #                                 inversePtWeighting = args$inversePtWeighting,
  #                                 includeCovariateIds = args$includeCovariateIds,
  #                                 excludeCovariateIds = args$excludeCovariateIds,
  #                                 interactionCovariateIds = args$interactionCovariateIds,
  #                                 prior = args$prior,
  #                                 control = args$control)
  saveRDS(outcomeModel, params$outcomeModelFile)
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
    rows <- data.frame(analysisId = cmAnalysis$analysisId,
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

  referenceTable <- do.call("rbind",
                            lapply(cmAnalysisList,
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
  analysisIdToLoadArgsId <- data.frame(analysisId = analysisIds, loadArgsId = loadArgsId)
  referenceTable <- merge(referenceTable, analysisIdToLoadArgsId)
  referenceTable$cohortMethodDataFolder <- .createCohortMethodDataFileName(loadId = referenceTable$loadArgsId,
                                                                           targetId = referenceTable$targetId,
                                                                           comparatorId = referenceTable$comparatorId)

  # Add studypop filenames
  studyPopArgsList <- unique(ParallelLogger::selectFromList(cmAnalysisList, "createStudyPopArgs"))
  studyPopArgsList <- lapply(studyPopArgsList, function(x) return(x[[1]]))
  studyPopArgsId <- sapply(cmAnalysisList,
                           function(cmAnalysis, studyPopArgsList) return(which.list(studyPopArgsList,
                                                                                    cmAnalysis$createStudyPopArgs)),
                           studyPopArgsList)
  analysisIdToStudyPopArgsId <- data.frame(analysisId = analysisIds,
                                           studyPopArgsId = studyPopArgsId)
  referenceTable <- merge(referenceTable, analysisIdToStudyPopArgsId)
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
  analysisIdToPsArgsId <- data.frame(analysisId = analysisIds, psArgsId = psArgsId)
  referenceTable <- merge(referenceTable, analysisIdToPsArgsId)
  idx <- !(referenceTable$psArgsId %in% noPsIds)
  referenceTable$psFile[idx] <- .createPsOutcomeFileName(loadId = referenceTable$loadArgsId[idx],
                                                         studyPopId = referenceTable$studyPopArgsId[idx],
                                                         psId = referenceTable$psArgsId[idx],
                                                         targetId = referenceTable$targetId[idx],
                                                         comparatorId = referenceTable$comparatorId[idx],
                                                         outcomeId = referenceTable$outcomeId[idx])
  referenceTable$psFile[!idx] <- ""
  if (refitPsForEveryOutcome) {
    referenceTable$sharedPsFile <- ""
  } else {
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
      analysisIdToStudyPopArgsEquivalentId <- data.frame(analysisId = analysisIds,
                                                         studyPopArgsEquivalentId = studyPopArgsEquivalentId)
      referenceTable <- merge(referenceTable, analysisIdToStudyPopArgsEquivalentId)
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
    referenceTable$sharedPsFile[!idx] <- ""
  }

  # Add strata filenames
  args <- c("trimByPs",
            "trimByPsArgs",
            "trimByPsToEquipoise",
            "trimByPsToEquipoiseArgs",
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
                                                                        strataArgs$trimByPsToEquipoise | strataArgs$matchOnPs | strataArgs$matchOnPsAndCovariates | strataArgs$stratifyByPs |
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
    analysisIdToStrataArgsId <- data.frame(analysisId = analysisIds, strataArgsId = strataArgsId)
    referenceTable <- merge(referenceTable, analysisIdToStrataArgsId)
  }
  idx <- referenceTable$strataArgsId != 0
  referenceTable$strataFile[idx] <- .createStratifiedPopFileName(loadId = referenceTable$loadArgsId[idx],
                                                                 studyPopId = referenceTable$studyPopArgsId[idx],
                                                                 psId = referenceTable$psArgsId[idx],
                                                                 strataId = referenceTable$strataArgsId[idx],
                                                                 targetId = referenceTable$targetId[idx],
                                                                 comparatorId = referenceTable$comparatorId[idx],
                                                                 outcomeId = referenceTable$outcomeId[idx])
  referenceTable$strataFile[!idx] <- ""

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
    referenceTable$prefilteredCovariatesFolder <- ""
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
    if (length(loadingFittingArgsList) == 0) {
      # No filtering needed
      referenceTable$prefilteredCovariatesFolder <- ""
    } else {
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
      analysisIdToPrefilterId <- data.frame(analysisId = analysisIds,
                                            prefilterId = sapply(matchingIds, function(matchingId, prefilterIds) if (is.null(matchingId)) -1 else prefilterIds[matchingId], prefilterIds))
      referenceTable <- merge(referenceTable, analysisIdToPrefilterId)
      referenceTable$prefilteredCovariatesFolder <- .createPrefilteredCovariatesFileName(loadId = referenceTable$loadArgsId,
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
                                       "cohortMethodDataFolder",
                                       "studyPopFile",
                                       "sharedPsFile",
                                       "psFile",
                                       "strataFile",
                                       "prefilteredCovariatesFolder",
                                       "outcomeModelFile")]
  referenceTable <- referenceTable[order(referenceTable$analysisId,
                                         referenceTable$targetId,
                                         referenceTable$comparatorId,
                                         referenceTable$outcomeId), ]
  row.names(referenceTable) <- NULL
  return(referenceTable)
}

.f <- function(x) {
  return(format(x, scientific = FALSE, trim = TRUE))
}

.createCohortMethodDataFileName <- function(loadId, targetId, comparatorId) {
  name <- paste("CmData_l", loadId, "_t", .f(targetId), "_c", .f(comparatorId), sep = "")
  return(name)
}


.createPrefilteredCovariatesFileName <- function(loadId, targetId, comparatorId, prefilterId) {
  name <- paste("Prefilter_l", loadId, "_t", .f(targetId), "_c", .f(comparatorId), "_p", prefilterId, sep = "")
  name[prefilterId == -1] <- rep("", sum(prefilterId == -1))
  return(name)
}

.createStudyPopulationFileName <- function(loadId,
                                           studyPopId,
                                           targetId,
                                           comparatorId,
                                           outcomeId) {
  name <- paste("StudyPop_l",
                loadId,
                "_s",
                studyPopId,
                "_t",
                .f(targetId),
                "_c",
                .f(comparatorId),
                sep = "")
  name <- paste(name, "_o", .f(outcomeId), sep = "")
  name <- paste(name, ".rds", sep = "")
  return(name)
}

.createPsFileName <- function(loadId, studyPopId, psId, targetId, comparatorId) {
  if (is.null(studyPopId)) {
    name <- paste("Ps_l",
                  loadId,
                  "_p",
                  psId,
                  "_t",
                  .f(targetId),
                  "_c",
                  .f(comparatorId),
                  sep = "")
  } else {
    name <- paste("Ps_l",
                  loadId,
                  "_s",
                  studyPopId,
                  "_p",
                  psId,
                  "_t",
                  .f(targetId),
                  "_c",
                  .f(comparatorId),
                  sep = "")
  }
  name <- paste(name, ".rds", sep = "")
  return(name)
}

.createPsOutcomeFileName <- function(loadId,
                                     studyPopId,
                                     psId,
                                     targetId,
                                     comparatorId,
                                     outcomeId) {
  name <- paste("Ps_l",
                loadId,
                "_s",
                studyPopId,
                "_p",
                psId,
                "_t",
                .f(targetId),
                "_c",
                .f(comparatorId),
                sep = "")
  name <- paste(name, "_o", .f(outcomeId), sep = "")
  name <- paste(name, ".rds", sep = "")
  return(name)
}

.createStratifiedPopFileName <- function(loadId,
                                         studyPopId,
                                         psId,
                                         strataId,
                                         targetId,
                                         comparatorId,
                                         outcomeId) {
  name <- paste("StratPop_l",
                loadId,
                "_s",
                studyPopId,
                "_p",
                psId,
                "_t",
                .f(targetId),
                "_c",
                .f(comparatorId),
                sep = "")
  name <- paste(name, "_s", strataId, sep = "")
  name <- paste(name, "_o", .f(outcomeId), sep = "")
  name <- paste(name, ".rds", sep = "")
  return(name)
}

.createOutcomeModelFileName <- function(folder, targetId, comparatorId, outcomeId) {
  name <- paste("om_t", .f(targetId), "_c", .f(comparatorId), sep = "")
  name <- paste(name, "_o", .f(outcomeId), sep = "")
  name <- paste(name, ".rds", sep = "")
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
#' @param referenceTable   A data.frame as created by the \code{\link{runCmAnalyses}} function.
#' @param outputFolder     Name of the folder where all the outputs have been written to.
#'
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{targetId} \tab The ID of the target drug.\cr
#' \verb{comparatorId} \tab The ID of the comparator group.\cr \verb{indicationConceptIds} \tab The
#' ID(s) of indications in which to nest to study. \cr \verb{outcomeId} \tab The ID of the outcome.\cr
#' \verb{rr} \tab The estimated effect size.\cr \verb{ci95lb} \tab The lower bound of the 95 percent
#' confidence interval.\cr \verb{ci95ub} \tab The upper bound of the 95 percent confidence
#' interval.\cr \verb{target} \tab The number of subjects in the target group (after any trimming
#' and matching).\cr \verb{comparator} \tab The number of subjects in the comparator group (after any
#' trimming and matching).\cr \verb{eventsTarget} \tab The number of outcomes in the target group
#' (after any trimming and matching).\cr \verb{eventsComparator} \tab The number of outcomes in the
#' comparator group (after any trimming and \cr \tab matching).\cr \verb{logRr} \tab The log of the
#' estimated relative risk.\cr \verb{seLogRr} \tab The standard error of the log of the estimated
#' relative risk.\cr }
#'
#' @export
summarizeAnalyses <- function(referenceTable, outputFolder) {

  summarizeOneAnalysis <- function(outcomeModelFile, outputFolder) {
    result <- data.frame(rr = 0,
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
      if (outcomeModel$outcomeModelType %in% c("cox", "poisson")) {
        result$targetDays <- outcomeModel$timeAtRisk$targetDays
        result$comparatorDays <- outcomeModel$timeAtRisk$comparatorDays
      }
      result$eventsTarget <- outcomeModel$outcomeCounts$targetOutcomes
      result$eventsComparator <- outcomeModel$outcomeCounts$comparatorOutcomes
      result$logRr <- if (is.null(coef(outcomeModel)))
        NA else coef(outcomeModel)
      result$seLogRr <- if (is.null(coef(outcomeModel)))
        NA else outcomeModel$outcomeModelTreatmentEstimate$seLogRr
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
  results <- do.call(plyr::rbind.fill, results)
  results <- cbind(referenceTable[, columns], results)
  return(results)
}

# @file RunAnalyses.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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
#' Run a list of analyses for the drug-comparator-outcomes of interest. This function will run all
#' specified analyses against all hypotheses of interest, meaning that the total number of outcome
#' models is `length(cmAnalysisList) * length(drugComparatorOutcomesList)` (if all analyses specify an
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
#' @param drugComparatorOutcomesList     A list of objects of type \code{drugComparatorOutcomes} as
#'                                       created using the \code{\link{createDrugComparatorOutcomes}}
#'                                       function.
#' @param refitPsForEveryOutcome         Should the propensity model be fitted for every outcome (i.e.
#'                                       after people who already had the outcome are removed)? If
#'                                       false, a single propensity model will be fitted, and people
#'                                       who had the outcome previously will be removed afterwards.
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
#' @param computeCovarBalThreads         The number of parallel threads to use for computing the
#'                                       covariate balance.
#' @param trimMatchStratifyThreads       The number of parallel threads to use for trimming, matching
#'                                       and stratifying.
#' @param fitOutcomeModelThreads         The number of parallel threads to use for fitting the outcome
#'                                       models.
#' @param outcomeCvThreads               The number of parallel threads to use for the cross-
#'                                       validation when estimating the hyperparameter for the outcome
#'                                       model. Note that the total number of CV threads at one time
#'                                       could be `fitOutcomeModelThreads * outcomeCvThreads`.
#' @param outcomeIdsOfInterest           If provided, creation of non-essential files will be skipped
#'                                       for all other outcome IDs. This could be helpful to speed up
#'                                       analyses with many controls.
#'
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{targetId} \tab The ID of the target drug.\cr
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
#' after any trimming, matching or stratifying, including their strata.\cr \verb{covariateBalanceFile}
#' \tab The name of the file containing the covariate balance (ie. the \cr \tab output of the
#' \code{computeCovariateBalance} function.\cr \verb{outcomeModelFile} \tab The name of the file
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
                          drugComparatorOutcomesList,
                          refitPsForEveryOutcome = FALSE,
                          getDbCohortMethodDataThreads = 1,
                          createPsThreads = 1,
                          psCvThreads = 1,
                          createStudyPopThreads = 1,
                          trimMatchStratifyThreads = 1,
                          computeCovarBalThreads = 1,
                          fitOutcomeModelThreads = 1,
                          outcomeCvThreads = 1,
                          outcomeIdsOfInterest) {
  if (!missing(outcomeIdsOfInterest) && !is.null(outcomeIdsOfInterest) && refitPsForEveryOutcome){
    stop("Cannot have both outcomeIdsOfInterest and refitPsForEveryOutcome set to TRUE")
  }
  for (drugComparatorOutcomes in drugComparatorOutcomesList) {
    stopifnot(class(drugComparatorOutcomes) == "drugComparatorOutcomes")
  }
  for (cmAnalysis in cmAnalysisList) {
    stopifnot(class(cmAnalysis) == "cmAnalysis")
  }
  uniquedrugComparatorOutcomesList <- unique(OhdsiRTools::selectFromList(drugComparatorOutcomesList,
                                                                         c("targetId",
                                                                           "comparatorId",
                                                                           "outcomeIds")))
  if (length(uniquedrugComparatorOutcomesList) != length(drugComparatorOutcomesList)) {
    stop("Duplicate drug-comparator-outcomes combinations are not allowed")
  }
  analysisIds <- unlist(OhdsiRTools::selectFromList(cmAnalysisList, "analysisId"))
  uniqueAnalysisIds <- unique(analysisIds)
  if (length(uniqueAnalysisIds) != length(analysisIds)) {
    stop("Duplicate analysis IDs are not allowed")
  }
  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  referenceTable <- createReferenceTable(cmAnalysisList,
                                         drugComparatorOutcomesList,
                                         outputFolder,
                                         refitPsForEveryOutcome,
                                         outcomeIdsOfInterest)

  saveRDS(referenceTable, file.path(outputFolder, "outcomeModelReference.rds"))

  writeLines("*** Creating cohortMethodData objects ***")
  objectsToCreate <- list()
  for (cohortMethodDataFolder in unique(referenceTable$cohortMethodDataFolder)) {
    if (cohortMethodDataFolder != "" && !file.exists(cohortMethodDataFolder)) {
      refRow <- referenceTable[referenceTable$cohortMethodDataFolder == cohortMethodDataFolder, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
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
      outcomeIds <- unique(referenceTable$outcomeId[referenceTable$cohortMethodDataFolder == cohortMethodDataFolder])
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
      objectsToCreate[[length(objectsToCreate) + 1]] <- list(args = args,
                                                             cohortMethodDataFolder = cohortMethodDataFolder)
    }
  }

  if (length(objectsToCreate) != 0) {
    cluster <- OhdsiRTools::makeCluster(getDbCohortMethodDataThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster, objectsToCreate, createCmDataObject)
    OhdsiRTools::stopCluster(cluster)
  }

  writeLines("*** Creating study populations ***")
  objectsToCreate <- list()
  for (studyPopFile in unique(referenceTable$studyPopFile)) {
    if (studyPopFile != "" && !file.exists(studyPopFile)) {
      refRow <- referenceTable[referenceTable$studyPopFile == studyPopFile, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      args <- analysisRow$createStudyPopArgs
      args$outcomeId <- refRow$outcomeId
      objectsToCreate[[length(objectsToCreate) + 1]] <- list(cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                                                             args = args,
                                                             studyPopFile = studyPopFile)
    }
  }

  if (length(objectsToCreate) != 0) {
    cluster <- OhdsiRTools::makeCluster(createStudyPopThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster, objectsToCreate, createStudyPopObject)
    OhdsiRTools::stopCluster(cluster)
  }

  if (refitPsForEveryOutcome) {
    writeLines("*** Fitting propensity score models ***")
    modelsToFit <- list()
    for (psFile in unique(referenceTable$psFile)) {
      if (psFile != "" && !file.exists((psFile))) {
        refRow <- referenceTable[referenceTable$psFile == psFile, ][1, ]
        analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                                list(analysisId = refRow$analysisId))[[1]]
        args <- analysisRow$createPsArgs
        args$control$threads <- psCvThreads
        modelsToFit[[length(modelsToFit) + 1]] <- list(cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                                                       studyPopFile = refRow$studyPopFile,
                                                       args = args,
                                                       psFile = psFile)
      }
    }

    if (length(modelsToFit) != 0) {
      cluster <- OhdsiRTools::makeCluster(createPsThreads)
      OhdsiRTools::clusterRequire(cluster, "CohortMethod")
      dummy <- OhdsiRTools::clusterApply(cluster, modelsToFit, fitPsModel)
      OhdsiRTools::stopCluster(cluster)
    }
  } else {
    writeLines("*** Fitting shared propensity score models ***")
    modelsToFit <- list()
    for (sharedPsFile in unique(referenceTable$sharedPsFile)) {
      if (sharedPsFile != "" && !file.exists(sharedPsFile)) {
        refRow <- referenceTable[referenceTable$sharedPsFile == sharedPsFile, ][1, ]
        analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                                list(analysisId = refRow$analysisId))[[1]]

        createPsArg <- analysisRow$createPsArg
        createPsArg$control$threads <- psCvThreads
        modelsToFit[[length(modelsToFit) + 1]] <- list(cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                                                       createPsArg = createPsArg,
                                                       createStudyPopArgs = analysisRow$createStudyPopArgs,
                                                       sharedPsFile = sharedPsFile)
      }
    }

    if (length(modelsToFit) != 0) {
      cluster <- OhdsiRTools::makeCluster(createPsThreads)
      OhdsiRTools::clusterRequire(cluster, "CohortMethod")
      dummy <- OhdsiRTools::clusterApply(cluster, modelsToFit, fitSharedPsModel)
      OhdsiRTools::stopCluster(cluster)
    }
    writeLines("*** Adding propensity scores to study population objects ***")
    psFiles <- unique(referenceTable$psFile)
    psFiles <- psFiles[psFiles != ""]
    psFiles <- psFiles[!file.exists(psFiles)]
    if (length(psFiles) > 0) {
      pb <- txtProgressBar(style = 3)
      for (i in 1:length(psFiles)) {
        psFile <- psFiles[i]
        refRow <- referenceTable[referenceTable$psFile == psFile, ][1, ]
        studyPop <- readRDS(refRow$studyPopFile)
        ps <- readRDS(refRow$sharedPsFile)
        newMetaData <- attr(studyPop, "metaData")
        newMetaData$psModelCoef <- attr(ps, "metaData")$psModelCoef
        newMetaData$psModelPriorVariance <- attr(ps, "metaData")$psModelPriorVariance
        idx <- match(studyPop$rowId, ps$rowId)
        studyPop$propensityScore <- ps$propensityScore[idx]
        ps <- studyPop
        attr(ps, "metaData") <- newMetaData
        saveRDS(ps, psFile)
        setTxtProgressBar(pb, i/length(psFiles))
      }
      close(pb)
    }
  }

  writeLines("*** Trimming/Matching/Stratifying ***")
  tasks <- list()
  for (strataFile in unique(referenceTable$strataFile)) {
    if (strataFile != "" && !file.exists((strataFile))) {
      refRow <- referenceTable[referenceTable$strataFile == strataFile, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]

      tasks[[length(tasks) + 1]] <- list(psFile = refRow$psFile,
                                         args = analysisRow,
                                         strataFile = strataFile)
    }
  }
  if (length(tasks) != 0) {
    cluster <- OhdsiRTools::makeCluster(trimMatchStratifyThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster, tasks, trimMatchStratify)
    OhdsiRTools::stopCluster(cluster)
  }

  writeLines("*** Computing covariate balance ***")
  tasks <- list()
  for (covariateBalanceFile in unique(referenceTable$covariateBalanceFile)) {
    if (covariateBalanceFile != "" && !file.exists((covariateBalanceFile))) {
      refRow <- referenceTable[referenceTable$covariateBalanceFile == covariateBalanceFile, ][1, ]
      tasks[[length(tasks) + 1]] <- list(strataFile = refRow$strataFile,
                                         cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                                         covariateBalanceFile = refRow$covariateBalanceFile)
    }
  }

  if (length(tasks) != 0) {
    cluster <- OhdsiRTools::makeCluster(computeCovarBalThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster, tasks, computeCovarBal)
    OhdsiRTools::stopCluster(cluster)
  }

  if (missing(outcomeIdsOfInterest) || is.null(outcomeIdsOfInterest)) {
    writeLines("*** Fitting outcome models ***")
  } else {
    writeLines("*** Fitting outcome models for outcomes of interest ***")
  }
  modelsToFit <- list()
  for (i in 1:nrow(referenceTable)) {
    if (referenceTable$outcomeOfInterest[i]) {
      outcomeModelFile <- referenceTable$outcomeModelFile[i]
      if (outcomeModelFile != "" && !file.exists((outcomeModelFile))) {
        refRow <- referenceTable[referenceTable$outcomeModelFile == outcomeModelFile, ][1, ]
        analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                                list(analysisId = refRow$analysisId))[[1]]
        args <- analysisRow$fitOutcomeModelArgs
        args$control$threads <- outcomeCvThreads
        if (refRow$strataFile != "") {
          studyPopFile <- refRow$strataFile
        } else {
          studyPopFile <- refRow$studyPopFile
        }
        modelsToFit[[length(modelsToFit) + 1]] <- list(cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                                                       args = args,
                                                       studyPopFile = studyPopFile,
                                                       outcomeModelFile = outcomeModelFile)
      }
    }
  }
  if (length(modelsToFit) != 0) {
    cluster <- OhdsiRTools::makeCluster(fitOutcomeModelThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster, modelsToFit, doFitOutcomeModel)
    OhdsiRTools::stopCluster(cluster)
  }

  if (!missing(outcomeIdsOfInterest) && !is.null(outcomeIdsOfInterest)) {
    writeLines("*** Fitting outcome models for other outcomes ***")
    subset <- referenceTable[!referenceTable$outcomeOfInterest & referenceTable$outcomeModelFile != "" & !file.exists(referenceTable$outcomeModelFile) , ]
    createArgs <- function(i) {
      refRow <- subset[i, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      analysisRow$fitOutcomeModelArgs$control$threads <- outcomeCvThreads
      analysisRow$createStudyPopArgs$outcomeId <- refRow$outcomeId
      params <- list(cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                     sharedPsFile = refRow$sharedPsFile,
                     args = analysisRow,
                     outcomeModelFile = refRow$outcomeModelFile)
      return(params)
    }
    if (nrow(subset) == 0) {
      modelsToFit <- list()
    } else {
      modelsToFit <- lapply(1:nrow(subset), createArgs)
    }

    if (length(modelsToFit) != 0) {
      cluster <- OhdsiRTools::makeCluster(fitOutcomeModelThreads)
      OhdsiRTools::clusterRequire(cluster, "CohortMethod")
      dummy <- OhdsiRTools::clusterApply(cluster, modelsToFit, doFitOutcomeModelPlus)
      OhdsiRTools::stopCluster(cluster)
    }
  }
  invisible(referenceTable)
}

getCohortMethodData <- function(cohortMethodDataFolder) {
  if (mget("cohortMethodDataFolder", envir = globalenv(), ifnotfound = "") == cohortMethodDataFolder) {
    cohortMethodData <- get("cohortMethodData", envir = globalenv())
  } else {
    cohortMethodData <- loadCohortMethodData(cohortMethodDataFolder, readOnly = TRUE)
    assign("cohortMethodData", cohortMethodData, envir = globalenv())
    assign("cohortMethodDataFolder", cohortMethodDataFolder, envir = globalenv())
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
  saveCohortMethodData(cohortMethodData, params$cohortMethodDataFolder)
  return(NULL)
}

createStudyPopObject <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder)
  args <- params$args
  args$cohortMethodData <- cohortMethodData
  studyPop <- do.call("createStudyPopulation", args)
  saveRDS(studyPop, params$studyPopFile)
  return(NULL)
}

fitPsModel <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder)
  studyPop <- readRDS(params$studyPopFile)
  args <- params$args
  args$cohortMethodData <- cohortMethodData
  args$population <- studyPop
  ps <- do.call("createPs", args)
  saveRDS(ps, params$psFile)
  return(NULL)
}

fitSharedPsModel <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder)
  args <- params$createStudyPopArgs
  args$cohortMethodData <- cohortMethodData
  studyPop <- do.call("createStudyPopulation", args)
  args <- params$createPsArg
  args$cohortMethodData <- cohortMethodData
  args$population <- studyPop
  ps <- do.call("createPs", args)
  saveRDS(ps, params$sharedPsFile)
  return(NULL)
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

computeCovarBal <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder)
  strata <- readRDS(params$strataFile)
  balance <- computeCovariateBalance(strata, cohortMethodData)
  saveRDS(balance, params$covariateBalanceFile)
  return(NULL)
}

doFitOutcomeModel <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder)
  studyPop <- readRDS(params$studyPopFile)
  args <- list(cohortMethodData = cohortMethodData, population = studyPop)
  args <- append(args, params$args)
  # outcomeModel <- do.call('fitOutcomeModel', args)
  outcomeModel <- fitOutcomeModel(population = args$population,
                                  cohortMethodData = args$cohortMethodData,
                                  modelType = args$modelType,
                                  stratified = args$stratified,
                                  useCovariates = args$useCovariates,
                                  prior = args$prior,
                                  control = args$control)
  saveRDS(outcomeModel, params$outcomeModelFile)
  return(NULL)
}

doFitOutcomeModelPlus <- function(params) {
  cohortMethodData <- getCohortMethodData(params$cohortMethodDataFolder)

  # Create study pop
  args <- params$args$createStudyPopArgs
  # args$cohortMethodData <- cohortMethodData
  # studyPop <- do.call("createStudyPopulation", args)
  studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                    population = NULL,
                                    outcomeId = args$outcomeId,
                                    firstExposureOnly = args$firstExposureOnly,
                                    washoutPeriod = args$washoutPeriod,
                                    removeDuplicateSubjects = args$removeDuplicateSubjects,
                                    removeSubjectsWithPriorOutcome = args$removeSubjectsWithPriorOutcome,
                                    priorOutcomeLookback = args$priorOutcomeLookback,
                                    minDaysAtRisk = args$minDaysAtRisk,
                                    riskWindowStart = args$riskWindowStart,
                                    addExposureDaysToStart = args$addExposureDaysToStart,
                                    riskWindowEnd = args$riskWindowEnd,
                                    addExposureDaysToEnd = args$addExposureDaysToEnd)

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
  # outcomeModel <- do.call('fitOutcomeModel', args)
  outcomeModel <- fitOutcomeModel(population = args$population,
                                  cohortMethodData = args$cohortMethodData,
                                  modelType = args$modelType,
                                  stratified = args$stratified,
                                  useCovariates = args$useCovariates,
                                  prior = args$prior,
                                  control = args$control)
  saveRDS(outcomeModel, params$outcomeModelFile)
  return(NULL)
}


createReferenceTable <- function(cmAnalysisList,
                                 drugComparatorOutcomesList,
                                 outputFolder,
                                 refitPsForEveryOutcome,
                                 outcomeIdsOfInterest) {
  # Create all rows per target-comparator-outcome-analysis combination:
  analysisIds <- unlist(OhdsiRTools::selectFromList(cmAnalysisList, "analysisId"))
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
    analysisFolder <- file.path(folder, paste("Analysis_", cmAnalysis$analysisId, sep = ""))
    if (!file.exists(analysisFolder))
      dir.create(analysisFolder)

    return(do.call("rbind", lapply(dcos, instantiateDco, cmAnalysis, analysisFolder)))
  }

  referenceTable <- do.call("rbind",
                            lapply(cmAnalysisList,
                                   instantiateDcos,
                                   dcos = drugComparatorOutcomesList,
                                   folder = outputFolder))

  # Find unique load operations
  which.list <- function(list, object) {
    return(do.call("c", lapply(1:length(list), function(i, list, object) {
      if (identical(list[[i]], object)) return(i) else return(c())
    }, list, object)))
  }

  loadingArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisList,
                                                        "getDbCohortMethodDataArgs"))
  loadingArgsList <- lapply(loadingArgsList, function(x) return(x[[1]]))
  loadArgsId <- sapply(cmAnalysisList,
                       function(cmAnalysis, loadingArgsList) return(which.list(loadingArgsList,
                                                                               cmAnalysis$getDbCohortMethodDataArgs)),
                       loadingArgsList)
  analysisIdToLoadArgsId <- data.frame(analysisId = analysisIds, loadArgsId = loadArgsId)
  referenceTable <- merge(referenceTable, analysisIdToLoadArgsId)
  referenceTable$cohortMethodDataFolder <- .createCohortMethodDataFileName(folder = outputFolder,
                                                                           loadId = referenceTable$loadArgsId,
                                                                           targetId = referenceTable$targetId,
                                                                           comparatorId = referenceTable$comparatorId)

  # Add studypop filenames
  studyPopArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisList, "createStudyPopArgs"))
  studyPopArgsList <- lapply(studyPopArgsList, function(x) return(x[[1]]))
  studyPopArgsId <- sapply(cmAnalysisList,
                           function(cmAnalysis, studyPopArgsList) return(which.list(studyPopArgsList,
                                                                                    cmAnalysis$createStudyPopArgs)),
                           studyPopArgsList)
  analysisIdToStudyPopArgsId <- data.frame(analysisId = analysisIds,
                                           studyPopArgsId = studyPopArgsId)
  referenceTable <- merge(referenceTable, analysisIdToStudyPopArgsId)
  referenceTable$studyPopFile <- .createStudyPopulationFileName(folder = outputFolder,
                                                                loadId = referenceTable$loadArgsId,
                                                                studyPopId = referenceTable$studyPopArgsId,
                                                                targetId = referenceTable$targetId,
                                                                comparatorId = referenceTable$comparatorId,
                                                                outcomeId = referenceTable$outcomeId)

  # Add PS filenames
  psArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisList, "createPsArgs"))
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
  referenceTable$psFile[idx] <- .createPsOutcomeFileName(folder = outputFolder,
                                                         loadId = referenceTable$loadArgsId[idx],
                                                         studyPopId = referenceTable$studyPopArgsId[idx],
                                                         psId = referenceTable$psArgsId[idx],
                                                         targetId = referenceTable$targetId[idx],
                                                         comparatorId = referenceTable$comparatorId[idx],
                                                         outcomeId = referenceTable$outcomeId[idx])
  referenceTable$psFile[!idx] <- ""
  if (refitPsForEveryOutcome) {
    referenceTable$sharedPsFile <- ""
  } else {
    # Find equivalent studyPopArgs, so we can reuse PS over those as well:
    studyPopArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisList, "createStudyPopArgs"))
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
    referenceTable$sharedPsFile[idx] <- .createPsFileName(folder = outputFolder,
                                                          loadId = referenceTable$loadArgsId[idx],
                                                          studyPopId = referenceTable$studyPopArgsEquivalentId[idx],
                                                          psId = referenceTable$psArgsId[idx],
                                                          targetId = referenceTable$targetId[idx],
                                                          comparatorId = referenceTable$comparatorId[idx])
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
  strataArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisList, args))
  strataArgsList <- strataArgsList[sapply(strataArgsList,
                                          function(strataArgs) return(strataArgs$trimByPs |
                                                                        strataArgs$trimByPsToEquipoise | strataArgs$matchOnPs | strataArgs$matchOnPsAndCovariates | strataArgs$stratifyByPs |
                                                                        strataArgs$stratifyByPsAndCovariates))]
  if (length(strataArgsList) == 0) {
    referenceTable$strataArgsId <- 0
  } else {
    strataArgsId <- sapply(cmAnalysisList, function(cmAnalysis, strataArgsList) {
      i <- which.list(strataArgsList, cmAnalysis[args][!is.na(names(cmAnalysis[args]))])
      if (is.null(i))
        i <- 0
      return(i)
    }, strataArgsList)
    # strataArgsId[strataArgsId %in% noStrataIds] <- NA
    analysisIdToStrataArgsId <- data.frame(analysisId = analysisIds, strataArgsId = strataArgsId)
    referenceTable <- merge(referenceTable, analysisIdToStrataArgsId)
  }
  idx <- referenceTable$strataArgsId != 0
  referenceTable$strataFile[idx] <- .createStratifiedPopFileName(folder = outputFolder,
                                                                 loadId = referenceTable$loadArgsId[idx],
                                                                 studyPopId = referenceTable$studyPopArgsId[idx],
                                                                 psId = referenceTable$psArgsId[idx],
                                                                 strataId = referenceTable$strataArgsId[idx],
                                                                 targetId = referenceTable$targetId[idx],
                                                                 comparatorId = referenceTable$comparatorId[idx],
                                                                 outcomeId = referenceTable$outcomeId[idx])
  referenceTable$strataFile[!idx] <- ""

  # Add covariateBalance filenames
  idx <- which.list(OhdsiRTools::selectFromList(cmAnalysisList, "computeCovariateBalance"),
                    list(computeCovariateBalance = TRUE))
  balIds <- analysisIds[idx]
  idx <- referenceTable$analysisId %in% balIds
  referenceTable$covariateBalanceFile[idx] <- .createCovariateBalanceFileName(folder = outputFolder,
                                                                              loadId = referenceTable$loadArgsId[idx],
                                                                              studyPopId = referenceTable$studyPopArgsId[idx],
                                                                              psId = referenceTable$psArgsId[idx],
                                                                              strataId = referenceTable$strataArgsId[idx],
                                                                              targetId = referenceTable$targetId[idx],
                                                                              comparatorId = referenceTable$comparatorId[idx],
                                                                              outcomeId = referenceTable$outcomeId[idx])
  referenceTable$covariateBalanceFile[!idx] <- ""

  # Add interest flag
  if (missing(outcomeIdsOfInterest) || is.null(outcomeIdsOfInterest)) {
    referenceTable$outcomeOfInterest <- TRUE
  } else {
    referenceTable$outcomeOfInterest <- FALSE
    referenceTable$outcomeOfInterest[referenceTable$outcomeId %in% outcomeIdsOfInterest] <- TRUE
    referenceTable$studyPopFile[!referenceTable$outcomeOfInterest] <- ""
    referenceTable$psFile[!referenceTable$outcomeOfInterest] <- ""
    referenceTable$strataFile[!referenceTable$outcomeOfInterest] <- ""
    referenceTable$covariateBalanceFile[!referenceTable$outcomeOfInterest] <- ""
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
                                       "covariateBalanceFile",
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

.createCohortMethodDataFileName <- function(folder, loadId, targetId, comparatorId) {
  name <- paste("CmData_l", loadId, "_t", .f(targetId), "_c", .f(comparatorId), sep = "")
  return(file.path(folder, name))
}

.createStudyPopulationFileName <- function(folder,
                                           loadId,
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
  return(file.path(folder, name))
}

.createPsFileName <- function(folder, loadId, studyPopId, psId, targetId, comparatorId) {
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
  name <- paste(name, ".rds", sep = "")
  return(file.path(folder, name))
}

.createPsOutcomeFileName <- function(folder,
                                     loadId,
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
  return(file.path(folder, name))
}

.createStratifiedPopFileName <- function(folder,
                                         loadId,
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
  return(file.path(folder, name))
}

.createCovariateBalanceFileName <- function(folder,
                                            loadId,
                                            studyPopId,
                                            psId,
                                            strataId,
                                            targetId,
                                            comparatorId,
                                            outcomeId) {
  name <- paste("Bal_l",
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
  return(file.path(folder, name))
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
#'
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{targetId} \tab The ID of the target drug.\cr
#' \verb{comparatorId} \tab The ID of the comparator group.\cr \verb{indicationConceptIds} \tab The
#' ID(s) of indications in which to nest to study. \cr \verb{outcomeId} \tab The ID of the outcome.\cr
#' \verb{rr} \tab The estimated effect size.\cr \verb{ci95lb} \tab The lower bound of the 95 percent
#' confidence interval.\cr \verb{ci95ub} \tab The upper bound of the 95 percent confidence
#' interval.\cr \verb{treated} \tab The number of subjects in the treated group (after any trimming
#' and matching).\cr \verb{comparator} \tab The number of subjects in the comparator group (after any
#' trimming and matching).\cr \verb{eventsTreated} \tab The number of outcomes in the treated group
#' (after any trimming and matching).\cr \verb{eventsComparator} \tab The number of outcomes in the
#' comparator group (after any trimming and \cr \tab matching).\cr \verb{logRr} \tab The log of the
#' estimated relative risk.\cr \verb{seLogRr} \tab The standard error of the log of the estimated
#' relative risk.\cr }
#'
#' @export
summarizeAnalyses <- function(referenceTable) {
  columns <- c("analysisId", "targetId", "comparatorId", "outcomeId")
  result <- referenceTable[, columns]
  result$rr <- 0
  result$ci95lb <- 0
  result$ci95ub <- 0
  result$p <- 1
  result$treated <- 0
  result$comparator <- 0
  result$treatedDays <- NA
  result$comparatorDays <- NA
  result$eventsTreated <- 0
  result$eventsComparator <- 0
  result$logRr <- 0
  result$seLogRr <- 0
  for (i in 1:nrow(referenceTable)) {
    if (referenceTable$outcomeModelFile[i] != "") {
      outcomeModel <- readRDS(referenceTable$outcomeModelFile[i])
      result$rr[i] <- if (is.null(coef(outcomeModel)))
        NA else exp(coef(outcomeModel))
      result$ci95lb[i] <- if (is.null(coef(outcomeModel)))
        NA else exp(confint(outcomeModel)[1])
      result$ci95ub[i] <- if (is.null(coef(outcomeModel)))
        NA else exp(confint(outcomeModel)[2])
      if (is.null(coef(outcomeModel))) {
        result$p[i] <- NA
      } else {
        z <- coef(outcomeModel)/outcomeModel$outcomeModelTreatmentEstimate$seLogRr
        result$p[i] <- 2 * pmin(pnorm(z), 1 - pnorm(z))
      }
      result$treated[i] <- outcomeModel$populationCounts$treatedPersons
      result$comparator[i] <- outcomeModel$populationCounts$comparatorPersons
      if (outcomeModel$outcomeModelType %in% c("cox", "poisson")) {
        result$treatedDays[i] <- outcomeModel$timeAtRisk$treatedDays
        result$comparatorDays[i] <- outcomeModel$timeAtRisk$comparatorDays
      }
      result$eventsTreated[i] <- outcomeModel$outcomeCounts$treatedOutcomes
      result$eventsComparator[i] <- outcomeModel$outcomeCounts$comparatorOutcomes

      # if (referenceTable$strataFile[i] == "") {
      #   studyPop <- readRDS(referenceTable$studyPopFile[i])
      # } else {
      #   studyPop <- readRDS(referenceTable$strataFile[i])
      # }
      # result$treated[i] <- sum(studyPop$treatment == 1)
      # result$comparator[i] <- sum(studyPop$treatment == 0)
      # if (outcomeModel$outcomeModelType == "cox") {
      #   result$treatedDays[i] <- sum(studyPop$survivalTime[studyPop$treatment == 1])
      #   result$comparatorDays[i] <- sum(studyPop$survivalTime[studyPop$treatment == 0])
      # } else if (outcomeModel$outcomeModelType == "poisson") {
      #   result$treatedDays[i] <- sum(studyPop$timeAtRisk[studyPop$treatment == 1])
      #   result$comparatorDays[i] <- sum(studyPop$timeAtRisk[studyPop$treatment == 0])
      # }
      # if (outcomeModel$outcomeModelType == "cox" || outcomeModel$outcomeModelType == "logistic") {
      #   result$eventsTreated[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 1] != 0)
      #   result$eventsComparator[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 0] != 0)
      # } else {
      #   result$eventsTreated[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 1])
      #   result$eventsComparator[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 0])
      # }

      result$logRr[i] <- if (is.null(coef(outcomeModel)))
        NA else coef(outcomeModel)
      result$seLogRr[i] <- if (is.null(coef(outcomeModel)))
        NA else outcomeModel$outcomeModelTreatmentEstimate$seLogRr
    }
  }
  return(result)
}

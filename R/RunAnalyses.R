# @file RunAnalyses.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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
#' @param connectionDetails                     An R object of type \code{connectionDetails} created
#'                                              using the function \code{createConnectionDetails} in
#'                                              the \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                     The name of the database schema that contains the OMOP
#'                                              CDM instance. Requires read permissions to this
#'                                              database. On SQL Server, this should specifiy both the
#'                                              database and the schema, so for example
#'                                              'cdm_instance.dbo'.
#' @param oracleTempSchema                      For Oracle only: the name of the database schema where
#'                                              you want all temporary tables to be managed. Requires
#'                                              create/insert permissions to this database.
#' @param exposureDatabaseSchema                The name of the database schema that is the location
#'                                              where the exposure data used to define the exposure
#'                                              cohorts is available. If exposureTable = DRUG_ERA,
#'                                              exposureDatabaseSchema is not used by assumed to be
#'                                              cdmSchema.  Requires read permissions to this database.
#' @param exposureTable                         The tablename that contains the exposure cohorts.  If
#'                                              exposureTable <> DRUG_ERA, then expectation is
#'                                              exposureTable has format of COHORT table:
#'                                              COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                              COHORT_END_DATE.
#' @param outcomeDatabaseSchema                 The name of the database schema that is the location
#'                                              where the data used to define the outcome cohorts is
#'                                              available. If exposureTable = CONDITION_ERA,
#'                                              exposureDatabaseSchema is not used by assumed to be
#'                                              cdmSchema.  Requires read permissions to this database.
#' @param outcomeTable                          The tablename that contains the outcome cohorts.  If
#'                                              outcomeTable <> CONDITION_OCCURRENCE, then expectation
#'                                              is outcomeTable has format of COHORT table:
#'                                              COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                              COHORT_END_DATE.
#' @param outputFolder                          Name of the folder where all the outputs will written
#'                                              to.
#' @param cmAnalysisList                        A list of objects of type \code{cmAnalysis} as created
#'                                              using the \code{\link{createCmAnalysis}} function.
#' @param drugComparatorOutcomesList            A list of objects of type \code{drugComparatorOutcomes}
#'                                              as created using the
#'                                              \code{\link{createdrugComparatorOutcomes}} function.
#' @param refitPsForEveryOutcome                Should the propensity model be fitted for every outcome
#'                                              (i.e. after people who already had the outcome are
#'                                              removed)? If false, a single propensity model will be
#'                                              fitted, and people who had the outcome previously will
#'                                              be removed afterwards.
#' @param underSampleComparatorToTreatedRatio   If the comparator group size exceeds the treated group
#'                                              size by this factor, the comparator group will be
#'                                              down-sampled before fitting the PS model. This can be
#'                                              useful when the comparator group is extremely large.
#' @param getDbCohortMethodDataThreads          The number of parallel threads to use for building the
#'                                              cohortMethod data objects.
#' @param createPsThreads                       The number of parallel threads to use for fitting the
#'                                              propensity models.
#' @param fitOutcomeModelThreads                The number of parallel threads to use for fitting the
#'                                              outcome models.
#'
#' @export
runCmAnalyses <- function(connectionDetails,
                          cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "condition_occurrence",
                          outputFolder = "./CohortMethodOutput",
                          cmAnalysisList,
                          drugComparatorOutcomesList,
                          refitPsForEveryOutcome = FALSE,
                          underSampleComparatorToTreatedRatio = 0,
                          getDbCohortMethodDataThreads = 1,
                          createPsThreads = 1,
                          trimMatchStratifyThreads = 1,
                          fitOutcomeModelThreads = 1) {
  for (drugComparatorOutcomes in drugComparatorOutcomesList) {
    stopifnot(class(drugComparatorOutcomes) == "drugComparatorOutcomes")
  }
  for (cmAnalysis in cmAnalysisList) {
    stopifnot(class(cmAnalysis) == "cmAnalysis")
  }
  uniquedrugComparatorOutcomesList <- unique(OhdsiRTools::selectFromList(drugComparatorOutcomesList,
                                                                         c("targetDrugConceptId",
                                                                                                       "comparatorDrugConceptId",
                                                                                                       "outcomeConceptIds",
                                                                                                       "indicationConceptIds")))
  if (length(uniquedrugComparatorOutcomesList) != length(drugComparatorOutcomesList)) {
    stop("Duplicate drug-comparator-indication-outcomes combinations are not allowed")
  }
  uniqueAnalysisIds <- unlist(unique(OhdsiRTools::selectFromList(cmAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(cmAnalysisList)) {
    stop("Duplicate analysis IDs are not allowed")
  }
  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  ### Create reference table ###
  allButFitOutcomeModelArgsList <- unique(OhdsiRTools::excludeFromList(cmAnalysisList,
                                                                       c("analysisId",
                                                                                         "description",
                                                                                         "fitOutcomeModel",
                                                                                         "fitOutcomeModelArgs")))
  outcomeReference <- data.frame()
  loadingArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisList,
                                                        c("getDbCohortMethodDataArgs",
                                                                          "targetType",
                                                                          "comparatorType",
                                                                          "indicationType")))
  for (i in 1:length(loadingArgsList)) {
    loadingArgs <- loadingArgsList[[i]]
    drugComparatorList <- unique(OhdsiRTools::selectFromList(drugComparatorOutcomesList,
                                                             c("targetDrugConceptId",
                                                                                           "comparatorDrugConceptId",
                                                                                           "indicationConceptIds",
                                                                                           "exclusionConceptIds",
                                                                                           "excludedCovariateConceptIds",
                                                                                           "includedCovariateConceptIds")))
    for (drugComparator in drugComparatorList) {
      drugComparatorOutcomes <- OhdsiRTools::matchInList(drugComparatorOutcomesList, drugComparator)
      outcomeConceptIds <- unique(unlist(OhdsiRTools::selectFromList(drugComparatorOutcomes,
                                                                     "outcomeConceptIds")))
      targetDrugConceptId <- .selectByType(loadingArgs$targetType,
                                           drugComparator$targetDrugConceptId,
                                           "target")
      comparatorDrugConceptId <- .selectByType(loadingArgs$comparatorType,
                                               drugComparator$comparatorDrugConceptId,
                                               "comparator")
      indicationConceptIds <- .selectByType(loadingArgs$indicationType,
                                            drugComparator$indicationConceptIds,
                                            "indication")
      cohortMethodDataFolder <- .createCohortMethodDataFileName(outputFolder,
                                                                i,
                                                                targetDrugConceptId,
                                                                comparatorDrugConceptId,
                                                                indicationConceptIds)
      cmAnalysisSubset <- OhdsiRTools::matchInList(cmAnalysisList, loadingArgs)
      createPsArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisSubset,
                                                             c("createPs", "createPsArgs")))
      for (j in 1:length(createPsArgsList)) {
        createPsArgs <- createPsArgsList[[j]]
        if (createPsArgs$createPs) {
          sharedPsFile <- .createPsFileName(outputFolder,
                                            i,
                                            targetDrugConceptId,
                                            comparatorDrugConceptId,
                                            indicationConceptIds,
                                            j)
        } else {
          sharedPsFile <- ""
        }
        allButFitOutcomeModelArgsSubset <- OhdsiRTools::matchInList(OhdsiRTools::matchInList(allButFitOutcomeModelArgsList,
                                                                                             loadingArgs), createPsArgs)
        for (k in 1:length(allButFitOutcomeModelArgsSubset)) {
          allButFitOutcomeModelArgs <- allButFitOutcomeModelArgsSubset[[k]]
          cmAnalysisSubset <- OhdsiRTools::matchInList(cmAnalysisList, allButFitOutcomeModelArgs)
          for (cmAnalysisArgs in cmAnalysisSubset) {
          analysisFolder <- file.path(outputFolder,
                                      paste("Analysis_", cmAnalysisArgs$analysisId, sep = ""))
          if (!file.exists(analysisFolder))
            dir.create(analysisFolder)
          for (outcomeConceptId in outcomeConceptIds) {
            if (cmAnalysisArgs$createPs) {
            psFileName <- .createPsOutcomeFileName(outputFolder,
                                                   i,
                                                   drugComparator$targetDrugConceptId,
                                                   drugComparator$comparatorDrugConceptId,
                                                   drugComparator$indicationConceptIds,
                                                   j,
                                                   outcomeConceptId)
            } else {
            psFileName <- ""
            }
            if (cmAnalysisArgs$trimByPs || cmAnalysisArgs$trimByPsToEquipoise || cmAnalysisArgs$matchOnPs ||
            cmAnalysisArgs$matchOnPsAndCovariates || cmAnalysisArgs$stratifyByPs || cmAnalysisArgs$stratifyByPsAndCovariates) {
            subPopFileName <- .createSubPopFileName(outputFolder,
                                                    i,
                                                    drugComparator$targetDrugConceptId,
                                                    drugComparator$comparatorDrugConceptId,
                                                    drugComparator$indicationConceptIds,
                                                    j,
                                                    k,
                                                    outcomeConceptId)
            } else {
            subPopFileName <- ""
            }
            if (cmAnalysisArgs$fitOutcomeModel) {
            outcomeModelFileName <- .createOutcomeModelFileName(analysisFolder,
                                                                drugComparator$targetDrugConceptId,
                                                                drugComparator$comparatorDrugConceptId,
                                                                drugComparator$indicationConceptIds,
                                                                outcomeConceptId)
            } else {
            outcomeModelFileName <- ""
            }
            outcomeReferenceRow <- data.frame(analysisId = cmAnalysisArgs$analysisId,
                                              targetDrugConceptId = drugComparator$targetDrugConceptId,
                                              comparatorDrugConceptId = drugComparator$comparatorDrugConceptId,
                                              indicationConceptIds = paste(drugComparator$indicationConceptIds,
                                                                           collapse = ","),
                                              exclusionConceptIds = paste(drugComparator$exclusionConceptIds,
                                                                          collapse = ","),
                                              excludedCovariateConceptIds = paste(drugComparator$excludedCovariateConceptIds,
                                                                                  collapse = ","),
                                              includedCovariateConceptIds = paste(drugComparator$includedCovariateConceptIds,
                                                                                  collapse = ","),
                                              outcomeConceptId = outcomeConceptId,
                                              cohortMethodDataFolder = cohortMethodDataFolder,
                                              sharedPsFile = sharedPsFile,
                                              psFile = psFileName,
                                              subPopFile = subPopFileName,
                                              outcomeModelFile = outcomeModelFileName,
                                              stringsAsFactors = FALSE)
            outcomeReference <- rbind(outcomeReference, outcomeReferenceRow)
          }
          }
        }
      }
    }
  }
  write.csv(outcomeReference, file.path(outputFolder,
                                        "outcomeModelReference.csv"), row.names = FALSE)

  writeLines("*** Creating cohortMethodData objects ***")
  objectsToCreate <- list()
  for (cohortMethodDataFolder in unique(outcomeReference$cohortMethodDataFolder)) {
    if (cohortMethodDataFolder != "" && !file.exists((cohortMethodDataFolder))) {
      refRow <- outcomeReference[outcomeReference$cohortMethodDataFolder == cohortMethodDataFolder, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      getDbCohortMethodDataArgs <- analysisRow$getDbCohortMethodDataArgs
      indicationConceptIds <- as.numeric(unlist(strsplit(refRow$indicationConceptIds, ",")))
      exclusionConceptIds <- unique(c(as.numeric(unlist(strsplit(refRow$exclusionConceptIds, ","))),
                                      getDbCohortMethodDataArgs$exclusionConceptIds))
      excludedCovariateConceptIds <- unique(c(as.numeric(unlist(strsplit(refRow$excludedCovariateConceptIds,
                                                                         ","))),
                                              getDbCohortMethodDataArgs$covariateSettings$excludedCovariateConceptIds))
      includedCovariateConceptIds <- unique(c(as.numeric(unlist(strsplit(refRow$includedCovariateConceptIds,
                                                                         ","))),
                                              getDbCohortMethodDataArgs$covariateSettings$includedCovariateConceptIds))
      outcomeConceptIds <- unique(outcomeReference$outcomeConceptId[outcomeReference$cohortMethodDataFolder ==
        cohortMethodDataFolder])
      args <- list(connectionDetails = connectionDetails,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   exposureDatabaseSchema = exposureDatabaseSchema,
                   exposureTable = exposureTable,
                   outcomeDatabaseSchema = outcomeDatabaseSchema,
                   outcomeTable = outcomeTable,
                   outcomeConceptIds = outcomeConceptIds,
                   targetDrugConceptId = targetDrugConceptId,
                   comparatorDrugConceptId = comparatorDrugConceptId,
                   indicationConceptIds = indicationConceptIds,
                   exclusionConceptIds = exclusionConceptIds)
      getDbCohortMethodDataArgs$exclusionConceptIds <- NULL
      getDbCohortMethodDataArgs$covariateSettings$excludedCovariateConceptIds <- excludedCovariateConceptIds
      getDbCohortMethodDataArgs$covariateSettings$includedCovariateConceptIds <- includedCovariateConceptIds
      args <- append(args, getDbCohortMethodDataArgs)
      objectsToCreate[[length(objectsToCreate) + 1]] <- list(args = args,
                                                             cohortMethodDataFolder = cohortMethodDataFolder)
    }
  }
  createCmDataObject <- function(params) {
    cohortMethodData <- do.call("getDbCohortMethodData", params$args)
    saveCohortMethodData(cohortMethodData, params$cohortMethodDataFolder)
  }
  if (length(objectsToCreate) != 0) {
    cluster <- OhdsiRTools::makeCluster(getDbCohortMethodDataThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster, objectsToCreate, createCmDataObject)
    OhdsiRTools::stopCluster(cluster)
  }

  writeLines("*** Fitting propensity score models ***")
  modelsToFit <- list()
  if (refitPsForEveryOutcome) {
    # Refit PS model for every outcome
    for (psFile in unique(outcomeReference$psFile)) {
      if (psFile != "" && !file.exists((psFile))) {
        refRow <- outcomeReference[outcomeReference$psFile == psFile, ][1, ]
        analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                                list(analysisId = refRow$analysisId))[[1]]
        outcomeConceptId <- unique(outcomeReference$outcomeConceptId[outcomeReference$psFile ==
          psFile])
        args <- analysisRow$createPsArgs
        args$outcomeConceptId <- outcomeConceptId
        modelsToFit[[length(modelsToFit) + 1]] <- list(args = args, psFile = psFile)
      }
    }
  } else {
    # Don't refit PS model for every outcome
    for (sharedPsFile in unique(outcomeReference$sharedPsFile)) {
      if (sharedPsFile != "") {
        refRow <- outcomeReference[outcomeReference$sharedPsFile == sharedPsFile, ][1, ]
        analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                                list(analysisId = refRow$analysisId))[[1]]
        idToFile <- unique(outcomeReference[outcomeReference$sharedPsFile == sharedPsFile,
                           c("outcomeConceptId", "psFile")])
        idToFile <- idToFile[!file.exists(idToFile$psFile), ]
        if (nrow(idToFile) != 0) {
          modelsToFit[[length(modelsToFit) + 1]] <- list(cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                                                         args = analysisRow$createPsArg,
                                                         idToFile = idToFile,
                                                         sharedPsFile = sharedPsFile)
        }
      }
    }
  }
  fitPSModel <- function(params, refitPsForEveryOutcome, underSampleComparatorToTreatedRatio) {

    fitModel <- function(cohortMethodData, args, underSampleComparatorToTreatedRatio) {
      if (underSampleComparatorToTreatedRatio != 0) {
        treatedSize <- sum(cohortMethodData$cohorts$treatment)
        comparatorSize <- nrow(cohortMethodData$cohorts) - treatedSize
        if (comparatorSize/treatedSize > underSampleComparatorToTreatedRatio) {
          underSample <- TRUE
        }
      }

      if (underSample) {
        cohortMethodDataSample <- sampleComparator(cohortMethodData,
                                                   underSampleComparatorToTreatedRatio)
        args$cohortMethodData <- cohortMethodDataSample
        psSample <- do.call("createPs", args)
        ps <- recomputePsForFullData(psSample, cohortMethodDataSample, cohortMethodData)
      } else {
        args$cohortMethodData < cohortMethodData
        ps <- do.call("createPs", args)
      }
      return(ps)
    }

    cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFolder, readOnly = TRUE)
    if (refitPsForEveryOutcome) {
      args <- params$args
      args$outcomeConceptId <- params$outcomeConceptId
      ps <- fitModel(cohortMethodData, args, underSampleComparatorToTreatedRatio)
      saveRDS(ps, params$psFile)
    } else {
      # Fit model once for all outcomes:
      if (file.exists(params$sharedPsFile)) {
        ps <- readRDS(params$sharedPsFile)
      } else {
        ps <- fitModel(cohortMethodData, params$args, underSampleComparatorToTreatedRatio)
        saveRDS(ps, params$sharedPsFile)
      }
      # For every outcome, filter people with prior outcomes:
      for (i in 1:nrow(params$idToFile)) {
        outcomeConceptId <- params$idToFile$outcomeConceptId[i]
        psFile <- params$idToFile$psFile[i]
        if (!file.exists(psFile)) {
          if (ffbase::any.ff(cohortMethodData$exclude$outcomeId == outcomeConceptId)) {
          filteredPs <- ps[!(ps$rowId %in% ff::as.ram(cohortMethodData$exclude$rowId[cohortMethodData$exclude$outcomeId ==
            outcomeConceptId])), ]
          } else {
          filteredPs <- ps
          }
          saveRDS(filteredPs, psFile)
        }
      }
    }
  }
  if (length(modelsToFit) != 0) {
    cluster <- OhdsiRTools::makeCluster(createPsThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster,
                                       modelsToFit,
                                       fitPSModel,
                                       refitPsForEveryOutcome,
                                       underSampleComparatorToTreatedRatio)
    OhdsiRTools::stopCluster(cluster)
  }

  writeLines("*** Trimming/Matching/Stratifying ***")
  tasks <- list()
  for (subPopFile in unique(outcomeReference$subPopFile)) {
    if (subPopFile != "" && !file.exists((subPopFile))) {
      refRow <- outcomeReference[outcomeReference$subPopFile == subPopFile, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      outcomeConceptId <- unique(outcomeReference$outcomeConceptId[outcomeReference$subPopFile ==
        subPopFile])
      tasks[[length(tasks) + 1]] <- list(psFile = refRow$psFile,
                                         args = analysisRow,
                                         subPopFile = subPopFile)
    }
  }
  trimMatchStratify <- function(params) {
    ps <- readRDS(params$psFile)
    if (params$args$trimByPs) {
      args <- list(data = ps)
      args <- append(args, params$args$trimByPsArgs)
      ps <- do.call("trimByPs", args)
    } else if (params$args$trimByPsToEquipoise) {
      args <- list(data = ps)
      args <- append(args, params$args$trimByPsToEquipoisesArgs)
      ps <- do.call("trimByPsToEquipoise", args)
    }
    if (params$args$matchOnPs) {
      args <- list(data = ps)
      args <- append(args, params$args$matchOnPsArgs)
      ps <- do.call("matchOnPs", args)
    } else if (params$args$matchOnPsAndCovariates) {
      args <- list(data = ps)
      args <- append(args, params$args$matchOnPsAndCovariatesArgs)
      ps <- do.call("matchOnPsAndCovariates", args)
    } else if (params$args$stratifyByPs) {
      args <- list(data = ps)
      args <- append(args, params$args$stratifyByPsArgs)
      ps <- do.call("stratifyByPs", args)
    } else if (params$args$stratifyByPsAndCovariates) {
      args <- list(data = ps)
      args <- append(args, params$args$stratifyByPsAndCovariatesArgs)
      ps <- do.call("stratifyByPsAndCovariates", args)
    }
    saveRDS(ps, params$subPopFile)
  }
  if (length(tasks) != 0) {
    cluster <- OhdsiRTools::makeCluster(trimMatchStratifyThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster, tasks, trimMatchStratify)
    OhdsiRTools::stopCluster(cluster)
  }

  writeLines("*** Fitting outcome models ***")
  modelsToFit <- list()
  for (outcomeModelFile in unique(outcomeReference$outcomeModelFile)) {
    if (outcomeModelFile != "" && !file.exists((outcomeModelFile))) {
      refRow <- outcomeReference[outcomeReference$outcomeModelFile == outcomeModelFile, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      outcomeConceptId <- unique(outcomeReference$outcomeConceptId[outcomeReference$outcomeModelFile ==
        outcomeModelFile])
      args <- analysisRow$fitOutcomeModelArgs
      args$outcomeConceptId <- outcomeConceptId
      modelsToFit[[length(modelsToFit) + 1]] <- list(cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                                                     args = args,
                                                     subPopFile = refRow$subPopFile,
                                                     outcomeModelFile = outcomeModelFile)
    }
  }
  doFitOutcomeModel <- function(params) {
    cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFolder, readOnly = TRUE)
    if (params$subPopFile != "") {
      subPopulation <- readRDS(params$subPopFile)
    } else {
      subPopulation <- NULL
    }
    args <- list(cohortMethodData = cohortMethodData, subPopulation = subPopulation)
    args <- append(args, params$args)
    outcomeModel <- do.call("fitOutcomeModel", args)
    saveRDS(outcomeModel, params$outcomeModelFile)
  }
  if (length(modelsToFit) != 0) {
    cluster <- OhdsiRTools::makeCluster(fitOutcomeModelThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster, modelsToFit, doFitOutcomeModel)
    OhdsiRTools::stopCluster(cluster)
  }
  invisible(outcomeReference)
}

.createCohortMethodDataFileName <- function(folder,
                                            argsId,
                                            targetDrugConceptId,
                                            comparatorDrugConceptId,
                                            indicationConceptIds) {
  name <- paste("CmData_a",
                argsId,
                "_t",
                targetDrugConceptId,
                "_c",
                comparatorDrugConceptId,
                sep = "")
  if (!is.null(indicationConceptIds) && length(indicationConceptIds) != 0) {
    name <- paste(name, "_i", indicationConceptIds[1], sep = "")
    if (length(indicationConceptIds) > 1) {
      name <- paste(name, "etc", sep = "")
    }
  }
  return(file.path(folder, name))
}

.createPsFileName <- function(folder,
                              argsId,
                              targetDrugConceptId,
                              comparatorDrugConceptId,
                              indicationConceptIds,
                              psArgsId) {
  name <- paste("ps_a", argsId, "_t", targetDrugConceptId, "_c", comparatorDrugConceptId, sep = "")
  if (!is.null(indicationConceptIds) && length(indicationConceptIds) != 0) {
    name <- paste(name, "_i", indicationConceptIds[1], sep = "")
    if (length(indicationConceptIds) > 1) {
      name <- paste(name, "etc", sep = "")
    }
  }
  name <- paste(name, "_p", psArgsId, sep = "")
  name <- paste(name, ".rds", sep = "")
  return(file.path(folder, name))
}

.createPsOutcomeFileName <- function(folder,
                                     argsId,
                                     targetDrugConceptId,
                                     comparatorDrugConceptId,
                                     indicationConceptIds,
                                     psArgsId,
                                     outcomeConceptId) {
  name <- paste("ps_a", argsId, "_t", targetDrugConceptId, "_c", comparatorDrugConceptId, sep = "")
  if (!is.null(indicationConceptIds) && length(indicationConceptIds) != 0) {
    name <- paste(name, "_i", indicationConceptIds[1], sep = "")
    if (length(indicationConceptIds) > 1) {
      name <- paste(name, "etc", sep = "")
    }
  }
  name <- paste(name, "_p", psArgsId, sep = "")
  name <- paste(name, "_o", outcomeConceptId, sep = "")
  name <- paste(name, ".rds", sep = "")
  return(file.path(folder, name))
}

.createSubPopFileName <- function(folder,
                                  argsId,
                                  targetDrugConceptId,
                                  comparatorDrugConceptId,
                                  indicationConceptIds,
                                  psArgsId,
                                  strataArgsId,
                                  outcomeConceptId) {
  name <- paste("sub_a", argsId, "_t", targetDrugConceptId, "_c", comparatorDrugConceptId, sep = "")
  if (!is.null(indicationConceptIds) && length(indicationConceptIds) != 0) {
    name <- paste(name, "_i", indicationConceptIds[1], sep = "")
    if (length(indicationConceptIds) > 1) {
      name <- paste(name, "etc", sep = "")
    }
  }
  name <- paste(name, "_p", psArgsId, sep = "")
  name <- paste(name, "_s", strataArgsId, sep = "")
  name <- paste(name, "_o", outcomeConceptId, sep = "")
  name <- paste(name, ".rds", sep = "")
  return(file.path(folder, name))
}

.createOutcomeModelFileName <- function(folder,
                                        targetDrugConceptId,
                                        comparatorDrugConceptId,
                                        indicationConceptIds,
                                        outcomeConceptId) {
  name <- paste("om_t", targetDrugConceptId, "_c", comparatorDrugConceptId, sep = "")
  if (!is.null(indicationConceptIds) && length(indicationConceptIds) != 0) {
    name <- paste(name, "_i", indicationConceptIds[1], sep = "")
    if (length(indicationConceptIds) > 1) {
      name <- paste(name, "etc", sep = "")
    }
  }
  name <- paste(name, "_o", outcomeConceptId, sep = "")
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
#' @param outcomeReference   A data.frame as created by the \code{\link{runAnalyses}} function.
#'
#' @export
summarizeAnalyses <- function(outcomeReference) {
  columns <- c("analysisId", "targetDrugConceptId", "comparatorDrugConceptId")
  if (!is.null(outcomeReference$indicationConceptIds)) {
    columns <- c(columns, "indicationConceptIds")
  }
  columns <- c(columns, "outcomeConceptId")
  result <- outcomeReference[, columns]
  result$rr <- 0
  result$ci95lb <- 0
  result$ci95ub <- 0
  result$treated <- 0
  result$comparator <- 0
  result$eventsTreated <- 0
  result$eventsComparator <- 0
  result$logRr <- 0
  result$seLogRr <- 0
  for (i in 1:nrow(outcomeReference)) {
    outcomeModel <- readRDS(outcomeReference$outcomeModelFile[i])
    result$rr[i] <- if (is.null(coef(outcomeModel)))
      NA else exp(coef(outcomeModel))
    result$ci95lb[i] <- if (is.null(coef(outcomeModel)))
      NA else exp(confint(outcomeModel)[1])
    result$ci95ub[i] <- if (is.null(coef(outcomeModel)))
      NA else exp(confint(outcomeModel)[2])
    result$treated[i] <- sum(outcomeModel$data$treatment == 1)
    result$comparator[i] <- sum(outcomeModel$data$treatment == 0)
    result$eventsTreated[i] <- sum(outcomeModel$data$y[outcomeModel$data$treatment == 1])
    result$eventsComparator[i] <- sum(outcomeModel$data$y[outcomeModel$data$treatment == 0])
    result$logRr[i] <- if (is.null(coef(outcomeModel)))
      NA else coef(outcomeModel)
    result$seLogRr[i] <- if (is.null(coef(outcomeModel)))
      NA else outcomeModel$treatmentEstimate$seLogRr
  }
  return(result)
}

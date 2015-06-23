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
#' Run a list of analyses for the drug-comparator-outcomes of interest.
#'
#' @param connectionDetails        An R object of type \code{connectionDetails} created using the
#'                                 function \code{createConnectionDetails} in the
#'                                 \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema        The name of the database schema that contains the OMOP CDM instance.
#'                                 Requires read permissions to this database. On SQL Server, this
#'                                 should specifiy both the database and the schema, so for example
#'                                 'cdm_instance.dbo'.
#' @param oracleTempSchema         For Oracle only: the name of the database schema where you want all
#'                                 temporary tables to be managed. Requires create/insert permissions
#'                                 to this database.
#' @param exposureDatabaseSchema   The name of the database schema that is the location where the
#'                                 exposure data used to define the exposure cohorts is available. If
#'                                 exposureTable = DRUG_ERA, exposureDatabaseSchema is not used by
#'                                 assumed to be cdmSchema.  Requires read permissions to this
#'                                 database.
#' @param outcomeDatabaseSchema    The name of the database schema that is the location where the data
#'                                 used to define the outcome cohorts is available. If exposureTable =
#'                                 CONDITION_ERA, exposureDatabaseSchema is not used by assumed to be
#'                                 cdmSchema.  Requires read permissions to this database.
#' @param outcomeTable             The tablename that contains the outcome cohorts.  If outcomeTable <>
#'                                 CONDITION_OCCURRENCE, then expectation is outcomeTable has format of
#'                                 COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                 COHORT_END_DATE.
#'
#' @export
runCohortMethodAnalyses <- function(connectionDetails,
                                    cdmDatabaseSchema,
                                    oracleTempSchema = cdmDatabaseSchema,
                                    exposureDatabaseSchema = cdmDatabaseSchema,
                                    exposureTable = "drug_era",
                                    outcomeDatabaseSchema = cdmDatabaseSchema,
                                    outcomeTable = "condition_occurrence",
                                    outputFolder = "./CohortMethodOutput",
                                    cohortMethodAnalysisList,
                                    drugComparatorOutcomeList,
                                    getDbCohortMethodDataThreads = 1,
                                    createPsThreads = 1,
                                    fitOutcomeModelThreads = 1) {
  for (drugComparatorOutcome in drugComparatorOutcomeList) {
    stopifnot(class(drugComparatorOutcome) == "drugComparatorOutcome")
  }
  for (cohortMethodAnalysis in cohortMethodAnalysisList) {
    stopifnot(class(cohortMethodAnalysis) == "cohortMethodAnalysis")
  }
  uniquedrugComparatorOutcomeList <- unique(OhdsiRTools::selectFromList(drugComparatorOutcomeList,
                                                                        c("targetDrugConceptId",
                                                                                                     "comparatorDrugConceptId",
                                                                                                     "outcomeConceptId",
                                                                                                     "indicationConceptIds")))
  if (length(uniquedrugComparatorOutcomeList) != length(drugComparatorOutcomeList)) {
    stop("Duplicate drug-comparator-indication-outcome combinations are not allowed")
  }
  uniqueAnalysisIds <- unlist(unique(OhdsiRTools::selectFromList(cohortMethodAnalysisList,
                                                                 "analysisId")))
  if (length(uniqueAnalysisIds) != length(cohortMethodAnalysisList)) {
    stop("Duplicate analysis IDs are not allowed")
  }

  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  writeLines("*** Creating cohortMethodData objects ***")
  objectsToCreate <- list()
  getDbCohortMethodDataArgsList <- unique(OhdsiRTools::selectFromList(cohortMethodAnalysisList,
                                                                      "getDbCohortMethodDataArgs"))
  for (i in 1:length(getDbCohortMethodDataArgsList)) {
    getDbCohortMethodDataArgs <- getDbCohortMethodDataArgsList[[i]]
    drugComparatorList <- unique(OhdsiRTools::selectFromList(drugComparatorOutcomeList,
                                                             c("targetDrugConceptId",
                                                                                          "comparatorDrugConceptId",
                                                                                          "indicationConceptIds",
                                                                                          "exclusionConceptIds")))
    for (drugComparator in drugComparatorList) {
      cohortMethodDataFolder <- .createCohortMethodDataFileName(outputFolder,
                                                                i,
                                                                drugComparator$targetDrugConceptId,
                                                                drugComparator$comparatorDrugConceptId,
                                                                drugComparator$indicationConceptIds)
      if (!file.exists(cohortMethodDataFolder)) {
        outcomeConceptIds <- unlist(OhdsiRTools::selectFromList(OhdsiRTools::matchInList(drugComparatorOutcomeList,
                                                                                         drugComparator), "outcomeConceptId"))
        args <- list(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     exposureDatabaseSchema = exposureDatabaseSchema,
                     exposureTable = exposureTable,
                     outcomeDatabaseSchema = outcomeDatabaseSchema,
                     outcomeTable = outcomeTable,
                     outcomeConceptIds = outcomeConceptIds)
        args <- append(args, getDbCohortMethodDataArgs$getDbCohortMethodDataArgs)
        args <- append(args, drugComparator)
        objectsToCreate[[length(objectsToCreate) + 1]] <- list(args = args,
                                                               cohortMethodDataFolder = cohortMethodDataFolder)
      }
    }
  }
  cluster <- OhdsiRTools::makeCluster(getDbCohortMethodDataThreads)
  OhdsiRTools::clusterRequire(cluster, "CohortMethod")
  createCmDataObject <- function(params) {
    cohortMethodData <- do.call("getDbCohortMethodData", params$args)
    saveCohortMethodData(cohortMethodData, params$cohortMethodDataFolder)
  }
  dummy <- OhdsiRTools::clusterApply(cluster, objectsToCreate, createCmDataObject)
  OhdsiRTools::stopCluster(cluster)

  writeLines("*** Fitting propensity score models ***")
  modelsToFit <- list()
  getDbCohortMethodDataArgsList <- unique(OhdsiRTools::selectFromList(cohortMethodAnalysisList,
                                                                      "getDbCohortMethodDataArgs"))
  for (i in 1:length(getDbCohortMethodDataArgsList)) {
    getDbCohortMethodDataArgs <- getDbCohortMethodDataArgsList[[i]]
    drugComparatorList <- unique(OhdsiRTools::selectFromList(drugComparatorOutcomeList,
                                                             c("targetDrugConceptId",
                                                                                          "comparatorDrugConceptId",
                                                                                          "indicationConceptIds",
                                                                                          "exclusionConceptIds")))
    for (drugComparator in drugComparatorList) {
      cohortMethodDataFolder <- .createCohortMethodDataFileName(outputFolder,
                                                                i,
                                                                drugComparator$targetDrugConceptId,
                                                                drugComparator$comparatorDrugConceptId,
                                                                drugComparator$indicationConceptIds)
      cohortMethodAnalysisSubset <- OhdsiRTools::matchInList(cohortMethodAnalysisList,
                                                             getDbCohortMethodDataArgs)
      createPsArgsList <- unique(OhdsiRTools::selectFromList(cohortMethodAnalysisSubset,
                                                             c("createPs", "createPsArgs")))
      for (j in 1:length(createPsArgsList)) {
        createPsArgs <- createPsArgsList[[j]]
        psFileName <- .createPsFileName(outputFolder,
                                        i,
                                        drugComparator$targetDrugConceptId,
                                        drugComparator$comparatorDrugConceptId,
                                        drugComparator$indicationConceptIds,
                                        j)
        if (!file.exists(psFileName)) {
          modelsToFit[[length(modelsToFit) + 1]] <- list(cohortMethodDataFolder = cohortMethodDataFolder,
                                                         createPsArgs = createPsArgs,
                                                         psFileName = psFileName)
        }
      }
    }
  }
  cluster <- OhdsiRTools::makeCluster(createPsThreads)
  OhdsiRTools::clusterRequire(cluster, "CohortMethod")
  fitPSModel <- function(params) {
    if (params$createPsArgs$createPs) {
      cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFolder, readOnly = TRUE)
      args <- list(cohortMethodData = cohortMethodData)
      args <- append(args, params$createPsArgs$createPsArgs)
      ps <- do.call("createPs", args)
      saveRDS(ps, file = params$psFileName)
    }
  }
  dummy <- OhdsiRTools::clusterApply(cluster, modelsToFit, fitPSModel)
  OhdsiRTools::stopCluster(cluster)

  writeLines("*** Fitting outcome models ***")
  modelsToFit <- list()
  allButFitOutcomeModelArgsList <- unique(OhdsiRTools::excludeFromList(cohortMethodAnalysisList,
                                                                       c("analysisId",
                                                                                                   "fitOutcomeModel",
                                                                                                   "fitOutcomeModelArgs")))
  getDbCohortMethodDataArgsList <- unique(OhdsiRTools::selectFromList(cohortMethodAnalysisList,
                                                                      "getDbCohortMethodDataArgs"))
  for (i in 1:length(getDbCohortMethodDataArgsList)) {
    getDbCohortMethodDataArgs <- getDbCohortMethodDataArgsList[[i]]
    drugComparatorList <- unique(OhdsiRTools::selectFromList(drugComparatorOutcomeList,
                                                             c("targetDrugConceptId",
                                                                                          "comparatorDrugConceptId",
                                                                                          "indicationConceptIds",
                                                                                          "exclusionConceptIds")))
    for (drugComparator in drugComparatorList) {
      cohortMethodDataFolder <- .createCohortMethodDataFileName(outputFolder,
                                                                i,
                                                                drugComparator$targetDrugConceptId,
                                                                drugComparator$comparatorDrugConceptId,
                                                                drugComparator$indicationConceptIds)
      cohortMethodAnalysisSubset <- OhdsiRTools::matchInList(cohortMethodAnalysisList,
                                                             getDbCohortMethodDataArgs)
      createPsArgsList <- unique(OhdsiRTools::selectFromList(cohortMethodAnalysisSubset,
                                                             "createPsArgs"))
      for (j in 1:length(createPsArgsList)) {
        createPsArgs <- createPsArgsList[[j]]
        psFileName <- .createPsFileName(outputFolder,
                                        i,
                                        drugComparator$targetDrugConceptId,
                                        drugComparator$comparatorDrugConceptId,
                                        drugComparator$indicationConceptIds,
                                        j)
        allButFitOutcomeModelArgsSubset <- OhdsiRTools::matchInList(OhdsiRTools::matchInList(allButFitOutcomeModelArgsList,
                                                                                             getDbCohortMethodDataArgs), createPsArgs)
        outcomeConceptIds <- unlist(OhdsiRTools::selectFromList(OhdsiRTools::matchInList(drugComparatorOutcomeList,
                                                                                         drugComparator), "outcomeConceptId"))
        for (outcomeConceptId in outcomeConceptIds) {
          for (allButFitOutcomeModelArgs in allButFitOutcomeModelArgsSubset) {
          modelsToFit[[length(modelsToFit) + 1]] <- list(cohortMethodDataFolder = cohortMethodDataFolder,
                                                         psFileName = psFileName,
                                                         outcomeConceptId = outcomeConceptId,
                                                         allButFitOutcomeModelArgs = allButFitOutcomeModelArgs)
          }
        }
      }
    }
  }
  cluster <- OhdsiRTools::makeCluster(fitOutcomeModelThreads)
  OhdsiRTools::clusterRequire(cluster, "CohortMethod")
  doFitOutcomeModel <- function(params, cohortMethodAnalysisList, outputFolder) {
    cohortMethodAnalysisSubset <- OhdsiRTools::matchInList(cohortMethodAnalysisList,
                                                           params$allButFitOutcomeModelArgs)
    fitOutcomeModels <- unlist(OhdsiRTools::selectFromList(cohortMethodAnalysisSubset,
                                                           "fitOutcomeModel"))
    if (any(fitOutcomeModels)) {
      cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFolder, readOnly = TRUE)
      if (!params$allButFitOutcomeModelArgs$createPs) {
        ps <- NULL
      } else {
        ps <- readRDS(file = params$psFileName)

        # Exclude people with prior outcome:
        ps <- ps[!(ps$rowId %in% ff::as.ram(cohortMethodData$exclude$rowId[cohortMethodData$exclude$outcomeId ==
          outcomeConceptId])), ]

        if (params$allButFitOutcomeModelArgs$trimByPs) {
          args <- list(data = ps)
          args <- append(args, params$allButFitOutcomeModelArgs$trimByPsArgs)
          ps <- do.call("trimByPs", args)
        } else if (params$allButFitOutcomeModelArgs$trimByPsToEquipoise) {
          args <- list(data = ps)
          args <- append(args, params$allButFitOutcomeModelArgs$trimByPsToEquipoisesArgs)
          ps <- do.call("trimByPsToEquipoise", args)
        }
        if (params$allButFitOutcomeModelArgs$matchOnPs) {
          args <- list(data = ps)
          args <- append(args, params$allButFitOutcomeModelArgs$matchOnPsArgs)
          ps <- do.call("matchOnPs", args)
        } else if (params$allButFitOutcomeModelArgs$matchOnPsAndCovariates) {
          args <- list(data = ps)
          args <- append(args, params$allButFitOutcomeModelArgs$matchOnPsAndCovariatesArgs)
          ps <- do.call("matchOnPsAndCovariates", args)
        } else if (params$allButFitOutcomeModelArgs$stratifyByPs) {
          args <- list(data = ps)
          args <- append(args, params$allButFitOutcomeModelArgs$stratifyByPsArgs)
          ps <- do.call("stratifyByPs", args)
        } else if (params$allButFitOutcomeModelArgs$stratifyByPsAndCovariates) {
          args <- list(data = ps)
          args <- append(args, params$allButFitOutcomeModelArgs$stratifyByPsAndCovariatesArgs)
          ps <- do.call("stratifyByPsAndCovariates", args)
        }
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
        name <- paste(name, ".rda", sep = "")
        return(file.path(folder, name))
      }

      for (cohortMethodAnalysis in cohortMethodAnalysisSubset) {
        analysisFolder <- paste(outputFolder,
                                "/Analysis_",
                                cohortMethodAnalysis$analysisId,
                                sep = "")
        if (!file.exists(analysisFolder))
          dir.create(analysisFolder)
        outcomeModelFile <- .createOutcomeModelFileName(analysisFolder,
                                                        drugComparator$targetDrugConceptId,
                                                        drugComparator$comparatorDrugConceptId,
                                                        drugComparator$indicationConceptIds,
                                                        outcomeConceptId)
        if (!file.exists(outcomeModelFile)) {
          args <- list(outcomeConceptId = outcomeConceptId,
                       cohortMethodData = cohortMethodData,
                       subPopulation = ps)
          args <- append(args, cohortMethodAnalysis$fitOutcomeModelArgs)
          outcomeModel <- do.call("fitOutcomeModel", args)
          saveRDS(outcomeModel, file = outcomeModelFile)
        }
      }
    }
  }
  dummy <- OhdsiRTools::clusterApply(cluster,
                                     modelsToFit,
                                     doFitOutcomeModel,
                                     cohortMethodAnalysisList = cohortMethodAnalysisList,
                                     outputFolder = outputFolder)
  OhdsiRTools::stopCluster(cluster)
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
  name <- paste(name, ".rda", sep = "")
  return(file.path(folder, name))
}

.createPsFileName <- function(folder,
                              argsId,
                              targetDrugConceptId,
                              comparatorDrugConceptId,
                              indicationConceptIds,
                              psArgsId) {
  name <- paste("Ps_a", argsId, "_t", targetDrugConceptId, "_c", comparatorDrugConceptId, sep = "")
  if (!is.null(indicationConceptIds) && length(indicationConceptIds) != 0) {
    name <- paste(name, "_i", indicationConceptIds[1], sep = "")
    if (length(indicationConceptIds) > 1) {
      name <- paste(name, "etc", sep = "")
    }
  }
  name <- paste(name, "_p", psArgsId, sep = "")
  name <- paste(name, ".rda", sep = "")
  return(file.path(folder, name))
}




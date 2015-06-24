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
#' @param connectionDetails              An R object of type \code{connectionDetails} created using the
#'                                       function \code{createConnectionDetails} in the
#'                                       \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema              The name of the database schema that contains the OMOP CDM
#'                                       instance. Requires read permissions to this database. On SQL
#'                                       Server, this should specifiy both the database and the schema,
#'                                       so for example 'cdm_instance.dbo'.
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
#'                                       created using the \code{\link{createdrugComparatorOutcomes}}
#'                                       function.
#' @param getDbCohortMethodDataThreads   The number of parallel threads to use for building the
#'                                       cohortMethod data objects.
#' @param createPsThreads                The number of parallel threads to use for fitting the
#'                                       propensity models.
#' @param fitOutcomeModelThreads         The number of parallel threads to use for fitting the outcome
#'                                       models.
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
                          getDbCohortMethodDataThreads = 1,
                          createPsThreads = 1,
                          fitOutcomeModelThreads = 1) {
  for (drugComparatorOutcomes in drugComparatorOutcomesList) {
    stopifnot(class(drugComparatorOutcomes) == "drugComparatorOutcomes")
  }
  for (cmAnalysis in cmAnalysisList) {
    stopifnot(class(cmAnalysis) == "cmAnalysis")
  }
  uniquedrugComparatorOutcomessList <- unique(OhdsiRTools::selectFromList(drugComparatorOutcomesList,
                                                                          c("targetDrugConceptId",
                                                                                                        "comparatorDrugConceptId",
                                                                                                        "outcomeConceptIds",
                                                                                                        "indicationConceptIds")))
  if (length(uniquedrugComparatorOutcomessList) != length(drugComparatorOutcomesList)) {
    stop("Duplicate drug-comparator-indication-outcomes combinations are not allowed")
  }
  uniqueAnalysisIds <- unlist(unique(OhdsiRTools::selectFromList(cmAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(cmAnalysisList)) {
    stop("Duplicate analysis IDs are not allowed")
  }
  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  writeLines("*** Creating cohortMethodData objects ***")
  objectsToCreate <- list()
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
      if (!file.exists(cohortMethodDataFolder)) {
        getDbCohortMethodDataArgs <- loadingArgs$getDbCohortMethodDataArgs
        exclusionConceptIds <- unique(c(drugComparatorOutcomess$exclusionConceptIds,
                                        getDbCohortMethodDataArgs$exclusionConceptIds))
        excludedCovariateConceptIds <- unique(c(drugComparatorOutcomess$excludedCovariateConceptIds,
                                                getDbCohortMethodDataArgs$excludedCovariateConceptIds))
        includedCovariateConceptIds <- unique(c(drugComparatorOutcomess$includedCovariateConceptIds,
                                                getDbCohortMethodDataArgs$includedCovariateConceptIds))

        drugComparatorOutcomess <- OhdsiRTools::matchInList(drugComparatorOutcomesList,
                                                            drugComparator)
        outcomeConceptIds <- unique(unlist(OhdsiRTools::selectFromList(drugComparatorOutcomess,
                                                                       "outcomeConceptIds")))
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
                     exclusionConceptIds = exclusionConceptIds,
                     excludedCovariateConceptIds = excludedCovariateConceptIds,
                     includedCovariateConceptIds = includedCovariateConceptIds)
        getDbCohortMethodDataArgs$exclusionConceptIds <- NULL
        getDbCohortMethodDataArgs$excludedCovariateConceptIds <- NULL
        getDbCohortMethodDataArgs$includedCovariateConceptIds <- NULL
        args <- append(args, getDbCohortMethodDataArgs)
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
  loadingArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisList,
                                                        c("getDbCohortMethodDataArgs",
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
        psFileName <- .createPsFileName(outputFolder,
                                        i,
                                        targetDrugConceptId,
                                        comparatorDrugConceptId,
                                        indicationConceptIds,
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
  allButFitOutcomeModelArgsList <- unique(OhdsiRTools::excludeFromList(cmAnalysisList,
                                                                       c("analysisId",
                                                                                         "fitOutcomeModel",
                                                                                         "fitOutcomeModelArgs")))
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
        psFileName <- .createPsFileName(outputFolder,
                                        i,
                                        targetDrugConceptId,
                                        comparatorDrugConceptId,
                                        indicationConceptIds,
                                        j)
        allButFitOutcomeModelArgsSubset <- OhdsiRTools::matchInList(OhdsiRTools::matchInList(allButFitOutcomeModelArgsList,
                                                                                             loadingArgs), createPsArgs)
        outcomeConceptIds <- unique(unlist(OhdsiRTools::selectFromList(drugComparatorOutcomess,
                                                                       "outcomeConceptIds")))
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
  doFitOutcomeModel <- function(params, cmAnalysisList, outputFolder) {
    cmAnalysisSubset <- OhdsiRTools::matchInList(cmAnalysisList, params$allButFitOutcomeModelArgs)
    fitOutcomeModels <- unlist(OhdsiRTools::selectFromList(cmAnalysisSubset, "fitOutcomeModel"))
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

      for (cmAnalysis in cmAnalysisSubset) {
        analysisFolder <- paste(outputFolder, "/Analysis_", cmAnalysis$analysisId, sep = "")
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
          args <- append(args, cmAnalysis$fitOutcomeModelArgs)
          outcomeModel <- do.call("fitOutcomeModel", args)
          saveRDS(outcomeModel, file = outcomeModelFile)
        }
      }
    }
  }
  dummy <- OhdsiRTools::clusterApply(cluster,
                                     modelsToFit,
                                     doFitOutcomeModel,
                                     cmAnalysisList = cmAnalysisList,
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
      stop(paste(label, "type not found:", loadingArgs$comparatorType))
    }
    return(value[type])
  }
}


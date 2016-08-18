# @file RunAnalyses.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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
#' @param cdmVersion                            Define the OMOP CDM version used: currently support "4" and "5".
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
#'                                              \code{\link{createDrugComparatorOutcomes}} function.
#' @param refitPsForEveryOutcome                Should the propensity model be fitted for every outcome
#'                                              (i.e. after people who already had the outcome are
#'                                              removed)? If false, a single propensity model will be
#'                                              fitted, and people who had the outcome previously will
#'                                              be removed afterwards.
#' @param getDbCohortMethodDataThreads          The number of parallel threads to use for building the
#'                                              cohortMethod data objects.
#' @param createPsThreads                       The number of parallel threads to use for fitting the
#'                                              propensity models.
#' @param psCvThreads                           The number of parallel threads to use for the cross-
#'                                              validation when estimating the hyperparameter for the
#'                                              propensity model. Note that the total number of CV
#'                                              threads at one time could be `createPsThreads *
#'                                              psCvThreads`.
#' @param createStudyPopThreads                 The number of parallel threads to use for creating the
#'                                              study population.
#' @param computeCovarBalThreads                The number of parallel threads to use for computing the
#'                                              covariate balance.
#' @param trimMatchStratifyThreads              The number of parallel threads to use for trimming,
#'                                              matching and stratifying.
#' @param fitOutcomeModelThreads                The number of parallel threads to use for fitting the
#'                                              outcome models.
#' @param outcomeCvThreads                      The number of parallel threads to use for the cross-
#'                                              validation when estimating the hyperparameter for the
#'                                              outcome model. Note that the total number of CV threads
#'                                              at one time could be `fitOutcomeModelThreads *
#'                                              outcomeCvThreads`.
#'
#' @return
#' A data frame with the following columns:
#' \tabular{ll}{
#' \verb{analysisId} \tab The unique identifier for a set of analysis choices.\cr
#' \verb{targetId} \tab The ID of the target drug.\cr
#' \verb{comparatorId} \tab The ID of the comparator group.\cr
#' \verb{excludedCovariateConceptIds} \tab The ID(s) of concepts that cannot be used to construct covariates. \cr
#' \verb{includedCovariateConceptIds} \tab The ID(s) of concepts that should be used to construct covariates. \cr
#' \verb{outcomeId} \tab The ID of the outcome \cr
#' \verb{cohortMethodDataFolder} \tab The ID of the outcome.\cr
#' \verb{sharedPsFile}                \tab The name of the file containing the propensity scores of the shared \cr
#'                                    \tab propensity model. This model is used to create the outcome-specific \cr
#'                                    \tab propensity scores by removing people with prior outcomes.\cr
#' \verb{studyPopFile}                 \tab The name of the file containing the study population (prior\cr
#'                                    \tab and trimming, matching, or stratification on the PS.\cr
#' \verb{psFile}                      \tab The name of file containing the propensity scores for a specific \cr
#'                                    \tab outcomes (ie after people with prior outcomes have been removed).\cr
#' \verb{strataFile}                  \tab The name of the file containing the identifiers of the population \cr
#'                                    \tab after any trimming, matching or stratifying, including their strata.\cr
#' \verb{covariateBalanceFile}        \tab The name of the file containing the covariate balance (ie. the \cr
#'                                    \tab output of the \code{computeCovariateBalance} function.\cr
#' \verb{outcomeModelFile} \tab The name of the file containing the outcome model.\cr
#' }
#'
#' @export
runCmAnalyses <- function(connectionDetails,
                          cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "condition_occurrence",
                          cdmVersion = 4,
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
                          outcomeCvThreads = 1) {
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
  uniqueAnalysisIds <- unlist(unique(OhdsiRTools::selectFromList(cmAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(cmAnalysisList)) {
    stop("Duplicate analysis IDs are not allowed")
  }
  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  ### Create reference table ###
  outcomeReference <- data.frame()
  loadingArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisList,
                                                        c("getDbCohortMethodDataArgs",
                                                          "targetType",
                                                          "comparatorType")))
  for (loadId in 1:length(loadingArgsList)) {
    loadingArgs <- loadingArgsList[[loadId]]
    drugComparatorList <- unique(OhdsiRTools::selectFromList(drugComparatorOutcomesList,
                                                             c("targetId",
                                                               "comparatorId",
                                                               "excludedCovariateConceptIds",
                                                               "includedCovariateConceptIds")))
    for (drugComparator in drugComparatorList) {
      drugComparatorOutcomes <- OhdsiRTools::matchInList(drugComparatorOutcomesList, drugComparator)
      outcomeIds <- unique(unlist(OhdsiRTools::selectFromList(drugComparatorOutcomes,
                                                              "outcomeIds")))
      targetId <- .selectByType(loadingArgs$targetType, drugComparator$targetId, "target")
      comparatorId <- .selectByType(loadingArgs$comparatorType,
                                    drugComparator$comparatorId,
                                    "comparator")
      cohortMethodDataFolder <- .createCohortMethodDataFileName(outputFolder,
                                                                loadId,
                                                                targetId,
                                                                comparatorId)
      cmAnalysisSubset <- OhdsiRTools::matchInList(cmAnalysisList, loadingArgs)

      createStudyPopArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisSubset,"createStudyPopArgs"))
      for (studyPopId in 1:length(createStudyPopArgsList)) {
        createStudyPopArgs <- createStudyPopArgsList[[studyPopId]]

        cmAnalysisSubset2 <- OhdsiRTools::matchInList(cmAnalysisSubset, createStudyPopArgs)

        createPsArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisSubset2,
                                                               c("createPs", "createPsArgs")))
        for (psArgsId in 1:length(createPsArgsList)) {
          createPsArgs <- createPsArgsList[[psArgsId]]

          cmAnalysisSubset3 <- OhdsiRTools::matchInList(cmAnalysisSubset2, createPsArgs)

          if (createPsArgs$createPs && !refitPsForEveryOutcome) {
            sharedPsFile <- .createPsFileName(folder = outputFolder,
                                              loadId = loadId,
                                              studyPopId = studyPopId,
                                              psArgsId = psArgsId,
                                              targetId = targetId,
                                              comparatorId = comparatorId)
          } else {
            sharedPsFile <- ""
          }
          for (outcomeId in outcomeIds) {
            studyPopFile <- .createStudyPopulationFileName(outputFolder,
                                                           loadId,
                                                           studyPopId,
                                                           targetId,
                                                           comparatorId,
                                                           outcomeId)
            if (createPsArgs$createPs) {
              psFileName <- .createPsOutcomeFileName(folder = outputFolder,
                                                     loadId = loadId,
                                                     studyPopId = studyPopId,
                                                     psArgsId = psArgsId,
                                                     targetId = targetId,
                                                     comparatorId = comparatorId,
                                                     outcomeId = outcomeId)
            } else {
              psFileName <- ""
            }

            strataArgsList <- unique(OhdsiRTools::selectFromList(cmAnalysisSubset3,
                                                                 c("trimByPs",
                                                                   "trimByPsArgs",
                                                                   "trimByPsToEquipoise",
                                                                   "trimByPsToEquipoiseArgs",
                                                                   "matchOnPs",
                                                                   "matchOnPsArgs",
                                                                   "matchOnPsAndCovariates",
                                                                   "matchOnPsAndCovariate",
                                                                   "stratifyByPs",
                                                                   "stratifyByPsArgs",
                                                                   "stratifyByPsAndCovariates",
                                                                   "stratifyByPsAndCovariatesArgs")))
            for (strataArgsId in 1:length(strataArgsList)) {
              strataArgs <- strataArgsList[[strataArgsId]]

              cmAnalysisSubset4 <- OhdsiRTools::matchInList(cmAnalysisSubset3, strataArgs)

              if (strataArgs$trimByPs || strataArgs$trimByPsToEquipoise || strataArgs$matchOnPs ||
                  strataArgs$matchOnPsAndCovariates || strataArgs$stratifyByPs || strataArgs$stratifyByPsAndCovariates) {
                strataFile <- .createStratifiedPopFileName(folder = outputFolder,
                                                           loadId = loadId,
                                                           studyPopId = studyPopId,
                                                           psArgsId = psArgsId,
                                                           strataArgsId = strataArgsId,
                                                           targetId = targetId,
                                                           comparatorId = comparatorId,
                                                           outcomeId = outcomeId)

                if (max(unlist(OhdsiRTools::selectFromList(cmAnalysisSubset4, "computeCovariateBalance")))) {
                  covarBalFileName <- .createCovariateBalanceFileName(folder = outputFolder,
                                                                      loadId = loadId,
                                                                      studyPopId = studyPopId,
                                                                      psArgsId = psArgsId,
                                                                      strataArgsId = strataArgsId,
                                                                      targetId = targetId,
                                                                      comparatorId = comparatorId,
                                                                      outcomeId = outcomeId)
                } else {
                  covarBalFileName <- ""
                }
              } else {
                strataFile <- ""
                covarBalFileName <- ""
              }
              for (cmAnalysis in cmAnalysisSubset4) {
                analysisFolder <- file.path(outputFolder,
                                            paste("Analysis_", cmAnalysis$analysisId, sep = ""))
                if (!file.exists(analysisFolder))
                  dir.create(analysisFolder)

                if (cmAnalysis$fitOutcomeModel) {
                  outcomeModelFileName <- .createOutcomeModelFileName(folder = analysisFolder,
                                                                      targetId = targetId,
                                                                      comparatorId = comparatorId,
                                                                      outcomeId = outcomeId)
                } else {
                  outcomeModelFileName <- ""
                }

                outcomeReferenceRow <- data.frame(analysisId = cmAnalysis$analysisId,
                                                  targetId = targetId,
                                                  comparatorId = comparatorId,
                                                  excludedCovariateConceptIds = paste(drugComparator$excludedCovariateConceptIds,
                                                                                      collapse = ","),
                                                  includedCovariateConceptIds = paste(drugComparator$includedCovariateConceptIds,
                                                                                      collapse = ","),
                                                  outcomeId = outcomeId,
                                                  cohortMethodDataFolder = cohortMethodDataFolder,
                                                  sharedPsFile = sharedPsFile,
                                                  studyPopFile = studyPopFile,
                                                  psFile = psFileName,
                                                  strataFile = strataFile,
                                                  covariateBalanceFile = covarBalFileName,
                                                  outcomeModelFile = outcomeModelFileName,
                                                  stringsAsFactors = FALSE)
                outcomeReference <- rbind(outcomeReference, outcomeReferenceRow)
              }
            }
          }
        }
      }
    }
  }
  saveRDS(outcomeReference, file.path(outputFolder, "outcomeModelReference.rds"))
  # write.csv(outcomeReference, file.path(outputFolder, "outcomeModelReference.csv"), row.names = FALSE)

  writeLines("*** Creating cohortMethodData objects ***")
  objectsToCreate <- list()
  for (cohortMethodDataFolder in unique(outcomeReference$cohortMethodDataFolder)) {
    if (cohortMethodDataFolder != "" && !file.exists(cohortMethodDataFolder)) {
      refRow <- outcomeReference[outcomeReference$cohortMethodDataFolder == cohortMethodDataFolder, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      getDbCohortMethodDataArgs <- analysisRow$getDbCohortMethodDataArgs
      covariateSettings <- getDbCohortMethodDataArgs$covariateSettings
      if (is(covariateSettings, "covariateSettings"))
          covariateSettings <- list(covariateSettings)
      for (i in 1:length(covariateSettings)) {
          covariateSettings[[i]]$excludedCovariateConceptIds <- unique(c(as.numeric(unlist(strsplit(refRow$excludedCovariateConceptIds, ","))),
                                                                         covariateSettings[[i]]$excludedCovariateConceptIds))
          covariateSettings[[i]]$includedCovariateConceptIds <- unique(c(as.numeric(unlist(strsplit(refRow$includedCovariateConceptIds, ","))),
                                                                         covariateSettings[[i]]$includedCovariateConceptIds))
      }
      getDbCohortMethodDataArgs$covariateSettings <- covariateSettings
      outcomeIds <- unique(outcomeReference$outcomeId[outcomeReference$cohortMethodDataFolder ==
                                                          cohortMethodDataFolder])
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

  writeLines("*** Creating study populations ***")
  objectsToCreate <- list()
  for (studyPopFile in unique(outcomeReference$studyPopFile)) {
    if (studyPopFile != "" && !file.exists(studyPopFile)) {
      refRow <- outcomeReference[outcomeReference$studyPopFile == studyPopFile, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      args <- analysisRow$createStudyPopArgs
      args$outcomeId <- refRow$outcomeId
      objectsToCreate[[length(objectsToCreate) + 1]] <- list(cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                                                             args = args,
                                                             studyPopFile = studyPopFile)
    }
  }
  createStudyPopObject <- function(params) {
    cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFolder, readOnly = TRUE)
    args <- params$args
    args$cohortMethodData <- cohortMethodData
    studyPop <- do.call("createStudyPopulation", args)
    saveRDS(studyPop, params$studyPopFile)
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
    for (psFile in unique(outcomeReference$psFile)) {
      if (psFile != "" && !file.exists((psFile))) {
        refRow <- outcomeReference[outcomeReference$psFile == psFile, ][1, ]
        analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                                list(analysisId = refRow$analysisId))[[1]]
        args <- analysisRow$createPsArgs
        args$control$threads <- psCvThreads
        args$outcomeId <- refRow$outcomeId
        modelsToFit[[length(modelsToFit) + 1]] <- list(cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                                                       studyPopFile = refRow$studyPopFile,
                                                       args = args,
                                                       psFile = psFile)
      }
    }
    fitPsModel <- function(params) {
      cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFolder, readOnly = TRUE)
      studyPop <- readRDS(params$studyPopFile)
      args <- params$args
      args$cohortMethodData <- cohortMethodData
      args$population <- studyPop
      ps <- do.call("createPs", args)
      saveRDS(ps, params$psFile)
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
    for (sharedPsFile in unique(outcomeReference$sharedPsFile)) {
      if (sharedPsFile != "" && !file.exists(sharedPsFile)) {
        refRow <- outcomeReference[outcomeReference$sharedPsFile == sharedPsFile, ][1, ]
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
    fitSharedPsModel <- function(params) {
      cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFolder, readOnly = TRUE)
      args <- params$createStudyPopArgs
      args$cohortMethodData <- cohortMethodData
      studyPop <- do.call("createStudyPopulation", args)
      args <- params$createPsArg
      args$cohortMethodData <- cohortMethodData
      args$population <- studyPop
      ps <- do.call("createPs", args)
      saveRDS(ps, params$sharedPsFile)
    }
    if (length(modelsToFit) != 0) {
      cluster <- OhdsiRTools::makeCluster(createPsThreads)
      OhdsiRTools::clusterRequire(cluster, "CohortMethod")
      dummy <- OhdsiRTools::clusterApply(cluster, modelsToFit, fitSharedPsModel)
      OhdsiRTools::stopCluster(cluster)
    }
    writeLines("*** Adding propensity scores to study population objects ***")
    psFiles <- unique(outcomeReference$psFile)
    psFiles <- psFiles[psFiles != ""]
    psFiles <- psFiles[!file.exists(psFiles)]
    if (length(psFiles) > 0) {
      pb <- txtProgressBar(style = 3)
      for (i in 1:length(psFiles)) {
        psFile <- psFiles[i]
        refRow <- outcomeReference[outcomeReference$psFile == psFile, ][1, ]
        studyPop <- readRDS(refRow$studyPopFile)
        ps <- readRDS(refRow$sharedPsFile)
        newMetaData <- attr(studyPop, "metaData")
        newMetaData$psModelCoef <- attr(ps, "metaData")$psModelCoef
        newMetaData$psModelPriorVariance <- attr(ps, "metaData")$psModelPriorVariance
        ps <- merge(studyPop, ps[, c("rowId", "propensityScore")])
        attr(ps, "metaData") <- newMetaData
        saveRDS(ps, psFile)
        setTxtProgressBar(pb, i/length(psFiles))
      }
      close(pb)
    }
  }

  writeLines("*** Trimming/Matching/Stratifying ***")
  tasks <- list()
  for (strataFile in unique(outcomeReference$strataFile)) {
    if (strataFile != "" && !file.exists((strataFile))) {
      refRow <- outcomeReference[outcomeReference$strataFile == strataFile, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]

      tasks[[length(tasks) + 1]] <- list(psFile = refRow$psFile,
                                         args = analysisRow,
                                         strataFile = strataFile)
    }
  }
  trimMatchStratify <- function(params) {
    ps <- readRDS(params$psFile)
    if (params$args$trimByPs) {
      args <- list(population = ps)
      args <- append(args, params$args$trimByPsArgs)
      ps <- do.call("trimByPs", args)
    } else if (params$args$trimByPsToEquipoise) {
      args <- list(population = ps)
      args <- append(args, params$args$trimByPsToEquipoisesArgs)
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
  }
  if (length(tasks) != 0) {
    cluster <- OhdsiRTools::makeCluster(trimMatchStratifyThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster, tasks, trimMatchStratify)
    OhdsiRTools::stopCluster(cluster)
  }

  writeLines("*** Computing covariate balance ***")
  tasks <- list()
  for (covariateBalanceFile in unique(outcomeReference$covariateBalanceFile)) {
    if (covariateBalanceFile != "" && !file.exists((covariateBalanceFile))) {
      refRow <- outcomeReference[outcomeReference$covariateBalanceFile == covariateBalanceFile, ][1, ]
      tasks[[length(tasks) + 1]] <- list(strataFile = refRow$strataFile,
                                         cohortMethodDataFolder = refRow$cohortMethodDataFolder,
                                         covariateBalanceFile = refRow$covariateBalanceFile)
    }
  }
  computeCovarBal <- function(params) {
    cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFolder, readOnly = TRUE)
    strata <- readRDS(params$strataFile)
    balance <- computeCovariateBalance(strata, cohortMethodData)
    saveRDS(balance, params$covariateBalanceFile)
  }
  if (length(tasks) != 0) {
    cluster <- OhdsiRTools::makeCluster(computeCovarBalThreads)
    OhdsiRTools::clusterRequire(cluster, "CohortMethod")
    dummy <- OhdsiRTools::clusterApply(cluster, tasks, computeCovarBal)
    OhdsiRTools::stopCluster(cluster)
  }

  writeLines("*** Fitting outcome models ***")
  modelsToFit <- list()
  for (outcomeModelFile in unique(outcomeReference$outcomeModelFile)) {
    if (outcomeModelFile != "" && !file.exists((outcomeModelFile))) {
      refRow <- outcomeReference[outcomeReference$outcomeModelFile == outcomeModelFile, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(cmAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      args <- analysisRow$fitOutcomeModelArgs
      args$control$threads <- outcomeCvThreads
      if (refRow$strataFile != ""){
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
  doFitOutcomeModel <- function(params) {
    cohortMethodData <- loadCohortMethodData(params$cohortMethodDataFolder, readOnly = TRUE)
    studyPop <- readRDS(params$studyPopFile)
    args <- list(cohortMethodData = cohortMethodData,
                 population = studyPop)
    args <- append(args, params$args)
    #outcomeModel <- do.call("fitOutcomeModel", args)
            outcomeModel <- fitOutcomeModel(population = args$population,
                            cohortMethodData = args$cohortMethodData,
                            modelType = args$modelType,
                            stratified = args$stratified,
                            useCovariates = args$useCovariates,
                            prior = args$prior,
                            control = args$control)
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
                                            loadId,
                                            targetId,
                                            comparatorId) {
  name <- paste("CmData_l", loadId, "_t", targetId, "_c", comparatorId, sep = "")
  return(file.path(folder, name))
}

.createStudyPopulationFileName <- function(folder,
                                           loadId,
                                           studyPopId,
                                           targetId,
                                           comparatorId,
                                           outcomeId) {
  name <- paste("StudyPop_l", loadId, "_s", studyPopId, "_t", targetId, "_c", comparatorId, sep = "")
  name <- paste(name, "_o", outcomeId, sep = "")
  name <- paste(name, ".rds", sep = "")
  return(file.path(folder, name))
}

.createPsFileName <- function(folder,
                              loadId,
                              studyPopId,
                              psArgsId,
                              targetId,
                              comparatorId) {
  name <- paste("Ps_l", loadId, "_s", studyPopId, "_p", psArgsId, "_t", targetId, "_c", comparatorId, sep = "")
  name <- paste(name, ".rds", sep = "")
  return(file.path(folder, name))
}

.createPsOutcomeFileName <- function(folder,
                                     loadId,
                                     studyPopId,
                                     psArgsId,
                                     targetId,
                                     comparatorId,
                                     outcomeId) {
  name <- paste("Ps_l", loadId, "_s", studyPopId, "_p", psArgsId, "_t", targetId, "_c", comparatorId, sep = "")
  name <- paste(name, "_o", outcomeId, sep = "")
  name <- paste(name, ".rds", sep = "")
  return(file.path(folder, name))
}

.createStratifiedPopFileName <- function(folder,
                                         loadId,
                                         studyPopId,
                                         psArgsId,
                                         strataArgsId,
                                         targetId,
                                         comparatorId,
                                         outcomeId) {
  name <- paste("StratPop_l", loadId, "_s", studyPopId, "_p", psArgsId, "_t", targetId, "_c", comparatorId, sep = "")
  name <- paste(name, "_s", strataArgsId, sep = "")
  name <- paste(name, "_o", outcomeId, sep = "")
  name <- paste(name, ".rds", sep = "")
  return(file.path(folder, name))
}

.createCovariateBalanceFileName <- function(folder,
                                            loadId,
                                            studyPopId,
                                            psArgsId,
                                            strataArgsId,
                                            targetId,
                                            comparatorId,
                                            outcomeId) {
  name <- paste("Bal_l", loadId, "_s", studyPopId, "_p", psArgsId, "_t", targetId, "_c", comparatorId, sep = "")
  name <- paste(name, "_s", strataArgsId, sep = "")
  name <- paste(name, "_o", outcomeId, sep = "")
  name <- paste(name, ".rds", sep = "")
  return(file.path(folder, name))
}

.createOutcomeModelFileName <- function(folder,
                                        targetId,
                                        comparatorId,
                                        outcomeId) {
  name <- paste("om_t", targetId, "_c", comparatorId, sep = "")
  name <- paste(name, "_o", outcomeId, sep = "")
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
#' @param outcomeReference   A data.frame as created by the \code{\link{runCmAnalyses}} function.
#'
#' @return
#' A data frame with the following columns:
#' \tabular{ll}{
#' \verb{analysisId} \tab The unique identifier for a set of analysis choices.\cr
#' \verb{targetId} \tab The ID of the target drug.\cr
#' \verb{comparatorId} \tab The ID of the comparator group.\cr
#' \verb{indicationConceptIds} \tab The ID(s) of indications in which to nest to study. \cr
#' \verb{outcomeId} \tab The ID of the outcome.\cr
#' \verb{rr} \tab The estimated effect size.\cr
#' \verb{ci95lb} \tab The lower bound of the 95 percent confidence interval.\cr
#' \verb{ci95ub} \tab The upper bound of the 95 percent confidence interval.\cr
#' \verb{treated} \tab The number of subjects in the treated group (after any trimming and matching).\cr
#' \verb{comparator} \tab The number of subjects in the comparator group (after any trimming and matching).\cr
#' \verb{eventsTreated} \tab The number of outcomes in the treated group (after any trimming and matching).\cr
#' \verb{eventsComparator} \tab The number of outcomes in the comparator group (after any trimming and \cr
#' \tab matching).\cr
#' \verb{logRr} \tab The log of the estimated relative risk.\cr
#' \verb{seLogRr} \tab The standard error of the log of the estimated relative risk.\cr
#' }
#'
#' @export
summarizeAnalyses <- function(outcomeReference) {
  columns <- c("analysisId", "targetId", "comparatorId", "outcomeId")
  result <- outcomeReference[, columns]
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
  for (i in 1:nrow(outcomeReference)) {
    if (outcomeReference$outcomeModelFile[i] != ""){
      outcomeModel <- readRDS(outcomeReference$outcomeModelFile[i])
      if (outcomeReference$strataFile[i] == "") {
        studyPop <- readRDS(outcomeReference$studyPopFile[i])
      } else {
        studyPop <- readRDS(outcomeReference$strataFile[i])
      }
      result$rr[i] <- if (is.null(coef(outcomeModel)))
        NA else exp(coef(outcomeModel))
      result$ci95lb[i] <- if (is.null(coef(outcomeModel)))
        NA else exp(confint(outcomeModel)[1])
      result$ci95ub[i] <- if (is.null(coef(outcomeModel)))
        NA else exp(confint(outcomeModel)[2])
      if (is.null(coef(outcomeModel))) {
        result$p[i] <- NA
      } else {
        z <- coef(outcomeModel)/ outcomeModel$outcomeModelTreatmentEstimate$seLogRr
        result$p[i] <- 2 * pmin(pnorm(z), 1 - pnorm(z))
      }
      result$treated[i] <- sum(studyPop$treatment == 1)
      result$comparator[i] <- sum(studyPop$treatment == 0)
      if (outcomeModel$outcomeModelType == "cox") {
        result$treatedDays[i] <- sum(studyPop$survivalTime[studyPop$treatment == 1])
        result$comparatorDays[i] <- sum(studyPop$survivalTime[studyPop$treatment == 0])
      } else if (outcomeModel$outcomeModelType == "poisson") {
        result$treatedDays[i] <- sum(studyPop$timeAtRisk[studyPop$treatment == 1])
        result$comparatorDays[i] <- sum(studyPop$timeAtRisk[studyPop$treatment == 0])
      }
      if (outcomeModel$outcomeModelType == "cox" || outcomeModel$outcomeModelType == "logistic") {
        result$eventsTreated[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 1] != 0)
        result$eventsComparator[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 0] != 0)
      } else {
        result$eventsTreated[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 1])
        result$eventsComparator[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 0])
      }
      result$logRr[i] <- if (is.null(coef(outcomeModel)))
        NA else coef(outcomeModel)
      result$seLogRr[i] <- if (is.null(coef(outcomeModel)))
        NA else outcomeModel$outcomeModelTreatmentEstimate$seLogRr
    }
  }
  return(result)
}

# @file Analyses.R
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

#' @export
createCohortMethodAnalysis <- function(analysisId = 1,
                                       getDbCohortMethodDataArgs,
                                       createPs = TRUE,
                                       createPsArgs,
                                       trimByPs = FALSE,
                                       trimByPsArgs = NULL,
                                       trimByPsToEquipoise = FALSE,
                                       trimByPsToEquipoiseArgs = NULL,
                                       matchOnPs = FALSE,
                                       matchOnPsArgs = NULL,
                                       matchOnPsAndCovariates = FALSE,
                                       matchOnPsAndCovariatesArgs = NULL,
                                       stratifyByPs = FALSE,
                                       stratifyByPsArgs = NULL,
                                       stratifyByPsAndCovariates = FALSE,
                                       stratifyByPsAndCovariatesArgs = NULL,
                                       fitOutcomeModel = FALSE,
                                       fitOutcomeModelArgs = NULL) {
  if (matchOnPs + matchOnPsAndCovariates + stratifyByPs + stratifyByPsAndCovariates > 1) {
    stop("Need to pick one matching or stratification function")
  }
  if (trimByPs && trimByPsToEquipoise) {
    stop("Cannot trim to fraction and equipoise at the same time")
  }
  if (!createPs && (trimByPs | matchOnPsAndCovariates | stratifyByPs)) {
    stop("Must create propensity score model to use it for trimming, matching, or stratification")
  }
  if (!createPs) {
    createPsArgs <- NULL
  }
  if (!trimByPs) {
    trimByPsArgs <- NULL
  }
  if (!trimByPsToEquipoise) {
    trimByPsToEquipoiseArgs <- NULL
  }
  if (!matchOnPs) {
    matchOnPsArgs <- NULL
  }
  if (!matchOnPsAndCovariates) {
    matchOnPsAndCovariatesArgs <- NULL
  }
  if (!stratifyByPs) {
    stratifyByPsArgs <- NULL
  }
  if (!stratifyByPsAndCovariates) {
    stratifyByPsAndCovariatesArgs <- NULL
  }
  if (!fitOutcomeModel) {
    fitOutcomeModelArgs <- NULL
  }

  # First: get the default values:
  analysis <- list()
  for (name in names(formals(createCohortMethodAnalysis))) {
    analysis[[name]] <- get(name)
  }

  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis)) {
      analysis[[name]] <- values[[name]]
    }
  }

  class(analysis) <- "cohortMethodAnalysis"
  return(analysis)
}

#' Save a list of cohortMethodAnalysis to file
#'
#' @description
#' Write a list of objects of type \code{cohortMethodAnalysis} to file. The file is in JSON format.
#'
#' @param cohortMethodAnalysisList   The cohortMethodAnalysis list to be written to file
#' @param file                       The name of the file where the results will be written
#'
#' @export
saveCohortMethodAnalysisList <- function(cohortMethodAnalysisList, file) {
  stopifnot(is.list(cohortMethodAnalysisList))
  stopifnot(length(cohortMethodAnalysisList) > 0)
  for (i in 1:length(cohortMethodAnalysisList)) {
    stopifnot(class(cohortMethodAnalysisList[[i]]) == "cohortMethodAnalysis")
  }
  write(rjson::toJSON(cohortMethodAnalysisList), file)
}

#' Load a list of cohortMethodAnalysis from file
#'
#' @description
#' Load a list of objects of type \code{cohortMethodAnalysis} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{cohortMethodAnalysis}.
#'
#' @export
loadCohortMethodAnalysisList <- function(file) {
  cohortMethodAnalysisList <- rjson::fromJSON(file = file)
  for (i in 1:length(cohortMethodAnalysisList)) {
    class(cohortMethodAnalysisList[[i]]) <- "cohortMethodAnalysis"
    for (j in 1:length(cohortMethodAnalysisList[[i]])) {
      if (is.list(cohortMethodAnalysisList[[i]][[j]])) {
        class(cohortMethodAnalysisList[[i]][[j]]) <- "args"
      }
    }
    class(cohortMethodAnalysisList[[i]]$createPsArgs$prior) <- "cyclopsPrior"
    class(cohortMethodAnalysisList[[i]]$createPsArgs$control) <- "cyclopsControl"
    class(cohortMethodAnalysisList[[i]]$fitOutcomeModelArgs$prior) <- "cyclopsPrior"
    class(cohortMethodAnalysisList[[i]]$fitOutcomeModelArgs$control) <- "cyclopsControl"
  }
  return(cohortMethodAnalysisList)
}

#' @export
createDrugComparatorOutcome <- function(targetDrugConceptId,
                                        comparatorDrugConceptId,
                                        outcomeConceptId,
                                        indicationConceptIds = c(),
                                        exclusionConceptIds = c()) {
  # First: get the default values:
  drugComparatorOutcome <- list()
  for (name in names(formals(createDrugComparatorOutcome))) {
    drugComparatorOutcome[[name]] <- get(name)
  }

  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(drugComparatorOutcome)) {
      drugComparatorOutcome[[name]] <- values[[name]]
    }
  }
  class(drugComparatorOutcome) <- "drugComparatorOutcome"
  return(drugComparatorOutcome)
}

#' Save a list of drugComparatorOutcome to file
#'
#' @description
#' Write a list of objects of type \code{drugComparatorOutcome} to file. The file is in JSON format.
#'
#' @param drugComparatorOutcomeList   The drugComparatorOutcome list to be written to file
#' @param file                        The name of the file where the results will be written
#'
#' @export
saveDrugComparatorOutcomeList <- function(drugComparatorOutcomeList, file) {
  stopifnot(is.list(drugComparatorOutcomeList))
  stopifnot(length(drugComparatorOutcomeList) > 0)
  for (i in 1:length(drugComparatorOutcomeList)) {
    stopifnot(class(drugComparatorOutcomeList[[i]]) == "drugComparatorOutcome")
  }
  write(rjson::toJSON(drugComparatorOutcomeList), file)
}

#' Load a list of drugComparatorOutcome from file
#'
#' @description
#' Load a list of objects of type \code{drugComparatorOutcome} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{drugComparatorOutcome}.
#'
#' @export
loadDrugComparatorOutcomeList <- function(file) {
  drugComparatorOutcomeList <- rjson::fromJSON(file = file)
  for (i in 1:length(drugComparatorOutcomeList)) {
    class(drugComparatorOutcomeList[[i]]) <- "drugComparatorOutcome"
  }
  return(drugComparatorOutcomeList)
}

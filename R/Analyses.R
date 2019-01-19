# @file Analyses.R
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

#' Create a CohortMethod analysis specification
#'
#' @details
#' Create a set of analysis choices, to be used with the \code{\link{runCmAnalyses}} function.
#'
#' @param analysisId                      An integer that will be used later to refer to this specific
#'                                        set of analysis choices.
#' @param description                     A short description of the analysis.
#' @param targetType                      If more than one target is provided for each
#'                                        drugComparatorOutcome, this field should be used to select
#'                                        the specific target to use in this analysis.
#' @param comparatorType                  If more than one comparator is provided for each
#'                                        drugComparatorOutcome, this field should be used to select
#'                                        the specific comparator to use in this analysis.
#' @param getDbCohortMethodDataArgs       An object representing the arguments to be used when calling
#'                                        the \code{\link{getDbCohortMethodData}} function.
#' @param createStudyPopArgs              An object representing the arguments to be used when calling
#'                                        the \code{\link{createStudyPopulation}} function.
#' @param createPs                        Should the \code{\link{createPs}} function be used in this
#'                                        analysis?
#' @param createPsArgs                    An object representing the arguments to be used when calling
#'                                        the \code{\link{createPs}} function.
#' @param trimByPs                        Should the \code{\link{trimByPs}} function be used in this
#'                                        analysis?
#' @param trimByPsArgs                    An object representing the arguments to be used when calling
#'                                        the \code{\link{trimByPs}} function.
#' @param trimByPsToEquipoise             Should the \code{\link{trimByPsToEquipoise}} function be used
#'                                        in this analysis?
#' @param trimByPsToEquipoiseArgs         An object representing the arguments to be used when calling
#'                                        the \code{\link{trimByPsToEquipoise}} function.
#' @param matchOnPs                       Should the \code{\link{matchOnPs}} function be used in this
#'                                        analysis?
#' @param matchOnPsArgs                   An object representing the arguments to be used when calling
#'                                        the \code{\link{matchOnPs}} function.
#' @param matchOnPsAndCovariates          Should the \code{\link{matchOnPsAndCovariates}} function be
#'                                        used in this analysis?
#' @param matchOnPsAndCovariatesArgs      An object representing the arguments to be used when calling
#'                                        the \code{\link{matchOnPsAndCovariates}} function.
#' @param stratifyByPs                    Should the \code{\link{stratifyByPs}} function be used in
#'                                        this analysis?
#' @param stratifyByPsArgs                An object representing the arguments to be used when calling
#'                                        the \code{\link{stratifyByPs}} function.
#' @param stratifyByPsAndCovariates       Should the \code{\link{stratifyByPsAndCovariates}} function
#'                                        be used in this analysis?
#' @param stratifyByPsAndCovariatesArgs   An object representing the arguments to be used when calling
#'                                        the \code{\link{stratifyByPsAndCovariates}} function.
#' @param fitOutcomeModel                 Should the \code{\link{fitOutcomeModel}} function be used in
#'                                        this analysis?
#' @param fitOutcomeModelArgs             An object representing the arguments to be used when calling
#'                                        the \code{\link{fitOutcomeModel}} function.
#'
#' @export
createCmAnalysis <- function(analysisId = 1,
                             description = "",
                             targetType = NULL,
                             comparatorType = NULL,
                             getDbCohortMethodDataArgs,
                             createStudyPopArgs,
                             createPs = FALSE,
                             createPsArgs = NULL,
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
  if (!createPs && (trimByPs | matchOnPs | matchOnPsAndCovariates | stratifyByPs | stratifyByPsAndCovariates)) {
    stop("Must create propensity score model to use it for trimming, matching, or stratification")
  }
  if (!(matchOnPs | matchOnPsAndCovariates | stratifyByPs | stratifyByPsAndCovariates) && !is.null(fitOutcomeModelArgs) &&
      fitOutcomeModelArgs$stratified) {
    stop("Must create strata by using matching or stratification to fit a stratified outcome model")
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
  for (name in names(formals(createCmAnalysis))) {
    analysis[[name]] <- get(name)
  }

  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis)) {
      analysis[[name]] <- values[[name]]
    }
  }

  class(analysis) <- "cmAnalysis"
  return(analysis)
}

#' Save a list of cmAnalysis to file
#'
#' @description
#' Write a list of objects of type \code{cmAnalysis} to file. The file is in JSON format.
#'
#' @param cmAnalysisList   The cmAnalysis list to be written to file
#' @param file             The name of the file where the results will be written
#'
#' @export
saveCmAnalysisList <- function(cmAnalysisList, file) {
  stopifnot(is.list(cmAnalysisList))
  stopifnot(length(cmAnalysisList) > 0)
  for (i in 1:length(cmAnalysisList)) {
    stopifnot(class(cmAnalysisList[[i]]) == "cmAnalysis")
  }
  ParallelLogger::logTrace("Saving cmAnalysisList to ", file)
  ParallelLogger::saveSettingsToJson(cmAnalysisList, file)
}

#' Load a list of cmAnalysis from file
#'
#' @description
#' Load a list of objects of type \code{cmAnalysis} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{cmAnalysis}.
#'
#' @export
loadCmAnalysisList <- function(file) {
  ParallelLogger::logTrace("Loading cmAnalysisList from ", file)
  return(ParallelLogger::loadSettingsFromJson(file))
}

#' Create target-comparator-outcomes combinations.
#'
#' @details
#' Create a set of hypotheses of interest, to be used with the \code{\link{runCmAnalyses}} function.
#'
#' @param targetId                      A concept ID indentifying the target drug in the exposure
#'                                      table. If multiple strategies for picking the target will be
#'                                      tested in the analysis, a named list of numbers can be provided
#'                                      instead. In the analysis, the name of the number to be used can
#'                                      be specified using the #' \code{targetType} parameter in the
#'                                      \code{\link{createCmAnalysis}} function.
#' @param comparatorId                  A concept ID indentifying the comparator drug in the exposure
#'                                      table. If multiple strategies for picking the comparator will
#'                                      be tested in the analysis, a named list of numbers can be
#'                                      provided instead. In the analysis, the name of the number to be
#'                                      used can be specified using the #' \code{comparatorType}
#'                                      parameter in the \code{\link{createCmAnalysis}} function.
#' @param outcomeIds                    A vector of concept IDs indentifying the outcome(s) in the
#'                                      outcome table.
#' @param excludedCovariateConceptIds   A list of concept IDs that cannot be used to construct
#'                                      covariates. This argument is to be used only for exclusion
#'                                      concepts that are specific to the drug-comparator combination.
#' @param includedCovariateConceptIds   A list of concept IDs that must be used to construct
#'                                      covariates. This argument is to be used only for inclusion
#'                                      concepts that are specific to the drug-comparator combination.
#'
#' @export
createTargetComparatorOutcomes <- function(targetId,
                                           comparatorId,
                                           outcomeIds,
                                           excludedCovariateConceptIds = c(),
                                           includedCovariateConceptIds = c()) {
  # First: get the default values:
  targetComparatorOutcomes <- list()
  for (name in names(formals(createTargetComparatorOutcomes))) {
    targetComparatorOutcomes[[name]] <- get(name)
  }

  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(targetComparatorOutcomes)) {
      targetComparatorOutcomes[[name]] <- values[[name]]
    }
  }
  class(targetComparatorOutcomes) <- "targetComparatorOutcomes"
  return(targetComparatorOutcomes)
}

#' Save a list of targetComparatorOutcomes to file
#'
#' @description
#' Write a list of objects of type \code{targetComparatorOutcomes} to file. The file is in JSON format.
#'
#' @param targetComparatorOutcomesList   The targetComparatorOutcomes list to be written to file
#' @param file                         The name of the file where the results will be written
#'
#' @export
saveTargetComparatorOutcomesList <- function(targetComparatorOutcomesList, file) {
  stopifnot(is.list(targetComparatorOutcomesList))
  stopifnot(length(targetComparatorOutcomesList) > 0)
  for (i in 1:length(targetComparatorOutcomesList)) {
    stopifnot(class(targetComparatorOutcomesList[[i]]) == "targetComparatorOutcomes")
  }
  ParallelLogger::saveSettingsToJson(targetComparatorOutcomesList, file)
}

#' Load a list of targetComparatorOutcomes from file
#'
#' @description
#' Load a list of objects of type \code{targetComparatorOutcomes} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{targetComparatorOutcomes}.
#'
#' @export
loadTargetComparatorOutcomesList <- function(file) {
  return(ParallelLogger::loadSettingsFromJson(file))
}

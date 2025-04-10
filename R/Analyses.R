# @file Analyses.R
#
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

#' Create a CohortMethod analysis specification
#'
#' @details
#' Create a set of analysis choices, to be used with the [runCmAnalyses()] function.
#'
#' Providing a NULL value for any of the argument applies the corresponding step will not be executed.
#' For example, if `createPsArgs = NULL`, no propensity scores will be computed.
#'
#' @param analysisId                      An integer that will be used later to refer to this specific
#'                                        set of analysis choices.
#' @param description                     A short description of the analysis.
#' @param getDbCohortMethodDataArgs       An object representing the arguments to be used when calling
#'                                        the [getDbCohortMethodData()] function.
#' @param createStudyPopArgs              An object representing the arguments to be used when calling
#'                                        the [createStudyPopulation()] function.
#' @param createPsArgs                    An object representing the arguments to be used when calling
#'                                        the [createPs()] function.
#' @param trimByPsArgs                    An object representing the arguments to be used when calling
#'                                        the [trimByPs()] function.
#' @param trimByPsToEquipoiseArgs         An object representing the arguments to be used when calling
#'                                        the [trimByPsToEquipoise()] function.
#' @param trimByIptwArgs                  An object representing the arguments to be used when calling
#'                                        the [trimByIptw()] function.
#' @param truncateIptwArgs                An object representing the arguments to be used when calling
#'                                        the [truncateIptw()] function.
#' @param matchOnPsArgs                   An object representing the arguments to be used when calling
#'                                        the [matchOnPs()] function.
#' @param matchOnPsAndCovariatesArgs      An object representing the arguments to be used when calling
#'                                        the [matchOnPsAndCovariates()] function.
#' @param stratifyByPsArgs                An object representing the arguments to be used when calling
#'                                        the [stratifyByPs()] function.
#' @param stratifyByPsAndCovariatesArgs   An object representing the arguments to be used when calling
#'                                        the [stratifyByPsAndCovariates()] function.
#' @param computeSharedCovariateBalanceArgs  An object representing the arguments to be used when calling
#'                                          the [computeCovariateBalance()] function per target-comparator-analysis.
#' @param computeCovariateBalanceArgs     An object representing the arguments to be used when calling
#'                                        the [computeCovariateBalance()] function per target-comparator-outcome-analysis.
#' @param fitOutcomeModelArgs             An object representing the arguments to be used when calling
#'                                        the [fitOutcomeModel()] function.
#'
#' @export
createCmAnalysis <- function(analysisId = 1,
                             description = "",
                             getDbCohortMethodDataArgs,
                             createStudyPopArgs,
                             createPsArgs = NULL,
                             trimByPsArgs = NULL,
                             trimByPsToEquipoiseArgs = NULL,
                             trimByIptwArgs = NULL,
                             truncateIptwArgs = NULL,
                             matchOnPsArgs = NULL,
                             matchOnPsAndCovariatesArgs = NULL,
                             stratifyByPsArgs = NULL,
                             stratifyByPsAndCovariatesArgs = NULL,
                             computeSharedCovariateBalanceArgs = NULL,
                             computeCovariateBalanceArgs = NULL,
                             fitOutcomeModelArgs = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(analysisId, add = errorMessages)
  checkmate::assertCharacter(description, len = 1, add = errorMessages)
  checkmate::assertClass(getDbCohortMethodDataArgs, "args", add = errorMessages)
  checkmate::assertClass(createStudyPopArgs, "args", add = errorMessages)
  checkmate::assertClass(createPsArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(trimByPsArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(trimByPsToEquipoiseArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(trimByIptwArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(truncateIptwArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(matchOnPsArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(matchOnPsAndCovariatesArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(stratifyByPsArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(stratifyByPsAndCovariatesArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(computeSharedCovariateBalanceArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(computeCovariateBalanceArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(fitOutcomeModelArgs, "args", null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if ((!is.null(matchOnPsArgs)) +
      (!is.null(matchOnPsAndCovariatesArgs)) +
      (!is.null(stratifyByPsArgs)) +
      (!is.null(stratifyByPsAndCovariatesArgs)) > 1) {
    stop("Need to pick one matching or stratification function")
  }
  if ((!is.null(trimByPsArgs)) +
      (!is.null(trimByPsToEquipoiseArgs)) +
      (!is.null(trimByIptwArgs)) > 1) {
    stop("Need to pick one trimming strategy")
  }
  if (is.null(createPsArgs) && (!is.null(trimByPsArgs) |
                                !is.null(trimByPsToEquipoiseArgs) |
                                !is.null(trimByIptwArgs) |
                                !is.null(truncateIptwArgs) |
                                !is.null(matchOnPsArgs) |
                                !is.null(matchOnPsAndCovariatesArgs) |
                                !is.null(stratifyByPsArgs) |
                                !is.null(stratifyByPsAndCovariatesArgs))) {
    stop("Must create propensity score model to use it for trimming, matching, or stratification")
  }
  if (!is.null(fitOutcomeModelArgs) && fitOutcomeModelArgs$stratified && (is.null(matchOnPsArgs) &
                                                                          is.null(matchOnPsAndCovariatesArgs) &
                                                                          is.null(stratifyByPsArgs) &
                                                                          is.null(stratifyByPsAndCovariatesArgs))) {
    stop("Must create strata by using matching or stratification to fit a stratified outcome model")
  }
  if (!is.null(createPsArgs) && (is.null(computeSharedCovariateBalanceArgs) && is.null(computeCovariateBalanceArgs))) {
    message("Note: Using propensity scores but not computing covariate balance")
  }

  analysis <- list()
  for (name in names(formals(createCmAnalysis))) {
    analysis[[name]] <- get(name)
  }

  class(analysis) <- "cmAnalysis"
  return(analysis)
}

#' Save a list of cmAnalysis to file
#'
#' @description
#' Write a list of objects of type `cmAnalysis` to file. The file is in JSON format.
#'
#' @param cmAnalysisList   The cmAnalysis list to be written to file
#' @param file             The name of the file where the results will be written
#'
#' @export
saveCmAnalysisList <- function(cmAnalysisList, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(cmAnalysisList, min.len = 1, add = errorMessages)
  for (i in 1:length(cmAnalysisList)) {
    checkmate::assertClass(cmAnalysisList[[i]], "cmAnalysis", add = errorMessages)
  }
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  ParallelLogger::logTrace("Saving cmAnalysisList to ", file)
  ParallelLogger::saveSettingsToJson(cmAnalysisList, file)
}

#' Load a list of cmAnalysis from file
#'
#' @description
#' Load a list of objects of type `cmAnalysis` from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type `cmAnalysis`.
#'
#' @export
loadCmAnalysisList <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  ParallelLogger::logTrace("Loading cmAnalysisList from ", file)
  return(ParallelLogger::loadSettingsFromJson(file))
}

#' Create outcome definition
#'
#' @param outcomeId                        An integer used to identify the outcome in the outcome cohort table.
#' @param outcomeOfInterest                Is this an outcome of interest? If not, creation of non-essential
#'                                         files will be skipped, including outcome=specific covariate balance
#'                                         files. This could be helpful to speed up analyses with many controls,
#'                                         for which we're only interested in the effect size estimate.
#' @param trueEffectSize                   For negative and positive controls: the known true effect size. To be used
#'                                         for empirical calibration. Negative controls have `trueEffectSize = 1`. If
#'                                         the true effect size is unknown, use `trueEffectSize = NA`
#' @param priorOutcomeLookback             How many days should we look back when identifying prior.
#'                                         outcomes?
#' @param riskWindowStart                  The start of the risk window (in days) relative to the `startAnchor`.
#' @param startAnchor                      The anchor point for the start of the risk window. Can be `"cohort start"`
#'                                         or `"cohort end"`.
#' @param riskWindowEnd                    The end of the risk window (in days) relative to the `endAnchor`.
#' @param endAnchor                        The anchor point for the end of the risk window. Can be `"cohort start"`
#'                                         or `"cohort end"`.
#'
#' @details
#' Any settings here that are not `NULL` will override any values set in [createCreateStudyPopulationArgs()].
#'
#'
#' @return
#' An object of type `outcome`, to be used in [createTargetComparatorOutcomes()].
#'
#' @export
createOutcome <- function(outcomeId,
                          outcomeOfInterest = TRUE,
                          trueEffectSize = NA,
                          priorOutcomeLookback = NULL,
                          riskWindowStart = NULL,
                          startAnchor = NULL,
                          riskWindowEnd = NULL,
                          endAnchor = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(outcomeId, add = errorMessages)
  checkmate::assertTRUE(all(outcomeId %% 1 == 0), add = errorMessages)
  checkmate::assertLogical(outcomeOfInterest, add = errorMessages)
  checkmate::assertNumeric(trueEffectSize, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertInt(riskWindowStart, null.ok = TRUE, add = errorMessages)
  checkmate::assertInt(riskWindowEnd, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (!is.null(startAnchor) && !grepl("start$|end$", startAnchor, ignore.case = TRUE)) {
    stop("startAnchor should have value 'cohort start' or 'cohort end'")
  }
  if (!is.null(riskWindowEnd) && !grepl("start$|end$", endAnchor, ignore.case = TRUE)) {
    stop("endAnchor should have value 'cohort start' or 'cohort end'")
  }

  outcome <- list()
  for (name in names(formals(createOutcome))) {
    outcome[[name]] <- get(name)
  }
  class(outcome) <- "outcome"
  return(outcome)
}

#' Create target-comparator-outcomes combinations.
#'
#' @details
#' Create a set of hypotheses of interest, to be used with the [runCmAnalyses()] function.
#'
#' @param targetId                      A cohort ID identifying the target exposure in the exposure
#'                                      table.
#' @param comparatorId                  A cohort ID identifying the comparator exposure in the exposure
#'                                      table.
#' @param outcomes                      A list of object of type `outcome` as created by
#'                                      [createOutcome()].
#' @param excludedCovariateConceptIds   A list of concept IDs that cannot be used to construct
#'                                      covariates. This argument is to be used only for exclusion
#'                                      concepts that are specific to the target-comparator combination.
#' @param includedCovariateConceptIds   A list of concept IDs that must be used to construct
#'                                      covariates. This argument is to be used only for inclusion
#'                                      concepts that are specific to the target-comparator combination.
#'
#' @return
#' An object of type `targetComparatorOutcomes`.
#'
#' @export
createTargetComparatorOutcomes <- function(targetId,
                                           comparatorId,
                                           outcomes,
                                           excludedCovariateConceptIds = c(),
                                           includedCovariateConceptIds = c()) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(targetId, add = errorMessages)
  checkmate::assertNumeric(comparatorId, add = errorMessages)
  checkmate::assertTRUE(all(c(targetId, comparatorId) %% 1 == 0), add = errorMessages)
  checkmate::assertList(outcomes, min.len = 1, add = errorMessages)
  for (i in seq_along(outcomes)) {
    checkmate::assertClass(outcomes[[i]], "outcome", add = errorMessages)
  }
  checkmate::assertIntegerish(excludedCovariateConceptIds, null.ok = TRUE, add = errorMessages)
  checkmate::assertIntegerish(includedCovariateConceptIds, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  outcomeIds <- rep(0, length(outcomes))
  for (i in seq_along(outcomes)) {
    outcomeIds[i] <- outcomes[[i]]$outcomeId
  }
  duplicatedIds <- outcomeIds[duplicated(outcomeIds)]
  if (length(duplicatedIds) > 0) {
    stop(sprintf("Found duplicate outcome IDs: %s", paste(duplicatedIds, paste = ", ")))
  }

  targetComparatorOutcomes <- list()
  for (name in names(formals(createTargetComparatorOutcomes))) {
    targetComparatorOutcomes[[name]] <- get(name)
  }
  class(targetComparatorOutcomes) <- "targetComparatorOutcomes"
  return(targetComparatorOutcomes)
}

#' Save a list of targetComparatorOutcomes to file
#'
#' @description
#' Write a list of objects of type `targetComparatorOutcomes` to file. The file is in JSON format.
#'
#' @param targetComparatorOutcomesList   The targetComparatorOutcomes list to be written to file
#' @param file                         The name of the file where the results will be written
#'
#' @export
saveTargetComparatorOutcomesList <- function(targetComparatorOutcomesList, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(targetComparatorOutcomesList, min.len = 1, add = errorMessages)
  for (i in 1:length(targetComparatorOutcomesList)) {
    checkmate::assertClass(targetComparatorOutcomesList[[i]], "targetComparatorOutcomes", add = errorMessages)
  }
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  ParallelLogger::saveSettingsToJson(targetComparatorOutcomesList, file)
}

#' Load a list of targetComparatorOutcomes from file
#'
#' @description
#' Load a list of objects of type `targetComparatorOutcomes` from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type `targetComparatorOutcomes`.
#'
#' @export
loadTargetComparatorOutcomesList <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  return(ParallelLogger::loadSettingsFromJson(file))
}

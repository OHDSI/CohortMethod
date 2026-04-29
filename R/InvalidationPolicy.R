# Copyright 2026 Observational Health Data Sciences and Informatics
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

#' Invalidation Policy
#'
#' @description
#' Determines which artifacts should be deleted when analysis settings change,
#' based on dependency relationships between components.
#'
#' @details
#' When settings change, not all artifacts need to be regenerated. This class
#' implements the logic to determine minimal invalidation scope based on which
#' settings components have changed.
#'
#' @export
InvalidationPolicy <- R6::R6Class(
  "InvalidationPolicy",
  public = list(
    #' @description
    #' Compute which file patterns should be deleted based on changed components
    #'
    #' @param changedComponents A list (as returned by SettingsHasher$compareSettingsComponents)
    #'   with logical values for each component
    #'
    #' @return
    #' Character vector of file patterns/globs to delete. Empty vector if no deletion needed.
    #'
    #' @details
    #' Deletion cascade logic:
    #' - If loadArgsChanged: all downstream artifacts (everything)
    #' - Else if studyPopArgsChanged: populations, PS models, strata, outcomes
    #' - Else if psArgsChanged: PS models, strata, outcomes
    #' - Else if strataArgsChanged: strata and outcomes
    #' - Else if outcomeModelArgsChanged: outcome models only
    #' - Else if only balanceArgsChanged: balance files only
    #' - Else if analyticsChanged (only new outcomes): nothing (outcomes are outcome-specific)
    computeInvalidationScope = function(changedComponents) {
      checkmate::assertList(changedComponents)

      filePatternsToDelete <- c()

      # Cascade of invalidation based on dependency graph
      if (isTRUE(changedComponents$loadArgsChanged)) {
        # If data loading changed, everything depends on it, so delete all analysis artifacts
        filePatternsToDelete <- c(
          filePatternsToDelete,
          "CmData_.*\\.zip$",      # All CohortMethodData
          "StudyPop_.*\\.rds$",    # All study populations
          "Ps_.*\\.rds$",          # All propensity scores
          "StratPop_.*\\.rds$",    # All stratified populations
          "Balance_.*\\.rds$",     # All balance files
          "Analysis_[0-9]+$"       # All analysis folders (contains outcome models)
        )
      } else if (isTRUE(changedComponents$studyPopArgsChanged)) {
        # Study population arguments changed: recompute populations and everything downstream
        filePatternsToDelete <- c(
          filePatternsToDelete,
          "StudyPop_.*\\.rds$",    # All study populations
          "Ps_.*\\.rds$",          # All PS (may depend on population)
          "StratPop_.*\\.rds$",    # All stratified populations
          "Balance_.*\\.rds$",     # All balance files
          "Analysis_[0-9]+$"       # All outcome models
        )
      } else if (isTRUE(changedComponents$psArgsChanged)) {
        # Propensity score arguments changed: recompute PS and downstream
        filePatternsToDelete <- c(
          filePatternsToDelete,
          "Ps_.*\\.rds$",          # All PS models
          "StratPop_.*\\.rds$",    # All stratified populations (depend on PS)
          "Balance_.*\\.rds$",     # All balance files
          "Analysis_[0-9]+$"       # All outcome models
        )
      } else if (isTRUE(changedComponents$strataArgsChanged)) {
        # Stratification (trim/match/stratify) changed: recompute strata and downstream
        filePatternsToDelete <- c(
          filePatternsToDelete,
          "StratPop_.*\\.rds$",    # All stratified populations
          "Balance_.*\\.rds$",     # All balance files
          "Analysis_[0-9]+$"       # All outcome models
        )
      } else if (isTRUE(changedComponents$outcomeModelArgsChanged)) {
        # Outcome model arguments changed: recompute outcome models only
        filePatternsToDelete <- c(
          filePatternsToDelete,
          "Analysis_[0-9]+$"       # All outcome model folders
        )
      } else if (isTRUE(changedComponents$balanceArgsChanged)) {
        # Balance computation arguments changed: delete balance files only
        filePatternsToDelete <- c(
          filePatternsToDelete,
          "Balance_.*\\.rds$"      # All balance files
        )
      }
      # If analyticsChanged but nothing else changed (e.g., only outcomes added):
      # No deletion needed - outcome models are outcome-specific, new ones will be computed

      return(filePatternsToDelete)
    },

    #' @description
    #' Get a human-readable message describing what will be deleted
    #'
    #' @param changedComponents A list of changed components
    #'
    #' @return
    #' Character string suitable for displaying to the user
    getInvalidationMessage = function(changedComponents) {
      checkmate::assertList(changedComponents)

      if (isTRUE(changedComponents$loadArgsChanged)) {
        return("Data loading arguments have changed. All analysis artifacts must be regenerated.")
      } else if (isTRUE(changedComponents$studyPopArgsChanged)) {
        return("Study population settings have changed. Study populations, propensity scores, and outcome models must be regenerated.")
      } else if (isTRUE(changedComponents$psArgsChanged)) {
        return("Propensity score settings have changed. Propensity scores and outcome models must be regenerated.")
      } else if (isTRUE(changedComponents$strataArgsChanged)) {
        return("Stratification settings (trimming/matching/stratifying) have changed. Stratified populations and outcome models must be regenerated.")
      } else if (isTRUE(changedComponents$outcomeModelArgsChanged)) {
        return("Outcome model settings have changed. Outcome models must be regenerated.")
      } else if (isTRUE(changedComponents$balanceArgsChanged)) {
        return("Covariate balance settings have changed. Balance files must be recomputed.")
      } else if (isTRUE(changedComponents$analyticsChanged)) {
        return("Analyses have changed (new outcomes or studies added). New analyses will be computed.")
      } else {
        return("No artifacts need to be deleted.")
      }
    }
  )
)

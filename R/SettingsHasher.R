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

#' Settings Hasher
#'
#' @description
#' Compute deterministic hashes for individual settings components to detect
#' which parts of the analysis specification have changed.
#'
#' @details
#' This class hashes each settings component independently using JSON serialization
#' and MD5 digests. This allows fine-grained detection of which artifact types need
#' to be invalidated when specifications change.
#'
#' @export
SettingsHasher <- R6::R6Class(
  "SettingsHasher",
  public = list(
    #' @description
    #' Hash the data loading arguments
    #'
    #' @param getDbCohortMethodDataArgs An object of type `GetDbCohortMethodDataArgs`
    #'
    #' @return
    #' Character string containing the MD5 hash (32 hex characters)
    hashLoadArgs = function(getDbCohortMethodDataArgs) {
      if (is.null(getDbCohortMethodDataArgs)) {
        return("")
      }
      json <- getDbCohortMethodDataArgs$toJson()
      return(digest::digest(json, algo = "md5"))
    },

    #' @description
    #' Hash the study population arguments
    #'
    #' @param createStudyPopulationArgs An object of type `CreateStudyPopulationArgs`
    #'
    #' @return
    #' Character string containing the MD5 hash (32 hex characters)
    hashStudyPopArgs = function(createStudyPopulationArgs) {
      if (is.null(createStudyPopulationArgs)) {
        return("")
      }
      json <- createStudyPopulationArgs$toJson()
      return(digest::digest(json, algo = "md5"))
    },

    #' @description
    #' Hash the propensity score arguments
    #'
    #' @param createPsArgs An object of type `CreatePsArgs`
    #'
    #' @return
    #' Character string containing the MD5 hash (32 hex characters)
    hashPsArgs = function(createPsArgs) {
      if (is.null(createPsArgs)) {
        return("")
      }
      json <- createPsArgs$toJson()
      return(digest::digest(json, algo = "md5"))
    },

    #' @description
    #' Hash arguments for propensity score trimming
    #'
    #' @param trimByPsArgs An object of type `TrimByPsArgs`
    #'
    #' @return
    #' Character string containing the MD5 hash (32 hex characters)
    hashTrimByPsArgs = function(trimByPsArgs) {
      if (is.null(trimByPsArgs)) {
        return("")
      }
      json <- trimByPsArgs$toJson()
      return(digest::digest(json, algo = "md5"))
    },

    #' @description
    #' Hash arguments for propensity score matching
    #'
    #' @param matchOnPsArgs An object of type `MatchOnPsArgs`
    #'
    #' @return
    #' Character string containing the MD5 hash (32 hex characters)
    hashMatchOnPsArgs = function(matchOnPsArgs) {
      if (is.null(matchOnPsArgs)) {
        return("")
      }
      json <- matchOnPsArgs$toJson()
      return(digest::digest(json, algo = "md5"))
    },

    #' @description
    #' Hash arguments for propensity score stratification
    #'
    #' @param stratifyByPsArgs An object of type `StratifyByPsArgs`
    #'
    #' @return
    #' Character string containing the MD5 hash (32 hex characters)
    hashStratifyByPsArgs = function(stratifyByPsArgs) {
      if (is.null(stratifyByPsArgs)) {
        return("")
      }
      json <- stratifyByPsArgs$toJson()
      return(digest::digest(json, algo = "md5"))
    },

    #' @description
    #' Hash arguments for outcome model fitting
    #'
    #' @param fitOutcomeModelArgs An object of type `FitOutcomeModelArgs`
    #'
    #' @return
    #' Character string containing the MD5 hash (32 hex characters)
    hashOutcomeModelArgs = function(fitOutcomeModelArgs) {
      if (is.null(fitOutcomeModelArgs)) {
        return("")
      }
      json <- fitOutcomeModelArgs$toJson()
      return(digest::digest(json, algo = "md5"))
    },

    #' @description
    #' Hash arguments for covariate balance computation
    #'
    #' @param computeCovariateBalanceArgs An object of type `ComputeCovariateBalanceArgs`
    #'
    #' @return
    #' Character string containing the MD5 hash (32 hex characters)
    hashBalanceArgs = function(computeCovariateBalanceArgs) {
      if (is.null(computeCovariateBalanceArgs)) {
        return("")
      }
      json <- computeCovariateBalanceArgs$toJson()
      return(digest::digest(json, algo = "md5"))
    },

    #' @description
    #' Compare old and new specifications to identify which components changed
    #'
    #' @param oldSpecs An object of type `CmAnalysesSpecifications` (old version)
    #' @param newSpecs An object of type `CmAnalysesSpecifications` (new version)
    #'
    #' @return
    #' A list of logical values indicating which components changed:
    #' - loadArgsChanged: whether data loading arguments differ
    #' - studyPopArgsChanged: whether study population arguments differ
    #' - psArgsChanged: whether propensity score arguments differ
    #' - strataArgsChanged: whether stratification arguments (trim/match/stratify) differ
    #' - outcomeModelArgsChanged: whether outcome model arguments differ
    #' - balanceArgsChanged: whether balance computation arguments differ
    #' - analyticsChanged: whether outcome of interest or analysis IDs changed
    compareSettingsComponents = function(oldSpecs, newSpecs) {
      checkmate::assertClass(oldSpecs, "CmAnalysesSpecifications")
      checkmate::assertClass(newSpecs, "CmAnalysesSpecifications")

      # Compare each analysis component's settings
      changedComponents <- list(
        loadArgsChanged = FALSE,
        studyPopArgsChanged = FALSE,
        psArgsChanged = FALSE,
        strataArgsChanged = FALSE,
        outcomeModelArgsChanged = FALSE,
        balanceArgsChanged = FALSE,
        analyticsChanged = FALSE
      )

      # Get the old and new analysis lists
      oldAnalyses <- oldSpecs$cmAnalysisList
      newAnalyses <- newSpecs$cmAnalysisList

      # If the number of analyses differs significantly or can't be matched, mark as changed
      if (length(oldAnalyses) != length(newAnalyses)) {
        # For now, mark as all changed if list lengths differ
        # A more sophisticated approach would match analyses by ID
        changedComponents$analyticsChanged <- TRUE
      } else {
        # Compare each analysis by ID (assuming they maintain order/ID)
        for (i in seq_along(newAnalyses)) {
          oldAna <- oldAnalyses[[i]]
          newAna <- newAnalyses[[i]]

          # Check data loading
          oldLoadHash <- self$hashLoadArgs(oldAna$getDbCohortMethodDataArgs)
          newLoadHash <- self$hashLoadArgs(newAna$getDbCohortMethodDataArgs)
          if (oldLoadHash != newLoadHash) {
            changedComponents$loadArgsChanged <- TRUE
          }

          # Check study population
          oldStudyPopHash <- self$hashStudyPopArgs(oldAna$createStudyPopulationArgs)
          newStudyPopHash <- self$hashStudyPopArgs(newAna$createStudyPopulationArgs)
          if (oldStudyPopHash != newStudyPopHash) {
            changedComponents$studyPopArgsChanged <- TRUE
          }

          # Check propensity score
          oldPsHash <- self$hashPsArgs(oldAna$createPsArgs)
          newPsHash <- self$hashPsArgs(newAna$createPsArgs)
          if (oldPsHash != newPsHash) {
            changedComponents$psArgsChanged <- TRUE
          }

          # Check stratification (trim/match/stratify all affect strata)
          oldTrimHash <- self$hashTrimByPsArgs(oldAna$trimByPsArgs)
          newTrimHash <- self$hashTrimByPsArgs(newAna$trimByPsArgs)
          oldMatchHash <- self$hashMatchOnPsArgs(oldAna$matchOnPsArgs)
          newMatchHash <- self$hashMatchOnPsArgs(newAna$matchOnPsArgs)
          oldStratifyHash <- self$hashStratifyByPsArgs(oldAna$stratifyByPsArgs)
          newStratifyHash <- self$hashStratifyByPsArgs(newAna$stratifyByPsArgs)

          if (oldTrimHash != newTrimHash || oldMatchHash != newMatchHash || oldStratifyHash != newStratifyHash) {
            changedComponents$strataArgsChanged <- TRUE
          }

          # Check outcome model
          oldOutcomeHash <- self$hashOutcomeModelArgs(oldAna$fitOutcomeModelArgs)
          newOutcomeHash <- self$hashOutcomeModelArgs(newAna$fitOutcomeModelArgs)
          if (oldOutcomeHash != newOutcomeHash) {
            changedComponents$outcomeModelArgsChanged <- TRUE
          }

          # Check balance args
          oldBalanceHash <- self$hashBalanceArgs(oldAna$computeSharedCovariateBalanceArgs)
          newBalanceHash <- self$hashBalanceArgs(newAna$computeSharedCovariateBalanceArgs)
          if (oldBalanceHash != newBalanceHash) {
            changedComponents$balanceArgsChanged <- TRUE
          }
        }
      }

      # Check if outcomes of interest or excluded analyses changed
      oldOutcomes <- oldSpecs$targetComparatorOutcomesList
      newOutcomes <- newSpecs$targetComparatorOutcomesList

      if (length(oldOutcomes) != length(newOutcomes)) {
        changedComponents$analyticsChanged <- TRUE
      } else {
        # Check if outcomes within TCOs differ
        for (i in seq_along(newOutcomes)) {
          oldTco <- oldOutcomes[[i]]
          newTco <- newOutcomes[[i]]

          # Simple check: compare target, comparator, nesting cohort
          if (oldTco$targetId != newTco$targetId ||
            oldTco$comparatorId != newTco$comparatorId ||
            !identical(oldTco$nestingCohortId, newTco$nestingCohortId)) {
            changedComponents$analyticsChanged <- TRUE
            break
          }

          # Check outcome changes (new outcomes added or old ones removed)
          oldOutcomeIds <- sort(sapply(oldTco$outcomes, function(x) x$outcomeId))
          newOutcomeIds <- sort(sapply(newTco$outcomes, function(x) x$outcomeId))
          if (!identical(oldOutcomeIds, newOutcomeIds)) {
            # Just outcomes added: no need to delete
            # Outcomes removed: potentially need to delete
            if (any(!(newOutcomeIds %in% oldOutcomeIds))) {
              # New outcomes added - this is OK, don't mark as changed
            } else {
              # Outcomes removed - this means regeneration may be needed
              changedComponents$analyticsChanged <- TRUE
            }
          }
        }
      }

      return(changedComponents)
    }
  )
)

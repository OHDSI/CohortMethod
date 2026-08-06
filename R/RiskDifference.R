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

computeRiskDifference <- function(population, cohortMethodData, computeRiskDifferenceArgs) {
  start <- Sys.time()
  riskDifference <- attr(population, "metaData")

  # For now: Use timeEL package to compute risk difference and confidence intervals (using Empirical Likelihood).
  # Not yet supporting stratification or weighting and likelihood profiling.
  result <- tryCatch(
    {
      timeEL::TwoSampleKaplanMeier(
        time = population$survivalTime,
        status = population$outcomeCount > 0,
        group = population$treatment,
        t = computeRiskDifferenceArgs$timePoint
      )
    },
    error = function(e) {
      e$message
    }
  )
  if (is.character(result)) {
    riskDifference$status <- result
    riskDifference$estimate <- tibble(
      timePoint = t,
      rd = as.numeric(NA),
      lb95 = as.numeric(NA),
      ub95 = as.numeric(NA),
      seRd = as.numeric(NA)
    ) |>
      mutate(seRd = (.data$ub95 - .data$lb95) / (2 * qnorm(0.975)))
  } else {
    riskDifference$status <- "OK"
    riskDifference$estimate <- tibble(
      timePoint = t,
      rd = result$table.Diff["EL", "est."],
      lb95 = result$table.Diff["EL", "lower"],
      ub95 = result$table.Diff["EL", "upper"]
    ) |>
      mutate(seRd = (.data$ub95 - .data$lb95) / (2 * qnorm(0.975)))
  }

  class(riskDifference) <- "RiskDifference"
  delta <- Sys.time() - start
  message(paste("Computing risk difference took", signif(delta, 3), attr(delta, "units")))
  ParallelLogger::logDebug("Risk difference status is: ", riskDifference$status)
  return(riskDifference)
}

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
  riskDifference$outcomeCounts <- getOutcomeCounts(population, "cox")
  riskDifference$attritionAtTimePoint <- computeAttritionAtTimePoint(
    population = population,
    timePoint = computeRiskDifferenceArgs$timePoint
  )
  if (any(riskDifference$attritionAtTimePoint$attrition > 0.9)) {
    warning(sprintf("Attrition is %s. More than 90 percent attrition can lead to unstable risk difference estimates",
                    paste(sprintf("%0.1f%%", 100 * riskDifference$attritionAtTimePoint$attrition), collapse = " and ")))
  }

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
      timePoint = computeRiskDifferenceArgs$timePoint,
      rd = as.numeric(NA),
      lb95 = as.numeric(NA),
      ub95 = as.numeric(NA),
      seRd = as.numeric(NA)
    ) |>
      mutate(seRd = (.data$ub95 - .data$lb95) / (2 * qnorm(0.975)))
  } else {
    riskDifference$status <- "OK"
    riskDifference$estimate <- tibble(
      timePoint = computeRiskDifferenceArgs$timePoint,
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

computeAttritionAtTimePoint <- function(population, timePoint) {
  attrition <- inner_join(
    population |>
      group_by(.data$treatment) |>
      summarise(exposuresAtStart = n(),
                subjectsAtStart = length(unique(personSeqId))),
    population |>
      filter(.data$survivalTime >= timePoint) |>
      group_by(.data$treatment) |>
      summarise(exposuresAtTimePoint = n(),
                subjectsAtTimePoint = length(unique(personSeqId))),
    by = join_by("treatment")
  ) |>
    mutate(attrition = 1 - (.data$exposuresAtTimePoint / .data$exposuresAtStart))
  return(attrition)
}

#' @export
coef.RiskDifference <- function(object, ...) {
  return(object$estimate$rd)
}

#' @export
confint.RiskDifference <- function(object, parm, level = 0.95, ...) {
  missing(parm) # suppresses R CMD check note
  if (level != 0.95) {
    stop("Only supporting 95% confidence interval")
  }
  return(c(
    object$estimate$lb95,
    object$estimate$ub95
  ))
}

#' @export
print.RiskDifference <- function(x, ...) {
  d <- x$estimate
  if (!is.null(d)) {
    rns <- "treatment"
    output <- data.frame(d$rd, d$lb95, d$ub95, d$seRd)
    colnames(output) <- c("Estimate", "lower .95", "upper .95", "se")
    rownames(output) <- rns
    printCoefmat(output)
  }
}

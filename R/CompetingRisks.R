# Copyright 2020 Observational Health Data Sciences and Informatics
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
#
# @author Observational Health Data Sciences and Informatics
# @author Patrick Ryan
# @author Marc Suchard
# @author Martijn Schuemie

#' Create competing risk study population
#'
#' @description
#' Creates competing risk study population from two overlaping study populations
#'
#' @param mainPopulation           A data frame describing the population. This should at least have a
#'                                 `rowId` column corresponding to the `rowId` column in the
#'                                 [CohortMethodData] covariates object and a `treatment` column.
#'
#' @param competingRiskPopulation  A data frame ....
#'
#' @examples
#' # TODO
#'
#' @export
combineCompetingStudyPopulations <- function(mainPopulation,
                                             competingRiskPopulation) {

  if (length(setdiff(competingRiskPopulation$subjectId, mainPopulation$subjectId)) > 0) {
    stop("Subjects in competing risk population do not exist in main population")
  }

  population <-
    left_join(mainPopulation %>% select(.data$rowId, .data$subjectId, .data$treatment,
                                        .data$outcomeCount, .data$timeAtRisk, .data$survivalTime),
              competingRiskPopulation %>% select(.data$subjectId, .data$treatment, .data$outcomeCount, .data$survivalTime),
              by = c("subjectId", "treatment")) %>%
    mutate(survivalTime = pmin(.data$survivalTime.x, .data$survivalTime.y, na.rm = TRUE)) %>%
    mutate(outcomeCount = 1 * (.data$outcomeCount.x > 0) * (.data$survivalTime.x == .data$survivalTime) +
             ifelse(!is.na(.data$outcomeCount.y),
                    2 * (.data$outcomeCount.y > 0) * (.data$survivalTime.y == .data$survivalTime), 0)) %>%
    select(.data$rowId, .data$subjectId, .data$treatment, .data$timeAtRisk,
           .data$outcomeCount, .data$survivalTime,
           .data$outcomeCount.x, .data$survivalTime.x, .data$outcomeCount.y, .data$survivalTime.y) # Leaving for debugging purposes

  if ("propensityScore" %in% names(mainPopulation)) {
    population <- population %>% left_join(mainPopulation %>% select(subjectId, treatment, propensityScore),
                                           by = c("subjectId", "treatment"))
  }

  if ("stratumId" %in% names(mainPopulation)) {
    population <- population %>% left_join(mainPopulation %>% select(subjectId, treatment, stratumId),
                                           by = c("subjectId", "treatment"))
  }

  return (population)
}

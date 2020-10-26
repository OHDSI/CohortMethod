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
    left_join(mainPopulation %>% select(rowId, subjectId, treatment, outcomeCount, timeAtRisk, survivalTime),
              competingRiskPopulation %>% select(subjectId, outcomeCount, survivalTime),
              by = "subjectId") %>%
    mutate(survivalTime = pmin(survivalTime.x, survivalTime.y, na.rm = TRUE)) %>%
    mutate(outcomeCount =
             1 * (outcomeCount.x > 0) * (survivalTime.x == survivalTime) +
             ifelse(!is.na(outcomeCount.y), 2 * (outcomeCount.y > 0) * (survivalTime.y == survivalTime), 0)) %>%
    select(rowId, subjectId, treatment, timeAtRisk, outcomeCount, survivalTime,
           outcomeCount.x, survivalTime.x, outcomeCount.y, survivalTime.y) # Leaving for debugging purposes

  return (population)
}

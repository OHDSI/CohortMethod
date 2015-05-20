# @file SimulationDataFetch.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
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

#' @keywords internal
.simulationDataFetch <- function(){
  # This function should be used to create the simulation profile used in some of the unit tests
  # library(CohortMethod)
  # setwd("s:/temp")
  # options("fftempdir" = "s:/temp")

  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07.jnj.com"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- NULL

  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms=dbms, server = server, user = user, password = pw, port=port)

  #Load data:
  cohortData <- getDbCohortData(connectionDetails,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                targetDrugConceptId = 755695,
                                comparatorDrugConceptId = 739138,
                                indicationConceptIds = 439926,
                                washoutWindow = 183,
                                indicationLookbackWindow = 183,
                                studyStartDate = "",
                                studyEndDate = "",
                                exclusionConceptIds = c(4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019),
                                outcomeConceptIds = 194133,
                                outcomeConditionTypeConceptIds = c(38000215,38000216,38000217,38000218,38000183,38000232),
                                exposureDatabaseSchema = cdmDatabaseSchema,
                                exposureTable = "drug_era",
                                outcomeDatabaseSchema = cdmDatabaseSchema,
                                outcomeTable = "condition_occurrence",
                                useCovariateDemographics = TRUE,
                                useCovariateConditionOccurrence = TRUE,
                                useCovariateConditionOccurrence365d = TRUE,
                                useCovariateConditionOccurrence30d = TRUE,
                                useCovariateConditionOccurrenceInpt180d = TRUE,
                                useCovariateConditionEra = FALSE,
                                useCovariateConditionEraEver = FALSE,
                                useCovariateConditionEraOverlap = FALSE,
                                useCovariateConditionGroup = FALSE,
                                useCovariateDrugExposure = FALSE,
                                useCovariateDrugExposure365d = FALSE,
                                useCovariateDrugExposure30d = FALSE,
                                useCovariateDrugEra = FALSE,
                                useCovariateDrugEra365d = FALSE,
                                useCovariateDrugEra30d = FALSE,
                                useCovariateDrugEraEver = FALSE,
                                useCovariateDrugEraOverlap = FALSE,
                                useCovariateDrugGroup = FALSE,
                                useCovariateProcedureOccurrence = FALSE,
                                useCovariateProcedureOccurrence365d = FALSE,
                                useCovariateProcedureOccurrence30d = FALSE,
                                useCovariateProcedureGroup = FALSE,
                                useCovariateObservation = FALSE,
                                useCovariateObservation365d = FALSE,
                                useCovariateObservation30d = FALSE,
                                useCovariateObservationBelow = FALSE,
                                useCovariateObservationAbove = FALSE,
                                useCovariateObservationCount365d = FALSE,
                                useCovariateConceptCounts = FALSE,
                                useCovariateRiskScores = TRUE,
                                useCovariateInteractionYear = FALSE,
                                useCovariateInteractionMonth = FALSE,
                                excludedCovariateConceptIds = c(4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019),
                                deleteCovariatesSmallCount = 100)

  saveCohortData(cohortData,"s:/temp/simulationCohortData")

  #cohortData <- loadCohortData("s:/temp/simulationCohortData")
  #ps <- createPs(cohortData, outcomeConceptId = 194133, prior=createPrior("laplace",0.1))

  cohortDataSimulationProfile <- createCohortDataSimulationProfile(cohortData)
  save(cohortDataSimulationProfile,file="s:/temp/cohortDataSimulationProfile.rda", compress = "xz")
}

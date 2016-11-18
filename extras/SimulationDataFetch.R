# @file SimulationDataFetch.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

# This code should be used to create the simulation profile used in some of the unit tests
library(CohortMethod)
options(fftempdir = "s:/fftemp")

pw <- NULL
dbms <- "sql server"
user <- NULL
server <- "RNDUSRDHIT07.jnj.com"
cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
port <- NULL
cdmVersion <- 4

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "cdm_truven_mdcd_v446.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- 17001
cdmVersion <- 5

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                             useCovariateDemographicsAge = TRUE,
                                             useCovariateDemographicsGender = TRUE,
                                             useCovariateDemographicsRace = TRUE,
                                             useCovariateDemographicsEthnicity = TRUE,
                                             useCovariateDemographicsYear = TRUE,
                                             useCovariateDemographicsMonth = TRUE,
                                             useCovariateConditionOccurrence = FALSE,
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
                                             useCovariateObservationCount365d = FALSE,
                                             useCovariateMeasurement365d = FALSE,
                                             useCovariateMeasurement30d = FALSE,
                                             useCovariateMeasurementCount365d = FALSE,
                                             useCovariateMeasurementBelow = FALSE,
                                             useCovariateMeasurementAbove = FALSE,
                                             useCovariateConceptCounts = FALSE,
                                             useCovariateRiskScores = FALSE,
                                             useCovariateRiskScoresCharlson = FALSE,
                                             useCovariateRiskScoresDCSI = FALSE,
                                             useCovariateRiskScoresCHADS2 = FALSE,
                                             useCovariateInteractionYear = FALSE,
                                             useCovariateInteractionMonth = FALSE,
                                             deleteCovariatesSmallCount = 100)
# Load data:
cohortMethodData <- getDbCohortMethodData(connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          targetId = 755695,
                                          comparatorId = 739138,
                                          outcomeIds = 194133,
                                          exposureDatabaseSchema = cdmDatabaseSchema,
                                          exposureTable = "drug_era",
                                          outcomeDatabaseSchema = cdmDatabaseSchema,
                                          outcomeTable = "condition_era",
                                          cdmVersion = cdmVersion,
                                          studyStartDate = "",
                                          studyEndDate = "",
                                          excludeDrugsFromCovariates = TRUE,
                                          firstExposureOnly = TRUE,
                                          washoutPeriod = 183,
                                          removeDuplicateSubjects = TRUE,
                                          covariateSettings = covariateSettings)

summary(cohortMethodData)
getAttritionTable(cohortMethodData)

saveCohortMethodData(cohortMethodData, "s:/temp/simulationCohortMethodData")

# cohortMethodData <- loadCohortMethodData('s:/temp/simulationCohortMethodData')

cohortMethodDataSimulationProfile <- createCohortMethodDataSimulationProfile(cohortMethodData)

save(cohortMethodDataSimulationProfile,
     file = "cohortMethodDataSimulationProfile.rda",
     compress = "xz")

# load('cohortMethodDataSimulationProfile.rda')

simData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = 1000)

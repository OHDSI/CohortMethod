# @file SimulationDataFetch.R
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

# This code should be used to create the simulation profile used in some of the unit tests
library(CohortMethod)
options(fftempdir = "s:/fftemp")

cdmDatabaseSchema <- "CDM_Truven_MDCD_V610.dbo"
resultsDatabaseSchema <- "scratch.dbo"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("PDW_SERVER"),
                                                                user = NULL,
                                                                password = NULL,
                                                                port = Sys.getenv("PDW_PORT"))

covariateSettings <- createCovariateSettings(useDemographicsAgeGroup = TRUE,
                                             useDemographicsGender = TRUE,
                                             useDemographicsIndexYear = TRUE,
                                             useDemographicsIndexMonth = TRUE,
                                             useConditionEraLongTerm = TRUE)
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
     file = "data/cohortMethodDataSimulationProfile.rda",
     compress = "xz")

# load('cohortMethodDataSimulationProfile.rda')

simData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = 1000)

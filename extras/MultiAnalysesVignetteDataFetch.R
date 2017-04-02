# @file MultiAnalysesVignetteDataFetch.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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

#### Fetch data for multiple analyses vignette ####
library(CohortMethod)
library(SqlRender)
options(fftempdir = "s:/fftemp")


pw <- NULL
dbms <- "sql server"
user <- NULL
server <- "RNDUSRDHIT09.jnj.com"
cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
# cdmDatabaseSchema <- 'cdm_truven_ccae_6k.dbo'
resultsDatabaseSchema <- "scratch.dbo"
resultsDatabaseSchema <- "cdm_truven_ccae_6k.dbo"
port <- NULL
cdmVersion <- "4"

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "CDM_Truven_mdcd_V464.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- 17001
cdmVersion <- "5"

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "cdm_truven_mdcd_v446.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- 17001
cdmVersion <- "5"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)
connection <- DatabaseConnector::connect(connectionDetails)

sql <- loadRenderTranslateSql("VignetteOutcomes.sql",
                              packageName = "CohortMethod",
                              dbms = dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)


DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsDatabaseSchema.outcomes GROUP BY cohort_definition_id"
sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
DatabaseConnector::querySql(connection, sql)

# Get all NSAIDs:
sql <- "SELECT concept_id FROM @cdmDatabaseSchema.concept_ancestor INNER JOIN @cdmDatabaseSchema.concept ON descendant_concept_id = concept_id WHERE ancestor_concept_id = 21603933"
sql <- SqlRender::renderSql(sql, cdmDatabaseSchema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
nsaids <- DatabaseConnector::querySql(connection, sql)
nsaids <- nsaids$CONCEPT_ID

RJDBC::dbDisconnect(connection)

dcos <- createDrugComparatorOutcomes(targetId = 1118084,
                                     comparatorId = 1124300,
                                     excludedCovariateConceptIds = nsaids,
                                     outcomeIds = c(192671,
                                                    24609,
                                                    29735,
                                                    73754,
                                                    80004,
                                                    134718,
                                                    139099,
                                                    141932,
                                                    192367,
                                                    193739,
                                                    194997,
                                                    197236,
                                                    199074,
                                                    255573,
                                                    257007,
                                                    313459,
                                                    314658,
                                                    316084,
                                                    319843,
                                                    321596,
                                                    374366,
                                                    375292,
                                                    380094,
                                                    433753,
                                                    433811,
                                                    436665,
                                                    436676,
                                                    436940,
                                                    437784,
                                                    438134,
                                                    440358,
                                                    440374,
                                                    443617,
                                                    443800,
                                                    4084966,
                                                    4288310))
drugComparatorOutcomesList <- list(dcos)

covarSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                         useCovariateDemographicsAge = TRUE,
                                         useCovariateDemographicsGender = TRUE,
                                         useCovariateDemographicsRace = TRUE,
                                         useCovariateDemographicsEthnicity = TRUE,
                                         useCovariateDemographicsYear = TRUE,
                                         useCovariateDemographicsMonth = TRUE,
                                         useCovariateConditionOccurrence = TRUE,
                                         useCovariateConditionOccurrence365d = TRUE,
                                         useCovariateConditionOccurrence30d = TRUE,
                                         useCovariateConditionOccurrenceInpt180d = TRUE,
                                         useCovariateConditionEra = TRUE,
                                         useCovariateConditionEraEver = TRUE,
                                         useCovariateConditionEraOverlap = TRUE,
                                         useCovariateConditionGroup = TRUE,
                                         useCovariateDrugExposure = TRUE,
                                         useCovariateDrugExposure365d = TRUE,
                                         useCovariateDrugExposure30d = TRUE,
                                         useCovariateDrugEra = TRUE,
                                         useCovariateDrugEra365d = TRUE,
                                         useCovariateDrugEra30d = TRUE,
                                         useCovariateDrugEraEver = TRUE,
                                         useCovariateDrugEraOverlap = TRUE,
                                         useCovariateDrugGroup = TRUE,
                                         useCovariateProcedureOccurrence = TRUE,
                                         useCovariateProcedureOccurrence365d = TRUE,
                                         useCovariateProcedureOccurrence30d = TRUE,
                                         useCovariateProcedureGroup = TRUE,
                                         useCovariateObservation = TRUE,
                                         useCovariateObservation365d = TRUE,
                                         useCovariateObservation30d = TRUE,
                                         useCovariateObservationCount365d = TRUE,
                                         useCovariateMeasurement365d = TRUE,
                                         useCovariateMeasurement30d = TRUE,
                                         useCovariateMeasurementCount365d = TRUE,
                                         useCovariateMeasurementBelow = TRUE,
                                         useCovariateMeasurementAbove = TRUE,
                                         useCovariateConceptCounts = TRUE,
                                         useCovariateRiskScores = TRUE,
                                         useCovariateRiskScoresCharlson = TRUE,
                                         useCovariateRiskScoresDCSI = TRUE,
                                         useCovariateRiskScoresCHADS2 = TRUE,
                                         useCovariateInteractionYear = FALSE,
                                         useCovariateInteractionMonth = FALSE,
                                         excludedCovariateConceptIds = c(),
                                         deleteCovariatesSmallCount = 100)
getDbCmDataArgs <- createGetDbCohortMethodDataArgs(washoutPeriod = 183,
                                                   firstExposureOnly = TRUE,
                                                   removeDuplicateSubjects = TRUE,
                                                   studyStartDate = "",
                                                   studyEndDate = "",
                                                   excludeDrugsFromCovariates = FALSE,
                                                   covariateSettings = covarSettings)

createStudyPopArgs <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                      minDaysAtRisk = 1,
                                                      riskWindowStart = 0,
                                                      addExposureDaysToStart = FALSE,
                                                      riskWindowEnd = 30,
                                                      addExposureDaysToEnd = TRUE)

fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(useCovariates = FALSE,
                                                  modelType = "cox",
                                                  stratified = FALSE)

cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                description = "No matching, simple outcome model",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

createPsArgs <- createCreatePsArgs(control = createControl(cvType = "auto",
                                                           startingVariance = 0.01,
                                                           noiseLevel = "quiet",
                                                           tolerance = 2e-07,
                                                           cvRepetitions = 10))

matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                description = "Matching plus simple outcome model",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                computeCovariateBalance = TRUE,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

stratifyByPsArgs <- createStratifyByPsArgs(numberOfStrata = 5)

fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(useCovariates = FALSE,
                                                  modelType = "cox",
                                                  stratified = TRUE)

cmAnalysis3 <- createCmAnalysis(analysisId = 3,
                                description = "Stratification plus stratified outcome model",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                stratifyByPs = TRUE,
                                stratifyByPsArgs = stratifyByPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

cmAnalysis4 <- createCmAnalysis(analysisId = 4,
                                description = "Matching plus stratified outcome model",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                computeCovariateBalance = TRUE,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(useCovariates = TRUE,
                                                  modelType = "cox",
                                                  stratified = TRUE,
                                                  control = createControl(cvType = "auto",
                                                                          startingVariance = 0.1,
                                                                          selectorType = "byPid",
                                                                          cvRepetitions = 1,
                                                                          tolerance = 2e-07,
                                                                          noiseLevel = "quiet"))

cmAnalysis5 <- createCmAnalysis(analysisId = 5,
                                description = "Matching plus full outcome model",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                computeCovariateBalance = TRUE,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs3)

cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5)

saveCmAnalysisList(cmAnalysisList, "s:/temp/cohortMethodVignette2/cmAnalysisList.txt")
saveDrugComparatorOutcomesList(drugComparatorOutcomesList,
                               "s:/temp/cohortMethodVignette2/drugComparatorOutcomesList.txt")

# cmAnalysisList <- loadCmAnalysisList('s:/temp/cohortMethodVignette2/cmAnalysisList.txt')

# drugComparatorOutcomesList <- loadDrugComparatorOutcomesList('s:/temp/cohortMethodVignette2/drugComparatorOutcomesList.txt')

result <- runCmAnalyses(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        exposureDatabaseSchema = cdmDatabaseSchema,
                        exposureTable = "drug_era",
                        outcomeDatabaseSchema = resultsDatabaseSchema,
                        outcomeTable = "outcomes",
                        outputFolder = "s:/temp/cohortMethodVignette2",
                        cdmVersion = cdmVersion,
                        cmAnalysisList = cmAnalysisList,
                        drugComparatorOutcomesList = drugComparatorOutcomesList,
                        getDbCohortMethodDataThreads = 1,
                        createPsThreads = 1,
                        psCvThreads = 16,
                        createStudyPopThreads = 3,
                        computeCovarBalThreads = 3,
                        trimMatchStratifyThreads = 5,
                        fitOutcomeModelThreads = 3,
                        outcomeCvThreads = 10,
                        outcomeIdsOfInterest = c(192671))
# result <- readRDS("s:/temp/cohortMethodVignette2/outcomeModelReference.rds")

analysisSum <- summarizeAnalyses(result)

saveRDS(analysisSum, "s:/temp/cohortMethodVignette2/analysisSummary.rds")
# cleanup:
sql <- "DROP TABLE @resultsDatabaseSchema.outcomes"
sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
connection <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::executeSql(connection, sql)
RJDBC::dbDisconnect(connection)

# result <- readRDS('s:/temp/cohortMethodVignette2/outcomeModelReference.rds')

# cohortMethodData <- loadCohortMethodData(result$cohortMethodDataFolder[1])
# summary(cohortMethodData)
analysisSummary <- summarizeAnalyses(result)
saveRDS(analysisSummary, file = "s:/temp/cohortMethodVignette2/analysisSummary.rds")

library(EmpiricalCalibration)
for (i in 1:5) {
  negControls <- analysisSummary[analysisSummary$analysisId == i & analysisSummary$outcomeId != 192671, ]
  hoi <- analysisSummary[analysisSummary$analysisId == i & analysisSummary$outcomeId == 192671, ]
  fileName <- paste("s:/temp/cohortMethodVignette2/CaliPlot_", i, ".png", sep = "")
  plotCalibrationEffect(negControls$logRr,
                        negControls$seLogRr,
                        hoi$logRr,
                        hoi$seLogRr,
                        fileName = fileName)
}

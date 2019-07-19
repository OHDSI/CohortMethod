# @file MultiAnalysesVignetteDataFetch.R
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

#### Fetch data for multiple analyses vignette ####
library(CohortMethod)
library(SqlRender)
options(fftempdir = "s:/fftemp")

# PDW
pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "cdm_truven_mdcd_v610.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- 17001
cdmVersion <- "5"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Eunomia
cdmDatabaseSchema <- "main"
resultsDatabaseSchema <- "main"
cdmVersion <- "5"

connectionDetails <- Eunomia::getEunomiaConnectionDetails()

connection <- DatabaseConnector::connect(connectionDetails)

sql <- loadRenderTranslateSql("VignetteOutcomes.sql",
                              packageName = "CohortMethod",
                              dbms = connectionDetails$dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)


DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsDatabaseSchema.outcomes GROUP BY cohort_definition_id"
sql <- SqlRender::render(sql, resultsDatabaseSchema = resultsDatabaseSchema)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

# Get all NSAIDs:
# sql <- "SELECT concept_id FROM @cdmDatabaseSchema.concept_ancestor INNER JOIN @cdmDatabaseSchema.concept ON descendant_concept_id = concept_id WHERE ancestor_concept_id = 21603933"
# sql <- SqlRender::renderSql(sql, cdmDatabaseSchema = cdmDatabaseSchema)$sql
# sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
# nsaids <- DatabaseConnector::querySql(connection, sql)
# nsaids <- nsaids$CONCEPT_ID

DatabaseConnector::disconnect(connection)

nsaids <- 21603933

tcos <- createTargetComparatorOutcomes(targetId = 1118084,
                                     comparatorId = 1124300,
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
targetComparatorOutcomesList <- list(tcos)

covarSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                                addDescendantsToExclude = TRUE)

getDbCmDataArgs <- createGetDbCohortMethodDataArgs(washoutPeriod = 183,
                                                   restrictToCommonPeriod = FALSE,
                                                   firstExposureOnly = TRUE,
                                                   removeDuplicateSubjects = "remove all",
                                                   studyStartDate = "",
                                                   studyEndDate = "",
                                                   maxCohortSize = 10000,
                                                   excludeDrugsFromCovariates = FALSE,
                                                   covariateSettings = covarSettings)

createStudyPopArgs <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                      minDaysAtRisk = 1,
                                                      riskWindowStart = 0,
                                                      startAnchor = "cohort start",
                                                      riskWindowEnd = 30,
                                                      endAnchor = "cohort end")

fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(modelType = "cox")

cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                description = "No matching, simple outcome model",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

createPsArgs <- createCreatePsArgs(maxCohortSizeForFitting = 100000,
                                   control = createControl(cvType = "auto",
                                                           startingVariance = 0.01,
                                                           tolerance = 1E-5,
                                                           noiseLevel = "quiet",
                                                           cvRepetitions = 1))

# createPsArgs <- createCreatePsArgs(maxCohortSizeForFitting = 100000,
#                                    prior = createPrior("laplace", variance = 0.0105))

matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)


fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE)

cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                description = "Matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

stratifyByPsArgs <- createStratifyByPsArgs(numberOfStrata = 5)

cmAnalysis3 <- createCmAnalysis(analysisId = 3,
                                description = "Stratification",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                stratifyByPs = TRUE,
                                stratifyByPsArgs = stratifyByPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  inversePtWeighting = TRUE)

trimByPsArgs <- createTrimByPsArgs(trimFraction = 0.01)

cmAnalysis4 <- createCmAnalysis(analysisId = 4,
                                description = "Inverse probability weighting",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                trimByPs = TRUE,
                                trimByPsArgs = trimByPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs3)

fitOutcomeModelArgs4 <- createFitOutcomeModelArgs(useCovariates = TRUE,
                                                  modelType = "cox",
                                                  stratified = TRUE,
                                                  control = createControl(cvType = "auto",
                                                                          startingVariance = 0.01,
                                                                          selectorType = "byPid",
                                                                          cvRepetitions = 1,
                                                                          noiseLevel = "quiet"))

cmAnalysis5 <- createCmAnalysis(analysisId = 5,
                                description = "Matching plus full outcome model",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs4)

interactionCovariateIds <- c(8532001, 201826210, 21600960413) # Female, T2DM, concurent use of antithrombotic agents

fitOutcomeModelArgs5 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE,
                                                  interactionCovariateIds = interactionCovariateIds,
                                                  control = createControl(threads = 6))

cmAnalysis6 <- createCmAnalysis(analysisId = 6,
                                description = "Stratification plus interaction terms",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                stratifyByPs = TRUE,
                                stratifyByPsArgs = stratifyByPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs5)
cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5, cmAnalysis6)

saveCmAnalysisList(cmAnalysisList, "s:/temp/cohortMethodVignette2/cmAnalysisList.json")
saveTargetComparatorOutcomesList(targetComparatorOutcomesList,
                               "s:/temp/cohortMethodVignette2/targetComparatorOutcomesList.json")

# cmAnalysisList <- loadCmAnalysisList('s:/temp/cohortMethodVignette2/cmAnalysisList.json')

# targetComparatorOutcomesList <- loadTargetComparatorOutcomesList('s:/temp/cohortMethodVignette2/targetComparatorOutcomesList.json')


result <- runCmAnalyses(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        exposureDatabaseSchema = cdmDatabaseSchema,
                        exposureTable = "drug_era",
                        outcomeDatabaseSchema = resultsDatabaseSchema,
                        outcomeTable = "outcomes",
                        outputFolder = "s:/temp/test2",
                        cdmVersion = cdmVersion,
                        cmAnalysisList = cmAnalysisList,
                        targetComparatorOutcomesList = targetComparatorOutcomesList,
                        getDbCohortMethodDataThreads = 1,
                        createPsThreads = 1,
                        psCvThreads = 16,
                        createStudyPopThreads = 1,
                        trimMatchStratifyThreads = 5,
                        prefilterCovariatesThreads = 3,
                        fitOutcomeModelThreads = 5,
                        outcomeCvThreads = 10,
                        outcomeIdsOfInterest = c(192671))
# result <- readRDS("s:/temp/cohortMethodVignette/outcomeModelReference.rds")

analysisSum <- summarizeAnalyses(result, outputFolder = "s:/temp/cohortMethodVignette2")

saveRDS(analysisSum, "s:/temp/cohortMethodVignette2/analysisSummary.rds")
# cleanup:
sql <- "DROP TABLE @resultsDatabaseSchema.outcomes"
sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
connection <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::executeSql(connection, sql)
DatabaseConnector::disconnect(connection)

# result <- readRDS('s:/temp/cohortMethodVignette2/outcomeModelReference.rds')

# cohortMethodData <- loadCohortMethodData(result$cohortMethodDataFolder[1])
# summary(cohortMethodData)
# analysisSummary <- summarizeAnalyses(result)
# saveRDS(analysisSummary, file = "s:/temp/cohortMethodVignette2/analysisSummary.rds")

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

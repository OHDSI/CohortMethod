# @file SingleStudyVignetteDataFetch.R
#
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

# This code should be used to fetch the data that is used in the vignettes.
library(SqlRender)
library(DatabaseConnector)
library(CohortMethod)
options(fftempdir = "s:/fftemp")


outputFolder <- "s:/temp/cohortMethodVignette"

# MDCD on PDW
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("PDW_SERVER"),
                                                                user = NULL,
                                                                password = NULL,
                                                                port = Sys.getenv("PDW_PORT"))
cdmDatabaseSchema <- "CDM_IBM_MDCD_V1105.dbo"
resultsDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
cdmVersion <- "5"
extraSettings <- NULL

# Eunomia
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
cdmDatabaseSchema <- "main"
resultsDatabaseSchema <- "main"
oracleTempSchema <- NULL
extraSettings <- NULL
cdmVersion <- "5"



connection <- DatabaseConnector::connect(connectionDetails)
sql <- loadRenderTranslateSql("coxibVsNonselVsGiBleed.sql",
                              packageName = "CohortMethod",
                              dbms = connectionDetails$dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)
DatabaseConnector::executeSql(connection, sql)

sql <- loadRenderTranslateSql("arthralgia.sql",
                              packageName = "CohortMethod",
                              dbms = connectionDetails$dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)
DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsDatabaseSchema.coxibVsNonselVsGiBleed GROUP BY cohort_definition_id"
sql <- SqlRender::render(sql, resultsDatabaseSchema = resultsDatabaseSchema)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

DatabaseConnector::disconnect(connection)

# CohortDiagnostics::launchCohortExplorer(connectionDetails = connectionDetails,
#                                         cdmDatabaseSchema = cdmDatabaseSchema,
#                                         cohortDatabaseSchema = resultsDatabaseSchema,
#                                         cohortTable = "coxibVsNonselVsGiBleed",
#                                         cohortId = 2)



nsaids <- c(1118084, 1124300)

covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                              addDescendantsToExclude = TRUE)

# Load data:
cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          oracleTempSchema = resultsDatabaseSchema,
                                          targetId = 1,
                                          comparatorId = 2,
                                          outcomeIds = 3,
                                          studyStartDate = "",
                                          studyEndDate = "",
                                          exposureDatabaseSchema = resultsDatabaseSchema,
                                          exposureTable = "coxibVsNonselVsGiBleed",
                                          outcomeDatabaseSchema = resultsDatabaseSchema,
                                          outcomeTable = "coxibVsNonselVsGiBleed",
                                          cdmVersion = cdmVersion,
                                          excludeDrugsFromCovariates = FALSE,
                                          firstExposureOnly = TRUE,
                                          removeDuplicateSubjects = "remove all",
                                          restrictToCommonPeriod = FALSE,
                                          washoutPeriod = 180,
                                          maxCohortSize = 100000,
                                          covariateSettings = covSettings)

saveCohortMethodData(cohortMethodData, file.path(outputFolder, "cohortMethodData"))

# cohortMethodData <- loadCohortMethodData(file.path(outputFolder, "cohortMethodData"))

studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                  outcomeId = 3,
                                  firstExposureOnly = FALSE,
                                  washoutPeriod = 0,
                                  removeDuplicateSubjects = FALSE,
                                  removeSubjectsWithPriorOutcome = TRUE,
                                  minDaysAtRisk = 1,
                                  riskWindowStart = 0,
                                  startAnchor = "cohort start",
                                  riskWindowEnd = 30,
                                  endAnchor = "cohort end")

plotTimeToEvent(cohortMethodData = cohortMethodData,
                outcomeId = 3,
                firstExposureOnly = FALSE,
                washoutPeriod = 0,
                removeDuplicateSubjects = FALSE,
                minDaysAtRisk = 1,
                riskWindowStart = 0,
                addExposureDaysToStart = FALSE,
                riskWindowEnd = 30,
                addExposureDaysToEnd = TRUE)



# getAttritionTable(studyPop)

saveRDS(studyPop, file.path(outputFolder, "studyPop.rds"))

# studyPop <- readRDS(file.path(outputFolder, "studyPop.rds"))

ps <- createPs(cohortMethodData = cohortMethodData,
               population = studyPop,
               prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
               control = createControl(cvType = "auto",
                                       startingVariance = 0.01,
                                       noiseLevel = "quiet",
                                       tolerance = 2e-07,
                                       cvRepetitions = 1,
                                       threads = 10))

plotPs(ps)

# computePsAuc(ps) plotPs(ps)
saveRDS(ps, file = file.path(outputFolder, "ps.rds"))

# ps <- readRDS(file.path(outputFolder, "ps.rds"))
model <- getPsModel(ps, cohortMethodData)
model[grepl("Charlson.*", model$covariateName), ]
model[model$id %% 1000 == 902, ]

plotPs(ps, showAucLabel = TRUE, showCountsLabel = TRUE, fileName = "extras/ps.png")
plotPs(ps)
plotPs(ps, scale = "propensity", showCountsLabel = TRUE, showEquiposeLabel = TRUE)
plotPs(ps, scale = "propensity", type = "histogram", showCountsLabel = TRUE, showEquiposeLabel = TRUE)

matchedPop <- matchOnPs(ps, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)


# getAttritionTable(matchedPop) plotPs(matchedPop, ps)

balance <- computeCovariateBalance(matchedPop, cohortMethodData)

saveRDS(balance, file = file.path(outputFolder, "balance.rds"))

# balance <- readRDS(file.path(outputFolder, "balance.rds'))

table1 <- createCmTable1(balance)
print(table1, row.names = FALSE, right = FALSE)
plotCovariateBalanceScatterPlot(balance, showCovariateCountLabel = TRUE, showMaxLabel = TRUE, fileName = "extras/balanceScatterplot.png")
# plotCovariateBalanceOfTopVariables(balance, fileName = "s:/temp/top.png")

outcomeModel <- fitOutcomeModel(population = studyPop,
                                modelType = "cox",
                                stratified = FALSE,
                                useCovariates = FALSE)
# getAttritionTable(outcomeModel) outcomeModel summary(outcomeModel) coef(outcomeModel)
# confint(outcomeModel)
saveRDS(outcomeModel, file = file.path(outputFolder, "OutcomeModel1.rds"))

outcomeModel <- fitOutcomeModel(population = matchedPop,
                                modelType = "cox",
                                stratified = TRUE,
                                useCovariates = FALSE)
saveRDS(outcomeModel, file = file.path(outputFolder, "OutcomeModel2.rds"))
weights <- ps$treatment / ps$propensityScore + (1-ps$treatment) / (1-ps$propensityScore)
max(weights)
min(weights)

outcomeModel <- fitOutcomeModel(population = ps,
                                modelType = "cox",
                                stratified = FALSE,
                                useCovariates = FALSE,
                                inversePtWeighting = TRUE)
outcomeModel
saveRDS(outcomeModel, file = file.path(outputFolder, "OutcomeModel2w.rds"))


outcomeModel <- fitOutcomeModel(population = matchedPop,
                                cohortMethodData = cohortMethodData,
                                modelType = "cox",
                                stratified = TRUE,
                                useCovariates = TRUE,
                                prior = createPrior("laplace", useCrossValidation = TRUE),
                                control = createControl(cvType = "auto",
                                                        startingVariance = 0.01,
                                                        selectorType = "byPid",
                                                        cvRepetitions = 1,
                                                        tolerance = 2e-06,
                                                        threads = 16,
                                                        noiseLevel = "quiet"))
saveRDS(outcomeModel, file = file.path(outputFolder, "OutcomeModel3.rds"))

population <- stratifyByPs(ps, numberOfStrata = 10)
interactionCovariateIds <- c(8532001, 201826210, 21600960413) # Female, T2DM, concurent use of antithrombotic agents
outcomeModel <- fitOutcomeModel(population = population,
                                cohortMethodData = cohortMethodData,
                                modelType = "cox",
                                stratified = TRUE,
                                useCovariates = FALSE,
                                inversePtWeighting = FALSE,
                                interactionCovariateIds = interactionCovariateIds,
                                control = createControl(threads = 6))
saveRDS(outcomeModel, file = file.path(outputFolder, "OutcomeModel4.rds"))

# balanceFemale <- computeCovariateBalance(matchedPop, cohortMethodData, subgroupCovariateId = 8532001)
# saveRDS(balanceFemale, file = "s:/temp/cohortMethodVignette/balanceFemale.rds")

# dummy <- plotCovariateBalanceScatterPlot(balanceFemale, fileName = "s:/temp/balanceFemales.png")


# balanceOverall <- computeCovariateBalance(population, cohortMethodData)
# dummy <- plotCovariateBalanceScatterPlot(balanceOverall, fileName = "s:/temp/balance.png")
# balanceFemale <- computeCovariateBalance(population, cohortMethodData, subgroupCovariateId = 8532001)
# dummy <- plotCovariateBalanceScatterPlot(balanceFemale, fileName = "s:/temp/balanceFemales.png")

# grepCovariateNames("ANTITHROMBOTIC AGENTS", cohortMethodData)

# outcomeModel <- readRDS(file = 's:/temp/cohortMethodVignette/OutcomeModel3.rds')
# drawAttritionDiagram(outcomeModel, fileName = 's:/temp/attrition.png') summary(outcomeModel)


# Disease risk scores -------------------------------------------------
plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              cdmVersion = cdmVersion,
                                              oracleTempSchema = oracleTempSchema,
                                              cohortId = 4,
                                              outcomeIds = 3,
                                              cohortDatabaseSchema = resultsDatabaseSchema,
                                              cohortTable = "coxibVsNonselVsGiBleed",
                                              outcomeDatabaseSchema = resultsDatabaseSchema,
                                              outcomeTable = "coxibVsNonselVsGiBleed",
                                              washoutPeriod = 180,
                                              sampleSize = 250000,
                                              covariateSettings = covSettings)

PatientLevelPrediction::savePlpData(plpData, file.path(outputFolder, "plpData"))

plpStudyPop <- PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                          outcomeId = 3,
                                                          includeAllOutcomes = TRUE,
                                                          removeSubjectsWithPriorOutcome = TRUE,
                                                          minTimeAtRisk = 1,
                                                          riskWindowStart = 0,
                                                          riskWindowEnd = 30)
sum(plpStudyPop$outcomeCount > 0)

model <- PatientLevelPrediction::fitGLMModel(population = plpStudyPop,
                                             plpData = plpData,
                                             modelType = "logistic")

betas <- model$coefficients
betas <- betas[betas != 0]
saveRDS(betas, file.path(outputFolder, "drBetas.rds"))
betas <- readRDS(file.path(outputFolder, "drBetas.rds"))

drs <- createDrs(cohortMethodData = cohortMethodData,
                 population = studyPop,
                 coefficients = betas)
plot <- plotDrs(drs)
computeDrsAuc(drs)




matchedOnDrsPop <- matchOnDrs(drs)
plotDrs(matchedOnDrsPop)
outcomeModel <- fitOutcomeModel(population = matchedOnDrsPop,
                                modelType = "cox",
                                stratified = TRUE,
                                useCovariates = FALSE)
saveRDS(outcomeModel, file = file.path(outputFolder, "OutcomeModel5.rds"))

balanceDrs <- computeCovariateBalance(matchedOnDrsPop, cohortMethodData)
plotCovariateBalanceScatterPlot(balanceDrs, fileName = "s:/temp/balDrs.png")

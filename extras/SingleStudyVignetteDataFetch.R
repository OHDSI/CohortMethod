# @file SingleStudyVignetteDataFetch.R
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

# This code should be used to fetch the data that is used in the vignettes.
library(SqlRender)
library(DatabaseConnector)
library(CohortMethod)
options(fftempdir = "s:/fftemp")

dbms <- "sql server"
user <- NULL
pw <- NULL
server <- "RNDUSRDHIT07.jnj.com"
cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- NULL
extraSettings <- NULL

dbms <- "postgresql"
user <- "postgres"
pw <- Sys.getenv("pwPostgres")
server <- "localhost/ohdsi"
cdmDatabaseSchema <- "vocabulary5"
resultsDatabaseSchema <- "scratch"
port <- NULL
extraSettings <- NULL

dbms <- "pdw"
user <- NULL
pw <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "cdm_truven_mdcd_v569.dbo"
resultsDatabaseSchema <- "scratch.dbo"
port <- 17001
cdmVersion <- "5"
extraSettings <- NULL

dbms <- "redshift"
user <- "mschuemi"
pw <- Sys.getenv("pwRedShift")
server <- "hicoe.cldcoxyrkflo.us-east-1.redshift.amazonaws.com/truven_mdcr"
cdmDatabaseSchema <- "cdm"
resultsDatabaseSchema <- "scratch_mschuemi_22"
port <- 5439
cdmVersion <- "5"
extraSettings <- "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port,
                                                                extraSettings = extraSettings)
connection <- DatabaseConnector::connect(connectionDetails)

sql <- loadRenderTranslateSql("coxibVsNonselVsGiBleed.sql",
                              packageName = "CohortMethod",
                              dbms = dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)
DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsDatabaseSchema.coxibVsNonselVsGiBleed GROUP BY cohort_definition_id"
sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
DatabaseConnector::querySql(connection, sql)

# Get all NSAIDs:
sql <- "SELECT concept_id FROM @cdmDatabaseSchema.concept_ancestor INNER JOIN @cdmDatabaseSchema.concept ON descendant_concept_id = concept_id WHERE ancestor_concept_id = 21603933"
sql <- SqlRender::renderSql(sql, cdmDatabaseSchema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
nsaids <- DatabaseConnector::querySql(connection, sql)
nsaids <- nsaids$CONCEPT_ID

DatabaseConnector::disconnect(connection)

covariateSettings <- createDefaultCovariateSettings()
covariateSettings$excludedCovariateConceptIds <- nsaids
covariateSettings$addDescendantsToExclude <- TRUE

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
                                          removeDuplicateSubjects = TRUE,
                                          restrictToCommonPeriod = FALSE,
                                          washoutPeriod = 180,
                                          covariateSettings = covariateSettings)

# Error executing SQL: Error in .jcall(res@jr, 'V', 'close'): java.sql.SQLException: [Amazon](500150)
# Error setting/closing connection: Not Connected.  After removing on.exit clearResults: Error
# executing SQL: Error in .jcall(rp, 'I', 'fetch', as.integer(n), block): java.sql.SQLException:
# [Amazon][JDBC](10060) Connection has been closed.  table has 148734670 rows

saveCohortMethodData(cohortMethodData, "s:/temp/cohortMethodVignette/cohortMethodData2")

# cohortMethodData <- loadCohortMethodData('s:/temp/cohortMethodVignette/cohortMethodData2')

# summary(cohortMethodData) getAttritionTable(cohortMethodData)

studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                  outcomeId = 3,
                                  firstExposureOnly = FALSE,
                                  washoutPeriod = 0,
                                  removeDuplicateSubjects = FALSE,
                                  removeSubjectsWithPriorOutcome = TRUE,
                                  minDaysAtRisk = 1,
                                  riskWindowStart = 0,
                                  addExposureDaysToStart = FALSE,
                                  riskWindowEnd = 30,
                                  addExposureDaysToEnd = TRUE)
# getAttritionTable(studyPop)

saveRDS(studyPop, "s:/temp/cohortMethodVignette/studyPop.rds")

# studyPop <- readRDS('s:/temp/cohortMethodVignette/studyPop.rds')

ps <- createPs(cohortMethodData = cohortMethodData,
               population = studyPop,
               prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
               control = createControl(cvType = "auto",
                                       startingVariance = 0.01,
                                       noiseLevel = "quiet",
                                       tolerance = 2e-07,
                                       cvRepetitions = 1,
                                       threads = 10))

# computePsAuc(ps) plotPs(ps)
saveRDS(ps, file = "s:/temp/cohortMethodVignette/ps2.rds")
# ps2 <- readRDS('s:/temp/cohortMethodVignette/ps2.rds')
model <- getPsModel(ps2, cohortMethodData)
model[grepl("Charlson.*", model$covariateName), ]
model[model$id %% 1000 == 902, ]

# insertDbPopulation(population = studyPop, cohortIds = c(101,100), connectionDetails =
# connectionDetails, cohortDatabaseSchema = resultsDatabaseSchema, cohortTable = 'mschuemi_test',
# createTable = TRUE, dropTableIfExists = TRUE, cdmVersion = 5)

# Check number of subjects per cohort: connection <- DatabaseConnector::connect(connectionDetails)
# sql <- 'SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsDatabaseSchema.mschuemi_test
# GROUP BY cohort_definition_id' sql <- SqlRender::renderSql(sql, resultsDatabaseSchema =
# resultsDatabaseSchema)$sql sql <- SqlRender::translateSql(sql, targetDialect =
# connectionDetails$dbms)$sql DatabaseConnector::querySql(connection, sql) dbDisconnect(connection)

# trimmed <- trimByPs(ps) trimmed <- trimByPsToEquipoise(ps) plotPs(trimmed, ps)

matchedPop <- matchOnPs(ps2, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)
# getAttritionTable(matchedPop) plotPs(matchedPop, ps)

balance <- computeCovariateBalance(matchedPop, cohortMethodData)

saveRDS(balance, file = "s:/temp/cohortMethodVignette/balance.rds")

# balance <- readRDS('s:/temp/cohortMethodVignette/balance.rds')

plotCovariateBalanceScatterPlot(balance, fileName = "s:/temp/scatter.png")
# plotCovariateBalanceOfTopVariables(balance, fileName = "s:/temp/top.png")

outcomeModel <- fitOutcomeModel(population = studyPop,
                                modelType = "cox",
                                stratified = FALSE,
                                useCovariates = FALSE)
# getAttritionTable(outcomeModel) outcomeModel summary(outcomeModel) coef(outcomeModel)
# confint(outcomeModel)
saveRDS(outcomeModel, file = "s:/temp/cohortMethodVignette/OutcomeModel1.rds")

outcomeModel <- fitOutcomeModel(population = matchedPop,
                                modelType = "cox",
                                stratified = TRUE,
                                useCovariates = FALSE)
saveRDS(outcomeModel, file = "s:/temp/cohortMethodVignette/OutcomeModel2.rds")

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
                                                        tolerance = 2e-07,
                                                        threads = 16,
                                                        noiseLevel = "quiet"))
saveRDS(outcomeModel, file = "s:/temp/cohortMethodVignette/OutcomeModel3.rds")

# outcomeModel <- readRDS(file = 's:/temp/cohortMethodVignette/OutcomeModel3.rds')
# drawAttritionDiagram(outcomeModel, fileName = 's:/temp/attrition.png') summary(outcomeModel)

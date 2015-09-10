# @file VignetteDataFetch.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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
.singleStudyVignetteDataFetch <- function() {
  # This function should be used to fetch the data that is used in the vignettes.
  # library(SqlRender);library(DatabaseConnector) ;library(CohortMethod);
  # setwd('s:/temp');options('fftempdir' = 's:/fftemp')


  # setwd('C:/Users/mschuemi/git/CohortMethod/data')

  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07.jnj.com"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- NULL

  dbms <- "postgresql"
  user <- "postgres"
  server <- "localhost/ohdsi"
  cdmDatabaseSchema <- "vocabulary5"
  resultsDatabaseSchema <- "scratch"
  port <- NULL

  pw <- NULL
  dbms <- "pdw"
  user <- NULL
  server <- "JRDUSAPSCTL01"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- 17001

  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = pw,
                                                                  port = port)
  sql <- loadRenderTranslateSql("coxibVsNonselVsGiBleed.sql",
                                packageName = "CohortMethod",
                                dbms = dbms,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                resultsDatabaseSchema = resultsDatabaseSchema)

  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, sql)

  # Check number of subjects per cohort:
  sql <- "SELECT cohort_concept_id, COUNT(*) AS count FROM @resultsDatabaseSchema.coxibVsNonselVsGiBleed GROUP BY cohort_concept_id"
  sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  DatabaseConnector::querySql(connection, sql)

  # Get all NSAIDs:
  sql <- "SELECT concept_id FROM @cdmDatabaseSchema.concept_ancestor INNER JOIN @cdmDatabaseSchema.concept ON descendant_concept_id = concept_id WHERE ancestor_concept_id = 21603933"
  sql <- SqlRender::renderSql(sql, cdmDatabaseSchema = cdmDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  nsaids <- DatabaseConnector::querySql(connection, sql)
  nsaids <- nsaids$CONCEPT_ID

  covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
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
                                               excludedCovariateConceptIds = nsaids,
                                               deleteCovariatesSmallCount = 100)

  # Load data:
  cohortMethodData <- getDbCohortMethodData(connectionDetails,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            oracleTempSchema = resultsDatabaseSchema,
                                            targetId = 1,
                                            comparatorId = 2,
                                            indicationConceptIds = c(),
                                            washoutWindow = 183,
                                            indicationLookbackWindow = 183,
                                            studyStartDate = "",
                                            studyEndDate = "",
                                            exclusionConceptIds = nsaids,
                                            outcomeIds = 3,
                                            outcomeConditionTypeConceptIds = c(),
                                            exposureDatabaseSchema = resultsDatabaseSchema,
                                            exposureTable = "coxibVsNonselVsGiBleed",
                                            outcomeDatabaseSchema = resultsDatabaseSchema,
                                            outcomeTable = "coxibVsNonselVsGiBleed",
                                            excludeDrugsFromCovariates = FALSE,
                                            covariateSettings = covariateSettings,
                                            cdmVersion = "4")

  saveCohortMethodData(cohortMethodData, "s:/temp/vignetteCohortMethodData")

  # cohortMethodData <- loadCohortMethodData('s:/temp/vignetteCohortMethodData')

  ps <- createPs(cohortMethodData, outcomeId = 3, control = createControl(cvType = "auto",
                                                                          startingVariance = 0.01,
                                                                          noiseLevel = "quiet",
                                                                          threads = 10))
  vignettePs <- ps
  save(vignettePs, file = "vignettePs.rda", compress = "xz")

  # load('vignettePs.rda'); ps <- vignettePs;

  strata <- matchOnPs(ps, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)
  vignetteBalance <- computeCovariateBalance(strata, cohortMethodData, outcomeId = 3)

  save(vignetteBalance, file = "vignetteBalance.rda", compress = "xz")

  # load('vignetteBalance.rda'); balance <- vignetteBalance

  outcomeModel <- fitOutcomeModel(outcomeId = 3,
                                  cohortMethodData = cohortMethodData,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 30,
                                  addExposureDaysToEnd = TRUE,
                                  useCovariates = FALSE,
                                  modelType = "cox",
                                  stratifiedCox = FALSE)
  vignetteOutcomeModel1 <- outcomeModel
  save(vignetteOutcomeModel1, file = "vignetteOutcomeModel1.rda", compress = "xz")

  outcomeModel <- fitOutcomeModel(outcomeId = 3,
                                  cohortMethodData = cohortMethodData,
                                  subPopulation = strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 30,
                                  addExposureDaysToEnd = TRUE,
                                  useCovariates = FALSE,
                                  modelType = "cox",
                                  stratifiedCox = TRUE)
  vignetteOutcomeModel2 <- outcomeModel
  save(vignetteOutcomeModel2, file = "vignetteOutcomeModel2.rda", compress = "xz")

  outcomeModel <- fitOutcomeModel(outcomeId = 3,
                                  cohortMethodData = cohortMethodData,
                                  subPopulation = strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 30,
                                  addExposureDaysToEnd = TRUE,
                                  useCovariates = TRUE,
                                  modelType = "cox",
                                  stratifiedCox = TRUE)
  vignetteOutcomeModel3 <- outcomeModel
  save(vignetteOutcomeModel3, file = "vignetteOutcomeModel3.rda", compress = "xz")
}


#' @keywords internal
.multiAnalysesVignetteDataFetch <- function() {
  # library(CohortMethod);library(SqlRender)
  options('fftempdir' = 's:/fftemp')
  setwd("s:/temp")
  dataFolder <- "C:/Users/mschuemi/git/CohortMethod/data"


  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT09.jnj.com"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  # cdmDatabaseSchema <- 'cdm_truven_ccae_6k.dbo'
  resultsDatabaseSchema <- "scratch.dbo"
  resultsDatabaseSchema <- "cdm_truven_ccae_6k.dbo"
  port <- NULL

  pw <- NULL
  dbms <- "pdw"
  user <- NULL
  server <- "JRDUSAPSCTL01"
  cdmDatabaseSchema <- "cdm_truven_ccae.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- 17001

  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = pw,
                                                                  port = port)

  sql <- loadRenderTranslateSql("VignetteOutcomes.sql",
                                packageName = "CohortMethod",
                                dbms = dbms,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                resultsDatabaseSchema = resultsDatabaseSchema)

  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, sql)

  # Check number of subjects per cohort:
  sql <- "SELECT cohort_concept_id, COUNT(*) AS count FROM @resultsDatabaseSchema.outcomes GROUP BY cohort_concept_id"
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
                                       exclusionConceptIds = nsaids,
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
  getDbCmDataArgs <- createGetDbCohortMethodDataArgs(washoutWindow = 183,
                                                     indicationLookbackWindow = 183,
                                                     studyStartDate = "",
                                                     studyEndDate = "",
                                                     excludeDrugsFromCovariates = FALSE,
                                                     covariateSettings = covarSettings)

  fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(riskWindowStart = 0,
                                                    riskWindowEnd = 30,
                                                    addExposureDaysToEnd = TRUE,
                                                    useCovariates = FALSE,
                                                    modelType = "cox",
                                                    stratifiedCox = FALSE)

  cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                  description = "No matching, simple outcome model",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

  createPsArgs <- createCreatePsArgs()  # Using only defaults

  matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

  cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                  description = "Matching plus simple outcome model",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  matchOnPs = TRUE,
                                  matchOnPsArgs = matchOnPsArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

  stratifyByPsArgs <- createStratifyByPsArgs(numberOfStrata = 5)

  cmAnalysis3 <- createCmAnalysis(analysisId = 3,
                                  description = "Stratification plus simple outcome model",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  stratifyByPs = TRUE,
                                  stratifyByPsArgs = stratifyByPsArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

  fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(riskWindowStart = 0,
                                                    riskWindowEnd = 30,
                                                    addExposureDaysToEnd = TRUE,
                                                    useCovariates = FALSE,
                                                    modelType = "cox",
                                                    stratifiedCox = TRUE)

  cmAnalysis4 <- createCmAnalysis(analysisId = 4,
                                  description = "Matching plus stratified outcome model",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  matchOnPs = TRUE,
                                  matchOnPsArgs = matchOnPsArgs,
                                  computeCovariateBalance = TRUE,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs2)

  fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(riskWindowStart = 0,
                                                    riskWindowEnd = 30,
                                                    addExposureDaysToEnd = TRUE,
                                                    useCovariates = TRUE,
                                                    modelType = "cox",
                                                    stratifiedCox = TRUE)

  cmAnalysis5 <- createCmAnalysis(analysisId = 5,
                                  description = "Matching plus full outcome model",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  matchOnPs = TRUE,
                                  matchOnPsArgs = matchOnPsArgs,
                                  computeCovariateBalance = TRUE,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs3)

  cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5)

  saveCmAnalysisList(cmAnalysisList, "s:/temp/cmAnalysisList.txt")
  saveDrugComparatorOutcomesList(drugComparatorOutcomesList,
                                 "s:/temp/drugComparatorOutcomesList.txt")

  # cmAnalysisList <- loadCmAnalysisList('s:/temp/cmAnalysisList.txt') drugComparatorOutcomesList <-
  # loadDrugComparatorOutcomesList('s:/temp/drugComparatorOutcomesList.txt')

  result <- runCmAnalyses(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          outcomeDatabaseSchema = resultsDatabaseSchema,
                          outcomeTable = "outcomes",
                          outputFolder = "./CohortMethodOutput",
                          cmAnalysisList = cmAnalysisList,
                          drugComparatorOutcomesList = drugComparatorOutcomesList,
                          getDbCohortMethodDataThreads = 1,
                          createPsThreads = 1,
                          psCvThreads = 10,
                          computeCovarBalThreads = 2,
                          trimMatchStratifyThreads = 10,
                          fitOutcomeModelThreads = 4,
                          outcomeCvThreads = 10)
  # cleanup:
  sql <- "DROP TABLE @resultsDatabaseSchema.outcomes"
  sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, sql)
  RJDBC::dbDisconnect(connection)

  #result <- readRDS("s:/temp/CohortMethodOutput/outcomeModelReference.rds")

  #cohortMethodData <- loadCohortMethodData(result$cohortMethodDataFolder[1])
  #summary(cohortMethodData)
  analysisSummary <- summarizeAnalyses(result)
  vignetteAnalysisSummary <- analysisSummary
  save(vignetteAnalysisSummary, file = file.path(dataFolder,"vignetteAnalysisSummary.rda"), compress = "xz")

  # library(EmpiricalCalibration)
  negControls <- analysisSummary[analysisSummary$analysisId == 5 & analysisSummary$outcomeId != 192671, ]
  hoi <-  analysisSummary[analysisSummary$analysisId == 5 & analysisSummary$outcomeId == 192671, ]

}

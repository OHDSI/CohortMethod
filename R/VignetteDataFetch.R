# @file VignetteDataFetch.R
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
.singleStudyVignetteDataFetch <- function() {
  # This function should be used to fetch the data that is used in the vignettes.
  # library(SqlRender);library(DatabaseConnector) ;library(CohortMethod); setwd('s:/temp'); options('fftempdir' = 's:/temp')



  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07.jnj.com"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- NULL

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

  # Load data:
  cohortMethodData <- getDbCohortMethodData(connectionDetails,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            oracleTempSchema = resultsDatabaseSchema,
                                            targetDrugConceptId = 1,
                                            comparatorDrugConceptId = 2,
                                            indicationConceptIds = c(),
                                            washoutWindow = 183,
                                            indicationLookbackWindow = 183,
                                            studyStartDate = "",
                                            studyEndDate = "",
                                            exclusionConceptIds = nsaids,
                                            outcomeConceptIds = 3,
                                            outcomeConditionTypeConceptIds = c(),
                                            exposureDatabaseSchema = resultsDatabaseSchema,
                                            exposureTable = "coxibVsNonselVsGiBleed",
                                            outcomeDatabaseSchema = resultsDatabaseSchema,
                                            outcomeTable = "coxibVsNonselVsGiBleed",
                                            useCovariateDemographics = TRUE,
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
                                            useCovariateObservationBelow = TRUE,
                                            useCovariateObservationAbove = TRUE,
                                            useCovariateObservationCount365d = TRUE,
                                            useCovariateConceptCounts = TRUE,
                                            useCovariateRiskScores = TRUE,
                                            useCovariateInteractionYear = FALSE,
                                            useCovariateInteractionMonth = FALSE,
                                            excludedCovariateConceptIds = nsaids,
                                            excludeDrugsFromCovariates = FALSE,
                                            deleteCovariatesSmallCount = 100)

  saveCohortMethodData(cohortMethodData, "c:/temp/vignetteCohortMethodData")

  # vignetteSimulationProfile <- createCohortMethodDataSimulationProfile(cohortMethodData)
  # save(vignetteSimulationProfile, file = 'vignetteSimulationProfile.rda')

  # cohortMethodData <- loadCohortMethodData('vignetteCohortMethodData')
  # setwd('C:/Users/mschuemi/git/CohortMethod')
  ps <- createPs(cohortMethodData, outcomeConceptId = 3, control = createControl(cvType = "auto",
                                                                                 startingVariance = 0.1,
                                                                                 noiseLevel = "quiet",
                                                                                 threads = 10))
  vignettePs <- ps
  save(vignettePs, file = "data/vignettePs.rda", compress = "xz")

  # load('vignettePs.rda'); ps <- vignettePs; psTrimmed <- trimByPsToEquipoise(ps)
  strata <- matchOnPs(ps, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)
  vignetteBalance <- computeCovariateBalance(strata, cohortMethodData, outcomeConceptId = 3)

    save(vignetteBalance, file = "vignetteBalance.rda", compress = "xz")

  # load('vignetteBalance.rda')

  outcomeModel <- fitOutcomeModel(outcomeConceptId = 3,
                                  cohortMethodData = cohortMethodData,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 30,
                                  addExposureDaysToEnd = TRUE,
                                  useCovariates = FALSE,
                                  modelType = "cox",
                                  stratifiedCox = FALSE)
  vignetteOutcomeModel1 <- outcomeModel
  save(vignetteOutcomeModel1, file = "data/vignetteOutcomeModel1.rda", compress = "xz")

  outcomeModel <- fitOutcomeModel(outcomeConceptId = 3,
                                  cohortMethodData = cohortMethodData,
                                  subPopulation = strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 30,
                                  addExposureDaysToEnd = TRUE,
                                  useCovariates = FALSE,
                                  modelType = "cox",
                                  stratifiedCox = TRUE)
  vignetteOutcomeModel2 <- outcomeModel
  save(vignetteOutcomeModel2, file = "data/vignetteOutcomeModel2.rda", compress = "xz")

  outcomeModel <- fitOutcomeModel(outcomeConceptId = 3,
                                  cohortMethodData = cohortMethodData,
                                  subPopulation = strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 30,
                                  addExposureDaysToEnd = TRUE,
                                  useCovariates = TRUE,
                                  modelType = "cox",
                                  stratifiedCox = TRUE)
  vignetteOutcomeModel3 <- outcomeModel
  save(vignetteOutcomeModel3, file = "data/vignetteOutcomeModel3.rda", compress = "xz")

}


#' @keywords internal
.multiAnalysesVignetteDataFetch <- function() {
  # library(CohortMethod);library(SqlRender);setwd('s:/temp');options('fftempdir' = 's:/temp')

  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT09.jnj.com"
  # cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  cdmDatabaseSchema <- "cdm_truven_ccae_6k.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  resultsDatabaseSchema <- "cdm_truven_ccae_6k.dbo"
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

  sql <- loadRenderTranslateSql("VignetteOutcomes.sql",
                                packageName = "CohortMethod",
                                dbms = dbms,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                resultsDatabaseSchema = resultsDatabaseSchema)

  connection <- DatabaseConnector::connect(connectionDetails)
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

  drugComparatorOutcomes <- createDrugComparatorOutcomes(targetDrugConceptId = 1118084,
                                                         comparatorDrugConceptId = 1124300,
                                                         exclusionConceptIds = nsaids,
                                                         outcomeConceptIds = c(192671,
                                                                               29735,
                                                                               140673,
                                                                               197494,
                                                                               198185,
                                                                               198199,
                                                                               200528,
                                                                               257315,
                                                                               314658,
                                                                               317376,
                                                                               321319,
                                                                               380731,
                                                                               432661,
                                                                               432867,
                                                                               433516,
                                                                               433701,
                                                                               433753,
                                                                               435140,
                                                                               435459,
                                                                               435524,
                                                                               435783,
                                                                               436665,
                                                                               436676,
                                                                               442619,
                                                                               444252,
                                                                               444429,
                                                                               4131756,
                                                                               4134120,
                                                                               4134454,
                                                                               4152280,
                                                                               4165112,
                                                                               4174262,
                                                                               4182210,
                                                                               4270490,
                                                                               4286201,
                                                                               4289933))

  drugComparatorOutcomesList <- list(drugComparatorOutcomes)

  getDbCmDataArgs <- createGetDbCohortMethodDataArgs(washoutWindow = 183,
                                                     indicationLookbackWindow = 183,
                                                     studyStartDate = "",
                                                     studyEndDate = "",
                                                     excludeDrugsFromCovariates = TRUE,
                                                     useCovariateDemographics = TRUE,
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
                                                     useCovariateObservationBelow = TRUE,
                                                     useCovariateObservationAbove = TRUE,
                                                     useCovariateObservationCount365d = TRUE,
                                                     useCovariateConceptCounts = TRUE,
                                                     useCovariateRiskScores = TRUE,
                                                     useCovariateInteractionYear = FALSE,
                                                     useCovariateInteractionMonth = FALSE,
                                                     deleteCovariatesSmallCount = 100)

  createPsArgs <- createCreatePsArgs( control = createControl(noiseLevel = "silent",
                                                              cvType = "auto",
                                                              startingVariance = 0.1,
                                                              threads = 10))  # Using only defaults

  matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

  fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(riskWindowStart = 0,
                                                    riskWindowEnd = 30,
                                                    addExposureDaysToEnd = TRUE,
                                                    useCovariates = FALSE,
                                                    modelType = "cox",
                                                    stratifiedCox = FALSE)

  cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                  description = "Matching plus simple outcome model",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  matchOnPs = TRUE,
                                  matchOnPsArgs = matchOnPsArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

  stratifyByPsArgs <- createStratifyByPsArgs(numberOfStrata = 5)

  cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                  description = "simple outcome model, no propensity score",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

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
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs2)

  fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(riskWindowStart = 0,
                                                    riskWindowEnd = 30,
                                                    addExposureDaysToEnd = TRUE,
                                                    useCovariates = TRUE,
                                                    modelType = "cox",
                                                    stratifiedCox = TRUE,
                                                    control = createControl(cvType = "auto",
                                                                            startingVariance = 0.1,
                                                                            selectorType = "byPid",
                                                                            noiseLevel = "quiet",
                                                                            fold = 5,
                                                                            minCVData = 25,
                                                                            threads = 10))

  cmAnalysis5 <- createCmAnalysis(analysisId = 5,
                                  description = "Matching plus full outcome model",
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  createPs = TRUE,
                                  createPsArgs = createPsArgs,
                                  matchOnPs = TRUE,
                                  matchOnPsArgs = matchOnPsArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fitOutcomeModelArgs3)

  cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5)

  saveCmAnalysisList(cmAnalysisList, "s:/temp/cmAnalysisList.txt")
  saveDrugComparatorOutcomesList(drugComparatorOutcomesList, "s:/temp/drugComparatorOutcomesList.txt")

  cmAnalysisList <- loadCmAnalysisList("s:/temp/cmAnalysisList.txt")
  drugComparatorOutcomesList <- loadDrugComparatorOutcomesList("s:/temp/drugComparatorOutcomesList.txt")

  outcomeReference <- runCmAnalyses(connectionDetails = connectionDetails,
                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                    exposureDatabaseSchema = cdmDatabaseSchema,
                                    exposureTable = "drug_era",
                                    outcomeDatabaseSchema = resultsDatabaseSchema,
                                    outcomeTable = "outcomes",
                                    outputFolder = "s:/temp/CohortMethodOutput",
                                    cmAnalysisList = cmAnalysisList,
                                    drugComparatorOutcomesList = drugComparatorOutcomesList,
                                    getDbCohortMethodDataThreads = 1,
                                    createPsThreads = 1,
                                    trimMatchStratifyThreads = 24,
                                    fitOutcomeModelThreads = 10)

  #cleanup:
  sql <- "DROP TABLE @resultsDatabaseSchema.outcomes"
  sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, sql);
  RJDBC::dbDisconnect(connection)

  cohortMethodData <- loadCohortMethodData(outcomeReference$cohortMethodDataFolder[1])
  summary(cohortMethodData)
  analysisSummary <- summarizeAnalyses(outcomeReference)
  library(EmpiricalCalibration)
  negControls <- analysisSummary[analysisSummary$analysisId == 4 & analysisSummary$outcomeConceptId != 192671,]
  plotForest(negControls$logRr, negControls$seLogRr, as.character(1:nrow(negControls)))
  null <- fitNull(negControls$logRr, negControls$seLogRr)
  plotCalibrationEffect(negControls$logRr, negControls$seLogRr, null = null)

  analysisSummary[analysisSummary$analysisId == 1 & analysisSummary$outcomeConceptId == 192671,]
  analysisSummary[analysisSummary$analysisId == 2 & analysisSummary$outcomeConceptId == 192671,]
  analysisSummary[analysisSummary$analysisId == 3 & analysisSummary$outcomeConceptId == 192671,]
  analysisSummary[analysisSummary$analysisId == 4 & analysisSummary$outcomeConceptId == 192671,]

  cohortMethodData1 <- loadCohortMethodData('s:/temp/vignetteCohortData')
  summary(cohortMethodData1)

  ps <- readRDS(outcomeReference$psFile[1])
  plotPs(ps)

  outcomeModel <- fitOutcomeModel(outcomeConceptId = 192671,
                                  cohortMethodData = cohortMethodData,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 30,
                                  addExposureDaysToEnd = TRUE,
                                  useCovariates = FALSE,
                                  modelType = "cox",
                                  stratifiedCox = FALSE)

  outcomeModel <- fitOutcomeModel(outcomeConceptId = 192671,
                                  cohortMethodData = cohortMethodData,
                                  subPopulation = strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 30,
                                  addExposureDaysToEnd = TRUE,
                                  useCovariates = FALSE,
                                  modelType = "cox",
                                  stratifiedCox = TRUE)

  ps <- ps[!(ps$rowId %in% ff::as.ram(cohortMethodData$exclude$rowId[cohortMethodData$exclude$outcomeId == 192671, ])),]
  strata <- matchOnPs(ps, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)
  summary(outcomeModel)
}

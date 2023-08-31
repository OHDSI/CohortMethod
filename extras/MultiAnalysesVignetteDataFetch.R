# Copyright 2023 Observational Health Data Sciences and Informatics
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
library(CohortMethod)
library(dplyr)
options(andromedaTempFolder = "d:/andromedaTemp")

folder <- "d:/temp/cohortMethodVignette2"
# unlink(folder, recursive = TRUE)
# dir.create(folder)

# Set connection details -------------------------------------------------------
# MDCR on RedShift
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcr"),
  user = keyring::key_get("redShiftUserName"),
  password = keyring::key_get("redShiftPassword")
)
cdmDatabaseSchema <- "cdm_truven_mdcr_v2540"
cohortDatabaseSchema <- "scratch_mschuemi"
cohortTable  <- "cm_vignette"

# Define exposure cohorts ------------------------------------------------------
library(Capr)
library(CirceR)

osteoArthritisOfKneeConceptId <- 4079750
celecoxibConceptId <- 1118084
diclofenacConceptId <- 1124300
osteoArthritisOfKnee <- cs(
  descendants(osteoArthritisOfKneeConceptId),
  name = "Osteoarthritis of knee"
)
attrition = attrition(
  "prior osteoarthritis of knee" = withAll(
    atLeast(1, condition(osteoArthritisOfKnee), duringInterval(eventStarts(-Inf, 0)))
  )
)
celecoxib <- cs(
  descendants(celecoxibConceptId),
  name = "Celecoxib"
)
diclofenac  <- cs(
  descendants(diclofenacConceptId),
  name = "Diclofenac"
)
celecoxibCohort <- cohort(
  entry = entry(
    drug(celecoxib, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition,
  exit = exit(endStrategy = drugExit(celecoxib,
                                     persistenceWindow = 30,
                                     surveillanceWindow = 0))
)
diclofenacCohort <- cohort(
  entry = entry(
    drug(diclofenac, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition,
  exit = exit(endStrategy = drugExit(diclofenac,
                                     persistenceWindow = 30,
                                     surveillanceWindow = 0))
)
exposureCohorts <- tibble(cohortId = c(1,2),
                          cohortName = c("Celecoxib", "Diclofenac"),
                          json = c(as.json(celecoxibCohort), as.json(diclofenacCohort)))
exposureCohorts$sql <- sapply(exposureCohorts$json,
                              buildCohortQuery,
                              options = createGenerateOptions())

# Define outcome cohort --------------------------------------------------------
library(PhenotypeLibrary)
outcomeCohorts <- getPlCohortDefinitionSet(77) # GI bleed

# Define negative control cohorts ----------------------------------------------
negativeControlIds <- c(29735, 140673, 197494,
                        198185, 198199, 200528, 257315,
                        314658, 317376, 321319, 380731,
                        432661, 432867, 433516, 433701,
                        433753, 435140, 435459, 435524,
                        435783, 436665, 436676, 442619,
                        444252, 444429, 4131756, 4134120,
                        4134454, 4152280, 4165112, 4174262,
                        4182210, 4270490, 4286201, 4289933)
negativeControlCohorts <- tibble(cohortId = negativeControlIds,
                                 cohortName = sprintf("Negative control %d", negativeControlIds),
                                 outcomeConceptId = negativeControlIds)


# Generate cohorts -------------------------------------------------------------
library(CohortGenerator)
allCohorts <- bind_rows(outcomeCohorts,
                        exposureCohorts)
cohortTableNames <- getCohortTableNames(cohortTable = cohortTable)
createCohortTables(connectionDetails = connectionDetails,
                   cohortDatabaseSchema = cohortDatabaseSchema,
                   cohortTableNames = cohortTableNames)
generateCohortSet(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTableNames = cohortTableNames,
                  cohortDefinitionSet = allCohorts)
generateNegativeControlOutcomeCohorts(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                      cohortTable = cohortTable,
                                      negativeControlOutcomeCohortSet = negativeControlCohorts)

# Check number of subjects per cohort:
connection <- DatabaseConnector::connect(connectionDetails)
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@cohortTable GROUP BY cohort_definition_id"
cohortCounts <- DatabaseConnector::renderTranslateQuerySql(connection, sql,  cohortDatabaseSchema = cohortDatabaseSchema, cohortTable = cohortTable)
saveRDS(cohortCounts, file.path(folder, "cohortCounts.rds"))
DatabaseConnector::disconnect(connection)
cohortCounts

# Create analysis specifications -----------------------------------------------
outcomeOfInterest <- createOutcome(outcomeId = 77,
                                   outcomeOfInterest = TRUE)
negativeControlOutcomes <- lapply(negativeControlIds,
                                  function(outcomeId) createOutcome(outcomeId = outcomeId,
                                                                    outcomeOfInterest = FALSE,
                                                                    trueEffectSize = 1))
tcos <- createTargetComparatorOutcomes(targetId = 1,
                                       comparatorId = 2,
                                       outcomes = append(list(outcomeOfInterest),
                                                         negativeControlOutcomes))
targetComparatorOutcomesList <- list(tcos)

covarSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(1118084, 1124300),
                                                addDescendantsToExclude = TRUE)

getDbCmDataArgs <- createGetDbCohortMethodDataArgs(washoutPeriod = 183,
                                                   restrictToCommonPeriod = TRUE,
                                                   firstExposureOnly = TRUE,
                                                   removeDuplicateSubjects = "remove all",
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
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

createPsArgs <- createCreatePsArgs(maxCohortSizeForFitting = 100000,
                                   control = createControl(cvType = "auto",
                                                           startingVariance = 0.01,
                                                           tolerance = 1E-5,
                                                           noiseLevel = "quiet",
                                                           cvRepetitions = 1))

matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

computeSharedCovBalArgs <- createComputeCovariateBalanceArgs()

computeCovBalArgs <- createComputeCovariateBalanceArgs(covariateFilter = CohortMethod::getDefaultCmTable1Specifications())

fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE)

cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                description = "Matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPsArgs = createPsArgs,
                                matchOnPsArgs = matchOnPsArgs,
                                computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
                                computeCovariateBalanceArgs = computeCovBalArgs,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

stratifyByPsArgs <- createStratifyByPsArgs(numberOfStrata = 5)

cmAnalysis3 <- createCmAnalysis(analysisId = 3,
                                description = "Stratification",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPsArgs = createPsArgs,
                                stratifyByPsArgs = stratifyByPsArgs,
                                computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
                                computeCovariateBalanceArgs = computeCovBalArgs,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  inversePtWeighting = TRUE)

trimByPsArgs <- createTrimByPsArgs(trimFraction = 0.01)

cmAnalysis4 <- createCmAnalysis(analysisId = 4,
                                description = "Inverse probability weighting",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPsArgs = createPsArgs,
                                trimByPsArgs = trimByPsArgs,
                                computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
                                computeCovariateBalanceArgs = computeCovBalArgs,
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
                                createPsArgs = createPsArgs,
                                matchOnPsArgs = matchOnPsArgs,
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
                                createPsArgs = createPsArgs,
                                stratifyByPsArgs = stratifyByPsArgs,
                                fitOutcomeModelArgs = fitOutcomeModelArgs5)
cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5, cmAnalysis6)

saveCmAnalysisList(cmAnalysisList, file.path(folder, "cmAnalysisList.json"))
saveTargetComparatorOutcomesList(targetComparatorOutcomesList, file.path(folder, "targetComparatorOutcomesList.json"))

# Run analyses -----------------------------------------------------------------
cmAnalysisList <- loadCmAnalysisList(file.path(folder, 'cmAnalysisList.json'))
targetComparatorOutcomesList <- loadTargetComparatorOutcomesList(file.path(folder, 'targetComparatorOutcomesList.json'))
multiThreadingSettings <- createDefaultMultiThreadingSettings(parallel::detectCores())

result <- runCmAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  exposureDatabaseSchema = cohortDatabaseSchema,
  exposureTable = cohortTable,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTable,
  outputFolder = folder,
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  multiThreadingSettings = multiThreadingSettings
)

# Export to CSV ----------------------------------------------------------------
exportToCsv(
  outputFolder = folder,
  minCellCount = 5,
  maxCores = 5,
  databaseId = "MDCD"
)

# Cleanup ----------------------------------------------------------------------
sql <- "DROP TABLE @resultsDatabaseSchema.outcomes"
connection <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::renderTranslateExecuteSql(connection, sql, resultsDatabaseSchema = resultsDatabaseSchema)
DatabaseConnector::disconnect(connection)

# Shiny app --------------------------------------------------------------------
cohorts <- data.frame(
  cohortId = c(
    1118084,
    1124300,
    192671),
  cohortName = c(
    "Celecoxib",
    "Diclofenac",
    "GI Bleed"
  )
)

insertExportedResultsInSqlite(sqliteFileName = file.path(folder, "export", "results.sqlite"),
                              exportFolder = file.path(folder, "export"),
                              cohorts = cohorts)
launchResultsViewerUsingSqlite(sqliteFileName = file.path(folder, "export", "results.sqlite"))

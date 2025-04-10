# Copyright 2025 Observational Health Data Sciences and Informatics
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
options(andromedaTempFolder = "e:/andromedaTemp")

folder <- "e:/temp/cohortMethodVignette"
# unlink(folder, recursive = TRUE)
# dir.create(folder)

# Set connection details -------------------------------------------------------
# MDCR on DataBricks
connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "merative_mdcr.cdm_merative_mdcr_v3045"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable  <- "cm_vignette"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")


# Define exposure cohorts ------------------------------------------------------
library(Capr)

osteoArthritisOfKneeConceptId <- 4079750
celecoxibConceptId <- 1118084
diclofenacConceptId <- 1124300
osteoArthritisOfKnee <- cs(
  descendants(osteoArthritisOfKneeConceptId),
  name = "Osteoarthritis of knee"
)
attrition = attrition(
  "prior osteoarthritis of knee" = withAll(
    atLeast(1, conditionOccurrence(osteoArthritisOfKnee), duringInterval(eventStarts(-Inf, 0)))
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
    drugExposure(celecoxib, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition,
  exit = exit(endStrategy = drugExit(celecoxib,
                                     persistenceWindow = 30,
                                     surveillanceWindow = 0))
)
diclofenacCohort <- cohort(
  entry = entry(
    drugExposure(diclofenac, firstOccurrence()),
    observationWindow = continuousObservation(priorDays = 365)
  ),
  attrition = attrition,
  exit = exit(endStrategy = drugExit(diclofenac,
                                     persistenceWindow = 30,
                                     surveillanceWindow = 0))
)
exposureCohorts <- makeCohortSet(celecoxibCohort, diclofenacCohort)

# Define outcome cohort --------------------------------------------------------
library(PhenotypeLibrary)
outcomeCohorts <- getPlCohortDefinitionSet(77) # GI bleed

# Generate cohorts -------------------------------------------------------------
allCohorts <- bind_rows(outcomeCohorts,
                        exposureCohorts)
library(CohortGenerator)
cohortTableNames <- getCohortTableNames(cohortTable = cohortTable)
createCohortTables(connectionDetails = connectionDetails,
                   cohortDatabaseSchema = cohortDatabaseSchema,
                   cohortTableNames = cohortTableNames)
generateCohortSet(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTableNames = cohortTableNames,
                  cohortDefinitionSet = allCohorts)

# Check number of subjects per cohort:
connection <- DatabaseConnector::connect(connectionDetails)
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@cohortTable GROUP BY cohort_definition_id"
DatabaseConnector::renderTranslateQuerySql(connection, sql,  cohortDatabaseSchema = cohortDatabaseSchema, cohortTable = cohortTable)
DatabaseConnector::disconnect(connection)

# Run analyses -----------------------------------------------------------------
covSettings <- createDefaultCovariateSettings(
  excludedCovariateConceptIds = c(diclofenacConceptId, celecoxibConceptId),
  addDescendantsToExclude = TRUE
)

# Load data:
cohortMethodData <- getDbCohortMethodData(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  targetId = 1,
  comparatorId = 2,
  outcomeIds = 77,
  exposureDatabaseSchema = cohortDatabaseSchema,
  exposureTable = cohortTable,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTable,
  covariateSettings = covSettings
)

saveCohortMethodData(cohortMethodData, file.path(folder, "cohortMethodData.zip"))

# cohortMethodData <- loadCohortMethodData(file.path(folder, "cohortMethodData.zip"))

cohortMethodData

summary(cohortMethodData)

getAttritionTable(cohortMethodData)

studyPop <- createStudyPopulation(
  cohortMethodData = cohortMethodData,
  outcomeId = 77,
  removeDuplicateSubjects = "keep first",
  removeSubjectsWithPriorOutcome = TRUE,
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 30,
  endAnchor = "cohort end"
)

plotTimeToEvent(
  cohortMethodData = cohortMethodData,
  outcomeId = 77,
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  removeDuplicateSubjects = "keep all",
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 30,
  endAnchor = "cohort end"
)

saveRDS(studyPop, file.path(folder, "studyPop.rds"))

# studyPop <- readRDS(file.path(folder, "studyPop.rds"))

ps <- createPs(
  cohortMethodData = cohortMethodData,
  population = studyPop,
  prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
  control = createControl(
    cvType = "auto",
    startingVariance = 0.01,
    noiseLevel = "quiet",
    tolerance = 2e-07,
    cvRepetitions = 1,
    threads = 10
  )
)

saveRDS(ps, file = file.path(folder, "ps.rds"))

# ps <- readRDS(file.path(folder, "ps.rds"))

plotPs(ps, showAucLabel = TRUE, fileName = file.path(folder, "ps.png"))

computePsAuc(ps)

model <- getPsModel(ps, cohortMethodData)
model[grepl("age group*", model$covariateName), ]

plotPs(ps, showAucLabel = TRUE, showCountsLabel = TRUE, fileName = "extras/ps.png")
plotPs(ps)
plotPs(ps, scale = "propensity", showCountsLabel = TRUE, showEquiposeLabel = TRUE)
plotPs(ps, scale = "propensity", type = "histogram", showCountsLabel = TRUE, showEquiposeLabel = TRUE)

trimmed <- trimByPs(ps)
plotPs(trimmed, ps)

matchedPop <- matchOnPs(ps, caliper = 0.25, caliperScale = "standardized", maxRatio = 100)

balance <- computeCovariateBalance(matchedPop, cohortMethodData)

saveRDS(balance, file = file.path(folder, "balance.rds"))

# balance <- readRDS(file.path(folder, "balance.rds"))

table1 <- createCmTable1(balance)
print(table1, row.names = FALSE, right = FALSE)
plotCovariateBalanceScatterPlot(balance, showCovariateCountLabel = TRUE, showMaxLabel = TRUE, fileName = "extras/balanceScatterplot.png")
getGeneralizabilityTable(balance)

balanceIptw <- computeCovariateBalance(ps, cohortMethodData)
saveRDS(balanceIptw, file = file.path(folder, "balanceIptw.rds"))
getGeneralizabilityTable(balanceIptw)

outcomeModel <- fitOutcomeModel(
  population = studyPop,
  modelType = "cox",
  stratified = FALSE,
  useCovariates = FALSE
)
getAttritionTable(outcomeModel)
outcomeModel
summary(outcomeModel)
coef(outcomeModel)
confint(outcomeModel)
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel1.rds"))

outcomeModel <- fitOutcomeModel(
  population = ps,
  modelType = "cox",
  stratified = FALSE,
  useCovariates = FALSE,
  inversePtWeighting = TRUE
)
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel1b.rds"))

outcomeModel <- fitOutcomeModel(
  population = matchedPop,
  modelType = "cox",
  stratified = TRUE,
  useCovariates = FALSE
)
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel2.rds"))

outcomeModel <- fitOutcomeModel(
  population = trimmed,
  modelType = "cox",
  stratified = FALSE,
  useCovariates = FALSE,
  inversePtWeighting = TRUE
)
outcomeModel
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel2w.rds"))


outcomeModel <- fitOutcomeModel(
  population = matchedPop,
  cohortMethodData = cohortMethodData,
  modelType = "cox",
  stratified = TRUE,
  useCovariates = TRUE,
  prior = createPrior("laplace", useCrossValidation = TRUE),
  control = createControl(
    cvType = "auto",
    startingVariance = 0.01,
    selectorType = "byPid",
    cvRepetitions = 1,
    tolerance = 2e-07,
    threads = 16,
    noiseLevel = "quiet"
  )
)
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel3.rds"))

population <- stratifyByPs(ps, numberOfStrata = 10)
interactionCovariateIds <- c(8532001, 201826210, 21600960413) # Female, T2DM, concurrent use of antithrombotic agents
outcomeModel <- fitOutcomeModel(
  population = population,
  cohortMethodData = cohortMethodData,
  modelType = "cox",
  stratified = TRUE,
  useCovariates = FALSE,
  inversePtWeighting = FALSE,
  interactionCovariateIds = interactionCovariateIds,
  control = createControl(threads = 6)
)
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel4.rds"))

balanceFemale <- computeCovariateBalance(matchedPop, cohortMethodData, subgroupCovariateId = 8532001)
saveRDS(balanceFemale, file = file.path(folder, "balanceFemale.rds"))


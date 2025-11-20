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
cdmDatabaseSchema <- "merative_mdcr.cdm_merative_mdcr_v3788"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable  <- "cm_vignette"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")


# Define exposure cohorts ------------------------------------------------------
library(Capr)

celecoxibConceptId <- 1118084
diclofenacConceptId <- 1124300
osteoArthritisOfKneeConceptId <- 4079750

celecoxib <- cs(
  descendants(celecoxibConceptId),
  name = "Celecoxib"
)

celecoxibCohort <- cohort(
  entry = entry(
    drugExposure(celecoxib)
  ),
  exit = exit(endStrategy = drugExit(celecoxib,
                                     persistenceWindow = 30,
                                     surveillanceWindow = 0))
)

diclofenac  <- cs(
  descendants(diclofenacConceptId),
  name = "Diclofenac"
)

diclofenacCohort <- cohort(
  entry = entry(
    drugExposure(diclofenac)
  ),
  exit = exit(endStrategy = drugExit(diclofenac,
                                     persistenceWindow = 30,
                                     surveillanceWindow = 0))
)

osteoArthritisOfKnee <- cs(
  descendants(osteoArthritisOfKneeConceptId),
  name = "Osteoarthritis of knee"
)

osteoArthritisOfKneeCohort <- cohort(
  entry = entry(
    conditionOccurrence(osteoArthritisOfKnee, firstOccurrence())
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
# Note: this will automatically assign cohort IDs 1,2, and 3, respectively:
exposuresAndIndicationCohorts <- makeCohortSet(celecoxibCohort,
                                               diclofenacCohort,
                                               osteoArthritisOfKneeCohort)

# Define outcome cohort --------------------------------------------------------
library(PhenotypeLibrary)
outcomeCohorts <- getPlCohortDefinitionSet(77) # GI bleed

# Generate cohorts -------------------------------------------------------------
allCohorts <- bind_rows(exposuresAndIndicationCohorts,
                        outcomeCohorts)
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
  nestingCohortDatabaseSchema = cohortDatabaseSchema,
  nestingCohortTable = cohortTable,
  getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs(
    firstExposureOnly = TRUE,
    removeDuplicateSubjects = "keep first, truncate to second",
    restrictToCommonPeriod = TRUE,
    washoutPeriod = 365,
    nestingCohortId = 3,
    covariateSettings = covSettings
  )
)

saveCohortMethodData(cohortMethodData, file.path(folder, "cohortMethodData.zip"))

# cohortMethodData <- loadCohortMethodData(file.path(folder, "cohortMethodData.zip"))

cohortMethodData

summary(cohortMethodData)

getAttritionTable(cohortMethodData)

studyPop <- createStudyPopulation(
  cohortMethodData = cohortMethodData,
  outcomeId = 77,
  createStudyPopulationArgs = createCreateStudyPopulationArgs(
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 365,
    minDaysAtRisk = 1,
    riskWindowStart = 0,
    startAnchor = "cohort start",
    riskWindowEnd = 30,
    endAnchor = "cohort end"
  )
)

getAttritionTable(studyPop)

plotTimeToEvent(
  cohortMethodData = cohortMethodData,
  outcomeId = 77,
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
  createPsArgs = createCreatePsArgs(
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
)

saveRDS(ps, file = file.path(folder, "ps.rds"))

# ps <- readRDS(file.path(folder, "ps.rds"))

plotPs(ps, showAucLabel = TRUE)

auc <- computePsAuc(ps)
saveRDS(auc, file.path(folder, "auc.rds"))

plotPs(ps,
       targetLabel = "Celexocib",
       comparatorLabel = "Diclofenac",
       showCountsLabel = TRUE,
       showAucLabel = TRUE,
       showEquiposeLabel = TRUE)

model <- getPsModel(ps, cohortMethodData)
model[grepl("age group*", model$covariateName), ]

plotPs(ps, showAucLabel = TRUE, showCountsLabel = TRUE, fileName = "extras/ps.png")
plotPs(ps)
plotPs(ps, scale = "propensity", showCountsLabel = TRUE, showEquiposeLabel = TRUE)
plotPs(ps, scale = "propensity", type = "histogram", showCountsLabel = TRUE, showEquiposeLabel = TRUE)

trimmedPop <- trimByPs(ps,
                       trimByPsArgs = createTrimByPsArgs(
                         equipoiseBounds = c(0.3, 0.7)
                       ))
# Note: we need to also provide the original PS object so the preference score
# is computed using the original relative sizes of the cohorts:
plotPs(trimmedPop, ps)


stratifiedPop <- stratifyByPs(ps,
                              stratifyByPsArgs = createStratifyByPsArgs(
                                numberOfStrata = 5
                              ))
plotPs(stratifiedPop)


matchedPop <- matchOnPs(ps,
                        matchOnPsArgs = createMatchOnPsArgs(
                          maxRatio = 1
                        ))
plotPs(matchedPop, ps)
drawAttritionDiagram(matchedPop)

balance <- computeCovariateBalance(matchedPop, cohortMethodData)

saveRDS(balance, file = file.path(folder, "balance.rds"))

# balance <- readRDS(file.path(folder, "balance.rds"))

table1 <- createCmTable1(balance)
print(table1, row.names = FALSE, right = FALSE)
plotCovariateBalanceScatterPlot(balance, showCovariateCountLabel = TRUE, showMaxLabel = TRUE, fileName = "extras/balanceScatterplot.png")
getGeneralizabilityTable(balance)

# balanceIptw <- computeCovariateBalance(ps, cohortMethodData)
# saveRDS(balanceIptw, file = file.path(folder, "balanceIptw.rds"))
# getGeneralizabilityTable(balanceIptw)

outcomeModel <- fitOutcomeModel(
  population = studyPop,
  fitOutcomeModelArgs = createFitOutcomeModelArgs(
    modelType = "cox"
  )
)
getAttritionTable(outcomeModel)
outcomeModel
summary(outcomeModel)
coef(outcomeModel)
confint(outcomeModel)
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel1.rds"))

# outcomeModel <- fitOutcomeModel(
#   population = ps,
#   fitOutcomeModelArgs = createFitOutcomeModelArgs(
#     modelType = "cox",
#     stratified = FALSE,
#     useCovariates = FALSE,
#     inversePtWeighting = TRUE
#   )
# )
# saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel1b.rds"))

outcomeModel <- fitOutcomeModel(
  population = matchedPop,
  fitOutcomeModelArgs = createFitOutcomeModelArgs(
    modelType = "cox"
  )
)
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel2.rds"))

outcomeModel <- fitOutcomeModel(
  population = ps,
  fitOutcomeModelArgs = createFitOutcomeModelArgs(
    modelType = "cox",
    stratified = FALSE,
    useCovariates = FALSE,
    inversePtWeighting = TRUE,
    bootstrapCi = TRUE,
    bootstrapReplicates = 200
  )
)
outcomeModel
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel3.rds"))


population <- stratifyByPs(ps, stratifyByPsArgs = createStratifyByPsArgs(numberOfStrata = 10))
interactionCovariateIds <- c(8532001, 201826210, 21600960413) # Female, T2DM, concurrent use of antithrombotic agents
outcomeModel <- fitOutcomeModel(
  population = matchedPop,
  cohortMethodData = cohortMethodData,
  fitOutcomeModelArgs = createFitOutcomeModelArgs(
    modelType = "cox",
    interactionCovariateIds = interactionCovariateIds,
    control = createControl(threads = 6)
  )
)
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel4.rds"))

balanceFemale <- computeCovariateBalance(
  matchedPop,
  cohortMethodData,
  computeCovariateBalanceArgs = createComputeCovariateBalanceArgs(
    subgroupCovariateId = 8532001
  )
)
saveRDS(balanceFemale, file = file.path(folder, "balanceFemale.rds"))

outcomeModel <- fitOutcomeModel(
  population = matchedPop,
  cohortMethodData = cohortMethodData,
  fitOutcomeModelArgs = createFitOutcomeModelArgs(
    modelType = "cox",
    stratified = TRUE,
    useCovariates = TRUE,
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
)
saveRDS(outcomeModel, file = file.path(folder, "OutcomeModel5.rds"))


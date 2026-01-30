# Some methods research into added value of concept group covariates.
library(SqlRender)
library(DatabaseConnector)
library(CohortMethod)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcr"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))
cdmDatabaseSchema <- "cdm_truven_mdcr_v1838"
cohortDatabaseSchema <- "scratch_mschuemi"
cohortTable <- "covariate_grouping_test"
cdmVersion <- "5"

folder <- "s:/temp/GroupingTest"
# dir.create(folder)

# Create outcome cohorts -------------------------------
connection <- DatabaseConnector::connect(connectionDetails)

sql <- "
IF OBJECT_ID('@cohortDatabaseSchema.@cohortTable', 'U') IS NOT NULL
DROP TABLE @cohortDatabaseSchema.@cohortTable;

SELECT ancestor_concept_id AS cohort_definition_id,
  MIN(condition_start_date) AS cohort_start_date,
  MIN(condition_end_date) AS cohort_end_date,
  condition_occurrence.person_id AS subject_id
INTO @cohortDatabaseSchema.@cohortTable
FROM @cdmDatabaseSchema.condition_occurrence
INNER JOIN @cdmDatabaseSchema.visit_occurrence
  ON condition_occurrence.visit_occurrence_id = visit_occurrence.visit_occurrence_id
INNER JOIN @cdmDatabaseSchema.concept_ancestor
  ON condition_concept_id = descendant_concept_id
WHERE (ancestor_concept_id = 192671
    AND visit_occurrence.visit_concept_id IN (262, 9201, 9203))
  OR (ancestor_concept_id IN (24609, 29735, 73754, 80004, 134718, 139099, 141932, 192367, 193739, 194997, 197236, 199074, 255573, 257007, 313459, 314658, 316084, 319843, 321596, 374366, 375292, 380094, 433753, 433811, 436665, 436676, 436940, 437784, 438134, 440358, 440374, 443617, 443800, 4084966, 4288310))
GROUP BY ancestor_concept_id,
  condition_occurrence.person_id;
"
sql <- SqlRender::render(sql = sql,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable)
sql <- SqlRender::translate(sql = sql,
                            targetDialect = connectionDetails$dbms)

DatabaseConnector::executeSql(connection, sql)

sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@cohortTable GROUP BY cohort_definition_id"
sql <- SqlRender::render(sql, cohortDatabaseSchema = cohortDatabaseSchema, cohortTable = cohortTable)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

DatabaseConnector::disconnect(connection)

# Define analysis ---------------------------------------------------

# Need extra concept IDs because of missing ancestor relatioships:
nsaids <- c(21603933, 40162333, 40162398, 40162316, 40162311, 40162315, 44785204, 40162464, 40162312, 40162321, 40162322, 40175472, 40228941)

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

covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                              addDescendantsToExclude = TRUE)

# createCovariateSettings(useDrugExposureLongTerm = TRUE,
#                         useDrugEraShortTerm = TRUE,
#                         useConditionOccurrenceLongTerm = TRUE,
#                         useConditionOccurrenceShortTerm = TRUE)

covSettings$ConditionOccurrenceLongTerm <- TRUE
covSettings$ConditionOccurrenceShortTerm <- TRUE
covSettings$DrugExposureLongTerm <- TRUE
covSettings$DrugEraShortTerm <- TRUE
covSettings$DrugGroupEraOverlapping <- NULL

# Fetch data manually ---------------------------------------------------------
cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          targetId = tcos$targetId,
                                          comparatorId = tcos$comparatorId,
                                          outcomeIds = tcos$outcomeIds,
                                          studyStartDate = "",
                                          studyEndDate = "",
                                          exposureDatabaseSchema = cdmDatabaseSchema,
                                          exposureTable = "drug_era",
                                          outcomeDatabaseSchema = cohortDatabaseSchema,
                                          outcomeTable = cohortTable,
                                          cdmVersion = cdmVersion,
                                          firstExposureOnly = TRUE,
                                          removeDuplicateSubjects = "remove all",
                                          restrictToCommonPeriod = FALSE,
                                          washoutPeriod = 180,
                                          covariateSettings = covSettings,
                                          maxCohortSize = 50000)
saveCohortMethodData(cohortMethodData, file.path(folder, "cohortMethodData.zip"))
cohortMethodData <- loadCohortMethodData(file.path(folder, "cohortMethodData.zip"))

# Without grouping variables -----------------------------------------------
groupingCovariates <- cohortMethodData$covariateRef |>
  collect() |>
  filter(grepl("drug_era group", .data$covariateName) |
           grepl("condition_era group", .data$covariateName) )

ps <- createPs(cohortMethodData = cohortMethodData,
               control = createControl(cvType = "auto",
                                       startingVariance = 0.01,
                                       tolerance = 1E-5,
                                       noiseLevel = "quiet",
                                       cvRepetitions = 1),
               excludeCovariateIds = groupingCovariates$covariateId)

saveRDS(ps, file.path(folder, "PsWithoutGroups.rds"))
ps <- readRDS(file.path(folder, "PsWithoutGroups.rds"))

plotPs(ps, fileName = file.path(folder, "PsWithoutGroups.png"))

strataPop <- matchOnPs(ps, maxRatio = 100)

balance <- computeCovariateBalance(strataPop, cohortMethodData)

saveRDS(balance, file.path(folder, "balanceWithoutGroups.rds"))

dummy <- plotCovariateBalanceScatterPlot(balance, fileName =  file.path(folder, "balanceScatterPlotWithoutGroups.png"))
dummy <- plotCovariateBalanceOfTopVariables(balance, fileName =  file.path(folder, "balanceTopPlotWithoutGroups.png"))

estimates <- tibble()
# outcomeId <- tcos$outcomeIds[1]
for (outcomeId in tcos$outcomeIds) {
  studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                    population = ps,
                                    outcomeId = outcomeId,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 1,
                                    riskWindowStart = 0,
                                    startAnchor = "cohort start",
                                    riskWindowEnd = 30,
                                    endAnchor = "cohort end")
  strataPop <- matchOnPs(studyPop, maxRatio = 100)
  model <- fitOutcomeModel(population = strataPop,
                           modelType = "cox",
                           stratified = TRUE)
  model$outcomeModelTreatmentEstimate$outcomeId <- outcomeId
  estimates <- bind_rows(estimates,
                         model$outcomeModelTreatmentEstimate)
}
saveRDS(estimates, file.path(folder, "estimatesWithoutGroups.png"))
estimates <- readRDS(file.path(folder, "estimatesWithoutGroups.png"))

ncs <- estimates[estimates$outcomeId != 192671, ]
pcs <- estimates[estimates$outcomeId == 192671, ]

EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = ncs$logRr,
                                            seLogRrNegatives = ncs$seLogRr,
                                            logRrPositives = pcs$logRr,
                                            seLogRrPositives = pcs$seLogRr,
                                            showCis = TRUE,
                                            fileName = file.path(folder, "calibrationWithoutGroups.png"))

ncsWithoutGroups <- ncs

# With grouping variables -----------------------------------------------
# verbatimCovariates <- cohortMethodData$covariateRef |>
#   collect() |>
#   filter(grepl("drug_exposure", .data$covariateName) |
#            grepl("condition_occurrence", .data$covariateName) )
#
# erroneousConceptIds <- verbatimCovariates |>
#   filter(grepl("diclofenac", .data$covariateName) |
#            grepl("celecoxib", .data$covariateName)) |>
#   pull(conceptId)
# writeLines(paste(erroneousConceptIds, collapse = ", "))

ps <- createPs(cohortMethodData = cohortMethodData,
               control = createControl(cvType = "auto",
                                       startingVariance = 0.01,
                                       tolerance = 1E-5,
                                       noiseLevel = "quiet",
                                       cvRepetitions = 1))

saveRDS(ps, file.path(folder, "PsWithGroups.rds"))
ps <- readRDS(file.path(folder, "PsWithGroups.rds"))

plotPs(ps, fileName = file.path(folder, "PsWithGroups.png"))

strataPop <- matchOnPs(ps, maxRatio = 100)

balance <- computeCovariateBalance(strataPop, cohortMethodData)

saveRDS(balance, file.path(folder, "balanceWithGroups.rds"))

dummy <- plotCovariateBalanceScatterPlot(balance, fileName =  file.path(folder, "balanceScatterPlotWithGroups.png"))
dummy <- plotCovariateBalanceOfTopVariables(balance, fileName =  file.path(folder, "balanceTopPlotWithGroups.png"))

estimates <- tibble()
# outcomeId <- tcos$outcomeIds[1]
for (outcomeId in tcos$outcomeIds) {
  studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                    population = ps,
                                    outcomeId = outcomeId,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    minDaysAtRisk = 1,
                                    riskWindowStart = 0,
                                    startAnchor = "cohort start",
                                    riskWindowEnd = 30,
                                    endAnchor = "cohort end")
  strataPop <- matchOnPs(studyPop, maxRatio = 100)
  model <- fitOutcomeModel(population = strataPop,
                           modelType = "cox",
                           stratified = TRUE)
  model$outcomeModelTreatmentEstimate$outcomeId <- outcomeId
  estimates <- bind_rows(estimates,
                         model$outcomeModelTreatmentEstimate)
}
saveRDS(estimates, file.path(folder, "estimatesWithGroups.png"))
estimates <- readRDS(file.path(folder, "estimatesWithGroups.png"))

ncs <- estimates[estimates$outcomeId != 192671, ]
pcs <- estimates[estimates$outcomeId == 192671, ]

EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = ncs$logRr,
                                            seLogRrNegatives = ncs$seLogRr,
                                            logRrPositives = pcs$logRr,
                                            seLogRrPositives = pcs$seLogRr,
                                            showCis = TRUE,
                                            fileName = file.path(folder, "calibrationWithGroups.png"))

ncsWithGroups <- ncs

EmpiricalCalibration::computeExpectedAbsoluteSystematicError(EmpiricalCalibration::fitMcmcNull(ncsWithoutGroups$logRr, ncsWithoutGroups$seLogRr))
# ease       ciLb      ciUb
# 1 0.05659901 0.01328614 0.1453492
EmpiricalCalibration::computeExpectedAbsoluteSystematicError(EmpiricalCalibration::fitMcmcNull(ncsWithGroups$logRr, ncsWithGroups$seLogRr))
# ease       ciLb      ciUb
# 1 0.04705144 0.01158418 0.1176077
EmpiricalCalibration::compareEase(logRr1 = ncsWithoutGroups$logRr,
                                  seLogRr1 = ncsWithoutGroups$seLogRr,
                                  logRr2 = ncsWithGroups$logRr,
                                  seLogRr2 = ncsWithGroups$seLogRr)
# delta       ciLb       ciUb     p
# 1 0.02113924 -0.0221847 0.06846655 0.227

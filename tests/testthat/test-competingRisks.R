library(CohortMethod)
library(Eunomia)
library(testthat)
library(cmprsk)
library(crrSC)

connectionDetails <- getEunomiaConnectionDetails()

Eunomia::createCohorts(connectionDetails)

createSillyCompetingRisk <- function(studyPopulation,
                                     populationProportion = 0.5) {
  len <- nrow(studyPopulation)
  riskPopulation <- studyPopulation
  hasEvent <- rbinom(len, size = 1, prob = populationProportion)
  newDaysToEvent <- ifelse(hasEvent == 1, floor(runif(len, studyPopulation$riskStart, studyPopulation$riskEnd)), NA)
  newSurvivalTime <- ifelse(hasEvent == 1, newDaysToEvent + 1, studyPopulation$timeAtRisk)

  riskPopulation <- studyPopulation %>%
    mutate(outcomeCount = hasEvent) %>%
    mutate(daysToEvent = newDaysToEvent,
           survivalTime = newSurvivalTime)

  return(riskPopulation)
}

createKnownCompetingRisk <- function(studyPopulation){

  len <- nrow(studyPopulation)
  hasEvent <- c(rep(1, 150), rep(0, len-150))
  oldDaysToEvent <- studyPopulation$daysToEvent
  newDaysToEvent <- oldDaysToEvent %>%
    replace(c(51:100), oldDaysToEvent[51:100] - 1) %>%
    replace(c(101:150), oldDaysToEvent[101:150] + 1)
  newDaysToEvent <- ifelse(hasEvent == 0, NA, newDaysToEvent)
  newDaysToEvent <- ifelse(is.na(newDaysToEvent) & hasEvent == 1, floor(runif(len, studyPopulation$riskStart, studyPopulation$riskEnd)), newDaysToEvent)
  newSurvivalTime <- ifelse(hasEvent == 1, newDaysToEvent + 1, studyPopulation$timeAtRisk)

  newPop <- studyPopulation %>%
    mutate(outcomeCount = hasEvent) %>%
    mutate(daysToEvent = newDaysToEvent,
           survivalTime = newSurvivalTime)

  return(newPop)
}

cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = "main",
                                          targetId = 1,
                                          comparatorId = 2,
                                          outcomeIds = 3,
                                          exposureDatabaseSchema = "main",
                                          outcomeDatabaseSchema = "main",
                                          exposureTable = "cohort",
                                          outcomeTable = "cohort",
                                          covariateSettings = createDefaultCovariateSettings())

studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                  outcomeId = 3,
                                  riskWindowEnd = 99999)
set.seed(123)
riskPop <- createSillyCompetingRisk(studyPop, populationProportion = 0.2)
riskTable <- riskPop %>% filter(outcomeCount == 1) %>%
  mutate(cohortStartDate = as.character(as.Date(cohortStartDate + daysToEvent, origin = "1970-01-01"))) %>%
  mutate(cohortEndDate = cohortStartDate) %>%
  mutate(cohortDefinitionId = 5) %>%
  select(cohortDefinitionId, subjectId, cohortStartDate, cohortEndDate)
names(riskTable) <- toupper(SqlRender::camelCaseToSnakeCase(names(riskTable)))

riskPopKnown <- createKnownCompetingRisk(studyPop)
riskTableKnown <- riskPopKnown %>% filter(outcomeCount == 1) %>%
  mutate(cohortStartDate = as.character(as.Date(cohortStartDate + daysToEvent, origin = "1970-01-01"))) %>%
  mutate(cohortEndDate = cohortStartDate) %>%
  mutate(cohortDefinitionId = 6) %>%
  select(cohortDefinitionId, subjectId, cohortStartDate, cohortEndDate)
names(riskTableKnown) <- toupper(SqlRender::camelCaseToSnakeCase(names(riskTableKnown)))

riskTable <- rbind(riskTable, riskTableKnown)

connect <- DatabaseConnector::connect(connectionDetails)
oT <- querySql(connect, "SELECT * FROM cohort")
insertTable(connect, "cohort",
            rbind(oT, riskTable),
            dropTableIfExists = TRUE, createTable = TRUE)
dbDisconnect(connect)

test_that("Competing risk population creation", {
  nsaids <- c(1118084, 1124300) # celecoxib, diclofenac

  covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                                addDescendantsToExclude = TRUE)

  sCohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                             cdmDatabaseSchema = "main",
                                             targetId = 1,
                                             comparatorId = 2,
                                             outcomeIds = c(3, 5, 6),
                                             exposureDatabaseSchema = "main",
                                             outcomeDatabaseSchema = "main",
                                             exposureTable = "cohort",
                                             outcomeTable = "cohort",
                                             covariateSettings = covSettings)
  studyPop3 <- createStudyPopulation(cohortMethodData = sCohortMethodData,
                                     outcomeId = 3,
                                     riskWindowEnd = 99999)
  studyPop5 <- createStudyPopulation(cohortMethodData = sCohortMethodData,
                                     outcomeId = 5,
                                     riskWindowEnd = 99999)
  studyPop6 <- createStudyPopulation(cohortMethodData = sCohortMethodData,
                                     outcomeId = 6,
                                     riskWindowEnd = 99999)

  expect_error(combineCompetingStudyPopulations(mainPopulation = studyPop3[-1,],
                                                competingRiskPopulation = studyPop4))

  #expect_no_error
  invisible(combineCompetingStudyPopulations(mainPopulation = studyPop3,
                                             competingRiskPopulation = studyPop5[-1,]))

  studyPopSimul <- combineCompetingStudyPopulations(mainPopulation = studyPop3,
                                                    competingRiskPopulation = studyPop6)
  studyPopReplace <- combineCompetingStudyPopulations(mainPopulation = studyPop3,
                                                      competingRiskPopulation = studyPop6,
                                                      removeSubjectsWithSimultaneousEvents = FALSE,
                                                      codeSimultaneousEventsAs = 1)
  simulEvents <- studyPop3 %>%
    subset(outcomeCount == 1) %>%
    inner_join(studyPop6, by = c("subjectId", "outcomeCount", "survivalTime"))

  expect_equivalent(nrow(studyPop3) - nrow(simulEvents), nrow(studyPopSimul))
  expect_equivalent(nrow(studyPop3), nrow(studyPopReplace))

})

test_that("Competing risks non stratified single analysis", {

  nsaids <- c(1118084, 1124300) # celecoxib, diclofenac

  covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                                addDescendantsToExclude = TRUE)

  sCohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = "main",
                                            targetId = 1,
                                            comparatorId = 2,
                                            outcomeIds = c(3, 5),
                                            exposureDatabaseSchema = "main",
                                            outcomeDatabaseSchema = "main",
                                            exposureTable = "cohort",
                                            outcomeTable = "cohort",
                                            covariateSettings = covSettings)

  studyPop3 <- createStudyPopulation(cohortMethodData = sCohortMethodData,
                                     outcomeId = 3,
                                     riskWindowEnd = 99999)

  studyPop5 <- createStudyPopulation(cohortMethodData = sCohortMethodData,
                                     outcomeId = 5,
                                     riskWindowEnd = 99999)

  studyPopCombined <- combineCompetingStudyPopulations(mainPopulation = studyPop3,
                                                       competingRiskPopulation = studyPop5)

  fitNoRisk1 <- fitOutcomeModel(studyPop3,
                               modelType = "cox")

  fitRisk1 <- fitOutcomeModel(studyPop3,
                              modelType = "fgr")

  fitRisk3 <- fitOutcomeModel(studyPopCombined,
                              modelType = "cox")

  expect_equal(coef(fitNoRisk1), coef(fitRisk1))

  fitRisk2 <- fitOutcomeModel(studyPopCombined,
                              modelType = "fgr")

  expect_false(coef(fitRisk1) == coef(fitRisk2))

  goodFit1 <- crr(ftime = studyPopCombined$survivalTime,
                  fstatus = studyPopCombined$outcomeCount,
                  cov1 = studyPopCombined$treatment)

  goodFit2 <- crr(ftime = studyPop3$survivalTime,
                  fstatus = studyPop3$outcomeCount,
                  cov1 = studyPop3$treatment)

  expect_equivalent(coef(fitRisk2), goodFit1$coef)
  expect_equivalent(coef(fitNoRisk1), goodFit2$coef)

})

test_that("Competing risks stratified single analysis", {
  nsaids <- c(1118084, 1124300) # celecoxib, diclofenac

  covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                                addDescendantsToExclude = TRUE)

  sCohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                             cdmDatabaseSchema = "main",
                                             targetId = 1,
                                             comparatorId = 2,
                                             outcomeIds = c(3, 5, 6),
                                             exposureDatabaseSchema = "main",
                                             outcomeDatabaseSchema = "main",
                                             exposureTable = "cohort",
                                             outcomeTable = "cohort",
                                             covariateSettings = covSettings)

  studyPop3 <- createStudyPopulation(cohortMethodData = sCohortMethodData,
                                     outcomeId = 3,
                                     riskWindowEnd = 99999)

  studyPop5 <- createStudyPopulation(cohortMethodData = sCohortMethodData,
                                     outcomeId = 5,
                                     riskWindowEnd = 99999)

  studyPopCombined <- combineCompetingStudyPopulations(mainPopulation = studyPop3,
                                                       competingRiskPopulation = studyPop5)

  psCombined <- createPs(cohortMethodData = sCohortMethodData,
                         population = studyPopCombined)
  stratPopCombined <- stratifyByPs(psCombined)

  fitRisk <- fitOutcomeModel(studyPopCombined,
                             modelType = "fgr")

  fitRiskStrat <- fitOutcomeModel(stratPopCombined,
                                  modelType = "fgr",
                                  stratified = TRUE)

  goodFitStrat <- crrs(ftime = stratPopCombined$survivalTime,
                       fstatus = stratPopCombined$outcomeCount,
                       cov1 = stratPopCombined$treatment,
                       strata = stratPopCombined$stratumId)

  expect_false(coef(fitRisk2) == coef(fitRiskStrat))
  expect_equivalent(coef(fitRiskStrat), goodFitStrat$coef)





})

test_that("Competing risks multiple analyses", {

  tco <- createTargetComparatorOutcomes(targetId = 1,
                                        comparatorId = 2,
                                        outcomeIds = 3)

  targetComparatorOutcomesList <- list(tco)

  covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(1118084, 1124300),
                                                addDescendantsToExclude = TRUE)

  getDbCmDataArgs <- createGetDbCohortMethodDataArgs(excludeDrugsFromCovariates = TRUE,
                                                    covariateSettings = covSettings)

  studyPopArgs <- createCreateStudyPopulationArgs(riskWindowEnd = 99999)

  fgrModelArgs <- createFitOutcomeModelArgs(modelType = "fgr",
                                            riskId = 5)

  expect_warning(createFitOutcomeModelArgs(modelType = "fgr"))

  coxModelArgs <- createFitOutcomeModelArgs(modelType = "cox")

  fgrAnalysis <- createCmAnalysis(analysisId = 1,
                                  description = "fgr",
                                  createStudyPopArgs = studyPopArgs,
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = fgrModelArgs)

  coxAnalysis <- createCmAnalysis(analysisId = 2,
                                  description = "cox",
                                  createStudyPopArgs = studyPopArgs,
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = coxModelArgs)


  outputFolder <- "./CohortMethodOutput"
  unlink(outputFolder, recursive=TRUE)
  result <- runCmAnalyses(connectionDetails = connectionDetails,
                          targetComparatorOutcomesList = targetComparatorOutcomesList,
                          cdmDatabaseSchema = "main",
                          exposureTable = "cohort",
                          outcomeTable = "cohort",
                          outputFolder = outputFolder,
                          cmAnalysisList = list(fgrAnalysis, coxAnalysis))

  fgrFit <- readRDS(file.path(outputFolder, "Analysis_1/om_t1_c2_o3.rds"))
  coxFit <- readRDS(file.path(outputFolder, "Analysis_2/om_t1_c2_o3.rds"))
  expect_false(fgrFit$outcomeModelCoefficients == coxFit$outcomeModelCoefficients)

  expect_false(is.na(result[result$analysisId == 1, ]$riskId))
  expect_true(is.na(result[result$analysisId != 1, ]$riskId))

})

unlink(connectionDetails$server)


library(CohortMethod)
library(Eunomia)
library(testthat)

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
                                  outcomeId = 1,
                                  riskWindowEnd = 99999)

set.seed(123)
riskPop <- createSillyCompetingRisk(studyPop, populationProportion = 0.2)
riskTable <- riskPop %>% filter(outcomeCount == 1) %>%
  mutate(cohortStartDate = as.character(as.Date(cohortStartDate + daysToEvent, origin = "1970-01-01"))) %>%
  mutate(cohortEndDate = cohortStartDate) %>%
  mutate(cohortDefinitionId = 5) %>%
  select(cohortDefinitionId, subjectId, cohortStartDate, cohortEndDate)
names(riskTable) <- toupper(SqlRender::camelCaseToSnakeCase(names(riskTable)))

connect <- DatabaseConnector::connect(connectionDetails)
oT <- querySql(connect, "SELECT * FROM cohort")
insertTable(connect, "cohort",
            rbind(oT, riskTable),
            dropTableIfExists = TRUE, createTable = TRUE)
dbDisconnect(connect)

test_that("Competing risks single analysis", {

  nsaids <- c(1118084, 1124300) # celecoxib, diclofenac

  covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                                addDescendantsToExclude = TRUE)

  cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = "main",
                                            targetId = 1,
                                            comparatorId = 2,
                                            outcomeIds = c(3,5),
                                            exposureDatabaseSchema = "main",
                                            outcomeDatabaseSchema = "main",
                                            exposureTable = "cohort",
                                            outcomeTable = "cohort",
                                            covariateSettings = covSettings)

  studyPop3 <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                     outcomeId = 3,
                                     riskWindowEnd = 99999)

  studyPop5 <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                     outcomeId = 5,
                                     riskWindowEnd = 99999)

  expect_error(combineCompetingStudyPopulations(mainPopulation = studyPop3[-1,],
                                                competingRiskPopulation = studyPop4))

  #expect_no_error
  invisible(combineCompetingStudyPopulations(mainPopulation = studyPop3,
                                   competingRiskPopulation = studyPop5[-1,]))


  studyPopCombined <- combineCompetingStudyPopulations(mainPopulation = studyPop3,
                                                       competingRiskPopulation = studyPop5)

  fitNoRisk1 <- fitOutcomeModel(studyPop3,
                               modelType = "cox")

  fitRisk1 <- fitOutcomeModel(studyPop3,
                              modelType = "fgr")

  expect_equal(coef(fitNoRisk1), coef(fitRisk1))

  fitRisk2 <- fitOutcomeModel(studyPopCombined,
                              modelType = "fgr")

  expect_false(coef(fitRisk1) == coef(fitRisk2))

  #outputFolder <- tempfile(pattern = "cmData")
  #unlink(outputFolder, recursive = TRUE)
})

test_that("Competing risks multiple analyses", {
  expect_false("riskIds" %in% names(
      createTargetComparatorOutcomes(targetId = 1,
                                         comparatorId = 2,
                                         outcomeIds = 3))
  )

  tcos <- createTargetComparatorOutcomes(targetId = 1,
                                         comparatorId = 2,
                                         outcomeIds = 3,
                                         riskIds = 5)
  expect_true("riskIds" %in% names(tcos))

  targetComparatorOutcomesList <- list(tcos)

  covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(1118084, 1124300),
                                                addDescendantsToExclude = TRUE)

  getDbCmDataArgs <- createGetDbCohortMethodDataArgs(covariateSettings = covSettings)

  spArgsRisk <- createCreateStudyPopulationArgs(riskWindowEnd = 99999)

  fgrArgs <- createFitOutcomeModelArgs(modelType = "fgr")

  coxArgs <- createFitOutcomeModelArgs(modelType = "cox")

  fgrAnalysis <- createCmAnalysis(analysisId = 1,
                               description = "whatever",
                               createStudyPopArgs = spArgsRisk,
                               getDbCohortMethodDataArgs = getDbCmDataArgs,
                               fitOutcomeModel = TRUE,
                               fitOutcomeModelArgs = fgrArgs)

  coxAnalysis <- createCmAnalysis(analysisId = 2,
                                  description = "whatever",
                                  createStudyPopArgs = spArgsRisk,
                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                  fitOutcomeModel = TRUE,
                                  fitOutcomeModelArgs = coxArgs)

  outputFolder <- "./CohortMethodOutput"
  unlink(outputFolder)
  result <- runCmAnalyses(connectionDetails = connectionDetails,
                          targetComparatorOutcomesList = targetComparatorOutcomesList,
                          cdmDatabaseSchema = "main",
                          exposureTable = "cohort",
                          outcomeTable = "cohort",
                          outputFolder = outputFolder,
                          cmAnalysisList = list(fgrAnalysis) # add coxFit$outcomeModelCoefficients back
                          )

  fgrFit <- readRDS(file.path(outputFolder, "Analysis_1/om_t1_c2_o3.rds"))
  coxFit <- readRDS(file.path(outputFolder, "Analysis_2/om_t1_c2_o3.rds"))

  expect_false(fgrFit$outcomeModelCoefficients == coxFit$outcomeModelCoefficients)

})

unlink(connectionDetails$server)

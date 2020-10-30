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

test_that("Competing risks single analysis", {

  nsaids <- c(1118084, 1124300) # celecoxib, diclofenac

  covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                                addDescendantsToExclude = TRUE)

  cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = "main",
                                            targetId = 1,
                                            comparatorId = 2,
                                            outcomeIds = c(3,4),
                                            exposureDatabaseSchema = "main",
                                            outcomeDatabaseSchema = "main",
                                            exposureTable = "cohort",
                                            outcomeTable = "cohort",
                                            covariateSettings = covSettings)

  studyPop3 <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                     outcomeId = 3,
                                     riskWindowEnd = 99999)

  studyPop4 <- createSillyCompetingRisk(studyPop3, populationProportion = 0.2)

  expect_error(combineCompetingStudyPopulations(mainPopulation = studyPop3[-1,],
                                                competingRiskPopulation = studyPop4))

  studyPopCombined <- combineCompetingStudyPopulations(mainPopulation = studyPop3,
                                                       competingRiskPopulation = studyPop4[-1,])

  fitNoRisk1 <- fitOutcomeModel(studyPop3,
                               modelType = "cox")

  fitRisk1 <- fitOutcomeModel(studyPop3,
                              modelType = "fgr")
  #
  # fitRisk2 <- fitOutcomeModel(studyPopCombined,
  #                             modelType = "fgr") # TODO modelType not yet implemented

  fitNoRisk2 <- fitOutcomeModel(studyPopCombined,
                                modelType = "cox") # TODO Cyclops should throw an error (2 %in% outcome type)

  #outputFolder <- tempfile(pattern = "cmData")
  #unlink(outputFolder, recursive = TRUE)
})

unlink(connectionDetails$server)

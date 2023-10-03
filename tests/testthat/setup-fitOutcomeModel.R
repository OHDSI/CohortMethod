# Study Population ----
nsaids <- c(1118084, 1124300)

covSettings <- createDefaultCovariateSettings(
  excludedCovariateConceptIds = nsaids,
  addDescendantsToExclude = TRUE
)

sCohortMethodData <- getDbCohortMethodData(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  targetId = 1,
  comparatorId = 2,
  outcomeIds = c(3, 4),
  exposureDatabaseSchema = "main",
  outcomeDatabaseSchema = "main",
  exposureTable = "cohort",
  outcomeTable = "cohort",
  covariateSettings = covSettings
)

studyPop <- createStudyPopulation(
  cohortMethodData = sCohortMethodData,
  outcomeId = 3,
  riskWindowEnd = 99999
)

# CohortMethod Data ----
sCohortMethodData <- getDbCohortMethodData(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  targetId = 1,
  comparatorId = 2,
  outcomeIds = c(3, 4),
  exposureDatabaseSchema = "main",
  outcomeDatabaseSchema = "main",
  exposureTable = "cohort",
  outcomeTable = "cohort",
  covariateSettings = covSettings
)

ps <- createPs(cohortMethodData = sCohortMethodData, population = studyPop)
studyPopStratisfied <- stratifyByPs(ps, 5)
studyPopMatched <- matchOnPs(population = ps)

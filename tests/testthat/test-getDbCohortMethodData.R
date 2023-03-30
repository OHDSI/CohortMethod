library(testthat)
library(CohortMethod)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
covarSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE)

cohortMethodData <- getDbCohortMethodData(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  exposureTable = "cohort",
  outcomeTable = "cohort",
  targetId = 1,
  comparatorId = 2,
  outcomeIds = c(3, 4),
  cdmVersion = "5",
  washoutPeriod = 183,
  firstExposureOnly = TRUE,
  removeDuplicateSubjects = TRUE,
  restrictToCommonPeriod = TRUE,
  maxCohortSize = 100000,
  covariateSettings = covarSettings
)

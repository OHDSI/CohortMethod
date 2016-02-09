library("testthat")
library("CohortMethod")

# options('fftempdir' = 's:/fftemp')

# These tests use a local database to test some of the logical of the data loading functionality. Ideally this would
# use a publicly available database.

connectionDetails <- createConnectionDetails(dbms = "sql server",
                                             server = "RNDUSRDHIT09")
conn <- tryCatch({
  connect(connectionDetails)
}, error = function(e) {
  "No connection"
})
hasConnection <- (class(conn) == "JDBCConnection")
dbDisconnect(conn)

cdmDatabaseSchema <- "CDM_TRUVEN_CCAE_6K.dbo"

test_that("Data loading functions", {
  if (hasConnection) {
    covariateSettings <- createCovariateSettings()

    cohortMethodData <- getDbCohortMethodData(connectionDetails,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              oracleTempSchema = NULL,
                                              targetId = 1118084,
                                              comparatorId = 1124300,
                                              indicationConceptIds = c(),
                                              washoutWindow = 183,
                                              indicationLookbackWindow = 183,
                                              studyStartDate = "20000101",
                                              studyEndDate = "20031230",
                                              outcomeIds = 192671,
                                              exposureTable = "drug_era",
                                              outcomeTable = "condition_era",
                                              excludeDrugsFromCovariates = TRUE,
                                              covariateSettings = covariateSettings,
                                              cdmVersion = "4")
    cohorts <- ff::as.ram(cohortMethodData$cohort)

    # Test if none of the observation period end dates exceeds the study end date (should truncate at study end date):
    opEndDatesPastStudyEnd <- sum(as.Date(cohorts$cohortStartDate) + cohorts$timeToObsPeriodEnd > as.Date("2003-12-30"))
    expect_equal(opEndDatesPastStudyEnd, 0)

    # Test if none of the cohort end dates exceeds the study end date (should truncate at study end date):
    cohortEndDatesPastStudyEnd <- sum(as.Date(cohorts$cohortStartDate) + cohorts$timeToCohortEnd > as.Date("2003-12-30"))
    expect_equal(cohortEndDatesPastStudyEnd, 0)

    # People with 0 days of observation time should have been removed:
    expect_equal(sum(cohorts$timeToObsPeriodEnd == 0), 0)

    # People with 0 days of exposure should have been removed:
    expect_equal(sum(cohorts$timeToCohortEnd == 0), 0)
  }
})

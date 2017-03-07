# library("testthat")
# library("CohortMethod")
#
# # options('fftempdir' = 's:/fftemp')
#
# # These tests use a local database to test some of the logic of the data loading functionality.
# # Ideally this would use a publicly available database.
#
# connectionDetails <- createConnectionDetails(dbms = "sql server", server = "RNDUSRDHIT09")
# conn <- tryCatch({
#   connect(connectionDetails)
# }, error = function(e) {
#   "No connection"
# })
# hasConnection <- (class(conn) == "JDBCConnection")
#
# if (hasConnection) {
#   dbDisconnect(conn)
# }
#
# cdmDatabaseSchema <- "CDM_TRUVEN_CCAE_6K.dbo"
#
# test_that("Data loading functions", {
#   if (hasConnection) {
#     covariateSettings <- createCovariateSettings()
#
#     cohortMethodData <- getDbCohortMethodData(connectionDetails,
#                                               cdmDatabaseSchema = cdmDatabaseSchema,
#                                               oracleTempSchema = NULL,
#                                               targetId = 1118084,
#                                               comparatorId = 1124300,
#                                               removeDuplicateSubjects = TRUE,
#                                               washoutPeriod = 183,
#                                               firstExposureOnly = TRUE,
#                                               studyStartDate = "20000101",
#                                               studyEndDate = "20031230",
#                                               outcomeIds = 192671,
#                                               exposureTable = "drug_era",
#                                               outcomeTable = "condition_era",
#                                               excludeDrugsFromCovariates = TRUE,
#                                               covariateSettings = covariateSettings,
#                                               cdmVersion = "4")
#     cohorts <- cohortMethodData$cohort
#
#     # Test if none of the observation period end dates exceeds the study end date (should truncate at
#     # study end date):
#     opEndDatesPastStudyEnd <- sum(as.Date(cohorts$cohortStartDate) + cohorts$daysToObsEnd - 1 > as.Date("2003-12-30"))
#     expect_equal(opEndDatesPastStudyEnd, 0)
#
#     # Test if none of the cohort end dates exceeds the study end date (should truncate at study end
#     # date):
#     cohortEndDatesPastStudyEnd <- sum(as.Date(cohorts$cohortStartDate) + cohorts$daysToCohortEnd - 1 > as.Date("2003-12-30"))
#     expect_equal(cohortEndDatesPastStudyEnd, 0)
#
#     # Test if everyone has washout period of observation:
#     expect_gte(min(cohorts$daysFromObsStart), 183)
#
#     # Test if no duplicates:
#     expect_equal(max(aggregate(rowId ~ subjectId, data = cohorts, length)$rowId), 1)
#   }
# })

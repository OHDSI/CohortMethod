# This is an example of the hdPS implementation. The code is very similar to that in SingleStudies.Rmd; it also includes a call
# to the function runHdps at the end that runs the hdPS implementation.
#
# Currently, only three "data dimensions" are implemented: ICD9 diagnosis codes, CPT4 procedure codes, and drug concept IDs. See
# runHdps documentation for ways to customize the hdPS implementation (inclusion / exclusion codes, selection of dimensions, setting
# various cutoff values, ...)

install.packages("devtools")
library(devtools)
install_github("ohdsi/OhdsiRTools")
install_github("ohdsi/SqlRender")
install_github("ohdsi/DatabaseConnector")
install_github("ohdsi/Cyclops")
install_github("ohdsi/PatientLevelPrediction", ref = "HDPS")
install_github("ohdsi/CohortMethod", ref = "hdps_clean")

library(CohortMethod)

# Preparing connection details
connectionDetails <- createConnectionDetails("YOUR CONNECTION DETAILS")
cdmDatabaseSchema <- "cdm4_sim"
resultsDatabaseSchema <- "my_results"
cdmVersion <- "4"


# Preparing exposures and outcomes
library(SqlRender)
sql <- readSql("inst/sql/sql_server/coxibVsNonselVsGiBleed.sql")
sql <- renderSql(sql,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 resultsDatabaseSchema = resultsDatabaseSchema)$sql
sql <- translateSql(sql, targetDialect = connectionDetails$dbms)$sql

connection <- connect(connectionDetails)
executeSql(connection, sql)


# View events per type
sql <- paste("SELECT cohort_concept_id, COUNT(*) AS count",
             "FROM @resultsDatabaseSchema.coxibVsNonselVsGiBleed",
             "GROUP BY cohort_concept_id")
sql <- renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
sql <- translateSql(sql, targetDialect = connectionDetails$dbms)$sql
querySql(connection, sql)


# Get all NSAID Concept IDs for exclusion:
sql <- paste("SELECT concept_id",
             "FROM @cdmDatabaseSchema.concept_ancestor",
             "INNER JOIN @cdmDatabaseSchema.concept",
             "ON descendant_concept_id = concept_id",
             "WHERE ancestor_concept_id = 21603933")
sql <- SqlRender::renderSql(sql, cdmDatabaseSchema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
nsaids <- querySql(connection, sql)
nsaids <- nsaids$CONCEPT_ID


covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                             useCovariateConditionOccurrence = TRUE,
                                             useCovariateConditionOccurrence365d = FALSE,
                                             useCovariateConditionOccurrence30d = FALSE,
                                             useCovariateConditionOccurrenceInpt180d = FALSE,
                                             useCovariate3DigitIcd9Inpatient180d = TRUE,
                                             useCovariate3DigitIcd9Inpatient180dMedF = TRUE,
                                             useCovariate3DigitIcd9Inpatient180d75F = TRUE,
                                             useCovariate3DigitIcd9Outpatient180d = TRUE,
                                             useCovariate3DigitIcd9Outpatient180dMedF = TRUE,
                                             useCovariate3DigitIcd9Outpatient180d75F = TRUE,
                                             useCovariateConditionEra = FALSE,
                                             useCovariateConditionEraEver = FALSE,
                                             useCovariateConditionEraOverlap = FALSE,
                                             useCovariateConditionGroup = FALSE,
                                             useCovariateDrugExposure = TRUE,
                                             useCovariateDrugExposure365d = FALSE,
                                             useCovariateDrugExposure30d = FALSE,
                                             useCovariateIngredientExposure180d = TRUE,
                                             useCovariateIngredientExposure180dMedF = TRUE,
                                             useCovariateIngredientExposure180d75F = TRUE,
                                             useCovariateDrugEra = FALSE,
                                             useCovariateDrugEra365d = FALSE,
                                             useCovariateDrugEra30d = FALSE,
                                             useCovariateDrugEraEver = FALSE,
                                             useCovariateDrugEraOverlap = FALSE,
                                             useCovariateDrugGroup = FALSE,
                                             useCovariateProcedureOccurrence = TRUE,
                                             useCovariateProcedureOccurrence365d = FALSE,
                                             useCovariateProcedureOccurrence30d = FALSE,
                                             useCovariateProcedureOccurrenceInpatient180d = TRUE,
                                             useCovariateProcedureOccurrenceInpatient180dMedF = TRUE,
                                             useCovariateProcedureOccurrenceInpatient180d75F = TRUE,
                                             useCovariateProcedureOccurrenceOutpatient180d = TRUE,
                                             useCovariateProcedureOccurrenceOutpatient180dMedF = TRUE,
                                             useCovariateProcedureOccurrenceOutpatient180d75F = TRUE,
                                             useCovariateProcedureGroup = FALSE,
                                             useCovariateObservation = FALSE,
                                             useCovariateObservation365d = FALSE,
                                             useCovariateObservation30d = FALSE,
                                             useCovariateObservationCount365d = FALSE,
                                             useCovariateMeasurement365d = FALSE,
                                             useCovariateMeasurement30d = FALSE,
                                             useCovariateMeasurementCount365d = FALSE,
                                             useCovariateMeasurementBelow = FALSE,
                                             useCovariateMeasurementAbove = FALSE,
                                             useCovariateConceptCounts = FALSE,
                                             useCovariateRiskScores = FALSE,
                                             useCovariateRiskScoresCharlson = FALSE,
                                             useCovariateRiskScoresDCSI = FALSE,
                                             useCovariateRiskScoresCHADS2 = FALSE,
                                             useCovariateInteractionYear = FALSE,
                                             useCovariateInteractionMonth = FALSE,
                                             deleteCovariatesSmallCount = 100)


# Load data:
cohortMethodData <- getDbCohortMethodData(connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          oracleTempSchema = resultsDatabaseSchema,
                                          targetId = 1,
                                          comparatorId = 2,
                                          indicationConceptIds = c(),
                                          washoutWindow = 183,
                                          indicationLookbackWindow = 183,
                                          studyStartDate = "",
                                          studyEndDate = "",
                                          exclusionConceptIds = nsaids,
                                          outcomeIds = 3,
                                          outcomeConditionTypeConceptIds = c(),
                                          exposureDatabaseSchema = resultsDatabaseSchema,
                                          exposureTable = "coxibVsNonselVsGiBleed",
                                          outcomeDatabaseSchema = resultsDatabaseSchema,
                                          outcomeTable = "coxibVsNonselVsGiBleed",
                                          excludeDrugsFromCovariates = FALSE,
                                          covariateSettings = covariateSettings,
                                          cdmVersion = cdmVersion)

# Save cohort method data:
saveCohortMethodData(cohortMethodData, "coxibVsNonselVsGiBleed")

# Load cohort method data:
cohortMethodData = loadCohortMethodData("coxibVsNonselVsGiBleed")

# HDPS implementation:
screenedCohortMethodData = runHdps(cohortData = cohortMethodData)
hdPs <- createPs(screenedCohortMethodData, outcomeId = 3,
                 prior = createPrior("none")) # turn-off regularization)
hdpsPropensityModel <- getPsModel(hdPs, screenedCohortMethodData)

# Example of selecting demographics to include
screenedCohortMethodData = runHdps(cohortData = cohortMethodData, demographicsAnalysisIds = c(3,5,6)) # excludes sex

# Example of selecting conceptIds to include/exclude if available
# For ICD9 codes, replace 'E' with '10' and 'V' with '11'
screenedCohortMethodData = runHdps(connectionDetails = connectionDetails, cohortData = cohortMethodData,
                                   predefinedIncludeConceptIds = c(2414398), predefinedIncludeICD9Dx = c(218)) # include covariateIds
screenedCohortMethodData = runHdps(connectionDetails = connectionDetails, cohortData = cohortMethodData,
                                   predefinedExcludeConceptIds = c(2414398), predefinedExcludeICD9Dx = c(218)) # exclude covariateIds


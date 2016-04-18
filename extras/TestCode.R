### Test code ###





















library(CohortMethod)
options(fftempdir = "s:/temp")

drugComparatorOutcomes1 <- createDrugComparatorOutcomes(targetId = 755695,
                                                        comparatorId = 739138,
                                                        outcomeIds = c(194133, 123))

drugComparatorOutcomesList <- list(drugComparatorOutcomes1)

saveDrugComparatorOutcomesList(drugComparatorOutcomesList,
                               "s:/temp/drugComparatorOutcomesList.json.txt")




covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                             useCovariateConditionOccurrence = TRUE,
                                             useCovariateConditionOccurrence365d = TRUE,
                                             useCovariateConditionOccurrence30d = TRUE,
                                             useCovariateConditionOccurrenceInpt180d = TRUE,
                                             useCovariateConditionEra = TRUE,
                                             useCovariateConditionEraEver = TRUE,
                                             useCovariateConditionEraOverlap = TRUE,
                                             useCovariateConditionGroup = TRUE,
                                             useCovariateDrugExposure = TRUE,
                                             useCovariateDrugExposure365d = TRUE,
                                             useCovariateDrugExposure30d = TRUE,
                                             useCovariateDrugEra = TRUE,
                                             useCovariateDrugEra365d = TRUE,
                                             useCovariateDrugEra30d = TRUE,
                                             useCovariateDrugEraEver = TRUE,
                                             useCovariateDrugEraOverlap = TRUE,
                                             useCovariateDrugGroup = TRUE,
                                             useCovariateProcedureOccurrence = TRUE,
                                             useCovariateProcedureOccurrence365d = TRUE,
                                             useCovariateProcedureOccurrence30d = TRUE,
                                             useCovariateProcedureGroup = TRUE,
                                             useCovariateObservation = TRUE,
                                             useCovariateObservation365d = TRUE,
                                             useCovariateObservation30d = TRUE,
                                             useCovariateObservationCount365d = TRUE,
                                             useCovariateMeasurement365d = TRUE,
                                             useCovariateMeasurement30d = TRUE,
                                             useCovariateMeasurementCount365d = TRUE,
                                             useCovariateMeasurementBelow = TRUE,
                                             useCovariateMeasurementAbove = TRUE,
                                             useCovariateConceptCounts = TRUE,
                                             useCovariateRiskScores = TRUE,
                                             useCovariateRiskScoresCharlson = TRUE,
                                             useCovariateRiskScoresDCSI = TRUE,
                                             useCovariateRiskScoresCHADS2 = TRUE,
                                             useCovariateInteractionYear = FALSE,
                                             useCovariateInteractionMonth = FALSE,
                                             deleteCovariatesSmallCount = 100)
getDbCmDataArgs <- createGetDbCohortMethodDataArgs(excludeDrugsFromCovariates = TRUE,
                                                   covariateSettings = covariateSettings)

createPsArgs <- createCreatePsArgs()  # Using only defaults
matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 1)
fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(riskWindowStart = 0,
                                                  riskWindowEnd = 365,
                                                  addExposureDaysToEnd = FALSE,
                                                  modelType = "cox",
                                                  stratifiedCox = TRUE,
                                                  useCovariates = TRUE)

cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)
fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(riskWindowStart = 0,
                                                  riskWindowEnd = 365,
                                                  addExposureDaysToEnd = FALSE,
                                                  modelType = "cox",
                                                  stratifiedCox = TRUE,
                                                  useCovariates = FALSE)


cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

cmAnalysisList <- list(cmAnalysis1, cmAnalysis2)

saveCmAnalysisList(cmAnalysisList, "s:/temp/cohortMethodAnalysisList.json.txt")

cmAnalysisList <- loadCmAnalysisList("s:/temp/cohortMethodAnalysisList.json.txt")
drugComparatorOutcomesList <- loadDrugComparatorOutcomesList("s:/temp/drugComparatorOutcomesList.json.txt")










cmAnalysisList <- loadCmAnalysisList("s:/temp/cohortMethodAnalysisList.json.txt")
drugComparatorOutcomesList <- loadDrugComparatorOutcomesList("s:/temp/drugComparatorOutcomesList.json.txt")


# Settings for running SQL against JnJ Sql Server:
pw <- NULL
dbms <- "sql server"
user <- NULL
server <- "RNDUSRDHIT09"
cdmDatabaseSchema <- "CDM_TRUVEN_CCAE_6k.dbo"
# cdmSchema <- 'CDM_Truven_MDCR'
port <- NULL

# Part one: loading the data:
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                schema = cdmDatabaseSchema,
                                                                port = port)


ref <- runCmAnalyses(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     outputFolder = "s:/temp/cmOutput",
                     cmAnalysisList = cmAnalysisList,
                     drugComparatorOutcomesList = drugComparatorOutcomesList,
                     underSampleComparatorToTreatedRatio = 1,
                     getDbCohortMethodDataThreads = 2,
                     createPsThreads = 1,
                     fitOutcomeModelThreads = 1)



#' @keywords internal
testCode <- function() {
  ### Test code ###
  library(CohortMethod)
  options(fftempdir = "s:/temp")

  drugComparatorOutcome1 <- createDrugComparatorOutcome(targetDrugConceptId = 755695,
                                                        comparatorDrugConceptId = 739138,
                                                        outcomeConceptId = 194133)

  drugComparatorOutcome2 <- createDrugComparatorOutcome(targetDrugConceptId = 755695,
                                                        comparatorDrugConceptId = 739138,
                                                        outcomeConceptId = 123)

  drugComparatorOutcomeList <- list(drugComparatorOutcome1, drugComparatorOutcome2)

  saveDrugComparatorOutcomeList(drugComparatorOutcomeList, "s:/temp/drugComparatorOutcomeList.json.txt")





  getDbCmDataArgs <- createGetDbCohortMethodDataArgs(excludeDrugsFromCovariates = TRUE,
                                                     useCovariateDemographics = TRUE,
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
                                                     useCovariateObservationBelow = TRUE,
                                                     useCovariateObservationAbove = TRUE,
                                                     useCovariateObservationCount365d = TRUE,
                                                     useCovariateConceptCounts = TRUE,
                                                     useCovariateRiskScores = TRUE,
                                                     useCovariateInteractionYear = FALSE,
                                                     useCovariateInteractionMonth = FALSE,
                                                     deleteCovariatesSmallCount = 100)

  createPsArgs <- createCreatePsArgs() # Using only defaults
  matchOnPsAndCovariatesArgs <- createMatchOnPsAndCovariatesArgs(maxRatio = 1, covariateIds = NULL)
  fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(riskWindowStart = 0,
                                                    riskWindowEnd = 365,
                                                    addExposureDaysToEnd = FALSE,
                                                    modelType = "cox",
                                                    stratifiedCox = TRUE,
                                                    useCovariates = TRUE)

  cmAnalysis1 <- createCohortMethodAnalysis(analysisId = 1,
                                            getDbCohortMethodDataArgs = getDbCmDataArgs,
                                            createPs = TRUE,
                                            createPsArgs = createPsArgs,
                                            matchOnPsAndCovariates = TRUE,
                                            matchOnPsAndCovariatesArgs = matchOnPsAndCovariatesArgs,
                                            fitOutcomeModel = TRUE,
                                            fitOutcomeModelArgs = fitOutcomeModelArgs1)
  fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(riskWindowStart = 0,
                                                    riskWindowEnd = 365,
                                                    addExposureDaysToEnd = FALSE,
                                                    modelType = "cox",
                                                    stratifiedCox = TRUE,
                                                    useCovariates = FALSE)


  cmAnalysis2 <- createCohortMethodAnalysis(analysisId = 2,
                                            getDbCohortMethodDataArgs = getDbCmDataArgs,
                                            createPs = TRUE,
                                            createPsArgs = createPsArgs,
                                            matchOnPsAndCovariates = TRUE,
                                            matchOnPsAndCovariatesArgs = matchOnPsAndCovariatesArgs,
                                            fitOutcomeModel = TRUE,
                                            fitOutcomeModelArgs = fitOutcomeModelArgs2)

  cohortMethodAnalysisList <- list(cmAnalysis1, cmAnalysis2)

  saveCohortMethodAnalysisList(cohortMethodAnalysisList, "s:/temp/cohortMethodAnalysisList.json.txt")



  cohortMethodAnalysisList <- loadCohortMethodAnalysisList("s:/temp/cohortMethodAnalysisList.json.txt")
  drugComparatorOutcomeList <- loadDrugComparatorOutcomeList("s:/temp/drugComparatorOutcomeList.json.txt")


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


  runCohortMethodAnalyses(connectionDetails =  connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outputFolder = "s:/temp/cmOutput",
                          cohortMethodAnalysisList = cohortMethodAnalysisList,
                          drugComparatorOutcomeList = drugComparatorOutcomeList, getDbCohortMethodDataThreads = 2, createPsThreads = 1, fitOutcomeModelThreads = 1)


  cohortMethodData <- getDbCohortMethodData(connectionDetails,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            targetDrugConceptId = 1,
                                            comparatorDrugConceptId = 2,
                                            indicationConceptIds = 439926,
                                            exclusionConceptIds = c(4027133,
                                                                    4032243,
                                                                    4146536,
                                                                    2002282,
                                                                    2213572,
                                                                    2005890,
                                                                    43534760,
                                                                    21601019),
                                            outcomeConceptIds = 194133,
                                            excludedCovariateConceptIds = c(4027133,
                                                                            4032243,
                                                                            4146536,
                                                                            2002282,
                                                                            2213572,
                                                                            2005890,
                                                                            43534760,
                                                                            21601019),
                                            useCovariateDemographics = TRUE,
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
                                            useCovariateObservationBelow = TRUE,
                                            useCovariateObservationAbove = TRUE,
                                            useCovariateObservationCount365d = TRUE,
                                            useCovariateConceptCounts = TRUE,
                                            useCovariateRiskScores = TRUE,
                                            useCovariateInteractionYear = FALSE,
                                            useCovariateInteractionMonth = FALSE,
                                            deleteCovariatesSmallCount = 0)

  saveCohortMethodData(cohortMethodData, "s:/temp/cohortMethodData")
  cohortMethodData <- loadCohortMethodData("s:/temp/cohortMethodData")



  summary(cohortMethodData)

  # Part two: Creating propensity scores, and match people on propensity score:
  ps <- createPs(cohortMethodData,
                 outcomeConceptId = 194133,
                 prior = createPrior("laplace", 1, exclude = c(0)))
  ps <- createPs(cohortMethodData, outcomeConceptId = 194133)

  computePsAuc(ps)
  # computePsAuc(ps2)

  # propensityModel <- getPsModel(ps,cohortMethodData)

  plotPs(ps)

  psTrimmed <- trimByPsToEquipoise(ps)

  plotPs(psTrimmed, ps)  #Plot trimmed PS distributions

  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)

  plotPs(strata, ps)  #Plot matched PS distributions

  balance <- computeCovariateBalance(strata, cohortMethodData, outcomeConceptId = 194133)

  plotCovariateBalanceScatterPlot(balance, fileName = "balanceScatterplot.png")

  plotCovariateBalanceOfTopVariables(balance, fileName = "balanceTopVarPlot.png")


  ####
  cohortMethodData <- loadCohortMethodData("cohortMethodData")
  ps <- createPs(cohortMethodData,
                 outcomeConceptId = 194133,
                 prior = createPrior("laplace", 0.1, exclude = c(0)))
  psTrimmed <- trimByPsToEquipoise(ps)
  strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized", maxRatio = 1)


  # Part three: Fit the outcome model:
  outcomeModel <- fitOutcomeModel(194133,
                                  cohortMethodData,
                                  strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 365,
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = TRUE,
                                  modelType = "cox",
                                  prior = createPrior("laplace", 0.1))

  outcomeModel <- fitOutcomeModel(194133,
                                  cohortMethodData,
                                  strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 365,
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = TRUE,
                                  modelType = "clr",
                                  prior = createPrior("laplace", 0.1))

  outcomeModel <- fitOutcomeModel(194133,
                                  cohortMethodData,
                                  strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 365,
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = TRUE,
                                  modelType = "pr",
                                  prior = createPrior("laplace", 0.1))

  outcomeModel <- fitOutcomeModel(194133,
                                  cohortMethodData,
                                  strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 365,
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = TRUE,
                                  modelType = "lr",
                                  prior = createPrior("laplace", 0.1))
  #
  outcomeModel <- fitOutcomeModel(194133,
                                  cohortMethodData,
                                  strata,
                                  riskWindowStart = 0,
                                  stratifiedCox = FALSE,
                                  riskWindowEnd = 365,
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = FALSE,
                                  modelType = "cox",
                                  prior = createPrior("laplace", 0.1))
  #
  outcomeModel <- fitOutcomeModel(194133,
                                  cohortMethodData,
                                  strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 365,
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = FALSE,
                                  modelType = "clr",
                                  prior = createPrior("laplace", 0.1))

  outcomeModel <- fitOutcomeModel(194133,
                                  cohortMethodData,
                                  strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 365,
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = FALSE,
                                  modelType = "pr",
                                  prior = createPrior("laplace", 0.1))

  outcomeModel <- fitOutcomeModel(194133,
                                  cohortMethodData,
                                  strata,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 365,
                                  addExposureDaysToEnd = FALSE,
                                  useCovariates = FALSE,
                                  modelType = "lr",
                                  prior = createPrior("laplace", 0.1))


  plotKaplanMeier(outcomeModel)

  # fullOutcomeModel <- getOutcomeModel(outcomeModel,cohortMethodData)

  summary(outcomeModel)

  coef(outcomeModel)

  confint(outcomeModel)

  drawAttritionDiagram(outcomeModel)
}

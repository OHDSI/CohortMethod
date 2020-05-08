# @file MultiAnalysesVignetteDataFetch.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#### Fetch data for multiple analyses vignette ####
library(CohortMethod)
library(SqlRender)
options(fftempdir = "s:/fftemp")

outputFolder <- "s:/temp/cohortMethodVignette2"

# PDW
pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "CDM_IBM_MDCD_V1105.dbo"
resultsDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
port <- 17001
cdmVersion <- "5"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Eunomia
cdmDatabaseSchema <- "main"
resultsDatabaseSchema <- "main"
cdmVersion <- "5"

connectionDetails <- Eunomia::getEunomiaConnectionDetails()

connection <- DatabaseConnector::connect(connectionDetails)

sql <- loadRenderTranslateSql("VignetteOutcomes.sql",
                              packageName = "CohortMethod",
                              dbms = connectionDetails$dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)


DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsDatabaseSchema.coxibVsNonselAlloutcomes GROUP BY cohort_definition_id"
sql <- SqlRender::render(sql, resultsDatabaseSchema = resultsDatabaseSchema)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

# Get all NSAIDs:
# sql <- "SELECT concept_id FROM @cdmDatabaseSchema.concept_ancestor INNER JOIN @cdmDatabaseSchema.concept ON descendant_concept_id = concept_id WHERE ancestor_concept_id = 21603933"
# sql <- SqlRender::renderSql(sql, cdmDatabaseSchema = cdmDatabaseSchema)$sql
# sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
# nsaids <- DatabaseConnector::querySql(connection, sql)
# nsaids <- nsaids$CONCEPT_ID

DatabaseConnector::disconnect(connection)

nsaids <- 21603933
outcomeIds <- c(192671, 24609, 29735, 73754, 80004, 134718, 139099, 141932, 192367, 193739, 194997, 197236, 199074, 255573, 257007, 313459, 314658, 316084, 319843, 321596, 374366, 375292, 380094, 433753, 433811, 436665, 436676, 436940, 437784, 438134, 440358, 440374, 443617, 443800, 4084966, 4288310)

diseaseRiskModelCoefficients <- list()
for (outcomeId in outcomeIds) {
  coefficients <- readRDS(file.path(outputFolder, sprintf("drBetas_o%s.rds", outcomeId)))
  diseaseRiskModelCoefficients[[as.character(outcomeId)]] <- coefficients
}


tcos <- createTargetComparatorOutcomes(targetId = 1,
                                       comparatorId = 2,
                                       diseaseRiskModelCoefficients = diseaseRiskModelCoefficients,
                                       outcomeIds = outcomeIds)
targetComparatorOutcomesList <- list(tcos)

covarSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                                addDescendantsToExclude = TRUE)

getDbCmDataArgs <- createGetDbCohortMethodDataArgs(washoutPeriod = 183,
                                                   restrictToCommonPeriod = FALSE,
                                                   firstExposureOnly = TRUE,
                                                   removeDuplicateSubjects = "remove all",
                                                   studyStartDate = "",
                                                   studyEndDate = "",
                                                   maxCohortSize = 100000,
                                                   excludeDrugsFromCovariates = FALSE,
                                                   covariateSettings = covarSettings)

createStudyPopArgs <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                      minDaysAtRisk = 1,
                                                      riskWindowStart = 0,
                                                      startAnchor = "cohort start",
                                                      riskWindowEnd = 30,
                                                      endAnchor = "cohort end")

fitOutcomeModelArgs1 <- createFitOutcomeModelArgs(modelType = "cox")

cmAnalysis1 <- createCmAnalysis(analysisId = 1,
                                description = "No adjustment, simple outcome model",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs1)

createPsArgs <- createCreatePsArgs(maxCohortSizeForFitting = 100000,
                                   control = createControl(cvType = "auto",
                                                           startingVariance = 0.01,
                                                           tolerance = 1E-5,
                                                           noiseLevel = "quiet",
                                                           cvRepetitions = 1))

matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)


fitOutcomeModelArgs2 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE)

cmAnalysis2 <- createCmAnalysis(analysisId = 2,
                                description = "PS matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

stratifyByPsArgs <- createStratifyByPsArgs(numberOfStrata = 5)

cmAnalysis3 <- createCmAnalysis(analysisId = 3,
                                description = "PS stratification",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                stratifyByPs = TRUE,
                                stratifyByPsArgs = stratifyByPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

fitOutcomeModelArgs3 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  inversePtWeighting = TRUE)

trimByPsArgs <- createTrimByPsArgs(trimFraction = 0.01)

cmAnalysis4 <- createCmAnalysis(analysisId = 4,
                                description = "IPTW",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                trimByPs = TRUE,
                                trimByPsArgs = trimByPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs3)

fitOutcomeModelArgs4 <- createFitOutcomeModelArgs(useCovariates = TRUE,
                                                  modelType = "cox",
                                                  stratified = TRUE,
                                                  control = createControl(cvType = "auto",
                                                                          startingVariance = 0.01,
                                                                          selectorType = "byPid",
                                                                          cvRepetitions = 1,
                                                                          noiseLevel = "quiet"))

cmAnalysis5 <- createCmAnalysis(analysisId = 5,
                                description = "PS matching plus full outcome model",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                matchOnPs = TRUE,
                                matchOnPsArgs = matchOnPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs4)

createDrsArgs <- createCreateDrsArgs(modelType = "logistic")

matchOnDrsArgs <- createMatchOnDrsArgs(maxRatio = 100)

createStudyPopArgs2 <- createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                       minDaysAtRisk = 1,
                                                       riskWindowStart = 1,
                                                       startAnchor = "cohort start",
                                                       riskWindowEnd = 30,
                                                       endAnchor = "cohort end")

cmAnalysis6 <- createCmAnalysis(analysisId = 6,
                                description = "DRS matching",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs2,
                                createDrs = TRUE,
                                createDrsArgs = createDrsArgs,
                                matchOnDrs = TRUE,
                                matchOnDrsArgs = matchOnDrsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

stratifyByDrsArgs <- createStratifyByDrsArgs(numberOfStrata = 5)

cmAnalysis7 <- createCmAnalysis(analysisId = 7,
                                description = "DRS stratification",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs2,
                                createDrs = TRUE,
                                createDrsArgs = createDrsArgs,
                                stratifyByDrs = TRUE,
                                stratifyByDrsArgs = stratifyByDrsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs2)

interactionCovariateIds <- c(8532001, 201826210, 21600960413) # Female, T2DM, concurent use of antithrombotic agents

fitOutcomeModelArgs5 <- createFitOutcomeModelArgs(modelType = "cox",
                                                  stratified = TRUE,
                                                  interactionCovariateIds = interactionCovariateIds,
                                                  control = createControl(threads = 6))
cmAnalysis8 <- createCmAnalysis(analysisId = 8,
                                description = "PS stratification plus interaction terms",
                                getDbCohortMethodDataArgs = getDbCmDataArgs,
                                createStudyPopArgs = createStudyPopArgs,
                                createPs = TRUE,
                                createPsArgs = createPsArgs,
                                stratifyByPs = TRUE,
                                stratifyByPsArgs = stratifyByPsArgs,
                                fitOutcomeModel = TRUE,
                                fitOutcomeModelArgs = fitOutcomeModelArgs5)


cmAnalysisList <- list(cmAnalysis1, cmAnalysis2, cmAnalysis3, cmAnalysis4, cmAnalysis5, cmAnalysis6, cmAnalysis7, cmAnalysis8)

saveCmAnalysisList(cmAnalysisList, file.path(outputFolder, "cmAnalysisList.json"))
saveTargetComparatorOutcomesList(targetComparatorOutcomesList, file.path(outputFolder, "targetComparatorOutcomesList.json"))

# cmAnalysisList <- loadCmAnalysisList(file.path(outputFolder, "cmAnalysisList.json"))

# targetComparatorOutcomesList <- loadTargetComparatorOutcomesList(file.path(outputFolder, "targetComparatorOutcomesList.json"))

ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
result <- runCmAnalyses(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        exposureDatabaseSchema = resultsDatabaseSchema,
                        exposureTable = "coxibVsNonselVsGiBleed",
                        outcomeDatabaseSchema = resultsDatabaseSchema,
                        outcomeTable = "coxibVsNonselAlloutcomes",
                        outputFolder = outputFolder,
                        cdmVersion = cdmVersion,
                        cmAnalysisList = cmAnalysisList,
                        targetComparatorOutcomesList = targetComparatorOutcomesList,
                        getDbCohortMethodDataThreads = 1,
                        createPsThreads = 1,
                        psCvThreads = 16,
                        createDrsThreads = 5,
                        createStudyPopThreads = 5,
                        trimMatchStratifyThreads = 5,
                        prefilterCovariatesThreads = 3,
                        fitOutcomeModelThreads = 5,
                        outcomeCvThreads = 10,
                        outcomeIdsOfInterest = c(192671))
# result <- readRDS(file.path(outputFolder, "outcomeModelReference.rds"))

analysisSummary <- summarizeAnalyses(result, outputFolder = outputFolder)

saveRDS(analysisSummary, file.path(outputFolder, "analysisSummary.rds"))


# cleanup:
sql <- "DROP TABLE @resultsDatabaseSchema.outcomes"
sql <- SqlRender::render(sql, resultsDatabaseSchema = resultsDatabaseSchema)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
connection <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::executeSql(connection, sql)
DatabaseConnector::disconnect(connection)

# analysisSummary <- readRDS(file.path(outputFolder, "analysisSummary.rds"))

library(EmpiricalCalibration)
for (cmAnalysis in cmAnalysisList) {
  negControls <- analysisSummary[analysisSummary$analysisId == cmAnalysis$analysisId & analysisSummary$outcomeId != 192671, ]
  # negControls <- negControls[negControls$outcomeId %in% goodOutcomeIds, ]
  hoi <- analysisSummary[analysisSummary$analysisId == cmAnalysis$analysisId & analysisSummary$outcomeId == 192671, ]
  fileName <- file.path(outputFolder, sprintf("CaliPlot_%s.png", cmAnalysis$analysisId))
  plotCalibrationEffect(negControls$logRr,
                        negControls$seLogRr,
                        hoi$logRr,
                        hoi$seLogRr,
                        showCis = TRUE,
                        title = cmAnalysis$description,
                        fileName = fileName)
}

# Z vs AUC plot
cmAnalysis <- cmAnalysisList[[7]]
negControls <- analysisSummary[analysisSummary$analysisId == cmAnalysis$analysisId & analysisSummary$outcomeId != 192671, ]
omr <- readRDS(file.path(outputFolder, "outcomeModelReference.rds"))


computeAuc <- function(row) {
  drs <- readRDS(file.path(outputFolder, omr$drsFile[omr$outcomeId == row$outcomeId & omr$analysisId == row$analysisId]))
  row$zScore <- abs(row$logRr / row$seLogRr)
  row$auc <- computeDrsAuc(drs)
  return(row)
}
negControls <- negControls[!is.na(negControls$seLogRr), ]
negControls <- purrr::map_df(split(negControls, 1:nrow(negControls)), computeAuc)
library(ggplot2)
ggplot(negControls, aes(x = auc, y = zScore)) +
  geom_point()

ggsave(file.path(outputFolder, "zScoreVsAuc.png"))

# AUC vs delta (compared to PS matching)
ncsPs <- analysisSummary[analysisSummary$analysisId == 3 & analysisSummary$outcomeId != 192671, ]
m <- merge(negControls,
           data.frame(outcomeId = ncsPs$outcomeId,
                      logRrPs = ncsPs$logRr,
                      pPs = ncsPs$p))
View(m)
m$delta <- abs(m$logRr - m$logRrPs)
ggplot(m, aes(x = auc, y = delta)) +
  geom_point()

ggsave(file.path(outputFolder, "DeltaVsAuc.png"))

goodNcs <- negControls[negControls$auc > 0.6, ]
plotCalibrationEffect(goodNcs$logRr,
                      goodNcs$seLogRr,
                      showCis = TRUE,
                      title = "DRS stratification, showing only AUC > 0.6",
                      fileName = file.path(outputFolder, "AucAbove06.png"))

goodNcs <- negControls[negControls$auc > 0.7, ]
plotCalibrationEffect(goodNcs$logRr,
                      goodNcs$seLogRr,
                      showCis = TRUE,
                      title = "DRS stratification, showing only AUC > 0.7",
                      fileName = file.path(outputFolder, "AucAbove07.png"))

goodNcs <- negControls[negControls$auc > 0.8, ]
plotCalibrationEffect(goodNcs$logRr,
                      goodNcs$seLogRr,
                      showCis = TRUE,
                      title = "DRS stratification, showing only AUC > 0.8",
                      fileName = file.path(outputFolder, "AucAbove08.png"))



# Disease risk scores ------------------------------------------------------------------------------------
nsaids <- 21603933
outcomeIds <- c(192671, 24609, 29735, 73754, 80004, 134718, 139099, 141932, 192367, 193739, 194997, 197236, 199074, 255573, 257007, 313459, 314658, 316084, 319843, 321596, 374366, 375292, 380094, 433753, 433811, 436665, 436676, 436940, 437784, 438134, 440358, 440374, 443617, 443800, 4084966, 4288310)

covarSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = nsaids,
                                                addDescendantsToExclude = TRUE)

plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              cdmVersion = cdmVersion,
                                              oracleTempSchema = oracleTempSchema,
                                              cohortId = 5,
                                              outcomeIds = outcomeIds,
                                              cohortDatabaseSchema = resultsDatabaseSchema,
                                              cohortTable = "coxibVsNonselVsGiBleed",
                                              outcomeDatabaseSchema = resultsDatabaseSchema,
                                              outcomeTable = "coxibVsNonselAlloutcomes",
                                              washoutPeriod = 180,
                                              sampleSize = 250000,
                                              covariateSettings = covarSettings)

PatientLevelPrediction::savePlpData(plpData, file.path(outputFolder, "plpData"))

# plpData <- PatientLevelPrediction::loadPlpData(file.path(outputFolder, "plpData"))
# outcomeCounts <- summary(plpData)$outcomeCounts
fitDiseaseRiskModel <- function(outcomeId, outputFolder) {
  fileName <- file.path(outputFolder, sprintf("drBetas_o%s.rds", outcomeId))
  if (!file.exists(fileName)) {
    writeLines(paste("Fitting model for outcome ID", outcomeId))
    plpData <- PatientLevelPrediction::loadPlpData(file.path(outputFolder, "plpData"))
    plpStudyPop <- PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                                 outcomeId = outcomeId,
                                                                 includeAllOutcomes = TRUE,
                                                                 removeSubjectsWithPriorOutcome = TRUE,
                                                                 minTimeAtRisk = 1,
                                                                 riskWindowStart = 1,
                                                                 riskWindowEnd = 30)
    if (sum(plpStudyPop$outcomeCount > 0) == 0) {
      # No outcomes
      betas <- c(0)
    } else {
      model <- PatientLevelPrediction::fitGLMModel(population = plpStudyPop,
                                                   plpData = plpData,
                                                   modelType = "logistic",
                                                   prior = Cyclops::createPrior(priorType = "normal",
                                                                                useCrossValidation = TRUE),
                                                   control = Cyclops::createControl(cvType = "auto",
                                                                                    fold = 3,
                                                                                    startingVariance = 0.01,
                                                                                    tolerance  = 2e-06,
                                                                                    cvRepetitions = 1,
                                                                                    selectorType = "auto",
                                                                                    noiseLevel = "silent",
                                                                                    threads = 3,
                                                                                    maxIterations = 3000))

      betas <- model$coefficients
      betas <- betas[betas != 0]
    }
    saveRDS(betas, fileName)
  }
}
cluster <- ParallelLogger::makeCluster(3)
ParallelLogger::clusterApply(cluster, outcomeIds, fitDiseaseRiskModel, outputFolder = outputFolder)
ParallelLogger::stopCluster(cluster)

# Debug
omr <- readRDS(file.path(outputFolder, "outcomeModelReference.rds"))

drs <- readRDS(file.path(outputFolder, omr$drsFile[omr$outcomeId == 319843 & omr$analysisId == 7]))
plotDrs(drs, fileName = file.path(outputFolder, "drs.png"))
computeDrsAuc(drs)
sum(drs$outcomeCount > 0)
coefficients <- attr(drs, "metaData")$drsModelCoef



strata <- stratifyByDrs(drs)
plotDrs(strata)

cohortMethodData <- loadCohortMethodData(file.path(outputFolder, omr$cohortMethodDataFolder[omr$outcomeId == 433753 & omr$analysisId == 7]))
m <- getDrsModel(drs, cohortMethodData)
View(m)





ps <- readRDS(file.path(outputFolder, omr$sharedPsFile[omr$sharedPsFile != ""][1]))
plotPs(ps)
psM <- getPsModel(ps, cohortMethodData)
View(psM)



ncs7 <- analysisSummary[analysisSummary$analysisId == 7 & analysisSummary$outcomeId != 192671, ]
ncs2 <- analysisSummary[analysisSummary$analysisId == 2 & analysisSummary$outcomeId != 192671, ]
m <- merge(data.frame(outcomeId = ncs2$outcomeId, logRr2 = ncs2$logRr, seLogRr = ncs2$seLogRr),
           data.frame(outcomeId = ncs7$outcomeId, logRr7 = ncs7$logRr))

m$delta <- abs(m$logRr2 - m$logRr7)

# Compare covariates in cases vs non cases:
omr <- readRDS(file.path(outputFolder, "outcomeModelReference.rds"))
cohortMethodData <- loadCohortMethodData(file.path(outputFolder, omr$cohortMethodDataFolder[omr$outcomeId == 319843 & omr$analysisId == 7]))

drs <- readRDS(file.path(outputFolder, omr$drsFile[omr$analysisId == 7 & omr$outcomeId ==  319843]))
drsModel <- getDrsModel(drs, cohortMethodData)

ps <- readRDS(file.path(outputFolder, omr$sharedPsFile[omr$analysisId == 3 & omr$outcomeId ==  319843]))
psModel <- getPsModel(ps, cohortMethodData)

ps <- merge(ps, drs[, c("rowId", "outcomeCount", "survivalTime")])
drs <- stratifyByDrs(drs, numberOfStrata = 5)
ps <- stratifyByPs(ps, numberOfStrata = 5)

balExposureDrs <- computeCovariateBalance(drs, cohortMethodData)
balExposurePs <- computeCovariateBalance(ps, cohortMethodData)
saveRDS(balExposureDrs, file.path(outputFolder, "balExposureDrs.rds"))
saveRDS(balExposurePs, file.path(outputFolder, "balOutcomePs.rds"))

drs$treatment <- drs$outcomeCount > 0
ps$treatment <- ps$outcomeCount > 0
balOutcomeDrs <- computeCovariateBalance(drs, cohortMethodData)
balOutcomePs <- computeCovariateBalance(ps, cohortMethodData)
saveRDS(balOutcomeDrs, file.path(outputFolder, "balOutcomeDrs.rds"))
saveRDS(balOutcomePs, file.path(outputFolder, "balOutcomePs.rds"))



vizData <- data.frame(covariateId = balOutcomeDrs$covariateId,
                      covariateName = balOutcomeDrs$covariateName,
                      meanCases = balOutcomeDrs$beforeMatchingMeanTarget,
                      meanNonCases = balOutcomeDrs$beforeMatchingMeanComparator)

vizDataDrs <- merge(vizData,
                    data.frame(covariateId = balExposureDrs$covariateId,
                               stdDiff = balExposureDrs$afterMatchingStdDiff,
                               strategy = "DRS"))
vizDataDrs <- merge(vizDataDrs,
                    data.frame(covariateId = drsModel$covariateId,
                               coefficient = drsModel$coefficient),
                    all.x = TRUE)

vizDataPs <- merge(vizData,
                    data.frame(covariateId = balExposurePs$covariateId,
                               stdDiff = balExposurePs$afterMatchingStdDiff,
                               strategy = "PS"))

vizDataPs <- merge(vizDataPs,
                    data.frame(covariateId = psModel$covariateId,
                               coefficient = psModel$coefficient),
                   all.x = TRUE)

vizData <- rbind(vizDataDrs, vizDataPs)
vizData$absStdDiff <- abs(vizData$stdDiff)
vizData$coefficient[is.na(vizData$coefficient)] <- 0
vizData$coefficient <- abs(vizData$coefficient)
vizData$label <- gsub("^.*: ", "", vizData$covariateName)
labelSubset <- vizData[vizData$covariateId %in% c(77074210, 80180210) & vizData$strategy == "DRS", ]

library(ggplot2)
max(vizData$meanCases, na.rm = TRUE)
max(vizData$meanNonCases, na.rm = TRUE)

plot <- ggplot(vizData, aes(x = meanNonCases, y = meanCases)) +
  ggplot2::geom_point(aes(color = absStdDiff, size = coefficient), alpha = 0.3, shape = 16) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_vline(xintercept = 0) +
  ggplot2::geom_text(aes(label = label), size = 2, vjust = 0, data = labelSubset) +
  ggplot2::ggtitle("Negative control outcome: Mitral valve disorder") +
  ggplot2::scale_x_continuous("Mean non-cases", limits = c(0,1)) +
  ggplot2::scale_y_continuous("Mean cases", limits = c(0,1)) +
  ggplot2::scale_size("Absolute\ncoefficient") +
  ggplot2::scale_color_gradient("Absolute\nStd. Diff.", low = "blue", high = "red", space = "Lab", na.value = "red") +
  ggplot2::facet_grid(~strategy)

ggsave(plot = plot, file.path(outputFolder, "casesVsNonCases.png"), width = 8, height = 4)
plotly::ggplotly(plot)

vizData[vizData$coefficient > 3.5, ]
# covariateId                                                                    covariateName meanCases meanNonCases    stdDiff strategy coefficient absStdDiff
# 1071    77074210 condition_era group during day -365 through 0 days relative to index: Joint pain 0.7977973       0.7797 0.07124944      DRS    3.552831 0.07124944

vizData[!is.na(vizData$absStdDiff) & vizData$absStdDiff > 0.37, ]
# covariateId                                                                                                         covariateName meanCases meanNonCases
# 1515     80180210                                  condition_era group during day -365 through 0 days relative to index: Osteoarthritis 0.5754619      0.38103
# 45574  4178680210 condition_era group during day -365 through 0 days relative to index: Degenerative disorder of musculoskeletal system 0.5756750      0.38128
# stdDiff strategy coefficient absStdDiff
# 1515  0.3719572      DRS           0  0.3719572
# 45574 0.3718537      DRS           0  0.3718537

# Balance in population where model was fitted
plpData <- PatientLevelPrediction::loadPlpData(file.path(outputFolder, "plpData"))
plpStudyPop <- PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                             outcomeId = 319843,
                                                             includeAllOutcomes = TRUE,
                                                             removeSubjectsWithPriorOutcome = TRUE,
                                                             minTimeAtRisk = 1,
                                                             riskWindowStart = 1,
                                                             riskWindowEnd = 30)
cases <- plpStudyPop[plpStudyPop$outcomeCount > 0, ]
covarsCases <- list(covariates = plpData$covariates[ffbase::`%in%`(plpData$covariates$rowId, cases$rowId), ],
                    covariateRef = plpData$covariateRef,
                    analysisRef  = plpData$analysisRef,
                    metaData = list(populationSize = nrow(cases)))
class(covarsCases) <- "covariateData"
covarsCases <- FeatureExtraction::aggregateCovariates(covarsCases)
covarsCases$covariates <- ff::as.ram(covarsCases$covariates)
covarsCases$covariateRef <- ff::as.ram(covarsCases$covariateRef)

nonCases <- plpStudyPop[plpStudyPop$outcomeCount == 0, ]
covarsNonCases <- list(covariates = plpData$covariates[ffbase::`%in%`(plpData$covariates$rowId, nonCases$rowId), ],
                    covariateRef = plpData$covariateRef,
                    analysisRef  = plpData$analysisRef,
                    metaData = list(populationSize = nrow(nonCases)))
class(covarsNonCases) <- "covariateData"
covarsNonCases <- FeatureExtraction::aggregateCovariates(covarsNonCases)
covarsNonCases$covariates <- ff::as.ram(covarsNonCases$covariates)

m <- merge(data.frame(covariateId = covarsCases$covariates$covariateId,
                      meanCases = covarsCases$covariates$averageValue),
           data.frame(covariateId = covarsNonCases$covariates$covariateId,
                      meanNonCases = covarsNonCases$covariates$averageValue))

m <- merge(m,
           data.frame(covariateId = covarsCases$covariateRef$covariateId,
                      covariateName = covarsCases$covariateRef$covariateName))


m[m$covariateId == 80180210, ]


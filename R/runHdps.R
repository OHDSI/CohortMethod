#' Runs hdps implementation
#'
#' @description
#' This function runs the hdps implementation on \code{cohortData}.
#'
#' @details
#' Currently implemented for ICD9 diagnosis codes, CPT4 procedure codes, and drug concept ID codes. The mapping from CDM concept Id to
#' source codes is obtained, and used to relabel the covariates and covariateRef components of cohortData. Granularity for ICD9 codes
#' is implemented by "truncating" the source codes to 3 digits. Codes with fewer than \code{lowPopCutoff} number of patients are dropped.
#' For each covariate, three covariates are constructed: "once", "sporadic", "frequent" for any occurrences, occurrences greater than
#' the median, and occurrences greater than 3rd quartile, respectively. For each covariate, the "bias" as specified in hdps model is
#' calculated, and the highest \code{biasCutoff} codes across all data dimensions are selected for inclusion. Demographic information of
#' age, sex, race, and year are also included. \code{cohortData$covariates} and \code{cohortData$covariateRef} are altered accordingly.
#'
#' @param connectionDetails An R object of type connectionDetails created using the function createConnectionDetails in the DatabaseConnector package.
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortMethodData}
#' @param useConditionICD9 Boolean to include ICD9 diagnosis codes
#' @param useDrug Boolean to include drugs
#' @param useProcedureCPT4 Boolean to include CPT4 procedure codes
#' @param demographicsAnalysisIds Vector of analysis identifiers that correspond to demographics covariates
#' @param predefinedIncludeConceptIds Vector of predefined concept IDs to include as covariates
#' @param predefinedExcludeConceptIds Vector of predefined concept IDs to exclude as covariates
#' @param lowPopCutoff Cutoff for discarding rare covariates
#' @param dimensionCutoff Number of most prevalent covariates to include from each data dimension
#' @param rankCutoff Number of covariates to include in propensity score model (minus the demographics covariates)
#' @param dropRareAfterExpand Boolean for dropping codes below lowPopCutoff after expanding covariates
#' @param fudge Constant to avoid 0/Inf in calculating RR. Setting equal to 0 will cause those covariates to be discarded
#' @param useExpRank Use exposure rank instead of bias rank
#'
#' @return
#' Returns the input \code{cohortData} with new entries for \code{covariates} and \code{covariateRef}
#' @export
runHdps <- function(
                    #connectionDetails,
                    cohortData,
                    useInpatientDiagnosis = TRUE,
                    useAmbulatoryDiagnosis = TRUE,
                    useDrugIngredient = TRUE,
                    useInpatientProcedure = TRUE,
                    useAmbulatoryProcedure = TRUE,
                    demographicsAnalysisIds = c(2,3,5,6),
                    predefinedIncludeICD9Dx = c(),
                    predefinedIncludeConceptIds = c(),
                    predefinedExcludeICD9Dx = c(),
                    predefinedExcludeConceptIds = c(),
                    icd9AnalysisIds = c(104, 105, 106, 107, 108, 109),
                    #lowPopCutoff = 100,
                    dimensionCutoff = 200,
                    rankCutoff = 500,
                    #dropRareAfterExpand = FALSE,
                    useExpRank = FALSE,
                    fudge = 0) {
  #connection <- connect(connectionDetails)
  dimensions = c()
  if (useInpatientDiagnosis == TRUE) {
    dimensions = c(dimensions, "inpatientDiagnosis")
  }
  if (useAmbulatoryDiagnosis == TRUE) {
    dimensions = c(dimensions, "ambulatoryDiagnosis")
  }
  if (useDrugIngredient == TRUE) {
    dimensions = c(dimensions, "drugIngredient")
  }
  if (useInpatientProcedure == TRUE) {
    dimensions = c(dimensions, "inpatientProcedure")
  }
  if (useAmbulatoryProcedure == TRUE) {
    dimensions = c(dimensions, "ambulatoryProcedure")
  }
#   if (useConditionICD9 == TRUE) {
#     dimensions = c(dimensions, "conditionICD9")
#   }
#   if (useDrug == TRUE) {
#     dimensions = c(dimensions, "drug")
#   }
#   if (useProcedureCPT4 == TRUE) {
#     dimensions = c(dimensions, "procedureCPT4")
#   }

  demographicsCovariateRef = getDemographicsCovariateRef(cohortData, demographicsAnalysisIds)
  demographicsCovariates = getDemographicsCovariates(cohortData, demographicsCovariateRef)
  predefinedCovariateRef = getPredefinedCovariateRef(cohortData, predefinedIncludeConceptIds, predefinedIncludeICD9Dx, icd9AnalysisIds)
  predefinedCovariates = getPredefinedCovariates(cohortData, predefinedCovariateRef)

  dimensionTable = getDimensionTable(dimensions)

  if (is.null(dimensionTable)) {
    newCovariates = combineFunction(list(demographicsCovariates, predefinedCovariates), ffbase::ffdfrbind.fill)
    newCovariateRef = combineFunction(list(demographicsCovariateRef, predefinedCovariateRef), ffbase::ffdfrbind.fill)
    cohortData$covariates = newCovariates
    cohortData$covariateRef = newCovariateRef
    return(cohortData)
  } else {
    #dimAnalysisId = sapply(dimensionTable, getDimensionAnalysisId)
    #dimSql = sapply(dimensionTable, getDimensionSql)
    #dimConceptId = sapply(dimAnalysisId, getConceptId, cohortData)
    #dimCovariateId = sapply(dimAnalysisId, getCovariateId, cohortData)

    newCovariates = removePredefinedCovariates(cohortData, c(predefinedIncludeConceptIds, predefinedExcludeConceptIds),
                                               c(predefinedIncludeICD9Dx, predefinedExcludeICD9Dx), icd9AnalysisIds)
    dimAnalysisId = sapply(dimensionTable, getDimensionAnalysisId)
    newData = sapply(dimAnalysisId, separateCovariates, newCovariates)
    totalPopulation = length(cohortData$cohorts$rowId)
    newData = sapply(newData, removeRareCovariates, dimensionCutoff, totalPopulation)
    newData = sapply(newData, addTreatmentAndOutcome, cohortData)
    rankings = sapply(newData, calculateRanks, cohortData, fudge)
    newData = removeLowRank(newData, rankings, rankCutoff, useExpRank)
    newCovariates = combineFunction(newData, ffbase::ffdfrbind.fill)
    newCovariates$treatment <- NULL
    newCovariates$outcome <- NULL
    newCovariateRef = getNewCovariateRef(newCovariates, cohortData)
    newCovariates = combineFunction(list(newCovariates, demographicsCovariates, predefinedCovariates), ffbase::ffdfrbind.fill)
    newCovariateRef = combineFunction(list(newCovariateRef, demographicsCovariateRef, predefinedCovariateRef), ffbase::ffdfrbind.fill)

    #rawCodes = sqlMapply(connection, dimSql, list("listIds"), list(dimConceptId))
    #rawCodes = sapply(rawCodes, covariateIdToFactor)
    #rawCodes = sapply(rawCodes, deletePredefinedCodes, predefinedIncludeConceptIds)
    #rawCodes = sapply(rawCodes, deletePredefinedCodes, predefinedExcludeConceptIds)
    #truncatedCodes = mapply(truncateRawCodes, rawCodes, dimensionTable)
    #uniqueCodes = sapply(truncatedCodes, deleteRepeatCodes)
    #uniqueCodes = mapply(appendAnalysisId, uniqueCodes, dimAnalysisId)

    #newData = combineData(cohortData, uniqueCodes, dimCovariateId)
    #newData = sapply(newData, removeRareCodes, lowPopCutoff, dimensionCutoff)
    #newData = sapply(newData, expandCovariates)
    #if (dropRareAfterExpand) {newData = sapply(newData, removeRareCodes, lowPopCutoff)}
    #rankings = sapply(newData, calculateRanks, cohortData, fudge)
    #newData = removeLowRank(newData, rankings, rankCutoff, useExpRank)

    #newCovariates = combineFunction(newData, ffbase::ffdfrbind.fill)
    #newCovariates = combineWithOtherCovariates(newCovariates, demographicsCovariates, predefinedCovariates)
    #covariateIdIndex = createCovariateIndex(newCovariates)
    #newCovariates = convertCovariateId(newCovariates, covariateIdIndex)

    #newCovariateRefList = mapply(createNewCovRef, newData, dimensionTable, rawCodes, list(covariateIdIndex), list(cohortData))
    #newCovariateRef = combineFunction(newCovariateRefList, ffbase::ffdfrbind.fill)
    #newCovariateRef = combineWithOtherCovariateRef(newCovariateRef, demographicsCovariateRef, predefinedCovariateRef, covariateIdIndex)

    cohortData$covariates = newCovariates
    cohortData$covariateRef = newCovariateRef
    return(cohortData)
  }
}

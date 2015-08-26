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
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
#' @param connectionDetails An R object of type connectionDetails created using the function createConnectionDetails in the DatabaseConnector package.
#' @param useConditionICD9 Boolean to include ICD9 diagnosis codes
#' @param useDrug Boolean to include drugs
#' @param useProcedureCPT4 Boolean to include CPT4 procedure codes
#' @param demographicsAnalysisIds Vector of analysis identifiers that correspond to demographics covariates
#' @param predefinedIncludeConceptIds Vector of predefined concept IDs to include as covariates
#' @param predefinedExcludeConceptIds Vector of predefined concept IDs to exclude as covariates
#' @param lowPopCutoff Cutoff for discarding rare covariates
#' @param dimensionCutoff Number of most prevalent covariates to include from each data dimension
#' @param biasCutoff Number of covariates to include in propensity score model (minus the demographics covariates)
#' @param dropRareAfterExpand Boolean for dropping codes below lowPopCutoff after expanding covariates
#' @param includeRR0 Boolean for including covariates with large bias due to zero RR
#' @param RR0Constant Large constant to use for handling zero RR covariates
#'
#' @return
#' Returns the input \code{cohortData} with new entries for \code{covariates} and \code{covariateRef}
#' @export
runHdps <- function(cohortData, connectionDetails,
                    useConditionICD9 = TRUE,
                    useDrug = TRUE,
                    useProcedureCPT4 = TRUE,
                    demographicsAnalysisIds = c(2,3,5,6),
                    predefinedIncludeConceptIds = c(),
                    predefinedExcludeConceptIds = c(),
                    lowPopCutoff = 100,
                    dimensionCutoff = 200,
                    biasCutoff = 500,
                    dropRareAfterExpand = FALSE,
                    includeRR0 = TRUE,
                    RR0Constant = 99999) {
  connection <- connect(connectionDetails)
  dimensions = c()
  if (useConditionICD9 == TRUE) {
    dimensions = c(dimensions, "conditionICD9")
  }
  if (useDrug == TRUE) {
    dimensions = c(dimensions, "drug")
  }
  if (useProcedureCPT4 == TRUE) {
    dimensions = c(dimensions, "procedureCPT4")
  }

  demographicsCovariateRef = getDemographicsCovariateRef(cohortData, demographicsAnalysisIds)
  demographicsCovariates = getDemographicsCovariates(cohortData, demographicsCovariateRef)
  predefinedCovariateRef = getPredefinedCovariateRef(cohortData, predefinedIncludeConceptIds)
  predefinedCovariates = getPredefinedCovariates(cohortData, predefinedCovariateRef)

  dimensionTable = getDimensionTable(dimensions)

  if (is.null(dimensionTable)) {
    newCovariates = combineFunction(list(demographicsCovariates, predefinedCovariates), ffbase::ffdfrbind.fill)
    newCovariateRef = combineFunction(list(demographicsCovariateRef, predefinedCovariateRef), ffbase::ffdfrbind.fill)
    cohortData$covariates = newCovariates
    cohortData$covariateRef = newCovariateRef
    return(cohortData)
  } else {
    dimAnalysisId = sapply(dimensionTable, getDimensionAnalysisId)
    dimSql = sapply(dimensionTable, getDimensionSql)
    dimConceptId = sapply(dimAnalysisId, getConceptId, cohortData)
    dimCovariateId = sapply(dimAnalysisId, getCovariateId, cohortData)

    rawCodes = sqlMapply(connection, dimSql, list("listIds"), list(dimConceptId))
    rawCodes = sapply(rawCodes, covariateIdToFactor)
    rawCodes = sapply(rawCodes, deletePredefinedCodes, predefinedIncludeConceptIds)
    rawCodes = sapply(rawCodes, deletePredefinedCodes, predefinedExcludeConceptIds)
    truncatedCodes = mapply(truncateRawCodes, rawCodes, dimensionTable)
    uniqueCodes = sapply(truncatedCodes, deleteRepeatCodes)

    newData = combineData(cohortData, uniqueCodes, dimCovariateId)
    newData = sapply(newData, removeRareCodes, lowPopCutoff, dimensionCutoff)
    newData = sapply(newData, expandCovariates)
    if (dropRareAfterExpand) {newData = sapply(newData, removeRareCodes, lowPopCutoff)}
    bias = sapply(newData, calculateBias, cohortData, RR0Constant)
    newData = removeLowBias(newData, bias, biasCutoff, includeRR0)

    newCovariates = combineFunction(newData, ffbase::ffdfrbind.fill)
    newCovariates = combineWithOtherCovariates(newCovariates, demographicsCovariates, predefinedCovariates)
    covariateIdIndex = createCovariateIndex(newCovariates)
    newCovariates = convertCovariateId(newCovariates, covariateIdIndex)

    newCovariateRefList = mapply(createNewCovRef, newData, dimensionTable, rawCodes, list(covariateIdIndex), list(cohortData))
    newCovariateRef = combineFunction(newCovariateRefList, ffbase::ffdfrbind.fill)
    newCovariateRef = combineWithOtherCovariateRef(newCovariateRef, demographicsCovariateRef, predefinedCovariateRef, covariateIdIndex)

    cohortData$covariates = newCovariates
    cohortData$covariateRef = newCovariateRef
    return(cohortData)
  }
}

#' Runs hdps implementation
#'
#' @description
#' This function runs the hdps implementation on \code{cohortMethodData}.
#'
#' @details
#' Currently implemented for ICD9 diagnosis codes, CPT4 procedure codes, and drug concept ID codes. The mapping from CDM concept Id to
#' source codes is obtained, and used to relabel the covariates and covariateRef components of cohortMethodData. Granularity for ICD9 codes
#' is implemented by "truncating" the source codes to 3 digits. Codes with fewer than \code{lowPopCutoff} number of patients are dropped.
#' For each covariate, three covariates are constructed: "once", "sporadic", "frequent" for any occurrences, occurrences greater than
#' the median, and occurrences greater than 3rd quartile, respectively. For each covariate, the "bias" as specified in hdps model is
#' calculated, and the highest \code{biasCutoff} codes across all data dimensions are selected for inclusion. Demographic information of
#' age, sex, race, and year are also included. \code{cohortMethodData$covariates} and \code{cohortMethodData$covariateRef} are altered accordingly.
#'
#' @param cohortMethodData \code{cohortMethodData} object constructed by \code{getDbCohortMethodData}
#' @param useInpatientDiagnosis boolean to use inpatient icd9 condition codes
#' @param useAmbulatoryDiagnosis boolean to use ambulatory icd9 condition codes
#' @param useDrugIngredient boolean to use drug ingredient codes
#' @param useInpatientProcedure boolean to use inpatient procedure codes
#' @param useAmbulatoryProcedure boolean to use ambulatory procedure codes
#' @param demographicsAnalysisIds Vector of analysis identifiers that correspond to demographics covariates
#' @param predefinedIncludeICD9Dx Vector of predefined icd9 condition codes to include as covariates
#' @param predefinedIncludeConceptIds Vector of predefined concept IDs to include as covariates
#' @param predefinedExcludeICD9Dx Vector of predefined icd9 condition codes to exclude as covariates
#' @param predefinedExcludeConceptIds Vector of predefined concept IDs to exclude as covariates
#' @param icd9AnalysisIds Analysis IDs that contain icd9 condition codes
#' @param dimensionCutoff Number of most prevalent covariates to include from each data dimension
#' @param rankCutoff Number of covariates to include in propensity score model (minus the demographics covariates)
#' @param fudge Constant to avoid 0/Inf in calculating RR. Setting equal to 0 will cause those covariates to be discarded
#' @param useExpRank Use exposure rank instead of bias rank
#'
#' @return
#' Returns the input \code{cohortMethodData} with new entries for \code{covariates} and \code{covariateRef}
#' @export
runHdps <- function(cohortMethodData,
                    outcomeId = 3,
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
                    dimensionCutoff = 200,
                    rankCutoff = 500,
                    fudge = 0,
                    useExpRank) {
  start <- Sys.time()
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
  demographicsCovariateRef = getDemographicsCovariateRef(cohortMethodData, demographicsAnalysisIds)
  demographicsCovariates = getDemographicsCovariates(cohortMethodData, demographicsCovariateRef)
  predefinedCovariateRef = getPredefinedCovariateRef(cohortMethodData, predefinedIncludeConceptIds, predefinedIncludeICD9Dx, icd9AnalysisIds)
  predefinedCovariates = getPredefinedCovariates(cohortMethodData, predefinedCovariateRef)

  dimensionTable = getDimensionTable(dimensions)

  preCovariates = combineFunction(list(demographicsCovariates, predefinedCovariates), ffbase::ffdfrbind.fill)
  preCovariateRef = combineFunction(list(demographicsCovariateRef, predefinedCovariateRef), ffbase::ffdfrbind.fill)
  newCovariates <- NULL
  newCovariateRef <- NULL

  if (!is.null(dimensionTable)) {
    newCovariates = removePredefinedCovariates(cohortMethodData, c(predefinedIncludeConceptIds, predefinedExcludeConceptIds),
                                               c(predefinedIncludeICD9Dx, predefinedExcludeICD9Dx), icd9AnalysisIds)
    dimAnalysisId = sapply(dimensionTable, getDimensionAnalysisId)
    newData = removeOtherCovariates(newCovariates, cohortMethodData$covariateRef, dimAnalysisId)
    newData = sapply(dimAnalysisId, separateCovariates, newCovariates)
    totalPopulation = length(cohortMethodData$cohorts$rowId)
    newData = sapply(newData, removeRareCovariates, dimensionCutoff, totalPopulation)
    #newData = sapply(newData, addTreatmentAndOutcome, cohortMethodData, outcomeId)
    newData = sapply(newData, addTreatment, cohortMethodData$cohorts)
    newData = sapply(newData, addOutcome, cohortMethodData$outcomes, outcomeId)

    rankings = sapply(newData, calculateRanks, cohortMethodData, fudge)

    newCovariates = removeLowRank(newData, rankings, rankCutoff, useExpRank = useExpRank)
    newCovariates = combineFunction(newCovariates, ffbase::ffdfrbind.fill)
    newCovariates$treatment <- NULL
    newCovariates$outcome <- NULL
    newCovariateRef = getNewCovariateRef(newCovariates, cohortMethodData)
  }
    covariates = combineFunction(list(preCovariates, newCovariates), ffbase::ffdfrbind.fill)
    covariateRef = combineFunction(list(newCovariateRef, preCovariateRef), ffbase::ffdfrbind.fill)

    result = list(cmd = list(cohorts = cohortMethodData$cohorts,
                             outcomes = cohortMethodData$outcomes,
                             metaData = cohortMethodData$metaData,
                             covariates = covariates,
                             covariateRef = covariateRef),
                  parts = list(preCovariates = preCovariates,
                               preCovariateRef = preCovariateRef,
                               outcomeId = outcomeId,
                               fudge = fudge,
                               rankCutoff = rankCutoff,
                               newData = newData))
  delta <- Sys.time() - start
  writeLines(paste("selecting hdps covariates took", signif(delta, 3), attr(delta, "units")))

  return(result)
}

#' @export
runHdpsNewOutcomes <- function(hdps, cohortMethodData, useExpRank) {
  start <- Sys.time()
  newData = sapply(hdps$parts$newData, addOutcome, cohortMethodData$outcomes, hdps$parts$outcomeId)
  rankings = sapply(newData, calculateRanks, cohortMethodData, hdps$parts$fudge)

  newCovariates = removeLowRank(newData, rankings, hdps$parts$rankCutoff, useExpRank = useExpRank)
  newCovariates = combineFunction(newCovariates, ffbase::ffdfrbind.fill)
  newCovariates$treatment <- NULL
  newCovariates$outcome <- NULL
  newCovariateRef = getNewCovariateRef(newCovariates, cohortMethodData)

  covariates = combineFunction(list(hdps$parts$preCovariates, newCovariates), ffbase::ffdfrbind.fill)
  covariateRef = combineFunction(list(newCovariateRef, hdps$parts$preCovariateRef), ffbase::ffdfrbind.fill)

  delta <- Sys.time() - start
  writeLines(paste("selecting hdps covariates took", signif(delta, 3), attr(delta, "units")))

  return(list(cohorts = cohortMethodData$cohorts,
              outcomes = cohortMethodData$outcomes,
              metaData = cohortMethodData$metaData,
              covariates = covariates,
              covariateRef = covariateRef))
}

#' @export
runHdps1 <- function(cohortMethodData,
                     outcomeId = 3,
                     demographicsAnalysisIds = c(2,3,5,6),
                     predefinedIncludeConceptIds = c(),
                     predefinedExcludeConceptIds = c(),
                     icd9AnalysisIds = c(104, 105, 106, 107, 108, 109),
                     rankCutoff = 500,
                     fudge = 0,
                     useExpRank) {

  start <- Sys.time()
  demographicsCovariateRef = getDemographicsCovariateRef(cohortMethodData, demographicsAnalysisIds)
  demographicsCovariates = getDemographicsCovariates(cohortMethodData, demographicsCovariateRef)
  predefinedCovariateRef = getPredefinedCovariateRef(cohortMethodData, predefinedIncludeConceptIds, NULL, icd9AnalysisIds)
  predefinedCovariates = getPredefinedCovariates(cohortMethodData, predefinedCovariateRef)

  preCovariates = combineFunction(list(demographicsCovariates, predefinedCovariates), ffbase::ffdfrbind.fill)
  preCovariateRef = combineFunction(list(demographicsCovariateRef, predefinedCovariateRef), ffbase::ffdfrbind.fill)
  newCovariates = NULL
  newCovariateRef = NULL

  newData = removeCovariates(cohortMethodData,
                                   ff::as.ff(c(predefinedIncludeConceptIds,
                                               predefinedExcludeConceptIds,
                                               demographicsCovariateRef$covariateId[])))$covariates
  #newCovariates = addTreatmentAndOutcome(newCovariates, cohortMethodData, outcomeId)[[1]]
  newData = addTreatment(newData, cohortMethodData$cohorts)[[1]]
  newData = addOutcome(newData, cohortMethodData$outcomes, outcomeId)[[1]]
  rankings = calculateRanks(newData, cohortMethodData, fudge)

  newCovariates = removeLowRank1(newData, rankings, rankCutoff, useExpRank = useExpRank)
  newCovariates$treatment <- NULL
  newCovariates$outcome <- NULL
  newCovariateRef = getNewCovariateRef(newCovariates, cohortMethodData)

  covariates = combineFunction(list(preCovariates, newCovariates), ffbase::ffdfrbind.fill)
  covariateRef = combineFunction(list(newCovariateRef, preCovariateRef), ffbase::ffdfrbind.fill)

  result = list(cmd = list(cohorts = cohortMethodData$cohorts,
                           outcomes = cohortMethodData$outcomes,
                           metaData = cohortMethodData$metaData,
                           covariates = covariates,
                           covariateRef = covariateRef),
                parts = list(preCovariates = preCovariates,
                            preCovariateRef = preCovariateRef,
                            outcomeId = outcomeId,
                            fudge = fudge,
                            rankCutoff = rankCutoff,
                            newData = newData))
  delta <- Sys.time() - start
  writeLines(paste("selecting hdps covariates took", signif(delta, 3), attr(delta, "units")))

  return(result)
}

#' @export
runHdps1NewOutcomes <- function(hdps, cohortMethodData, useExpRank) {
  start <- Sys.time()

  newData = addOutcome(hdps$parts$newData, cohortMethodData$outcomes, hdps$parts$outcomeId)[[1]]
  rankings = calculateRanks(newData, cohortMethodData, hdps$parts$fudge)

  newCovariates = removeLowRank1(newData, rankings, hdps$parts$rankCutoff, useExpRank = useExpRank)
  newCovariates$treatment <- NULL
  newCovariates$outcome <- NULL
  newCovariateRef = getNewCovariateRef(newCovariates, cohortMethodData)

  covariates = combineFunction(list(hdps$parts$preCovariates, newCovariates), ffbase::ffdfrbind.fill)
  covariateRef = combineFunction(list(newCovariateRef, hdps$parts$preCovariateRef), ffbase::ffdfrbind.fill)

  delta <- Sys.time() - start
  writeLines(paste("selecting hdps covariates took", signif(delta, 3), attr(delta, "units")))

  return(list(cohorts = cohortMethodData$cohorts,
              outcomes = cohortMethodData$outcomes,
              metaData = cohortMethodData$metaData,
              covariates = covariates,
              covariateRef = covariateRef))
}











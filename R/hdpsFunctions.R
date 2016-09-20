# Return data frame containing analysisId for dimensions
#
# @param dimensions names of data dimensions
#
# @return list of data frames containing the data dimension names and analysisId
getDimensionTable <- function(dimensions) {
  if (is.null(dimensions)) {return(NULL)}
  dimensionTable = data.frame(dimName = c(), analysisId = c(), names = c(), row.names = NULL, stringsAsFactors = FALSE)

  inpatientDiagnosis = data.frame(dimName = "inpatientDiagnosis", analysisId = 104, row.names = NULL, stringsAsFactors = FALSE)
  dimensionTable = rbind(dimensionTable, inpatientDiagnosis)

  ambulatoryDiagnosis = data.frame(dimName = "ambulatoryDiagnosis", analysisId = 107, row.names = NULL, stringsAsFactors = FALSE)
  dimensionTable = rbind(dimensionTable, ambulatoryDiagnosis)

  drugIngredient = data.frame(dimName = "drugIngredient", analysisId = 403, row.names = NULL, stringsAsFactors = FALSE)
  dimensionTable = rbind(dimensionTable, drugIngredient)

  inpatientProcedure = data.frame(dimName = "inpatientProcedure", analysisId = 703, row.names = NULL, stringsAsFactors = FALSE)
  dimensionTable = rbind(dimensionTable, inpatientProcedure)

  ambulatoryProcedure = data.frame(dimName = "ambulatoryProcedure", analysisId = 706, row.names = NULL, stringsAsFactors = FALSE)
  dimensionTable = rbind(dimensionTable, ambulatoryProcedure)

  f <- function(dimName) {
    row = match(dimName, dimensionTable$dimName)
    if (is.na(row)) {stop(paste("must give valid data dimension: ", dimName, sep=""))}
    return(list(dimensionTable[row,]))
  }
  return(sapply(dimensions, f))
}


# Retrieves analysisId
# @description Retrieves \code{analysisId} from data dimension information
# @param dimensionInfo Information for a data dimension, as acquired from \code{getDimensionTable}
# @return
# Returns the numeric value for \code{analysisId}
getDimensionAnalysisId <- function(dimensionInfo) {
  return(list(dimensionInfo$analysisId))
}

# Extracts covariates for data dimension
#
# @description Extracts covariates for data dimension into 3 categories - input analysisId, analysisId+1, analysisId+2
# for at least once, more than median, and more than 75 quantile codes
#
# @param analysisId Analysis ID for data dimension
# @param covariates Covariate information from cohortMethodData
#
# @return
# List of ffdf containing covariates that correspond to analysisId, analysisId+1 (>median), analysisId+2 (>q75)
separateCovariates <- function(analysisId, covariates) {
  result = sapply(c(analysisId, analysisId+1, analysisId+2), separateCovariatesHelper, covariates)
  return(list(result))
}

separateCovariatesHelper <- function(analysisId, covariates) {
  t = covariates$covariateId %% 1000 == analysisId
  if (all(!t)) {return(list(NULL))}
  return(list(covariates[ffbase::ffwhich(t,t==TRUE),]))
}

# Keeps only a certain number of covariates per data dimensions, ranked by prevalence
#
# @description Keeps the top number of covariates, by prevalence, from covariates for data dimension. Codes will only be
# kept for (>median) and (>q75) if they are kept in the base category.
#
# @param data List of 3 ffdf of covariates for data dimension and expanded codes
# @param dimensionCutoff Number of covariates to keep
# @param totalPopulation Total population in cohort study
#
# @return
# Returns list of 3 ffdf with only the kept covariates
removeRareCovariates <- function(data, dimensionCutoff, totalPopulation) {
  data1 = data[[1]]
  if (!is.null(data1)) {
    counts = ffbase::table.ff(ff::ff(vmode = "integer", data1$covariateId %/% 1000))
    counts[which(counts>(totalPopulation/2))] = totalPopulation - counts[which(counts>(totalPopulation/2))]
    counts = counts[order(counts, decreasing = TRUE)]
    finalCutoff = min(length(counts), dimensionCutoff)
    x = ff::as.ff(as.double(names(counts[1:finalCutoff])))
    y = ffbase::ffmatch(data1$covariateId %/% 1000, x)
    data1 = data1[ffbase::ffwhich(y,!is.na(y)),]
  }

  data2 = data[[2]]
  if (!is.null(data2)) {
    y = ffbase::ffmatch(data[[2]]$covariateId %/% 1000, x)
    data2 = data[[2]][ffbase::ffwhich(y,!is.na(y)),]
  }

  data3 = data[[3]]
  if (!is.null(data3)) {
    y = ffbase::ffmatch(data[[3]]$covariateId %/% 1000, x)
    data3 = data[[3]][ffbase::ffwhich(y,!is.na(y)),]
  }

  return(list(data1, data2, data3))
}

# Add treatment and outcome data
#
# @description Appends columns to covariates that includes treatment and outcome for that patient
#
# @param data Covariates for data dimension
# @param cohortData cohortMethodData object
#
# @return Returns input covariates, with extra columns treatment and outcome that are 1/0
addTreatmentAndOutcome <- function(data, cohortData) {
  if (is.null(data)) {return(list(NULL))}
  treatment = ff::as.ff(cohortData$cohorts$treatment)[ffbase::ffmatch(data$rowId, ff::as.ff(cohortData$cohorts$rowId))]
  outcome = ff::ff(vmode = "double", initdata = 0, length = dim(data)[[1]])
  x = ffbase::ffmatch(data$rowId, ff::as.ff(cohortData$outcomes$rowId))
  y = ffbase::ffwhich(x, !is.na(x))
  outcome[y] = ff::as.ff(rep(1,length(y)))
  data$treatment = treatment
  data$outcome = outcome
  return(list(data))
}

# Calculates bias and exposure ranks
#
# @param data Covariate information for data dimension with treatment and outcome
# @param cohortData CohortMethodData object
# @param fudge Fudge factor to use to avoid divide by 0. If fudge = 0, relative risks and bias with 0 or Inf values are thrown out
#
# @return Returns ffdf with columns \describe{
# \item{covariateId}{covariate ID}
# \item{biasRank}{bias score for covariate = abs(log(bias))}
# \item{expRank}{exposure score for covariate = abs(log(prevalence_treated/prevalence_control))}
# \item{PC0}{prevalence of covariate among control, for breaking ties}
# \item{PC1}{prevalence of covariate among treated, for breaking ties}}
calculateRanks <- function(data, cohortData, fudge) {
  if (is.null(data)) {return(list(NULL))}
  totalPopulation = length(cohortData$cohorts$rowId)
  t = ff::as.ff(cohortData$cohorts$treatment)
  totalExposedPopulation = length(ffbase::ffwhich(t,t==1))
  totalComparatorPopulation = length(ffbase::ffwhich(t,t==0))
  totalOutcomePopulation = length(unique(cohortData$outcomes$rowId))

  uniqueCovariateId = unique(data$covariateId)
  foo = ff::ffdf(x1 = uniqueCovariateId, x2 = uniqueCovariateId)

  xt = data$covariateId * 10 + data$treatment
  xo = data$covariateId * 10 + data$outcome

  PC1 = ff::ffapply(X=foo, AFUN=function(a){return(length(ffbase::ffwhich(xt, xt==a["x1"]*10+1)))}, MARGIN=1, RETURN=TRUE, CFUN="list")[[1]]
  PC0 = ff::ffapply(X=foo, AFUN=function(a){return(length(ffbase::ffwhich(xt, xt==a["x1"]*10)))}, MARGIN=1, RETURN=TRUE, CFUN="list")[[1]]
  RRa = ff::ffapply(X=foo, AFUN=function(a){return(length(ffbase::ffwhich(xo, xo==a["x1"]*10+1)))}, MARGIN=1, RETURN=TRUE, CFUN="list")[[1]]
  RRb = ff::ffapply(X=foo, AFUN=function(a){return(length(ffbase::ffwhich(xo, xo==a["x1"]*10)))}, MARGIN=1, RETURN=TRUE, CFUN="list")[[1]]

  RRc = totalOutcomePopulation - RRa
  RRd = totalPopulation - RRa - RRb - RRc
  PC1 = PC1 / totalExposedPopulation
  PC0 = PC0 / totalComparatorPopulation

  PC1[PC1>0.5] = (1-PC1)[PC1>0.5]
  PC0[PC0>0.5] = (1-PC0)[PC0>0.5]

  RRcd = ((RRa+fudge)*(RRc+RRd+fudge)) / ((RRc+fudge)*(RRa+RRb+fudge))
  RRce = PC1/PC0

  bias = rep(0,length(RRcd))
  bias[RRcd>=1] = (PC1[RRcd>=1]*(RRcd[RRcd>=1]-1)+1) / (PC0[RRcd>=1]*(RRcd[RRcd>=1]-1)+1)
  bias[RRcd<1] = (PC1[RRcd<1]*((1/RRcd[RRcd<1])-1)+1) / (PC0[RRcd<1]*((1/RRcd[RRcd<1])-1)+1)

  RRcd[RRcd==0] = NA
  RRcd[RRcd==Inf] = NA
  RRce[RRce==0] = NA
  RRce[RRce==Inf] = NA
  bias[is.na(RRcd)] = NA

  biasRank = abs(log(bias))
  expRank = abs(log(RRce))

  result = ff::ffdf(covariateId=uniqueCovariateId, biasRank = ff::as.ff(biasRank), expRank = ff::as.ff(expRank), PC0 = ff::as.ff(PC0), PC1 = ff::as.ff(PC1))
  return(list(result))
}

# Keeps top covariates by bias or exposure rank
#
# @description Keeps top number of covariates by bias or exposure rank for inclusion in propensity score
#
# @param data List of covariates, corresponding to data dimensions
# @param rankings List of rankings generated by \code{calculateRanks}
# @param rankCutoff Number of covariates to keep
# @param useExpRank Boolean to use exposure instead of bias
#
# @return
# Removes input covariates with only top covariates kept
removeLowRank <- function(data, rankings, rankCutoff, useExpRank) {

  rankings = combineFunction(rankings, ffbase::ffdfrbind.fill)
  if (is.null(rankings)) {return(list(NULL))}
  if (useExpRank) {
    rankings = rankings[ff::fforder(rankings$expRank*-1, rankings$biasRank*-1, rankings$PC1*-1, rankings$PC0*-1, rankings$covariateId, decreasing = FALSE),]
    t = ffbase::ffwhich(rankings, is.na(rankings$expRank))
  }
  else {
    rankings = rankings[ff::fforder(rankings$biasRank*-1, rankings$expRank*-1, rankings$covariateId, decreasing = FALSE),]
    t = ffbase::ffwhich(rankings, is.na(rankings$biasRank))
  }
  finalCutoff = if(is.null(t)) dim(rankings)[1] else t[1]
  finalCutoff = min(finalCutoff, rankCutoff)
  if (finalCutoff==0) {return(list(NULL))}
  toKeep = rankings$covariateId[ff::as.ff(1:finalCutoff)]
  data = sapply(data, removeLowRankHelper, toKeep)
  return(data)
}

removeLowRankHelper <- function(data, toKeep) {
  if (is.null(data)) {return(list(NULL))}
  t = ffbase::ffmatch(data$covariateId, toKeep)
  t = ffbase::ffwhich(t, !is.na(t))
  if (is.null(t)) {return(list(NULL))}
  data = data[t,]
  return(list(data))
}

# Get covariateRef entries for covariates
#
# @param covariates Ffdf of covariates
# @param cohortData CohortMethodData object
#
# @return Returns ffdf of covariateRef entries corresponding to covariates
getNewCovariateRef <- function(covariates, cohortData) {
  covariateId = unique(covariates$covariateId)
  t = ffbase::ffmatch(cohortData$covariateRef$covariateId, covariateId)
  return(cohortData$covariateRef[ffbase::ffwhich(t, !is.na(t)),])
}

# Get covariateRef for predefined codes
#
# @description Get covariateRef entries for predefined codes to include
#
# @param cohortData CohortMethodData object
# @param conceptIds List of conceptIds to include
# @param icd9 List of icd9 condition codes to include, with E replaced by 10 and V replaced by 11
# @param icd9AnalysisIds List of analysisIds that include icd9 condition codes
#
# @return
# Returns entries of covariateRef that correspond to predefined codes to include
getPredefinedCovariateRef <- function(cohortData, conceptIds, icd9, icd9AnalysisIds) {
  if (is.null(conceptIds) & is.null(icd9)) {return(NULL)}
  y = ffbase::ffmatch(cohortData$covariateRef$analysisId, ff::as.ff(icd9AnalysisIds))
  result1 = NULL
  if (!is.null(conceptIds)) {
    x = ffbase::ffmatch(cohortData$covariateRef$conceptId, ff::as.ff(conceptIds))
    t = ffbase::ffwhich(cohortData$covariateRef, !is.na(x) & is.na(y))
    if (!is.null(t)) result1 = cohortData$covariateRef[t,]
  }
  result2 = NULL
  if (!is.null(icd9)) {
    x = ffbase::ffmatch(cohortData$covariateRef$covariateId %/% 1000, ff::as.ff(icd9))
    t = ffbase::ffwhich(cohortData$covariateRef, !is.na(x) & !is.na(y))
    if (!is.null(t)) result2 = cohortData$covariateRef[t,]
  }
  result = combineFunction(list(result1, result2), ffbase::ffdfrbind.fill)
  if (!is.null(result)) result$covariateName = ffbase::droplevels.ff(result$covariateName)
  return(result)
}

# Get predefined covariates
#
# @description Get predefined covariates entries for predefined codes to include
#
# @param cohortData CohortMethodData object
# @param predefinedCovariateRef ffdf returned by \code{getPredefinedCovariateRef}
#
# @return
# Returns entries of covariates that correspond to predefined codes to include
getPredefinedCovariates <- function(cohortData, predefinedCovariateRef) {

  if (is.null(predefinedCovariateRef)) {return(NULL)}
  t = ffbase::ffmatch(cohortData$covariates$covariateId, predefinedCovariateRef$covariateId)
  return(cohortData$covariates[ffbase::ffwhich(t, !is.na(t)),])
}

# Remove predefined covariates
#
# @description Removes from covariates table of CohortMethodData the predefined codes to include and exclude. The included covariates have already been
# extracted
#
# @param cohortData CohortMethodData object
# @param conceptIds Predefined concept IDs to include and exclude
# @param icd9 Predefined icd9 condition codes to include and exclude, with E replaced by 10 and V replaced by 11
# @param icd9AnalysisIds List of analysisIds that include icd9 condition codes
#
# @return
# Returns covariates table of cohortData with predefined codes removed
removePredefinedCovariates <- function(cohortData, conceptIds, icd9, icd9AnalysisIds) {
  covariates = cohortData$covariates
  x = ffbase::ffmatch(cohortData$covariateRef$analysisId, ff::as.ff(icd9AnalysisIds))
  if (!is.null(conceptIds)) {
    y = ffbase::ffmatch(cohortData$covariateRef$conceptId, ff::as.ff(conceptIds))
    t = ffbase::ffwhich(cohortData$covariateRef, !is.na(y) & is.na(x))
    if (!is.null(t)) {
      t = ffbase::ffmatch(covariates$covariateId, cohortData$covariateRef$covariateId[t])
      t = ffbase::ffwhich(t, is.na(t))
      covariates = covariates[t,]
    }
  }
  if (!is.null(icd9)) {
    y = ffbase::ffmatch(cohortData$covariateRef$covariateId %/% 1000, ff::as.ff(icd9))
    t = ffbase::ffwhich(cohortData$covariateRef, !is.na(y) & !is.na(x))
    if (!is.null(t)) {
      t = ffbase::ffmatch(covariates$covariateId, cohortData$covariateRef$covariateId[t])
      t = ffbase::ffwhich(t, is.na(t))
      covariates = covariates[t,]
    }
  }
  return(covariates)
}

# Sequentially applies FUN to elements of data
#
# @description
# This function applies and combines a function over successive elements of a list. For example, if \code{data} is a list of
# \code{data.frame} and \code{FUN = rbind} it will return a \code{data.frame} of all the elements combined.
#
# @param data List of elements
# @param FUN Function to apply, takes two arguments
#
# @return
# Returns aggregated output of applying FUN to data
#' @export
combineFunction <- function(data, FUN) {
  t = sapply(data, function(x){return(!is.null(x))})
  data = data[which(t==TRUE)]
  d = length(data)
  if(d==0) return(NULL)
  if (d==1) {return(data[[1]])}
  result = data[[1]]
  for (i in 1:(d-1)) {
    result = FUN(result, data[[i+1]])
  }
  return(result)
}

# Retrieves demographics elements of covariate Ref
#
# @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
# @param analysisIds Vector of analysis identifiers associated with demographic covariates.
#
# @return
# Returns ffdf of elements of \code{cohortData$covariateRef} that relate to demographic information.
getDemographicsCovariateRef <- function(cohortData, analysisIds) {
  if (is.null(analysisIds)) {return(NULL)}
  t = ffbase::ffmatch(cohortData$covariateRef$analysisId, ff::as.ff(analysisIds))
  result = cohortData$covariateRef[ffbase::ffwhich(t,!is.na(t)),]
  result$covariateName = ffbase::droplevels.ff(result$covariateName)
  return(result)
}


# Retrieves demographics elements of covariates
#
# @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
# @param covariateRef Covariate reference information associated with demographic covariates.
#
# @return
# Returns ffdf of elements of \code{cohortData$covariates} that relate to demographic information.
getDemographicsCovariates <- function(cohortData, covariateRef) {
  if (is.null(covariateRef)) {return(NULL)}
  t = ffbase::ffmatch(cohortData$covariates$covariateId, covariateRef$covariateId)
  return(cohortData$covariates[ffbase::ffwhich(t,!is.na(t)),])
}

removeOtherCovariates <- function(newCovariates, covariateRef, dimAnalysisId) {
  analysisIds = combineFunction(dimAnalysisId, function(x,y) c(x,y))
  analysisIds = c(analysisIds, analysisIds+1, analysisIds+2)
  t = ffbase::ffmatch(covariateRef$analysisId, ff::as.ff(analysisIds))
  covariateIds = covariateRef$covariateId[ffbase::ffwhich(t, !is.na(t))]
  t = ffbase::ffmatch(newCovariates$covariateId, covariateIds)
  return(newCovariates[ffbase::ffwhich(t, !is.na(t)),])
}

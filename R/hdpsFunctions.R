#' Loads up data dimensions.
#'
#' @description
#' This function loads up information related to each data dimension provided. Data dimensions are hard coded here
#'
#' @details
#' If a code is not truncated, it's covariate name remains unchanged. If it is truncated and no name table is available, truncatedNames = ""
#' and the covariate name will become "covariate name not available."
#'
#' @param dimensions A character vector of data dimension names
#'
#' @return
#' Returns an list of \code{data.frame}, one for each data dimension. Each \code{data.frame} has the following columns: \describe{
#' \item{name}{Name of data dimension}
#' \item{analysisId}{Analysis identifier}
#' \item{sql}{Sql query (as string) that will retrieve mapping from OMOP concept ID to source code for specific data dimension}
#' \item{truncate}{String that can be parsed and evaluated into a function that truncates source code to desired granularity}
#' \item{truncatedNames}{String that can be parsed and evluated to return a \code{data.frame} with columns \code{code} and \code{name}.
#' This is for cases where the source code is truncated and new names for truncated codes are available} }
#' @export
getDimensionTable <- function(dimensions) {
  if (is.null(dimensions)) {return(NULL)}
  dimensionTable = data.frame(name = c(), analysisId = c(), sql = c(), truncate = c(), row.names = NULL, stringsAsFactors = FALSE)

  # Condition, ICD9 Code
  conditionICD9Dimension = data.frame(name = "conditionICD9", analysisId = 104,
                                      sql = "SELECT source_code, target_concept_id FROM source_to_concept_map WHERE source_vocabulary_id=2 AND target_concept_id in (@listIds)",
                                      truncate = "function(code){return(gsub(\"\\\\..*\",\"\",code))}",
                                      truncatedNames = "getICD9Dx(\"vignettes/ICD9Dx.Rdata\")",
                                      row.names = NULL, stringsAsFactors = FALSE)
  dimensionTable = rbind(dimensionTable, conditionICD9Dimension)

  # Drug, Concept Id
  drugDimension = data.frame(name = "drug", analysisId = 505,
                             sql = "SELECT concept_id as source_code, concept_id as target_concept_id FROM concept WHERE concept_id in (@listIds)",
                             truncate = "",
                             truncatedNames = "",
                             row.names = NULL, stringsAsFactors = FALSE)
  dimensionTable = rbind(dimensionTable, drugDimension)

  # Procedure, CPT4 Code
  procedureCPT4Dimension = data.frame(name = "procedureCPT4", analysisId = 703,
                                      sql = "SELECT source_code, target_concept_id FROM source_to_concept_map WHERE source_vocabulary_id=4 AND target_concept_id in (@listIds)",
                                      truncate = "",
                                      truncatedNames = "",
                                      row.names = NULL, stringsAsFactors = FALSE)
  dimensionTable = rbind(dimensionTable, procedureCPT4Dimension)

  f <- function(dimName) {
    row = match(dimName, dimensionTable$name)
    if (is.na(row)) {stop(paste("must give valid data dimension: ", dimName, sep=""))}
    return(list(dimensionTable[row,]))
  }
  return(sapply(dimensions, f))
}


#' Retrieves analysisId
#' @description Retrieves \code{analysisId} from data dimension information
#' @param dimensionInfo Information for a data dimension, as acquired from \code{getDimensionTable}
#' @return
#' Returns the numeric value for \code{analysisId}
#' @export
getDimensionAnalysisId <- function(dimensionInfo) {
  return(dimensionInfo$analysisId)
}


#' Retrieves sql query
#' @description Retrieves \code{sql} from data dimension information
#' @param dimensionInfo Information for a data dimension, as acquired from \code{getDimensionTable}
#' @return
#' Returns the sql query string
#' @export
getDimensionSql <- function(dimensionInfo) {
  return(dimensionInfo$sql)
}


#' Retrieves conceptId that matches analysisId
#' @description Retrieves list of concept IDs that match the analysisId for a data dimension. These concept IDs will be used to query the CDM for
#' the source codes for that data dimension.
#' @param analysisId Analysis ID for data dimension
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
#' @return
#' Returns a \code{ff_vector} of type \code{double} of concept IDs
#' @export
getConceptId <- function(analysisId, cohortData) {
  return(cohortData$covariateRef$conceptId[cohortData$covariateRef$analysisId==analysisId])
}


#' Retrieves covariateId that matches analysisId
#' @description Retrieves list of covariate IDs that match the analysisId for a data dimension.
#' @param analysisId Analysis ID for data dimension
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
#' @return
#' Returns a \code{ff_vector} of type \code{double} of covariate IDs
#' @export
getCovariateId <- function(analysisId, cohortData) {
  return(cohortData$covariateRef$covariateId[cohortData$covariateRef$analysisId==analysisId])
}


#' Applies mapply to sql queries
#'
#' @description
#' This function works like \code{mapply} for a list of sql queries. Each sql query will be rendered, translated, and executed with proper
#' parameters.
#'
#' @details
#' This function currently assumes that all sql queries use the same parameters, and that values for each parameter are provided.
#' Here is a visualization of how the function works:
#'
#' sqlMapply(connection, sqlList = list("query1 @@a @@b @@c", "query2 @@a @@b @@c"), paramNames = list("a", "b", "c"),
#' parameters = list(list(1,2), list(3,4), list(5,6)))
#'
#' -> list(SqlRender::renderSql("query1 @@a @@b @@c", a = 1, b = 3, c = 5), SqlRender::renderSql("query2 @@a @@b @@c", a = 2, b = 4, c = 6)
#'
#' @param connection Connection to sql database
#' @param sqlList List of sql queries to execute
#' @param paramNames List of parameter names
#' @param parameters List of list of parameters to substitute. length(parameters) = length(paramNames); length(parameters[[1]]) = length(sqlList)
#'
#' @return
#' Returns list of completed sql queries.
#' @export
sqlMapply <- function(connection, sqlList, paramNames, parameters) {
  d = length(sqlList)
  p = length(paramNames)
  result = list()

  for (i in 1:d) {
    t = ""
    for (k in 1:p) {
      t = paste(t, paramNames[[k]], "=", "parameters[[", k, "]][[", i, "]][]", if(k==p) ")" else ",", sep="")
    }
    t = paste("renderedSql <- SqlRender::renderSql(\"",sqlList[[i]],"\",",t,sep="")
    eval(parse(text=t))
    result[[i]] = DatabaseConnector::querySql.ffdf(connection, renderedSql$sql)
  }
  names(result) = names(sqlList)
  return(result)
}

#' Converts covariateId of rawCodes to a factor
#'
#' @description
#' This function converts the SOURCE_CODE column acquired from the OMOP database to a factor, if it weren't already a factor
#'
#' @param codes Ffdf object with columns \code{SOURCE_CODE} and \code{TARGET_CONCEPT_ID}
#'
#' @return
#' Returns the input codes with \code{SOURCE_CODE} as a factor
#' @export
covariateIdToFactor <- function(codes) {
  if (!ff::is.factor.ff(codes$SOURCE_CODE)) {
    codes$SOURCE_CODE = ff::ff(vmode = "integer", factor(codes$SOURCE_CODE[]))
  }
  return(list(codes))
}

#' Removes predefined codes from data dimensions
#'
#' @param rawCodes Ffdf object with columns: \code{SOURCE_CODE} and \code{TARGET_CONCEPT_ID}
#' @param predefinedConceptIds Vector of concept identifiers for predefined covariates
#'
#' @return
#' Returns \code{rawCodes} with all instances of (\code{TARGET_CONCEPT_ID} = predefined covariate) removed
#' @export
deletePredefinedCodes <- function(rawCodes, predefinedConceptIds) {
  if (dim(rawCodes)[[1]] == 0) {stop("no source code mappings found for data dimension. check sql query or turn of flag for data dimension")}
  if (!is.null(predefinedConceptIds)) {
    t = ffbase::ffmatch(rawCodes$TARGET_CONCEPT_ID, ff::as.ff(predefinedConceptIds))
    rows = ffbase::ffwhich(t, is.na(t))
    if (is.null(rows)) {return(list(NULL))}
    rawCodes = rawCodes[rows,]
    rawCodes$SOURCE_CODE = ffbase::droplevels.ff(rawCodes$SOURCE_CODE)
  }
    return(list(rawCodes))
}


#' Applies truncating function to source codes
#'
#' @description
#' Truncates source codes according to data dimension
#'
#' @details
#' The input is obtained from the CDM, and the \code{SOURCE_CODE} can be a factor (integer) or a number (double).
#' Returns a list to work properly with mapply.
#'
#' @param rawCodes Ffdf object with columns: \code{SOURCE_CODE} and \code{TARGET_CONCEPT_ID}
#' @param dimensionTable Data dimension information, including truncating function
#'
#' @return
#' Returns \code{rawCodes} with \code{SOURCE_CODE} truncated if required
#' @export
truncateRawCodes <- function(rawCodes, dimensionTable) {
  if (is.null(rawCodes)) {return(list(NULL))}
  f = dimensionTable$truncate
  if (f!="") {
    rawCodes$SOURCE_CODE = ff::ff(vmode = "integer", factor(sapply(rawCodes$SOURCE_CODE[], eval(parse(text=f)))))
    rawCodes$SOURCE_CODE = ffbase::droplevels.ff(rawCodes$SOURCE_CODE)
  }
  return(list(rawCodes))
}


#' Keeps only unique code mappings
#'
#' @description
#' This function deletes duplicate entries in the truncated codes, and also ambiguous mappings from one
#' \code{TARGET_CONCEPT_ID} to multiple \code{SOURCE_CODE}
#'
#' @param codes \code{ffdf} with columns: \code{SOURCE_CODE} and \code{TARGET_CONCEPT_ID}
#'
#' @return
#' Returns \code{codes} with relevant rows deleted.
#' @export
deleteRepeatCodes <- function(codes) {
  if (is.null(codes)) {return(list(NULL))}
  codes = unique(codes)
  counts = table(codes$TARGET_CONCEPT_ID[])
  codes = ff::as.ffdf(codes[counts[as.character(codes$TARGET_CONCEPT_ID[])]==1,])
  rownames(codes) = NULL
  codes$SOURCE_CODE = ffbase::droplevels.ff(codes$SOURCE_CODE)
  return(list(codes))
}


#' Combines covariate, treatment, and outcome
#'
#' @description
#' This function combines covariate, treatment, and outcome information, then lists them by data dimension.
#' It breaks down \code{cohortData$covariates} by data dimension, converts the \code{covariateId} to the \code{SOURCE_CODE}
#' that matches the \code{conceptId} associated with that \code{covariateId}, then adds \code{treatment} and \code{outcome}
#' as binary indicators for that patient (\code{rowId}).
#'
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
#' @param codes List of \code{ffdf} source code mappings for each data dimension with columns:
#' \code{SOURCE_CODE} and \code{TARGET_CONCEPT_ID}
#' @param dimCovariateId List of \code{ff_vector} covariate identifiers for each data dimension
#'
#' @return
#' Returns \code{list} of \code{ffdf} by data dimension with the following columns: \describe{
#' \item{rowId}{Patient identifier}
#' \item{covariateId}{New source code for data dimension}
#' \item{treatment}{1 if patient is in treatment cohort; 0 otherwise}
#' \item{outcome}{1 if patient is in outcome cohort; 0 otherwise} }
#' @export
combineData <- function(cohortData, codes, dimCovariateId) {
  covariateIds = combineFunction(dimCovariateId, ffbase::ffappend)
  if (is.null(covariateIds)) {return(NULL)}
  covariateIdKeep = ff::ff(vmode = "double", length = dim(cohortData$covariates)[1])
  t = ffbase::ffmatch(cohortData$covariates$covariateId, covariateIds)
  r = ffbase::ffwhich(t, !is.na(t))
  if (!is.null(r)) {covariateIdKeep[r[]] = 1}

  rowIds = cohortData$covariates$rowId
  conceptIds = cohortData$covariateRef$conceptId[ffbase::ffmatch(cohortData$covariates$covariateId, cohortData$covariateRef$covariateId)]
  conceptIds = conceptIds * covariateIdKeep
  sourceIds = sapply(codes, f <- function(code) {if (is.null(code)) {return(NULL)} else return(code$SOURCE_CODE[ffbase::ffmatch(conceptIds, code$TARGET_CONCEPT_ID)])})
  treatments = cohortData$cohorts$treatment[ffbase::ffmatch(cohortData$covariates$rowId, cohortData$cohorts$rowId)]
  outcomes = !is.na(ffbase::ffmatch(cohortData$covariates$rowId, cohortData$outcomes$rowId))
  outcomes = ff::as.ff(sapply(outcomes[], function(bool){if(bool){return(1)}else return(0)}))

  newData = sapply(sourceIds, combineDataHelper, rowIds, treatments, outcomes)
  return(newData)
}


#' Helper for combineData
#'
#' @param sourceId An ff vector of with length = length(\code{cohortData$covariates}) of converted source codes for a data
#' dimension.
#' @param rowIds An ff vector of patient identifiers
#' @param treatments An ff vector of binary treatment identifier for each entry of \code{cohortData$covariates}
#' @param outcomes An ff vector of binary outcome identifier for each entry of \code{cohortData$covariates}
#'
#' @return
#' Returns ffdf object with the four combined ff vectors, only on covariate indices appropriate for that data dimension.
#' \code{covariateId} is converted to a factor, if it is not already one.
#' @export
combineDataHelper <- function(sourceId, rowIds, treatments, outcomes) {
  if (is.null(sourceId)) {return(NULL)}
  toKeep = ffbase::ffwhich(sourceId, !is.na(sourceId))
  result = ff::ffdf(rowId = rowIds[toKeep], covariateId = sourceId[toKeep], treatment = treatments[toKeep], outcome = outcomes[toKeep])
  if (!is.factor(result$covariateId[1])) {
    result$covariateId = ff::ff(vmode="integer", initdata = factor(result$covariateId[]))
  }
  result$covariateId = ffbase::droplevels.ff(result$covariateId)
  return(result)
}


#' Removes codes with insufficient patients
#'
#' @description
#' This function removes data for covariates that have insufficient number of unique pataients.
#'
#' @details
#' If prevalence of a covariate is > 0.5 among people in data dimension, prevalence is subtracted from 1
#'
#' @param data Covariate information for data dimension, as ffdf object with columns \code{rowId}, \code{covariateId},
#' \code{treatment}, \code{outcome}
#' @param lowPopCutoff Threshold for number of unique patients that need to have a \code{covariateId} to keep data
#' @param dimensionCutoff Maximum number of covariates to keep from this data dimension, ordered by prevalence.
#'
#' @return
#' Returns \code{data} with rare codes removed. Returns a list to work with sapply.
#' @export
removeRareCodes <- function(data, lowPopCutoff, dimensionCutoff=NULL) {
  if (is.null(data)) {return(list(NULL))}
  data1 = unique(data)
  totalPeople = length(unique(data$rowId))
  counts = ffbase::table.ff(data1$covariateId)
  counts[which(counts>(totalPeople/2))] = totalPeople - counts[which(counts>(totalPeople/2))]
  counts = counts[order(counts, decreasing = TRUE)]
  dimensionCutoff = if(is.null(dimensionCutoff)) length(counts) else (min(length(counts), dimensionCutoff))

  lowPopCutoff = which(counts<lowPopCutoff)[1]
  finalCutoff = if(is.na(lowPopCutoff)) dimensionCutoff else min(lowPopCutoff-1, dimensionCutoff)
  if (finalCutoff==0) {return(NULL)}
  #dimensionCutoff = counts[dimensionCutoff]
  #data = as.ffdf(data[counts[data$covariateId[]]>=max(lowPopCutoff, dimensionCutoff),])
  t = ffbase::ffmatch(data$covariateId, ff::as.ff(factor(names(counts[1:finalCutoff]))))
  data = data[ffbase::ffwhich(t,!is.na(t)),]
  #rownames(data) <- NULL
  data$covariateId = ffbase::droplevels.ff(data$covariateId)
  return(list(data))
}


#' Adds new covariates for sporadic and frequent occurrences
#'
#' @description
#' This function adds new covariates named sporadic and frequent according to hdps implementation, for a data dimension.
#'
#' @details
#' Sporadic indicates a code occurs for a patient more than the median occurrences for that code; frequent indicates more than 75th percentile.
#' Once the new codes are generated, repeated codes in the original data are dropped so that "occurs at least once" is a binary variable.
#'
#' @param data Covariate information for data dimension, as ffdf object with columns \code{rowId}, \code{covariateId},
#' \code{treatment}, \code{outcome}
#'
#' @return
#' Returns data as ffdf object with same columns, with added lines for sporadic and frequent codes. Sporadic codes are designated by
#' appending "_sporadic" to the \code{covariateId}, while frequent codes are designated by appending "_frequent" to the \code{covariateId}.
#' @export
expandCovariates <- function(data) {
  if (is.null(data)) {return(list(NULL))}
  data = data[ff::fforder(data$covariateId, data$rowId),]
  uniqueData = unique(data)
  uniqueData = uniqueData[ff::fforder(uniqueData$covariateId, uniqueData$rowId),]

  uniqueDataIndex = ffbase::ffdfmatch(uniqueData, data)
  uniqueDataNextIndex = c(uniqueDataIndex[2:length(uniqueDataIndex)], length(data$rowId)+1)
  uniqueData$counts = ff::as.ff(uniqueDataNextIndex) - uniqueDataIndex

  uniqueCovariateId = unique(data$covariateId)
  foo = ff::ffdf(x1 = uniqueCovariateId, x2 = uniqueCovariateId)
  covariateIdCounts = ff::ffapply(X=foo, AFUN=function(a){return(uniqueData$counts[ffbase::ffwhich(uniqueData, uniqueData$covariateId==a["x1"])])}, MARGIN=1,RETURN=TRUE, CFUN="list")[[1]]

  covariateIdStats = sapply(covariateIdCounts, function(a){return(ffbase::quantile.ff(a))})
  covariateIdStats = ff::ffdf(covariateId = uniqueCovariateId, median = ff::as.ff(covariateIdStats[3,]), thirdQuartile = ff::as.ff(covariateIdStats[4,]))
  uniqueData$median = covariateIdStats$median[ffbase::ffmatch(uniqueData$covariateId, covariateIdStats$covariateId)]
  uniqueData$thirdQuartile = covariateIdStats$thirdQuartile[ffbase::ffmatch(uniqueData$covariateId, covariateIdStats$covariateId)]

  sporadicIndex = ffbase::ffwhich(uniqueData, uniqueData$counts > uniqueData$median)
  frequentIndex = ffbase::ffwhich(uniqueData, uniqueData$counts > uniqueData$thirdQuartile)

  if(!is.null(sporadicIndex)) {
    sporadicData = uniqueData[sporadicIndex,]
    sporadicCovariateId = as.character(sporadicData$covariateId[])
    sporadicCovariateId = paste(sporadicCovariateId, "_sporadic", sep = "")
    sporadicData$covariateId = ff::ff(vmode="integer", factor(sporadicCovariateId))
    sporadicData$counts <- NULL
    sporadicData$median <- NULL
    sporadicData$thirdQuartile <- NULL
    data = ffbase::ffdfrbind.fill(data, sporadicData)
  }

  if(!is.null(frequentIndex)) {
    frequentData = uniqueData[frequentIndex,]
    frequentCovariateId = as.character(frequentData$covariateId[])
    frequentCovariateId = paste(frequentCovariateId, "_frequent", sep = "")
    frequentData$covariateId = ff::ff(vmode="integer", factor(frequentCovariateId))
    frequentData$counts <- NULL
    frequentData$median <- NULL
    frequentData$thirdQuartile <- NULL
    data = ffbase::ffdfrbind.fill(data, frequentData)
  }
  data = unique(data)
  return(list(data))
}


#' Calculates bias and relative risk for each covariate
#'
#' @description
#' This function calculates the bias, as described in the hdps implementation, and the relative risk of each \code{covariateId} for a
#' data dimension.
#'
#' @param data Covariate information for data dimension, as ffdf object with columns \code{rowId}, \code{covariateId},
#' \code{treatment}, \code{outcome}
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
#' @param RR0Constant Large constant to use when RR = 0 to prevent divide by zero error
#'
#' @return
#' Returns ffdf object with the following columns: \describe{
#' \item{covariateId}{Covariate identifier}
#' \item{RR}{Relative risk of covariate on outcome}
#' \item{bias}{The value of abs(log(bias)) for each covariate, ranked decreasingly}}
#' Returns list to work with sapply.
#' @export
calculateBias <- function(data, cohortData, RR0Constant) {
  if (is.null(data)) {return(list(NULL))}
  totalPopulation = length(cohortData$cohorts$rowId)
  t = cohortData$cohorts$treatment
  totalExposedPopulation = length(ffbase::ffwhich(t,t==1))
  totalComparatorPopulation = length(ffbase::ffwhich(t,t==0))
  totalOutcomePopulation = length(unique(cohortData$outcomes$rowId))

  tempCovariatesId = c()
  PC1 = c(0)
  PC0 = c(0)
  RRa = c(0)
  RRb = c(0)

  d = dim(data)[1]
  if (d==0) {
    return()
  }

  data = data[ff::fforder(data$covariateId),]

  f = function(a) {return(as.character(a))}
  covariateIds = sapply(data$covariateId[], f)
  treatments = data$treatment[]
  outcomes = data$outcome[]

  tempCovariatesId = c(covariateIds[1])

  for(i in 1:d) {
    if (covariateIds[i]!=tempCovariatesId[1]) {
      tempCovariatesId = c(covariateIds[i],tempCovariatesId)
      PC1=c(0,PC1)
      PC0=c(0,PC0)
      RRa=c(0,RRa)
      RRb=c(0,RRb)
    }
    if (treatments[i]==1) {PC1[1]=PC1[1]+1}
    if (treatments[i]==0) {PC0[1]=PC0[1]+1}
    if (outcomes[i]==1) {RRa[1]=RRa[1]+1}
    if (outcomes[i]==0) {RRb[1]=RRb[1]+1}
  }

  RRc = totalOutcomePopulation - RRa
  RRd = totalPopulation - RRa - RRb - RRc
  PC1 = PC1 / totalExposedPopulation
  PC0 = PC0 / totalComparatorPopulation

  RR = (RRa*(RRc+RRd)) / (RRc*(RRa+RRb))

  bias = rep(0,length(RR))
  for(i in which(RR>=1)) {
    bias[i]=(PC1[i]*(RR[i]-1)+1) / (PC0[i]*(RR[i]-1)+1)
  }
  for(i in which(RR<1 & RR>0)) {
    bias[i]=(PC1[i]*((1/RR[i])-1)+1) / (PC0[i]*((1/RR[i])-1)+1)
  }
  for(i in which(RR==0)) {
    bias[i]=(PC1[i] * RR0Constant + 1) / (PC0[i] * RR0Constant + 1)
  }
  bias = abs(log(bias))

  result = data.frame(covariateId=tempCovariatesId, RR=RR, bias=bias)
  result = result[order(result$bias,decreasing=TRUE),]
  result = ff::as.ffdf(result)
  rownames(result) <- NULL

  return(list(result))
}


#' Keeps only covariates with highest bias.
#'
#' @description
#' This function eliminates data associated with covariates that have low bias. Only the top \code{cutoffIndex} codes are kept.
#'
#' @param data List covariate information for data dimensions, as ffdf objects with columns \code{rowId}, \code{covariateId},
#' \code{treatment}, \code{outcome}
#' @param bias List of bias for each data dimension's covariates, as ffdf objects with columns \code{covariateId}, \code{RR}, \code{bias}
#' @param cutoffIndex Number of total covariates to keep
#' @param includeRR0 Boolean to keep covariates with RR = 0
#'
#' @return
#' Returns \code{data} with only the highest bias covariates.
#' @export
removeLowBias <- function(data, bias, cutoffIndex, includeRR0) {
  bias = combineFunction(bias, ffbase::ffdfrbind.fill)
  if (is.null(bias)) {return(list(NULL))}
  if (!includeRR0) {
    t = ffbase::ffwhich(bias, bias$RR > 0)
    if (is.null(t)) {return(list(NULL))}
    bias = bias[t,]
  }
  bias = bias[ff::fforder(bias$bias, decreasing = TRUE),]
  finalIndex = min(dim(bias)[1], cutoffIndex)
  #data = sapply(data, removeLowBiasHelper, bias, bias$bias[finalIndex])
  data = sapply(data, removeLowBiasHelper, bias, finalIndex)
  return(data)
}


#' Helper for removeLowBias
#'
#' @param data Covariate information for each data dimension,
#' @param bias Bias information for all covariates
#' @param index Index to cut off
#'
#' @return
#' Returns \code{data} with proper codes removed
#' @export
removeLowBiasHelper <- function(data, bias, index) {
  #t = bias$bias[ffmatch(data$covariateId, bias$covariateId)] >= index
  #data = data[ffwhich(t,t==TRUE),]
  if (is.null(data)) {return(list(NULL))}
  t = ffbase::ffmatch(data$covariateId, ff::as.ff(bias$covariateId[1:index]))
  rows = ffbase::ffwhich(t, !is.na(t))
  if (is.null(rows)) {return(list(NULL))}
  data = data[rows,]
  data$covariateId = ffbase::droplevels.ff(data$covariateId)
  return(list(data))
}


#' Sequentially applies FUN to elements of data
#'
#' @description
#' This function applies and combines a function over successive elements of a list. For example, if \code{data} is a list of
#' \code{data.frame} and \code{FUN = rbind} it will return a \code{data.frame} of all the elements combined.
#'
#' @param data List of elements
#' @param FUN Function to apply, takes two arguments
#'
#' @return
#' Returns aggregated output of applying FUN to data
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


#' Combines covariate information from data dimensions with that from demographics and predefined.
#'
#' @description
#' This function combines the covariate information from all the data dimensions with that from demographics covariates and predefined covariates.
#'
#' @details
#' The \code{covariateValue} for the data dimensions is set to be 1. The \code{treatment} and \code{outcome} columns are dropped to match
#' the format of \code{cohortData}.
#'
#' @param newCovariates An ffdf object with columns \code{rowId}, \code{covariateId}, \code{treatment}, \code{outcome}. This includes all
#' of the data dimensions and the expanded covariates with "_sporadic" and "_frequent" tags.
#' @param demographicsCovariates An ffdf object with columns \code{rowId}, \code{covariateId}, \code{covariateValue}.
#' @param predefinedCovariates An ffdf object with columns \code{rowId}, \code{covariateId}, \code{covariateValue}.
#'
#' @return
#' Returns one ffdf object with columns \code{rowId}, \code{covariateId}, \code{covariateValue}.
#' @export
combineWithOtherCovariates <- function(newCovariates, demographicsCovariates, predefinedCovariates) {
  if (!is.null(newCovariates)) {
    newCovariates = ff::ffdf(rowId = newCovariates$rowId,
                         covariateId = newCovariates$covariateId,
                         covariateValue = ff::ff(vmode="double", initdata = 1, length=dim(newCovariates)[1]))
  }
  if (!is.null(demographicsCovariates)) {
    demographicsCovariates$covariateId = ff::ff(vmode="integer", initdata = factor(demographicsCovariates$covariateId[]))
  }
  if (!is.null(predefinedCovariates)) {
    predefinedCovariates$covariateId = ff::ff(vmode="integer", initdata = factor(predefinedCovariates$covariateId[]))
  }
  return(combineFunction(list(newCovariates, demographicsCovariates, predefinedCovariates), ffbase::ffdfrbind.fill))
}


#' Combines new covariateRef entries with those from demographics and predefined.
#'
#' @description
#' This function combines the new \code{covariateRef} entries for the data dimensions with the demographics \code{covariateRef} entries
#' from cohortData. The demographics \code{covariateId} column is changed into the new index given in \code{covariateIdIndex}.
#'
#' @param newCovariateRef New covariate reference entries generated via \code{createNewCovRef}
#' @param demographicsCovariateRef Covariate reference entries for demographics covariates
#' @param predefinedCovariateRef Covariate reference entries for predefined covariates
#' @param covariateIdIndex Index from \code{covariateId} of data dimensions and demographics to a numeric index that fits format of
#' \code{cohortData}. Generated by \code{createCovariateIndex}, has columns \code{covariateId} and \code{index}.
#'
#' @return
#' Returns combined \code{covariateRef}, with demographics covariates changed to new index.
#' @export
combineWithOtherCovariateRef <- function(newCovariateRef, demographicsCovariateRef, predefinedCovariateRef, covariateIdIndex) {
  if (!is.null(demographicsCovariateRef)) {
    demographicsCovariateRef$covariateId = covariateIdIndex$index[ffbase::ffmatch(demographicsCovariateRef$covariateId, covariateIdIndex$covariateId)]
    demographicsCovariateRef$covariateName = ffbase::droplevels.ff(demographicsCovariateRef$covariateName)
  }
  if (!is.null(predefinedCovariateRef)) {
    predefinedCovariateRef$covariateId = covariateIdIndex$index[ffbase::ffmatch(predefinedCovariateRef$covariateId, covariateIdIndex$covariateId)]
    predefinedCovariateRef$covariateName = ffbase::droplevels.ff(predefinedCovariateRef$covariateName)
  }
  return(combineFunction(list(newCovariateRef, demographicsCovariateRef, predefinedCovariateRef), ffbase::ffdfrbind.fill))
}


#' Generates new covariateRef entries
#'
#' @description
#' This function creates \code{covariateRef} entries for each data dimension. The new \code{covariateName} is taken from cohortData, if there
#' was no truncation of codes, or generated via the \code{truncatedNames} entry in \code{dimensionInfo}. Additionally, the appropriate
#' marker of "(once)", "(sporadic)", or "(frequent)" is appended to the beginning of the \code{covariateName}.
#'
#' @details
#' If there is no \code{truncatedNames} for a truncated code, the \code{covariateName} is "covariate name not available."
#' Furthermore, if a code was truncated for a data dimension, then \code{conceptId} is meaningless and will display 0.
#'
#' @param data Covariate information for data dimension, as ffdf object with columns \code{rowId}, \code{covariateId},
#' \code{treatment}, \code{outcome}
#' @param dimensionInfo The dimensionTable entry for data dimension, which includes the \code{truncatedNames} entry
#' @param rawCodes An ffdf object of the mapping from concept ID (column \code{TARGET_CONCEPT_ID}) to data dimension code
#' (column \code{SOURCE_CODE})
#' @param covariateIdIndex Index to convert \code{covariateId} of \code{data}, which is type \code{factor}, to a numeric index.
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
#'
#' @return
#' Returns ffdf object with columns: \describe{
#' \item{covariateId}{Newly indexed covariate identifier}
#' \item{covariateName}{New name for covariate, with appropriate recurrence tag}
#' \item{analysisId}{Analysis identifier for data dimension}
#' \item{conceptId}{Concept identifier for covariate. 0 if code was truncated} }
#' @export
createNewCovRef <- function(data, dimensionInfo, rawCodes, covariateIdIndex, cohortData) {
  if (is.null(data)) {return(NULL)}
  truncatedNames = dimensionInfo$truncatedNames
  analysisId = dimensionInfo$analysisId
  TRUNCATE = (dimensionInfo$truncate != "")

  uniqueCovariateId = unique(data$covariateId)
  uniqueCovariateIdRoots = gsub("_.*$","",uniqueCovariateId[])
  recurring = grep("_", uniqueCovariateId[], invert = FALSE)
  notRecurring = grep("_", uniqueCovariateId[], invert = TRUE)

  uniqueCovariateIdRecurrence = rep("", length(uniqueCovariateId[]))
  uniqueCovariateIdRecurrence[notRecurring] = "(once)"
  uniqueCovariateIdRecurrence[recurring] = gsub(".*_", "(", uniqueCovariateId[recurring])
  uniqueCovariateIdRecurrence[recurring] = gsub("$", ")", uniqueCovariateIdRecurrence[recurring])

  if (TRUNCATE) {
    newCovariateId = covariateIdIndex$index[ffbase::ffmatch(uniqueCovariateId, covariateIdIndex$covariateId)]
    if (truncatedNames == "") {
      covariateName = rep("covariate name not available", length(newCovariateId))
    } else {
      newNames = eval(parse(text = truncatedNames))
      covariateName = newNames$name[match(uniqueCovariateIdRoots, newNames$code)]
      f <- function(a,b){if (is.na(b)) {return(paste(a, "covariate name not available", sep=" "))} else return(paste(a,b,sep=" "))}
      covariateName = mapply(f,uniqueCovariateIdRecurrence, covariateName, USE.NAMES = FALSE)
    }

    d = length(newCovariateId)
    return(ff::ffdf(covariateId = newCovariateId,
                covariateName = ff::ff(vmode = "integer", factor(covariateName)),
                analysisId = ff::ff(vmode = "double", length = d, initdata = analysisId),
                conceptId = ff::ff(vmode = "double", length = d, initdata = 0)))
  } else {
    newCovariateId = covariateIdIndex$index[ffbase::ffmatch(uniqueCovariateId, covariateIdIndex$covariateId)]
    conceptIds = rawCodes$TARGET_CONCEPT_ID[ffbase::ffmatch(ff::ff(vmode = "integer", factor(uniqueCovariateIdRoots)), rawCodes$SOURCE_CODE)]
    #covariateName = cohortData$covariateRef$covariateName[ffmatch(conceptIds, cohortData$covariateRef$conceptId)]
    foo = ff::ffdf(x1 = conceptIds, x2 = conceptIds)
    t = ff::ffapply(X=foo, AFUN=function(a){return(ffbase::ffwhich(cohortData$covariateRef, cohortData$covariateRef$conceptId==a["x1"] & cohortData$covariateRef$analysisId == analysisId))}, MARGIN=1, RETURN=TRUE, CFUN="list")[[1]]
    t = combineFunction(t, ffbase::ffappend)
    covariateName = cohortData$covariateRef$covariateName[t]

    f <- function(a,b){if (is.na(b)) {return(a)} else return(paste(a,b,sep=" "))}
    covariateName = mapply(f, uniqueCovariateIdRecurrence, covariateName[], USE.NAMES = FALSE)

    d = length(newCovariateId)

    return(ff::ffdf(covariateId = newCovariateId,
                covariateName = ff::ff(vmode = "integer", factor(covariateName)),
                analysisId = ff::ff(vmode = "double", length = d, initdata = analysisId),
                conceptId = conceptIds))
  }
}


#' Creates numeric covariateId index
#'
#' @description
#' Creates numeric covariateId index from covariates.
#'
#' @param covariates Covariate information of combined data dimension and demographic data. Includes column \code{covariateId}
#'
#' @return
#' Returns ffdf object with columns \code{covariateId} and \code{index}
#' @export
createCovariateIndex <- function(covariates) {
  if (is.null(covariates)) {return(NULL)}
  uniqueCovariateId = unique(covariates$covariateId)
  d = length(uniqueCovariateId)
  result = ff::ffdf(covariateId=uniqueCovariateId,index=ff::ff(vmode="double",1:d))
  return(result)
}


#' Converts covariateId to new index
#'
#' @param covariates Covariates to convert to new index; covariateId is a factor. Has columns \code{rowId}, \code{covariateId}, \code{covariateValue}
#' @param index Index to use. Has column \code{covariateId} which match those from \code{covariates}, and column \code{index}
#'
#' @return
#' Returns input \code{covariates} with \code{covariateId} converted to new index, which is numeric
#' @export
convertCovariateId <- function(covariates, index) {
  if (is.null(covariates)) {return(NULL)}
  covariates$covariateId = index$index[ffbase::ffmatch(covariates$covariateId, index$covariateId)]
  return(covariates)
}

#' Retrieves demographics elements of covariate Ref
#'
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
#' @param analysisIds Vector of analysis identifiers associated with demographic covariates.
#'
#' @return
#' Returns ffdf of elements of \code{cohortData$covariateRef} that relate to demographic information.
#' @export
getDemographicsCovariateRef <- function(cohortData, analysisIds) {
  if (is.null(analysisIds)) {return(NULL)}
  t = ffbase::ffmatch(cohortData$covariateRef$analysisId, ff::as.ff(analysisIds))
  result = cohortData$covariateRef[ffbase::ffwhich(t,!is.na(t)),]
  result$covariateName = ffbase::droplevels.ff(result$covariateName)
  return(result)
}


#' Retrieves demographics elements of covariates
#'
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
#' @param covariateRef Covariate reference information associated with demographic covariates.
#'
#' @return
#' Returns ffdf of elements of \code{cohortData$covariates} that relate to demographic information.
#' @export
getDemographicsCovariates <- function(cohortData, covariateRef) {
  if (is.null(covariateRef)) {return(NULL)}
  t = ffbase::ffmatch(cohortData$covariates$covariateId, covariateRef$covariateId)
  return(cohortData$covariates[ffbase::ffwhich(t,!is.na(t)),])
}


#' Retrieves predefined elements of covariate Ref
#'
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
#' @param conceptIds Vector of concept identifiers associated with predfined covariates.
#'
#' @return
#' Returns ffdf of elements of \code{cohortData$covariateRef} that relate to predefined covariates.
#' @export
getPredefinedCovariateRef <- function(cohortData, conceptIds) {
  if (is.null(conceptIds)) {return(NULL)}
  t = ffbase::ffmatch(cohortData$covariateRef$conceptId, ff::as.ff(conceptIds))
  result = cohortData$covariateRef[ffbase::ffwhich(t,!is.na(t)),]
  result$covariateName = ffbase::droplevels.ff(result$covariateName)
  return(result)
}


#' Retrieves prefefined elements of covariates
#'
#' @param cohortData \code{cohortData} object constructed by \code{getDbCohortData}
#' @param predefinedCovariateRef Ffdf of covariate reference entries associated with predefined covariates.
#'
#' @return
#' Returns ffdf of elements of \code{cohortData$covariates} that relate to predefined covariates.
#' @export
getPredefinedCovariates <- function(cohortData, predefinedCovariateRef) {
  if (is.null(predefinedCovariateRef)) {return(NULL)}
  t = ffbase::ffmatch(cohortData$covariates$covariateId, predefinedCovariateRef$covariateId)
  return(cohortData$covariates[ffbase::ffwhich(t,!is.na(t)),])
}


#' Saves a list of ffdf objects
#' @description
#' This function saves a list of ffdf objects in a new folder. The names of the ffdf elements are also preserved.
#' @param ffdfList List of ffdf objects to save
#' @param file Name of folder to create / save in
#' @export
saveFfdfList <- function (ffdfList, file) {
  front = "ffbase::save.ffdf("
  back = "dir = file, overwrite=T)"

  middle = ""
  d = length(ffdfList)

  for (i in 1:d) {
    text = paste("item", i, "<- ffdfList[[", i, "]]", sep="")
    eval(parse(text=text))
    middle = paste(middle, "item", i,",", sep="")
  }

  text = paste(front, middle, back, sep="")
  eval(parse(text=text))

  listNames = names(ffdfList)
  nItems = length(ffdfList)
  save(listNames, file = file.path(file, "listNames.Rdata"))
  save(nItems, file = file.path(file, "nItems.Rdata"))
}


#' Loads list of ffdf objects
#' @description
#' Loads list of ffdf objects saved by \code{saveFfdfList}
#' @param file Name of folder
#' @export
loadFfdfList <- function (file) {

  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))

  temp <- setwd(file)
  absolutePath <- setwd(temp)

  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  load(file.path(absolutePath, "listNames.Rdata"), e)
  load(file.path(absolutePath, "nItems.Rdata"), e)

  listNames = get("listNames", envir = e)
  nItems = get("nItems", envir = e)

  front = "list("
  back = ")"
  middle = ""

  for (i in 1:nItems) {
    text = paste(if(!is.null(listNames)) listNames[i] else "",
                 if(!is.null(listNames)) "=" else "",
                 "get(\"item", i, "\",envir=e)", sep="")
    if (i==1) {
      middle = paste(middle, text, sep="")
    } else {
      middle = paste(middle, text, sep=",")
    }
  }
  text = paste(front, middle, back, sep="")
  result = eval(parse(text=text))
  return(result)
}


#' Function used to load and process ICD9 codes
#'
#' @description
#' This function was used in processing ICD9Dx.txt
#' @param file File name
#'
#' @return
#' Returns \code{data.frame} with columns: \describe{
#' \item{code}{Truncated ICD9 codes to 3 digits}
#' \item{name}{Name of ICD9 code} }
#' @export
processICD9Dx <- function(file) {

  icd9codes = read.delim(file, header=FALSE)
  f <- function(x){return(as.character(x))}
  codes = sapply(icd9codes, f)
  codes = as.vector(codes)

  f <- function(x){return(strsplit(x, " "))}
  codes = sapply(codes,f)

  f <- function(x){return(list(data.frame(code=x[[1]], name=combineFunction(x[-1], function(a,b){return(paste(a,b,sep=" "))}))))}
  codes = sapply(codes, f)
  codes = combineFunction(codes, rbind)
  codes = codes[grep("^...0{0,}$", codes$code),]
  codes$code = substr(codes$code, 1, 3)

  return(codes)
}


#' Loads ICD9 truncated names table
#'
#' @description
#' This function loads the saved table that contains the names of ICD9 codes truncated to 3 digits from the file ICD9Dx.Rdata
#' @param file Name of file
#'
#' @return
#' Returns \code{data.frame} with columns: \describe{
#' \item{code}{Truncated ICD9 codes to 3 digits}
#' \item{name}{Name of ICD9 code} }
#' @export
getICD9Dx <- function(file) {
  e = new.env()
  load(file, envir = e)
  return(get("codes", e))
}


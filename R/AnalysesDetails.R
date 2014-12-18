# @file AnalysesDetails.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
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

#' @export
createCmAnalysisDetails <- function (analysisId,
                                     washoutWindow = 183,
                                     indicationLookbackWindow = 183,
                                     studyStartDate = "",
                                     studyEndDate = "",
                                     maxOutcomeCount = 1,
                                     outcomeConditionTypeConceptIds = "",
                                     useCovariateDemographics = TRUE,
                                     useCovariateConditionOccurrence = TRUE,
                                     useCovariateConditionEra = TRUE,
                                     useCovariateConditionGroup = TRUE,
                                     useCovariateDrugExposure = TRUE,
                                     useCovariateDrugEra = TRUE,
                                     useCovariateDrugGroup = TRUE,
                                     useCovariateProcedureOccurrence = TRUE,
                                     useCovariateProcedureGroup = TRUE,
                                     useCovariateObservation = TRUE,
                                     useCovariateConceptCounts = TRUE,
                                     useCovariateRiskScores = TRUE,
                                     useCovariateInteractionYear = TRUE,
                                     useCovariateInteractionMonth = FALSE,
                                     deleteCovariatesSmallCount = FALSE,
                                     propensityModelPrior = "",
                                     trimByPs = FALSE,
                                     trimByPsFraction = 0.05,
                                     trimByPSToEquipoise = TRUE,
                                     trimEquipoiseLowerBound = 0.25,
                                     trimEquipoiseUpperBound = 0.75,
                                     matchOnPs = TRUE,
                                     matchCaliper = 0.25,
                                     matchCaliperScale = "standardized",
                                     matchMaxRatio = 100,
                                     matchCovariates = c(),
                                     stratifyOnPs = FALSE,
                                     stratifyOnPsStrata = 5,
                                     stratifyCovariates = c(),
                                     outcomeModelPrior = "",
                                     outcomeModel = "cox",
                                     outcomeModelRiskWindowStart = 1,
                                     outcomeModelRiskWindowEnd = 30,
                                     outcomeModelAddExposureDaysToEnd = TRUE,
                                     outcomeModelUseStrata = TRUE,
                                     outcomeModelUseCovariates = TRUE
){
  #First: get the default values:
  analysisDetails <- list()
  for (name in names(formals(createSccAnalysisDetails))){
    analysisDetails[[name]] = get(name)
  }
  
  #Next: overwrite defaults with actual values if specified:
  #values <- as.list(match.call())
  #Note: need this funky code to make sure parameters are stored as values, not symbols:
  values <- c(list(as.character(match.call()[[1]])),lapply(as.list(match.call())[-1],function(x) eval(x,envir=sys.frame(-3))))
  for (name in names(values)){
    if (name %in% names(analysisDetails))
      analysisDetails[[name]] = values[[name]]
  }
  
  class(analysisDetails) <- "cmAnalysisDetails"
  analysisDetails
}

#' @export
appendToCmAnalysesDetails <- function(cmAnalysisDetails,cmAnalysesDetails = NULL){
  stopifnot(class(cmAnalysisDetails) == "cmAnalysisDetails")
  if (is.null(cmAnalysesDetails)){
    cmAnalysesDetails = list()
    class(cmAnalysesDetails) <- "cmAnalysesDetails"
  }
  cmAnalysesDetails[[length(cmAnalysesDetails)+1]] <- cmAnalysisDetails
  cmAnalysesDetails
}

#' @export
writeCmAnalysesDetailsToFile <- function(cmAnalysesDetails, file){
  stopifnot(class(cmAnalysesDetails) == "cmAnalysesDetails")
  
  #Convert cmAnalysesDetails to a data.frame, converting any nested vectors into semicolon-delimited strings:
  f <- sccAnalysesDetails
  d <- data.frame()
  for (row in 1:length(f)){
    class(f[[row]]) <- "list"
    for (column in 1:length(f[[row]])){
      if ((class(f[[row]][[column]]) == "numeric") && (length(f[[row]][[column]]) > 1))
        f[[row]][[column]] = paste(f[[row]][[column]],collapse=";")
    }
    d <- rbind(d,as.data.frame(f[[row]]))
  }
  
  write.csv(d,file=file, row.names=FALSE)
}

#' @export
readCmAnalysesDetailsFromFile <- function(file){
  d <- read.csv(file)
  d[is.na(d)] <- ""
  cmAnalysesDetails <- list()
  for (row in 1:nrow(d)){
    cmAnalysisDetails <- as.list(d[row,])
    for (column in c("drugTypeConceptIdList","conditionTypeConceptIdList","conditionTypeConceptIdList","genderConceptIdList")){
      cmAnalysisDetails[[column]] <- as.numeric(unlist(strsplit(as.character(d[row,column]),";")))
    }
    class(cmAnalysisDetails) = "mAnalysisDetails"
    cmAnalysesDetails[[length(cmAnalysesDetails)+1]] <- cmAnalysisDetails
  }
  class(cmAnalysesDetails) <- "cmAnalysesDetails"
  return(cmAnalysesDetails)
}

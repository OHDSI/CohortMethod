# @file CohortMethod.R
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
#
# @author Observational Health Data Sciences and Informatics
# @author Patrick Ryan
# @author Marc Suchard
# @author Martijn Schuemie

snakeCaseToCamelCase <- function(string){
  string <- tolower(string)
  for(letter in letters){
    string = gsub(paste("_",letter,sep=""),toupper(letter),string)
  }
  return(string)
}

#' Get the cohort data from the server
#'
#' @description
#' Todo: add description
#'
#' @details
#' Todo: add details
#'
#' @param connectionDetails  	An R object of type \code{connectionDetails} created using the function \code{createConnectionDetails} in the \code{DatabaseConnector} package.
#' @param cdmSchema 		
#' @param resultsSchema 		
#' @param targetDrugConceptId 		
#' @param comparatorDrugConceptId 		
#' @param indicationConceptIds 		
#' @param washoutWindow 		
#' @param indicationLookbackWindow 		
#' @param studyStartDate 		
#' @param studyEndDate 		
#' @param exclusionConceptIds 		
#' @param outcomeConceptIds 		
#' @param outcomeConditionTypeConceptIds 		
#' @param maxOutcomeCount 		
#' @param exposureSchema   	
#' @param exposureTable 		
#' @param outcomeSchema
#' @param outcomeTable 		
#' @param useCovariateDemographics 		
#' @param useCovariateConditionOccurrence 		
#' @param useCovariateConditionEra 		
#' @param useCovariateConditionGroup 		
#' @param useCovariateDrugExposure 		
#' @param useCovariateDrugEra 		
#' @param useCovariateDrugGroup 		
#' @param useCovariateProcedureOccurrence 		
#' @param useCovariateProcedureGroup 		
#' @param useCovariateObservation 		
#' @param useCovariateConceptCounts 		
#' @param useCovariateRiskScores 		
#' @param useCovariateInteractionYear 		
#' @param useCovariateInteractionMonth 		
#' @param excludedCovariateConceptIds 		
#' @param deleteCovariatesSmallCount 		
#' 
#' @return
#' Returns an object of type \code{cohortData}, containing information on the cohorts, their outcomes,
#' and baseline covariates.
#'
#' @export
getDbCohortData <- function(connectionDetails,
                            cdmSchema = "CDM4_SIM",
                            resultsSchema = "scratch",
                            targetDrugConceptId = 755695,
                            comparatorDrugConceptId = 739138,
                            indicationConceptIds = 439926,
                            washoutWindow = 183,
                            indicationLookbackWindow = 183,
                            studyStartDate = "",
                            studyEndDate = "",
                            exclusionConceptIds = c(4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019),
                            outcomeConceptIds = 194133,
                            outcomeConditionTypeConceptIds = c(38000215,38000216,38000217,38000218,38000183,38000232),
                            maxOutcomeCount = 1,
                            exposureSchema = cdmSchema,
                            exposureTable = "drug_era",
                            outcomeSchema = cdmSchema,
                            outcomeTable = "condition_occurrence",
                            useCovariateDemographics = TRUE,
                            useCovariateConditionOccurrence = TRUE,
                            useCovariateConditionEra = FALSE,
                            useCovariateConditionGroup = FALSE,
                            useCovariateDrugExposure = FALSE,
                            useCovariateDrugEra = FALSE,
                            useCovariateDrugGroup = FALSE,
                            useCovariateProcedureOccurrence = FALSE,
                            useCovariateProcedureGroup = FALSE,
                            useCovariateObservation = FALSE,
                            useCovariateConceptCounts = FALSE,
                            useCovariateRiskScores = FALSE,
                            useCovariateInteractionYear = FALSE,
                            useCovariateInteractionMonth = FALSE,
                            excludedCovariateConceptIds = c(4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019),
                            deleteCovariatesSmallCount = 100){
  renderedSql <- SqlRender::loadRenderTranslateSql("CohortMethod.sql",
                                                   packageName = "CohortMethod",
                                                   dbms = connectionDetails$dbms,
                                                   cdm_schema = cdmSchema,
                                                   results_schema = resultsSchema,
                                                   target_drug_concept_id = targetDrugConceptId,
                                                   comparator_drug_concept_id = comparatorDrugConceptId,
                                                   indication_concept_ids = indicationConceptIds,
                                                   washout_window = washoutWindow,
                                                   indication_lookback_window = indicationLookbackWindow,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   exclusion_concept_ids = exclusionConceptIds,
                                                   outcome_concept_ids = outcomeConceptIds,
                                                   outcome_condition_type_concept_ids = outcomeConditionTypeConceptIds,
                                                   max_outcome_count = maxOutcomeCount,
                                                   exposure_schema = exposureSchema,
                                                   exposure_table = exposureTable,
                                                   outcome_schema = outcomeSchema,
                                                   outcome_table = outcomeTable,
                                                   use_covariate_demographics = useCovariateDemographics,
                                                   use_covariate_condition_occurrence = useCovariateConditionOccurrence,
                                                   use_covariate_condition_era = useCovariateConditionEra,
                                                   use_covariate_condition_group = useCovariateConditionGroup,
                                                   use_covariate_drug_exposure = useCovariateDrugExposure,
                                                   use_covariate_drug_era = useCovariateDrugEra,
                                                   use_covariate_drug_group = useCovariateDrugGroup,
                                                   use_covariate_procedure_occurrence = useCovariateProcedureOccurrence,
                                                   use_covariate_procedure_group = useCovariateProcedureGroup,
                                                   use_covariate_observation = useCovariateObservation,
                                                   use_covariate_concept_counts = useCovariateConceptCounts,
                                                   use_covariate_risk_scores = useCovariateRiskScores,
                                                   use_covariate_interaction_year = useCovariateInteractionYear,
                                                   use_covariate_interaction_month = useCovariateInteractionMonth,
                                                   excluded_covariate_concept_ids = excludedCovariateConceptIds,
                                                   delete_covariates_small_count = deleteCovariatesSmallCount)
  
  conn <- DatabaseConnector::connect(connectionDetails)
  
  writeLines("Executing multiple queries. This could take a while")
  DatabaseConnector::executeSql(conn,renderedSql)
  
  cohortSql <-"SELECT row_id, cohort_id AS treatment, person_id, datediff(dd, cohort_start_date, observation_period_end_date) AS time_to_obs_period_end, datediff(dd, cohort_start_date, cohort_end_date) AS time_to_cohort_end FROM #cohort_person ORDER BY row_id"
  cohortSql <- SqlRender::translateSql(cohortSql,"sql server",connectionDetails$dbms)$sql
  
  covariateSql <-"SELECT row_id, covariate_id,covariate_value FROM #cohort_covariate ORDER BY row_id, covariate_id"
  covariateSql <- SqlRender::translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql
  
  outcomeSql <-"SELECT row_id, outcome_id, time_to_event FROM #cohort_outcome ORDER BY outcome_id, row_id"
  outcomeSql <- SqlRender::translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql
  
  excludeSql <-"SELECT row_id, outcome_id FROM #cohort_excluded_person ORDER BY outcome_id, row_id"
  excludeSql <- SqlRender::translateSql(excludeSql,"sql server",connectionDetails$dbms)$sql
  
  covariateRefSql <-"SELECT covariate_id, covariate_name, analysis_id, concept_id  FROM #cohort_covariate_ref ORDER BY covariate_id"
  covariateRefSql <- SqlRender::translateSql(covariateRefSql,"sql server",connectionDetails$dbms)$sql
  
  writeLines("Fetching data from server")
  start <- Sys.time()
  outcomes <- DatabaseConnector::dbGetQuery.ffdf(conn,outcomeSql)
  cohorts <-  DatabaseConnector::dbGetQuery.ffdf(conn,cohortSql)
  covariates <- DatabaseConnector::dbGetQuery.ffdf(conn,covariateSql)
  exclude <- DatabaseConnector::dbGetQuery.ffdf(conn,excludeSql)
  covariateRef <- DatabaseConnector::dbGetQuery.ffdf(conn,covariateRefSql)
  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta,3), attr(delta,"units")))
  #Remove temp tables:
  if (connectionDetails$dbms == "oracle"){
    renderedSql <- SqlRender::loadRenderTranslateSql("CMRemoveTempTables.sql",
                                                     packageName = "CohortMethod",
                                                     dbms = connectionDetails$dbms)
    
    DatabaseConnector::executeSql(conn,renderedSql,progressBar = FALSE,reportOverallTime=FALSE,profile=TRUE)
  }
  colnames(outcomes) <- snakeCaseToCamelCase(colnames(outcomes))
  colnames(cohorts) <- snakeCaseToCamelCase(colnames(cohorts))
  colnames(covariates) <- snakeCaseToCamelCase(colnames(covariates))
  colnames(exclude) <- snakeCaseToCamelCase(colnames(exclude))
  colnames(covariateRef) <- snakeCaseToCamelCase(colnames(covariateRef))
  dummy <- RJDBC::dbDisconnect(conn)
  metaData <- list(sql = renderedSql,
                   targetDrugConceptId = targetDrugConceptId,
                   comparatorDrugConceptId = comparatorDrugConceptId,
                   outcomeConceptIds = outcomeConceptIds,
                   call = match.call()
  )
  
  
  result <- list(outcomes = outcomes,
                 cohorts = cohorts,
                 covariates = covariates,
                 exclude = exclude,
                 covariateRef = covariateRef,
                 metaData = metaData
  )
  
  class(result) <- "cohortData"
  return(result)
}

#' Save the cohort data to folder
#'
#' @description
#' \code{saveCohortData} saves an object of type cohortData to folder.
#' 
#' @param cohortData          An object of type \code{cohortData} as generated using \code{getDbCohortData}.
#' @param file                The name of the folder where the data will be written. The folder should
#' not yet exist.
#' 
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'  
#' @examples 
#' #todo
#' 
#' @export
saveCohortData <- function(cohortData, file){
  if (missing(cohortData))
    stop("Must specify cohortData")
  if (missing(file))
    stop("Must specify file")
  if (class(cohortData) != "cohortData")
    stop("Data not of class cohortData")
  
  out1 <- cohortData$outcomes
  out2 <- cohortData$cohorts
  out3 <- cohortData$covariates
  out4 <- cohortData$exclude
  out5 <- cohortData$covariateRef
  ffbase::save.ffdf(out1,out2,out3,out4,out5,dir=file)
  metaData <- cohortData$metaData
  save(metaData,file=file.path(file,"metaData.Rdata"))
}

#' Load the cohort data from a folder
#'
#' @description
#' \code{loadCohortData} loads an object of type cohortData from a folder in the file system.
#' 
#' @param file                The name of the folder containing the data.
#' 
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#' 
#' @return
#' An object of class cohortData.
#'  
#' @examples 
#' #todo
#' 
#' @export
loadCohortData <- function(file){
  if (!file.exists(file))
    stop(paste("Cannot find folder",file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder",file))
  
  temp <- setwd(file)
  absolutePath <- setwd(temp)
  
  e <- new.env()  
  load.ffdf(absolutePath,e)
  load(file.path(absolutePath,"metaData.Rdata"),e)
  result <- list(outcomes = get("out1", envir=e),
                 cohorts = get("out2", envir=e),
                 covariates = get("out3", envir=e),
                 exclude = get("out4", envir=e),
                 covariateRef = get("out5", envir=e),
                 metaData = mget("metaData",envir=e,ifnotfound=list(NULL))[[1]] #For backwards compatibility
  )
  class(result) <- "cohortData"
  rm(e)
  return(result)
}

print.cohortData <- function(cohortData){
  writeLines("CohortData object")
  writeLines("")
  writeLines(paste("Treatment concept ID:",cohortData$metaData$targetDrugConceptId))
  writeLines(paste("Comparator concept ID:",cohortData$metaData$comparatorDrugConceptId))
  writeLines(paste("Outcome concept ID(s):",paste(cohortData$metaData$outcomeConceptIds,collapse=",")))
}

summary.cohortData <- function(cohortData){
  treatedPersons = ffbase::sum.ff(cohortData$cohorts$treatment)  
  comparatorPersons = nrow(cohortData$cohorts)-treatedPersons
  outcomeCounts = data.frame(outcomeConceptId = cohortData$metaData$outcomeConceptIds, eventCount = 0, personCount = 0)
  for (i in 1:nrow(outcomeCounts)){
    outcomeCounts$eventCount[i] = ffbase::sum.ff(cohortData$outcomes$outcomeId == cohortData$metaData$outcomeConceptIds[i])
    outcomeCounts$personCount[i] = length(ffbase::unique.ff(cohortData$outcomes$rowId[cohortData$outcomes$outcomeId == cohortData$metaData$outcomeConceptIds[i]]) )   
  }
  
  result <- list(metaData = cohortData$metaData,
                 treatedPersons = treatedPersons,
                 comparatorPersons = comparatorPersons,
                 outcomeCounts = outcomeCounts,
                 covariateCount = nrow(cohortData$covariateRef),
                 covariateValueCount = nrow(cohortData$covariates)                 
  )
  class(result) <- "summary.cohortData"
  return(result)
}

print.summary.cohortData <- function(data){
  writeLines("CohortData object summary")
  writeLines("")
  writeLines(paste("Treatment concept ID:",data$metaData$targetDrugConceptId))
  writeLines(paste("Comparator concept ID:",data$metaData$comparatorDrugConceptId))
  writeLines(paste("Outcome concept ID(s):",paste(data$metaData$outcomeConceptIds,collapse=",")))
  writeLines("")
  writeLines(paste("Treated persons:",paste(data$treatedPersons)))
  writeLines(paste("Comparator persons:",paste(data$comparatorPersons)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- data$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeConceptId
  outcomeCounts$outcomeConceptId <- NULL
  colnames(outcomeCounts) <- c("Event count","Person count")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Covariates:")
  writeLines(paste("Number of covariates:",data$covariateCount))
  writeLines(paste("Number of non-zero covariate values:",data$covariateValueCount)) 
}


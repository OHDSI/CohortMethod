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

#' @export
dbGetCohortData <- function(connectionDetails, 
                            cdmSchema = "CDM4_SIM",
                            resultsSchema = cdmSchema,
                            targetDrugConceptId = 755695,
                            comparatorDrugConceptIds = 739138,
                            indicationConceptIds = 439926,
                            washoutWindow = 183,
                            indicationLookbackWindow = 183,
                            exposureExtensionWindow = 7,
                            studyStartDate = "",
                            studyEndDate = "",
                            exclusionConceptIds = c(4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019),
                            outcomeConceptIds = c(194133),
                            outcomeConditionTypeConceptIds = c(38000215,38000216,38000217,38000218,38000183,38000232),
                            maxOutcomeCount = 1){
  renderedSql <- loadRenderTranslateSql("CohortMethod.sql",
                                        packageName = "CohortMethod",
                                        dbms = connectionDetails$dbms,
                                        CDM_schema = cdmSchema,
                                        results_schema = resultsSchema,
                                        target_drug_concept_id = targetDrugConceptId,
                                        comparator_drug_concept_ids = comparatorDrugConceptIds,
                                        indication_concept_ids = indicationConceptIds,
                                        washout_window = washoutWindow,
                                        indication_lookback_window = indicationLookbackWindow,
                                        exposure_extension_window = exposureExtensionWindow,
                                        study_start_date = studyStartDate,
                                        study_end_date = studyEndDate,
                                        exclusion_concept_ids = exclusionConceptIds,
                                        outcome_concept_ids = outcomeConceptIds,
                                        outcome_condition_type_concept_ids = outcomeConditionTypeConceptIds,
                                        max_outcome_count = maxOutcomeCount)
  
  conn <- connect(connectionDetails)
  
  writeLines("Executing multiple queries. This could take a while")
  executeSql(conn,connectionDetails$dbms,renderedSql)
  
  outcomeSql <-"SELECT person_id AS row_id,outcome_concept_id,time_to_outcome FROM #outcomes ORDER BY person_id"
  outcomeSql <- translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql
  
  cohortSql <-"SELECT cohort_id AS treatment, person_id AS row_id, datediff(dd, cohort_start_date, cohort_censor_date) AS time_to_censor FROM #cohorts ORDER BY person_id"
  cohortSql <- translateSql(cohortSql,"sql server",connectionDetails$dbms)$sql
  
  covariateSql <-"SELECT person_id AS row_id,covariate_id,covariate_value FROM #covariates ORDER BY person_id,covariate_id"
  covariateSql <- translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql
  
  writeLines("Fetching data from server")
  start <- Sys.time()
  outcomes <- dbGetQuery.ffdf(conn,outcomeSql)
  cohorts <-  dbGetQuery.ffdf(conn,cohortSql)
  covariates <- dbGetQuery.ffdf(conn,covariateSql)
  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta,3), attr(delta,"units")))
  #Remove temp tables:
  renderedSql <- loadRenderTranslateSql("CMRemoveTempTables.sql",
                                        packageName = "CohortMethod",
                                        dbms = connectionDetails$dbms,
                                        CDM_schema = cdmSchema)
  
  executeSql(conn,connectionDetails$dbms,renderedSql,progressBar = FALSE,reportTime=FALSE)
  
  colnames(outcomes) <- toupper(colnames(outcomes))
  colnames(cohorts) <- toupper(colnames(cohorts))
  colnames(covariates) <- toupper(colnames(covariates))
  dummy <- dbDisconnect(conn)
  result <- list(outcomes = outcomes,
                 cohorts = cohorts,
                 covariates = covariates 
  )
  
  class(result) <- "cohortData"
  result
}

#' @export
save.cohortData <- function(cohortData, file){
  if (missing(cohortData))
    stop("Must specify cohortData")
  if (missing(file))
    stop("Must specify file")
  if (class(cohortData) != "cohortData")
    stop("Data not of class cohortData")
  
  out1 <- cohortData$outcomes
  out2 <- cohortData$cohorts
  out3 <- cohortData$covariates
  save.ffdf(out1,out2,out3,dir=file)
}

#' @export
load.cohortData <- function(file){
  if (!file.exists(file))
    stop(paste("Cannot find folder",file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder",file))
  
  
  e <- new.env()
  load.ffdf(file,e)
  result <- list(outcomes = get("out1", envir=e),
                 cohorts = get("out2", envir=e),
                 covariates = get("out3", envir=e),
                 useFf = TRUE    
  )
  class(result) <- "cohortData"
  rm(e)
  result 
}

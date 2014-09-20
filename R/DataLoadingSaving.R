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
                            cdmSchema = "CDM_TRUVEN_MDCD",
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
                            exposureTable = "DRUG_ERA",
                            outcomeTable = "CONDITION_OCCURRENCE",
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
  renderedSql <- loadRenderTranslateSql("cohortMethod.sql",
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
                                        exposure_table = exposureTable,
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
  
  conn <- connect(connectionDetails)
  
  writeLines("Executing multiple queries. This could take a while")
  executeSql(conn,renderedSql)
  
  cohortSql <-"SELECT row_id, cohort_id AS treatment, person_id, datediff(dd, cohort_start_date, observation_period_end_date) AS time_to_obs_period_end, datediff(dd, cohort_start_date, cohort_end_date) AS time_to_cohort_end FROM #cohort_person ORDER BY row_id"
  cohortSql <- translateSql(cohortSql,"sql server",connectionDetails$dbms)$sql
  
  covariateSql <-"SELECT row_id, covariate_id,covariate_value FROM #cohort_covariate ORDER BY row_id, covariate_id"
  covariateSql <- translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql
  
  outcomeSql <-"SELECT row_id, outcome_id, time_to_event FROM #cohort_outcome ORDER BY outcome_id, row_id"
  outcomeSql <- translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql
    
  excludeSql <-"SELECT row_id, outcome_id FROM #cohort_excluded_person ORDER BY outcome_id, row_id"
  excludeSql <- translateSql(excludeSql,"sql server",connectionDetails$dbms)$sql

  covariateRefSql <-"SELECT covariate_id, covariate_name, analysis_id, concept_id  FROM #cohort_covariate_ref ORDER BY covariate_id"
  covariateRefSql <- translateSql(covariateRefSql,"sql server",connectionDetails$dbms)$sql
  
  writeLines("Fetching data from server")
  start <- Sys.time()
  outcomes <- dbGetQuery.ffdf(conn,outcomeSql)
  cohorts <-  dbGetQuery.ffdf(conn,cohortSql)
  covariates <- dbGetQuery.ffdf(conn,covariateSql)
  exclude <- dbGetQuery.ffdf(conn,excludeSql)
  covariateRef <- dbGetQuery.ffdf(conn,covariateRefSql)
  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta,3), attr(delta,"units")))
  #Remove temp tables:
  renderedSql <- loadRenderTranslateSql("CMRemoveTempTables.sql",
                                        packageName = "CohortMethod",
                                        dbms = connectionDetails$dbms,
                                        CDM_schema = cdmSchema)
  
  executeSql(conn,renderedSql,progressBar = FALSE,reportOverallTime=FALSE)
  
  colnames(outcomes) <- toupper(colnames(outcomes))
  colnames(cohorts) <- toupper(colnames(cohorts))
  colnames(covariates) <- toupper(colnames(covariates))
  colnames(exclude) <- toupper(colnames(exclude))
  colnames(covariateRef) <- toupper(colnames(covariateRef))
  dummy <- dbDisconnect(conn)
  result <- list(outcomes = outcomes,
                 cohorts = cohorts,
                 covariates = covariates,
                 exclude = exclude,
                 covariateRef = covariateRef
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
  out4 <- cohortData$exclude
  out5 <- cohortData$covariateRef
  save.ffdf(out1,out2,out3,out4,out5,dir=file)
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
                 exclude = get("out4", envir=e),
                 covariateRef = get("out5", envir=e)
  )
  class(result) <- "cohortData"
  rm(e)
  result 
}

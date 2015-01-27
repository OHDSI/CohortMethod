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
#' This function executes a large set of SQL statements against the database in OMOP CDM format to extract the data needed to perform the analysis.
#'
#' @details
#' Based on the parameters, the treatment and comparator cohorts are constructed. Baseline covariates at or before the index date are extracted, as well as outcomes occurring on or after the index date.
#' The treatment and comparator cohorts can be identified using the drug_era table, or through user-defined cohorts in a cohort table either inside the CDM instance or in a separate schema. 
#' Similarly, outcomes are identified using the condition_occurrence or condition_era table, or through user-defined cohorts in a cohort table either inside the CDM instance or in a separate schema. 
#' Covariates are automatically extracted from the appropriate tables within the CDM.
#'
#' @param connectionDetails  	An R object of type \code{connectionDetails} created using the function \code{createConnectionDetails} in the \code{DatabaseConnector} package.
#' 
#' @param sourceName    The name of the source database, to be used to name temporary files and distinguish results within organizations with multiple databases.   	
#' @param cdmSchema    The name of the database schema that contains the OMOP CDM instance.  Requires read permissions to this database.   		
#' @param resultsSchema    The name of the database schema that is the location where you want all temporary tables to be managed and all results tables to persist.  Requires create/insert permissions to this database. 	
#' @param exposureSchema     The name of the database schema that is the location where the exposure data used to define the exposure cohorts is available.  If exposureTable = DRUG_ERA, exposureSchema is not used by assumed to be cdmSchema.  Requires read permissions to this database.    
#' @param exposureTable   The tablename that contains the exposure cohorts.  If exposureTable <> DRUG_ERA, then expectation is exposureTable has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.	
#' @param outcomeSchema     The name of the database schema that is the location where the data used to define the outcome cohorts is available.  If exposureTable = CONDITION_ERA, exposureSchema is not used by assumed to be cdmSchema.  Requires read permissions to this database.    
#' @param outcomeTable   The tablename that contains the outcome cohorts.  If outcomeTable <> CONDITION_OCCURRENCE, then expectation is outcomeTable has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE. 	
#'  	
#' @param targetDrugConceptId 		A unique identifier to define the target cohort.  If exposureTable = DRUG_ERA, targetDrugConceptId is a CONCEPT_ID and all descendant concepts within that CONCEPT_ID will be used to define the cohort.  If exposureTable <> DRUG_ERA, targetDrugConceptId is used to select the COHORT_DEFINITION_ID in the cohort-like table.
#' @param comparatorDrugConceptId   	A unique identifier to define the comparator cohort.  If exposureTable = DRUG_ERA, comparatorDrugConceptId is a CONCEPT_ID and all descendant concepts within that CONCEPT_ID will be used to define the cohort.  If exposureTable <> DRUG_ERA, comparatorDrugConceptId is used to select the COHORT_DEFINITION_ID in the cohort-like table.	
#' @param indicationConceptIds   A list of CONCEPT_IDs used to restrict the target and comparator cohorts, based on any descendant condition of this list occurring at least once within the indicationLookbackWindow prior to the cohort index date.  	
#' @param exclusionConceptIds   A list of CONCEPT_IDs used to restrict the cohorts, based on any descendant conditions/drugs/procedures occurring at least once anytime prior to the cohort index date.	
#' @param outcomeConceptIds 	A list of CONCEPT_IDs used to define outcomes.  If outcomeTable=CONDITION_OCCURRENCE, the list is a set of ancestor CONCEPT_IDs, and all occurrences of all descendant concepts will be selected.  If outcomeTable<>CONDITION_OCCURRENCE, the list contains records found in COHORT_DEFINITION_ID field.
#' @param excludedCovariateConceptIds    A list of Covariate Ids that should be removed from the COVARIATE table prior to fitting any model (either propensity score model or outcome model).  Generally required if any covariates perfectly predict exposure status (e.g. the target drug itself).
#' 
#' @param washoutWindow 		The mininum required continuous observation time prior to index date for a person to be included in the cohort.
#' @param indicationLookbackWindow 		The window to look back prior to cohort index date to identify records of a indication condition.  Only applicable if indicationConceptIds != ''.
#' @param studyStartDate 		A calendar date specifying the minimum date that a cohort index date can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate 		A calendar date specifying the maximum date that a cohort index date can appear. Date format is 'yyyymmdd'.
#' @param outcomeConditionTypeConceptIds   A list of TYPE_CONCEPT_ID values that will restrict condition occurrences.  Only applicable if outcomeTable = CONDITION_OCCURRENCE.
#' @param useCovariateDemographics 		A boolean value (TRUE/FALSE) to determine if demographic covariates (age in 5-yr increments, gender, race, ethnicity, year of index date, month of index date) will be created and included in future models.
#' @param useCovariateConditionOccurrence   A boolean value (TRUE/FALSE) to determine if covariates derived from CONDITION_OCCURRENCE table will be created and included in future models.
#' @param useCovariateConditionOccurrence365d   	A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of condition in 365d window prior to or on cohort index date.  Only applicable if useCovariateConditionOccurrence = TRUE.	
#' @param useCovariateConditionOccurrence30d     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of condition in 30d window prior to or on cohort index date.  Only applicable if useCovariateConditionOccurrence = TRUE. 
#' @param useCovariateConditionOccurrenceInpt180d     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of condition within inpatient type in 180d window prior to or on cohort index date.  Only applicable if useCovariateConditionOccurrence = TRUE.
#' @param useCovariateConditionEra      A boolean value (TRUE/FALSE) to determine if covariates derived from CONDITION_ERA table will be created and included in future models.
#' @param useCovariateConditionEraEver     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of condition era anytime prior to or on cohort index date.  Only applicable if useCovariateConditionEra = TRUE.	
#' @param useCovariateConditionEraOverlap     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of condition era that overlaps the cohort index date.  Only applicable if useCovariateConditionEra = TRUE. 		
#' @param useCovariateConditionGroup   A boolean value (TRUE/FALSE) to determine if all CONDITION_OCCURRENCE and CONDITION_ERA covariates should be aggregated or rolled-up to higher-level concepts based on vocabluary classification.
#' @param useCovariateDrugExposure    A boolean value (TRUE/FALSE) to determine if covariates derived from DRUG_EXPOSURE table will be created and included in future models.   
#' @param useCovariateDrugExposure365d     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of drug in 365d window prior to or on cohort index date.  Only applicable if useCovariateDrugExposure = TRUE.	
#' @param useCovariateDrugExposure30d     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of drug in 30d window prior to or on cohort index date.  Only applicable if useCovariateDrugExposure = TRUE.   		
#' @param useCovariateDrugEra    A boolean value (TRUE/FALSE) to determine if covariates derived from DRUG_ERA table will be created and included in future models. 
#' @param useCovariateDrugEra365d     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of drug era in 365d window prior to or on cohort index date.  Only applicable if useCovariateDrugEra = TRUE.
#' @param useCovariateDrugEra30d    A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of drug era in 30d window prior to or on cohort index date.  Only applicable if useCovariateDrugEra = TRUE.
#' @param useCovariateDrugEraEver    A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of drug era anytime prior to or on cohort index date.  Only applicable if useCovariateDrugEra = TRUE.
#' @param useCovariateDrugEraOverlap      A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of drug era that overlaps the cohort index date.  Only applicable if useCovariateDrugEra = TRUE.		
#' @param useCovariateDrugGroup 	   A boolean value (TRUE/FALSE) to determine if all DRUG_EXPOSURE and DRUG_ERA covariates should be aggregated or rolled-up to higher-level concepts of drug classes based on vocabluary classification.	
#' @param useCovariateProcedureOccurrence    A boolean value (TRUE/FALSE) to determine if covariates derived from PROCEDURE_OCCURRENCE table will be created and included in future models.  
#' @param useCovariateProcedureOccurrence365d     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of procedure in 365d window prior to or on cohort index date.  Only applicable if useCovariateProcedureOccurrence = TRUE.	
#' @param useCovariateProcedureOccurrence30d     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of procedure in 30d window prior to or on cohort index date.  Only applicable if useCovariateProcedureOccurrence = TRUE.		
#' @param useCovariateProcedureGroup       A boolean value (TRUE/FALSE) to determine if all PROCEDURE_OCCURRENCE covariates should be aggregated or rolled-up to higher-level concepts based on vocabluary classification.			
#' @param useCovariateObservation    A boolean value (TRUE/FALSE) to determine if covariates derived from OBSERVATION table will be created and included in future models. 
#' @param useCovariateObservation365d     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of observation in 365d window prior to or on cohort index date.  Only applicable if useCovariateObservation = TRUE.  
#' @param useCovariateObservation30d     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of observation in 30d window prior to or on cohort index date.  Only applicable if useCovariateObservation = TRUE.
#' @param useCovariateObservationBelow     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of observation with a numeric value below normal range for latest value within 180d of cohort index.  Only applicable if useCovariateObservation = TRUE.
#' @param useCovariateObservationAbove     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for presence/absence of observation with a numeric value above normal range for latest value within 180d of cohort index.  Only applicable if useCovariateObservation = TRUE.
#' @param useCovariateObservationCount365d     A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that look for the count of each observation concept in 365d window prior to or on cohort index date.  Only applicable if useCovariateObservation = TRUE.		
#' @param useCovariateConceptCounts 		A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that count the number of concepts that a person has within each domain (CONDITION, DRUG, PROCEDURE, OBSERVATION)
#' @param useCovariateRiskScores 		A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that calculate various Risk Scores, including Charlson, DCSI.  
#' @param useCovariateInteractionYear 	A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that represent interaction terms between all other covariates and the year of the cohort index date.  	
#' @param useCovariateInteractionMonth    A boolean value (TRUE/FALSE) to determine if covariates will be created and used in models that represent interaction terms between all other covariates and the month of the cohort index date.  		
#' @param deleteCovariatesSmallCount 		A numeric value used to remove covariates that occur in both cohorts fewer than deleteCovariateSmallCounts time.
#' 
#' @return
#' Returns an object of type \code{cohortData}, containing information on the cohorts, their outcomes,
#' and baseline covariates. Information about multiple outcomes can be captured at once for efficiency reasons. This object is a list with the following components:
#' \describe{
#'   \item{outcomes}{An ffdf object listing the outcomes per person, including the time to event, and the outcome conncept ID. Outcomes are not yet filtered based on risk window, since this is done at a later stage.}
#'   \item{cohorts}{An ffdf object listing the persons in each cohort, listing their exposure status as well as the time to the end of the observation period and time to the end of the cohort (usually the end of the exposure era).}
#'   \item{covariates}{An ffdf object listing the baseline covariates per person in the two cohorts. This is done using a sparse representation: covariates with a value of 0 are omitted to save space.}
#'   \item{exclude}{An ffdf object listing for each outcome concept ID the persons that need to be excluded from the analysis because of prior outcomes.}
#'   \item{covariateRef}{An ffdf object describing the covariates that have been extracted.}
#'   \item{metaData}{A list of objects with information on how the cohortData object was constructed.}
#' }
#' 
#' The generic \code{summary()} function has been implemented for this object.
#'
#' @export
getDbCohortData <- function(connectionDetails,
                            cdmSchema,
                            resultsSchema,
                            targetDrugConceptId,
                            comparatorDrugConceptId,
                            indicationConceptIds = c(),
                            washoutWindow = 183,
                            indicationLookbackWindow = 183,
                            studyStartDate = "",
                            studyEndDate = "",
                            exclusionConceptIds = c(),
                            outcomeConceptIds,
                            outcomeConditionTypeConceptIds = c(),
                            exposureSchema = cdmSchema,
                            exposureTable = "drug_era",
                            outcomeSchema = cdmSchema,
                            outcomeTable = "condition_occurrence",
                            useCovariateDemographics = TRUE,
                            useCovariateConditionOccurrence = TRUE,
                            useCovariateConditionOccurrence365d = TRUE,
                            useCovariateConditionOccurrence30d = TRUE,
                            useCovariateConditionOccurrenceInpt180d = TRUE,
                            useCovariateConditionEra = FALSE,
                            useCovariateConditionEraEver = FALSE,
                            useCovariateConditionEraOverlap = FALSE,
                            useCovariateConditionGroup = FALSE,
                            useCovariateDrugExposure = FALSE,
                            useCovariateDrugExposure365d = FALSE,
                            useCovariateDrugExposure30d = FALSE,
                            useCovariateDrugEra = FALSE,
                            useCovariateDrugEra365d = FALSE,
                            useCovariateDrugEra30d = FALSE,
                            useCovariateDrugEraOverlap = FALSE,
                            useCovariateDrugEraEver = FALSE,
                            useCovariateDrugGroup = FALSE,
                            useCovariateProcedureOccurrence = FALSE,
                            useCovariateProcedureOccurrence365d = FALSE,
                            useCovariateProcedureOccurrence30d = FALSE,
                            useCovariateProcedureGroup = FALSE,
                            useCovariateObservation = FALSE,
                            useCovariateObservation365d = FALSE,
                            useCovariateObservation30d = FALSE,
                            useCovariateObservationBelow = FALSE,
                            useCovariateObservationAbove = FALSE,
                            useCovariateObservationCount365d = FALSE,
                            useCovariateConceptCounts = FALSE,
                            useCovariateRiskScores = FALSE,
                            useCovariateInteractionYear = FALSE,
                            useCovariateInteractionMonth = FALSE,
                            excludedCovariateConceptIds = c(),
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
                                                   exposure_schema = exposureSchema,
                                                   exposure_table = tolower(exposureTable),
                                                   outcome_schema = outcomeSchema,
                                                   outcome_table = tolower(outcomeTable),
                                                   use_covariate_demographics = useCovariateDemographics,
                                                   use_covariate_condition_occurrence = useCovariateConditionOccurrence,
                                                   use_covariate_condition_occurrence_365d = useCovariateConditionOccurrence365d,
                                                   use_covariate_condition_occurrence_30d = useCovariateConditionOccurrence30d,
                                                   use_covariate_condition_occurrence_inpt180d = useCovariateConditionOccurrenceInpt180d,
                                                   use_covariate_condition_era = useCovariateConditionEra,
                                                   use_covariate_condition_era_ever = useCovariateConditionEraEver,
                                                   use_covariate_condition_era_overlap = useCovariateConditionEraOverlap,
                                                   use_covariate_condition_group = useCovariateConditionGroup,
                                                   use_covariate_drug_exposure = useCovariateDrugExposure,
                                                   use_covariate_drug_exposure_365d = useCovariateDrugExposure365d,
                                                   use_covariate_drug_exposure_30d = useCovariateDrugExposure30d,
                                                   use_covariate_drug_era = useCovariateDrugEra,
                                                   use_covariate_drug_era_365d = useCovariateDrugEra365d,
                                                   use_covariate_drug_era_30d = useCovariateDrugEra30d,
                                                   use_covariate_drug_era_overlap = useCovariateDrugEraOverlap,
                                                   use_covariate_drug_era_ever = useCovariateDrugEraEver,
                                                   use_covariate_drug_group = useCovariateDrugGroup,
                                                   use_covariate_procedure_occurrence = useCovariateProcedureOccurrence,
                                                   use_covariate_procedure_occurrence_365d = useCovariateProcedureOccurrence365d,
                                                   use_covariate_procedure_occurrence_30d = useCovariateProcedureOccurrence30d,
                                                   use_covariate_procedure_group = useCovariateProcedureGroup,
                                                   use_covariate_observation = useCovariateObservation,
                                                   use_covariate_observation_365d = useCovariateObservation365d,
                                                   use_covariate_observation_30d = useCovariateObservation30d,
                                                   use_covariate_observation_below = useCovariateObservationBelow,
                                                   use_covariate_observation_above = useCovariateObservationAbove,
                                                   use_covariate_observation_count365d = useCovariateObservationCount365d,
                                                   use_covariate_concept_counts = useCovariateConceptCounts,
                                                   use_covariate_risk_scores = useCovariateRiskScores,
                                                   use_covariate_interaction_year = useCovariateInteractionYear,
                                                   use_covariate_interaction_month = useCovariateInteractionMonth,
                                                   excluded_covariate_concept_ids = excludedCovariateConceptIds,
                                                   delete_covariates_small_count = deleteCovariatesSmallCount)
  
  conn <- DatabaseConnector::connect(connectionDetails)
  
  writeLines("Executing multiple queries. This could take a while")
  DatabaseConnector::executeSql(conn, renderedSql)
  
  # Build queries for extracting data:
  cohortSql <-"SELECT row_id, cohort_id AS treatment, person_id, datediff(dd, cohort_start_date, observation_period_end_date) AS time_to_obs_period_end, datediff(dd, cohort_start_date, cohort_end_date) AS time_to_cohort_end FROM #cohort_person ORDER BY row_id"
  cohortSql <- SqlRender::translateSql(cohortSql, "sql server", connectionDetails$dbms)$sql
  
  covariateSql <-"SELECT row_id, covariate_id,covariate_value FROM #cohort_covariate ORDER BY row_id, covariate_id"
  covariateSql <- SqlRender::translateSql(covariateSql, "sql server", connectionDetails$dbms)$sql
  
  outcomeSql <-"SELECT row_id, outcome_id, time_to_event FROM #cohort_outcome ORDER BY outcome_id, row_id"
  outcomeSql <- SqlRender::translateSql(outcomeSql, "sql server", connectionDetails$dbms)$sql
  
  excludeSql <-"SELECT row_id, outcome_id FROM #cohort_excluded_person ORDER BY outcome_id, row_id"
  excludeSql <- SqlRender::translateSql(excludeSql, "sql server", connectionDetails$dbms)$sql
  
  covariateRefSql <-"SELECT covariate_id, covariate_name, analysis_id, concept_id  FROM #cohort_covariate_ref ORDER BY covariate_id"
  covariateRefSql <- SqlRender::translateSql(covariateRefSql, "sql server", connectionDetails$dbms)$sql
  
  rawCountSql <- SqlRender::loadRenderTranslateSql("CountOverallExposedPopulation.sql",
                                                   packageName = "CohortMethod",
                                                   dbms = connectionDetails$dbms,
                                                   cdm_schema = cdmSchema,
                                                   target_drug_concept_id = targetDrugConceptId,
                                                   comparator_drug_concept_id = comparatorDrugConceptId,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   exposure_schema = exposureSchema,
                                                   exposure_table = tolower(exposureTable))
  
  newUserCountSql <-"SELECT COUNT(*) AS new_user_count,cohort_id FROM #new_user_cohort GROUP BY cohort_id"
  newUserCountSql <- SqlRender::translateSql(newUserCountSql, "sql server", connectionDetails$dbms)$sql
  
  indicatedCountSql <-"SELECT COUNT(*) AS indicated_count,cohort_id FROM #indicated_cohort GROUP BY cohort_id"
  indicatedCountSql <- SqlRender::translateSql(indicatedCountSql, "sql server", connectionDetails$dbms)$sql
  
  nonOverlapCountSql <-"SELECT COUNT(*) AS non_overlap_count,cohort_id FROM #non_overlap_cohort GROUP BY cohort_id"
  nonOverlapCountSql <- SqlRender::translateSql(nonOverlapCountSql, "sql server", connectionDetails$dbms)$sql
  
  notExcludedCountSql <-"SELECT COUNT(*) AS not_excluded_count,cohort_id FROM #cohort_person GROUP BY cohort_id"
  notExcludedCountSql <- SqlRender::translateSql(notExcludedCountSql, "sql server", connectionDetails$dbms)$sql
  
  writeLines("Fetching data from server")
  start <- Sys.time()
  outcomes <- DatabaseConnector::dbGetQuery.ffdf(conn, outcomeSql)
  cohorts <-  DatabaseConnector::dbGetQuery.ffdf(conn, cohortSql)
  covariates <- DatabaseConnector::dbGetQuery.ffdf(conn, covariateSql)
  exclude <- DatabaseConnector::dbGetQuery.ffdf(conn, excludeSql)
  covariateRef <- DatabaseConnector::dbGetQuery.ffdf(conn, covariateRefSql)
  rawCount <- DatabaseConnector::querySql(conn, rawCountSql)
  newUserCount <- DatabaseConnector::querySql(conn, newUserCountSql)
  counts <- merge(rawCount, newUserCount)
  if (length(indicationConceptIds) != 0){
    indicatedCount <- DatabaseConnector::querySql(conn, indicatedCountSql)
    counts <- merge(counts, indicatedCount)
  }
  nonOverlapCount <- DatabaseConnector::querySql(conn, nonOverlapCountSql)
  counts <- merge(counts, nonOverlapCount)
  notExcludedCount <- DatabaseConnector::querySql(conn, notExcludedCountSql)
  counts <- merge(counts, notExcludedCount)  
  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta,3), attr(delta,"units")))
  
  #Remove temp tables:
  if (connectionDetails$dbms == "oracle"){
    renderedSql <- SqlRender::loadRenderTranslateSql("CMRemoveTempTables.sql",
                                                     packageName = "CohortMethod",
                                                     dbms = connectionDetails$dbms,
                                                     indication_concept_ids = indicationConceptIds)
    DatabaseConnector::executeSql(conn,renderedSql,progressBar = FALSE,reportOverallTime=FALSE)
  }
  
  colnames(outcomes) <- snakeCaseToCamelCase(colnames(outcomes))
  colnames(cohorts) <- snakeCaseToCamelCase(colnames(cohorts))
  colnames(covariates) <- snakeCaseToCamelCase(colnames(covariates))
  colnames(exclude) <- snakeCaseToCamelCase(colnames(exclude))
  colnames(covariateRef) <- snakeCaseToCamelCase(colnames(covariateRef))
  colnames(counts) <- snakeCaseToCamelCase(colnames(counts))
  counts <- counts[order(counts$cohortId),]
  
  dummy <- RJDBC::dbDisconnect(conn)
  metaData <- list(sql = renderedSql,
                   targetDrugConceptId = targetDrugConceptId,
                   comparatorDrugConceptId = comparatorDrugConceptId,
                   outcomeConceptIds = outcomeConceptIds,
                   counts = counts,
                   call = match.call()
  )
  
  result <- list(outcomes = outcomes,
                 cohorts = cohorts,
                 covariates = covariates,
                 exclude = exclude,
                 covariateRef = covariateRef,
                 metaData = metaData
  )
  
  #Open all ffdfs to prevent annoying messages later:
  if (nrow(result$outcomes) == 0 | nrow(result$covariates) == 0){
    warning("No data found")
  } else {
    open(result$outcomes)
    open(result$cohorts)
    open(result$covariates)
    open(result$exclude)
    open(result$covariateRef)
  }
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
#' @param readOnly            If true, the data is opened read only.
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
loadCohortData <- function(file, readOnly = FALSE){
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
  #Open all ffdfs to prevent annoying messages later:
  open(result$outcomes,readonly = readOnly)
  open(result$cohorts,readonly = readOnly)
  open(result$covariates,readonly = readOnly)
  open(result$exclude,readonly = readOnly)
  open(result$covariateRef,readonly = readOnly)
  
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
    if (outcomeCounts$eventCount[i] == 0)
      outcomeCounts$personCount[i] = 0
    else
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
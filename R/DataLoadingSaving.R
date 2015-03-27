# @file CohortMethod.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' Get the cohort data from the server
#'
#' @description
#' This function executes a large set of SQL statements against the database in OMOP CDM format to extract the data needed to perform the analysis.
#'
#' @details
#' Based on the parameters, the treatment and comparator cohorts are constructed. Baseline covariates at or before the index date are extracted, as well as outcomes occurring on or after the index date.
#' The treatment and comparator cohorts can be identified using the drug_era table, or through user-defined cohorts in a cohort table either inside the CDM instance or in a separate schema. 
#' Similarly, outcomes are identified using the condition_occurrence or condition_era table, or through user-defined cohorts in a cohort table either inside the CDM instance or in a separate schema. 
#' Covariates are automatically extracted from the appropriate tables within the CDM.This function calls the \code{getDbCovariates} and \code{getDbOutcomes} functions.
#'
#' @param connectionDetails  	An R object of type\cr\code{connectionDetails} created using the function \code{createConnectionDetails} in the \code{DatabaseConnector} package.
#' 
#' @param cdmDatabaseSchema    The name of the database schema that contains the OMOP CDM instance.  Requires read permissions to this database. On SQL Server, this should specifiy both the database and the schema, so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema    For Oracle only: the name of the database schema where you want all temporary tables to be managed. Requires create/insert permissions to this database. 
#' @param exposureDatabaseSchema     The name of the database schema that is the location where the exposure data used to define the exposure cohorts is available.  If exposureTable = DRUG_ERA, exposureDatabaseSchema is not used by assumed to be cdmSchema.  Requires read permissions to this database.    
#' @param exposureTable   The tablename that contains the exposure cohorts.  If exposureTable <> DRUG_ERA, then expectation is exposureTable has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.	
#' @param excludeDrugsFromCovariates  Should the target and comparator drugs (and their descendant concepts) be excluded from the covariates? Note
#' that this will work if the drugs are actualy drug concept IDs (and not cohort IDs).
#' @template GetCovariatesParams
#' @template GetOutcomesParams
#' @param targetDrugConceptId 		A unique identifier to define the target cohort.  If exposureTable = DRUG_ERA, targetDrugConceptId is a CONCEPT_ID and all descendant concepts within that CONCEPT_ID will be used to define the cohort.  If exposureTable <> DRUG_ERA, targetDrugConceptId is used to select the COHORT_DEFINITION_ID in the cohort-like table.
#' @param comparatorDrugConceptId   	A unique identifier to define the comparator cohort.  If exposureTable = DRUG_ERA, comparatorDrugConceptId is a CONCEPT_ID and all descendant concepts within that CONCEPT_ID will be used to define the cohort.  If exposureTable <> DRUG_ERA, comparatorDrugConceptId is used to select the COHORT_DEFINITION_ID in the cohort-like table.	
#' @param indicationConceptIds   A list of CONCEPT_IDs used to restrict the target and comparator cohorts, based on any descendant condition of this list occurring at least once within the indicationLookbackWindow prior to the cohort index date. If no concept IDs are specified, the cohorts are not restricted to any indication.	
#' @param exclusionConceptIds   A list of CONCEPT_IDs used to restrict the cohorts, based on any descendant conditions/drugs/procedures occurring at least once anytime prior to the cohort index date.	
#' @param washoutWindow 		The mininum required continuous observation time prior to index date for a person to be included in the cohort.
#' @param indicationLookbackWindow 		The window to look back prior to cohort index date to identify records of a indication condition.  Only applicable if indicationConceptIds != ''.
#' @param studyStartDate 		A calendar date specifying the minimum date that a cohort index date can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate 		A calendar date specifying the maximum date that a cohort index date can appear. Date format is 'yyyymmdd'.
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
                            cdmDatabaseSchema,
                            oracleTempSchema = cdmDatabaseSchema,
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
                            exposureDatabaseSchema = cdmDatabaseSchema,
                            exposureTable = "drug_era",
                            outcomeDatabaseSchema = cdmDatabaseSchema,
                            outcomeTable = "condition_occurrence",
                            excludeDrugsFromCovariates = TRUE,
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
  cdmDatabase <- strsplit(cdmDatabaseSchema ,"\\.")[[1]][1]
  conn <- DatabaseConnector::connect(connectionDetails)
  
  if (excludeDrugsFromCovariates) {
    if (exposureTable != "drug_era")
      warning("Removing drugs from covariates, but not sure if exposure IDs are valid drug concepts")
    sql <- "SELECT descendant_concept_id FROM @cdm_database_schema.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id)"
    sql <- SqlRender::renderSql(sql, 
                                cdm_database_schema = cdmDatabaseSchema,
                                target_drug_concept_id = targetDrugConceptId,
                                comparator_drug_concept_id = comparatorDrugConceptId)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    conceptIds <- DatabaseConnector::querySql(conn, sql)
    names(conceptIds) <- SqlRender::snakeCaseToCamelCase(names(conceptIds))
    conceptIds <- conceptIds$descendantConceptId
    excludedCovariateConceptIds <- c(excludedCovariateConceptIds, conceptIds)
  }
  
  writeLines("\nConstructing treatment and comparator cohorts")
  renderedSql <- SqlRender::loadRenderTranslateSql("GetCohorts.sql",
                                                   packageName = "CohortMethod",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database = cdmDatabase,
                                                   target_drug_concept_id = targetDrugConceptId,
                                                   comparator_drug_concept_id = comparatorDrugConceptId,
                                                   indication_concept_ids = indicationConceptIds,
                                                   washout_window = washoutWindow,
                                                   indication_lookback_window = indicationLookbackWindow,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   exclusion_concept_ids = exclusionConceptIds,
                                                   exposure_database_schema = exposureDatabaseSchema,
                                                   exposure_table = exposureTable)
  
  writeLines("Executing multiple queries. This could take a while")
  DatabaseConnector::executeSql(conn,renderedSql)
  
  writeLines("Fetching data from server")
  start <- Sys.time()
  cohortSql <- SqlRender::loadRenderTranslateSql("FetchCohortData.sql",
                                                 packageName = "CohortMethod",
                                                 dbms = connectionDetails$dbms,
                                                 oracleTempSchema = oracleTempSchema)
  rawCountSql <- SqlRender::loadRenderTranslateSql("CountOverallExposedPopulation.sql",
                                                   packageName = "CohortMethod",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema, 
                                                   target_drug_concept_id = targetDrugConceptId,
                                                   comparator_drug_concept_id = comparatorDrugConceptId,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   exposure_database_schema = exposureDatabaseSchema,
                                                   exposure_table = tolower(exposureTable))
  newUserCountSql <-"SELECT COUNT(*) AS new_user_count, treatment FROM #new_user_cohort GROUP BY treatment"
  newUserCountSql <- SqlRender::translateSql(newUserCountSql, "sql server", connectionDetails$dbms, oracleTempSchema)$sql
  indicatedCountSql <-"SELECT COUNT(*) AS indicated_count, treatment FROM #indicated_cohort GROUP BY treatment"
  indicatedCountSql <- SqlRender::translateSql(indicatedCountSql, "sql server", connectionDetails$dbms, oracleTempSchema)$sql
  nonOverlapCountSql <-"SELECT COUNT(*) AS non_overlap_count, treatment FROM #non_overlap_cohort GROUP BY treatment"
  nonOverlapCountSql <- SqlRender::translateSql(nonOverlapCountSql, "sql server", connectionDetails$dbms, oracleTempSchema)$sql
  notExcludedCountSql <-"SELECT COUNT(*) AS not_excluded_count, cohort_definition_id AS treatment FROM #cohort_person GROUP BY cohort_definition_id"
  notExcludedCountSql <- SqlRender::translateSql(notExcludedCountSql, "sql server", connectionDetails$dbms, oracleTempSchema)$sql
  cohorts <-  DatabaseConnector::dbGetQuery.ffdf(conn, cohortSql)
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
  colnames(cohorts) <- SqlRender::snakeCaseToCamelCase(colnames(cohorts))
  colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))
  counts <- counts[order(counts$treatment),]
  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta,3), attr(delta,"units")))
  
  writeLines("\nConstructing baseline covariates")
  covariateData <- getDbCovariates(connection = conn, 
                                   oracleTempSchema = oracleTempSchema, 
                                   cdmDatabaseSchema = cdmDatabaseSchema, 
                                   useExistingCohortPerson = TRUE, 
                                   useCovariateDemographics = useCovariateDemographics,
                                   useCovariateConditionOccurrence = useCovariateConditionOccurrence,
                                   useCovariateConditionOccurrence365d = useCovariateConditionOccurrence365d,
                                   useCovariateConditionOccurrence30d = useCovariateConditionOccurrence30d,
                                   useCovariateConditionOccurrenceInpt180d = useCovariateConditionOccurrenceInpt180d,
                                   useCovariateConditionEra = useCovariateConditionEra,
                                   useCovariateConditionEraEver = useCovariateConditionEraEver,
                                   useCovariateConditionEraOverlap = useCovariateConditionEraOverlap,
                                   useCovariateConditionGroup = useCovariateConditionGroup,
                                   useCovariateDrugExposure = useCovariateDrugExposure,
                                   useCovariateDrugExposure365d = useCovariateDrugExposure365d,
                                   useCovariateDrugExposure30d = useCovariateDrugExposure30d,
                                   useCovariateDrugEra = useCovariateDrugEra,
                                   useCovariateDrugEra365d = useCovariateDrugEra365d,
                                   useCovariateDrugEra30d = useCovariateDrugEra30d,
                                   useCovariateDrugEraOverlap = useCovariateDrugEraOverlap,
                                   useCovariateDrugEraEver = useCovariateDrugEraEver,
                                   useCovariateDrugGroup = useCovariateDrugGroup,
                                   useCovariateProcedureOccurrence = useCovariateProcedureOccurrence,
                                   useCovariateProcedureOccurrence365d = useCovariateProcedureOccurrence365d,
                                   useCovariateProcedureOccurrence30d = useCovariateProcedureOccurrence30d,
                                   useCovariateProcedureGroup = useCovariateProcedureGroup,
                                   useCovariateObservation = useCovariateObservation,
                                   useCovariateObservation365d = useCovariateObservation365d,
                                   useCovariateObservation30d = useCovariateObservation30d,
                                   useCovariateObservationBelow = useCovariateObservationBelow,
                                   useCovariateObservationAbove = useCovariateObservationAbove,
                                   useCovariateObservationCount365d = useCovariateObservationCount365d,
                                   useCovariateConceptCounts = useCovariateConceptCounts,
                                   useCovariateRiskScores = useCovariateRiskScores,
                                   useCovariateInteractionYear = useCovariateInteractionYear,
                                   useCovariateInteractionMonth = useCovariateInteractionMonth,
                                   excludedCovariateConceptIds = excludedCovariateConceptIds,
                                   deleteCovariatesSmallCount = deleteCovariatesSmallCount)
  names(covariateData$covariates)[names(covariateData$covariates) == "personId"] <- "rowId"
  covariateData$covariates$cohortStartDate <- NULL
  covariateData$covariates$cohortDefinitionId <- NULL
  metaData <- list(sql = c(renderedSql, covariateData$metaData$sql),
                   targetDrugConceptId = targetDrugConceptId,
                   comparatorDrugConceptId = comparatorDrugConceptId,
                   counts = counts,
                   call = match.call()
  )
  
  result <- list(cohorts = cohorts,
                 covariates = covariateData$covariates,
                 covariateRef = covariateData$covariateRef,
                 metaData = metaData
  )
  open(result$cohorts)
  class(result) <- "cohortData"
  
  if (!missing(outcomeConceptIds) && !is.null(outcomeConceptIds)){  
    writeLines("\nConstructing outcomes")
    result <- getDbOutcomes(connection = conn, 
                            oracleTempSchema = oracleTempSchema, 
                            cdmDatabaseSchema = cdmDatabaseSchema, 
                            cohortData = result,
                            outcomeDatabaseSchema = outcomeDatabaseSchema,
                            outcomeTable = outcomeTable,
                            outcomeConceptIds = outcomeConceptIds,
                            outcomeConditionTypeConceptIds = outcomeConditionTypeConceptIds)
  }
  
  #Remove temp tables:
  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveCohortTempTables.sql",
                                                   packageName = "CohortMethod",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   indication_concept_ids = indicationConceptIds)
  DatabaseConnector::executeSql(conn,renderedSql,progressBar = FALSE,reportOverallTime=FALSE)
  
  
  dummy <- RJDBC::dbDisconnect(conn)
  
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
  ffbase::load.ffdf(absolutePath,e)
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

#' @export
print.cohortData <- function(x, ...){
  writeLines("CohortData object")
  writeLines("")
  writeLines(paste("Treatment concept ID:",x$metaData$targetDrugConceptId))
  writeLines(paste("Comparator concept ID:",x$metaData$comparatorDrugConceptId))
  writeLines(paste("Outcome concept ID(s):",paste(x$metaData$outcomeConceptIds,collapse=",")))
}

#' @export
summary.cohortData <- function(object, ...){
  treatedPersons = ffbase::sum.ff(object$cohorts$treatment)  
  comparatorPersons = nrow(object$cohorts)-treatedPersons
  outcomeCounts = data.frame(outcomeConceptId = object$metaData$outcomeConceptIds, eventCount = 0, personCount = 0)
  for (i in 1:nrow(outcomeCounts)){
    outcomeCounts$eventCount[i] = ffbase::sum.ff(object$outcomes$outcomeId == object$metaData$outcomeConceptIds[i])
    if (outcomeCounts$eventCount[i] == 0)
      outcomeCounts$personCount[i] = 0
    else {
      t <- (object$outcomes$outcomeId == object$metaData$outcomeConceptIds[i])
      outcomeCounts$personCount[i] = length(ffbase::unique.ff(object$outcomes$rowId[ffbase::ffwhich(t, t == TRUE)]))   
    }
  }
  
  result <- list(metaData = object$metaData,
                 treatedPersons = treatedPersons,
                 comparatorPersons = comparatorPersons,
                 outcomeCounts = outcomeCounts,
                 covariateCount = nrow(object$covariateRef),
                 covariateValueCount = nrow(object$covariates)                 
  )
  class(result) <- "summary.cohortData"
  return(result)
}

#' @export
print.summary.cohortData <- function(x, ...){
  writeLines("CohortData object summary")
  writeLines("")
  writeLines(paste("Treatment concept ID:",x$metaData$targetDrugConceptId))
  writeLines(paste("Comparator concept ID:",x$metaData$comparatorDrugConceptId))
  writeLines(paste("Outcome concept ID(s):",paste(x$metaData$outcomeConceptIds,collapse=",")))
  writeLines("")
  writeLines(paste("Treated persons:",paste(x$treatedPersons)))
  writeLines(paste("Comparator persons:",paste(x$comparatorPersons)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- x$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeConceptId
  outcomeCounts$outcomeConceptId <- NULL
  colnames(outcomeCounts) <- c("Event count","Person count")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Covariates:")
  writeLines(paste("Number of covariates:",x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:",x$covariateValueCount)) 
}
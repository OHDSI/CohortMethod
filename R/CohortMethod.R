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

executeSql <- function(conn, dbms, sql, profile = FALSE){
  sqlStatements = splitSql(sql)
  if (!profile)
    progressBar <- txtProgressBar(style=3)
  start <- Sys.time()
  for (i in 1:length(sqlStatements)){
    sqlStatement <- sqlStatements[i]
    if (profile){
      sink(paste("statement_",i,".sql",sep=""))
      cat(sqlStatement)
      sink()
    }
    tryCatch ({   
      startQuery <- Sys.time()
      dbSendUpdate(conn, sqlStatement)
      if (profile){
        delta <- Sys.time() - startQuery
        writeLines(paste("Statement ",i,"took", delta, attr(delta,"units")))
      }
    } , error = function(err) {
      writeLines(paste("Error executing SQL:",err))
      
      #Write error report:
      filename <- paste(getwd(),"/errorReport.txt",sep="")
      sink(filename)
      error <<- err
      cat("DBMS:\n")
      cat(dbms)
      cat("\n\n")
      cat("Error:\n")
      cat(err$message)
      cat("\n\n")
      cat("SQL:\n")
      cat(sqlStatement)
      sink()
      
      writeLines(paste("An error report has been created at ", filename))
      break
    })
    if (!profile)
      setTxtProgressBar(progressBar, i/length(sqlStatements))
  }
  if (!profile)
    close(progressBar)
  delta <- Sys.time() - start
  writeLines(paste("Analysis took", signif(delta,3), attr(delta,"units")))
}



#' Trim, match or stratify persons by propensity score
#'
#' @description
#' \code{trimMatchStratify} uses the provided propensity scores to trim subjects with extreme
#' scores, and either match or stratify persons.
#' 
#' @param propensityScores  A data frame with three columns: \code{row_id} (person ID), \code{treat} and \code{propensityScore}.
#' @param method			      Whether \code{matching} or \code{stratification} should be performed.
#' @param caliper		        The caliper for matching. A caliper is the distance which is acceptable for 
#' any match. Observations which are outside of the caliper are dropped. The caliper is interpreted to be in 
#' standardized units. For example, caliper=.25 means that all matches not equal to or within .25 standard 
#' deviations are dropped. A caliper of 0 means no caliper is used.
#' @param matchRatio		    Number of persons in comparator arm to be matched to each person in the treatment arm.
#' @param strata		        If stratification is performed, how many strata?
#' If not specified, all analyses will be executed. See \code{data(analysesDetails)} for a list of all Achilles analyses and their Ids.
#' @param trimFraction      This fraction will be removed from each treatment group. In the treatment group, persons
#' with the highest propensity scores will be removed, in the comparator group person with the lowest scores will be removed.
#' 
#' @details
#' Todo
#' 
#' @return Returns a date frame with two columns: \code{row_id} and \code{stratum_id} 
#' @examples \dontrun{
#'   
#' }
#' @export
trimMatchStratify <- function(propensityScores, method="matching", caliper=0, matchRatio=1, strata=5,trimFraction=0.05){
  #need to implement
}


#' @export
cohortMethod <- function(connectionDetails, 
                         cdmSchema = "CDM4_SIM",
                         resultsSchema = "CDM4_SIM",
                         targetDrugConceptId = 755695,
                         comparatorDrugConceptId = 739138,
                         indicationConceptIds = 439926,
                         washoutWindow = 183,
                         indicationLookbackWindow = 183,
                         exposureExtensionWindow = 7,
                         studyStartDate = "",
                         studyEndDate = "",
                         exclusionConceptIds = c(4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019),
                         outcomeConceptId = 194133,
                         outcomeConditionTypeConceptIds = c(38000215,38000216,38000217,38000218,38000183,38000232),
                         maxOutcomeCount = 1){
  renderedSql <- loadRenderTranslateSql("CohortMethod.sql",
                                        packageName = "CohortMethod",
                                        dbms = connectionDetails$dbms,
                                        CDM_schema = cdmSchema,
                                        results_schema = resultsSchema,
                                        target_drug_concept_id = targetDrugConceptId,
                                        comparator_drug_concept_ids = comparatorDrugConceptIds,
                                        indication_concept_id = indicationConceptId,
                                        washout_window = washoutWindow,
                                        indication_lookback_window = indicationLookbackWindow,
                                        exposure_extension_window = exposureExtensionWindow,
                                        study_start_date = studyStartDate,
                                        study_end_date = studyEndDate,
                                        exclusion_concept_ids = exclusionConceptIds,
                                        outcome_concept_id = outcomeConceptId,
                                        outcome_condition_type_concept_ids = outcomeConditionTypeConceptIds,
                                        max_outcome_count = maxOutcomeCount)
  
  conn <- connect(connectionDetails)
  
  writeLines("Executing multiple queries. This could take a while")
  executeSql(conn,connectionDetails$dbms,renderedSql)
  
  outcomeSql <-"SELECT * FROM #ccd_outcome_input_for_ps ORDER BY stratum_id, row_id"
  outcomeSql <- translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql
  
  
  covariateSql <-"SELECT * FROM #ccd_covariate_input_for_ps ORDER BY stratum_id, row_id, covariate_id"
  covariateSql <- translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql
  
  writeLines("Loading data for propensity model")
  ccdData <- dbGetCcdInput(conn,outcomeSql,covariateSql,modelType = "clr")
  
  writeLines("Fitting propensity model")
  ccdFit <- fitCcdModel(ccdData, prior = prior("normal",0.01))
  
  #Remove temp tables:
  renderedSql <- loadRenderTranslateSql("CMRemoveTempTables.sql",
                                        packageName = "CohortMethod",
                                        dbms = connectionDetails$dbms,
                                        CDM_schema = cdmSchema)
  
  executeSql(conn,connectionDetails$dbms,renderedSql)
  
  
  dummy <- dbDisconnect(conn)
}

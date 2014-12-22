# @file VignetteDataFetch.R
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

vignetteDataFetch <- function(){
  # This function should be used to fetch the data that is used in the vignettes.
  library(SqlRender)
  library(DatabaseConnector)
  library(CohortMethod)
  setwd("c:/temp")
  
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07.jnj.com"
  cdmSchema <- "cdm_truven_mdcd"
  resultsSchema <- "scratch"
  port <- NULL
  
  connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema,port=port)
  sql <- readSql("coxibVsNonselVsGiBleed.sql")
  sql <- renderSql(sql,cdmSchema = cdmSchema, resultsSchema = resultsSchema)$sql
  sql <- translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  
  connection <- connect(connectionDetails)
  executeSql(connection, sql)
  
  
  # Check number of subjects per cohort:
  sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsSchema.dbo.coxibVsNonselVsGiBleed GROUP BY cohort_definition_id"
  sql <- renderSql(sql, resultsSchema = resultsSchema)$sql
  sql <- translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  querySql(connection, sql)
  
  # Get all NSAIDs:
  sql <- "SELECT concept_id FROM concept_ancestor INNER JOIN concept ON descendant_concept_id = concept_id WHERE ancestor_concept_id = 21603933"
  nsaids <- querySql(connection, sql)
  nsaids <- nsaids$CONCEPT_ID
  
  #Load data:
  cohortData <- getDbCohortData(connectionDetails,
                                cdmSchema = cdmSchema,
                                resultsSchema = resultsSchema,
                                targetDrugConceptId = 1,
                                comparatorDrugConceptId = 2, 
                                indicationConceptIds = "",
                                washoutWindow = 183, 
                                indicationLookbackWindow = 183,
                                studyStartDate = "", 
                                studyEndDate = "", 
                                exclusionConceptIds = nsaids,
                                outcomeConceptIds = 3, 
                                outcomeConditionTypeConceptIds = "", 
                                maxOutcomeCount = 1,
                                exposureSchema = resultsSchema,
                                exposureTable = "coxibVsNonselVsGiBleed",
                                outcomeSchema = resultsSchema,
                                outcomeTable = "coxibVsNonselVsGiBleed",
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
                                useCovariateInteractionYear = FALSE, 
                                useCovariateInteractionMonth = FALSE,
                                excludedCovariateConceptIds = nsaids, 
                                deleteCovariatesSmallCount = 100)
  
  saveCohortData(cohortData,"coxibVsNonselVsGiBleed")
  
  vignetteSimulationProfile <- createCohortDataSimulationProfile(cohortData)
  save(vignetteSimulationProfile, file = "vignetteSimulationProfile.rData")
}
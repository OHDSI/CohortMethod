TestCodePatrick <- function(){
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07"
  #cdmSchema <- "cdm4_sim"
  cdmSchema <- "CDM_Truven_MDCR"
  resultsSchema <- "scratch"
  port <- NULL
  
  connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema,port=port)
  
  
  cohortData <- getDbCohortData(connectionDetails,cdmSchema=cdmSchema,resultsSchema=resultsSchema,
                                      targetDrugConceptId = 1,
                                      comparatorDrugConceptId = 2, 
                                      indicationConceptIds = "",
                                      washoutWindow = 183, 
                                      indicationLookbackWindow = 183,
                                      studyStartDate = "", 
                                      studyEndDate = "", 
                                      exclusionConceptIds = "",
                                      outcomeConceptIds = c(101,102,103,104,105,106,107,108,109,110), 
                                      outcomeConditionTypeConceptIds = "", 
                                      maxOutcomeCount = 1,
                                      exposureSchema = resultsSchema,
                                      exposureTable = "MDCR_RivaWarfCER_cohort",
                                      outcomeSchema = resultsSchema,
                                      outcomeTable = "MDCR_RivaWarfCER_cohort",
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
                                      excludedCovariateConceptIds = "", 
                                      deleteCovariatesSmallCount = 100)
  
  
  
  #SELECT cohort_definition_id,
  #count(subject_id) as num_persons
  #FROM [Scratch].[dbo].[MDCR_RivaWarfCER_cohort]
  #group by cohort_definition_id
  #order by cohort_definition_id
  
}
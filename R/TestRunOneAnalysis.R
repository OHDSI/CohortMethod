testRunOneAnalysis <- function(){
  library(CohortMethod)
  library(snow)
  setwd("c:/temp")
  options("fftempdir" = "c:/temp")
  cluster <- makeCluster(16)
  
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07"
  cdmSchema <- "CDM_Truven_MDCR"
  resultsSchema <- "scratch"
  port <- NULL
  
  connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema,port=port)
  
  conn <- connect(connectionDetails)
  excludeSql <-"
select concept_id
from concept
where concept_id in (40241331, 43534760, 21601027, 21600960, 1310149, 21600962)

union

select descendant_concept_id as concept_id
from concept_ancestor
where ancestor_concept_id in (40241331, 21600960)
"
  excludeSql <- SqlRender::translateSql(excludeSql,targetDialect = connectionDetails$dbms)$sql
  excludedCovariates <- dbGetQuery(conn,excludeSql)
  save(excludedCovariates,file="mdcrExcludedCovariates")
  
  sql <- "SELECT cohort_definition_id,count(subject_id) as num_persons FROM [Scratch].[dbo].[MDCR_RivaWarfCER_cohort] group by cohort_definition_id order by cohort_definition_id"
  x <-querySql(connect(connectionDetails),sql)
  ###
  
  cohortData <- getDbCohortData(connectionDetails,cdmSchema=cdmSchema,resultsSchema=resultsSchema,
                                targetDrugConceptId = 1,
                                comparatorDrugConceptId = 2, 
                                indicationConceptIds = "",
                                washoutWindow = 183, 
                                indicationLookbackWindow = 183,
                                studyStartDate = "", 
                                studyEndDate = "", 
                                exclusionConceptIds = "",
                                outcomeConceptIds = c(1000028060,1000134438,1035406331,1035406349,1035406361,1035406391,1035406402,1035506612,1035506621,1035606949,1035607026,1035607337,1035607461,1036110386,1036110587,1036110933,1036110951,1036211101,1036313741,1036314156,1036315380,1036315910,1036315934,1036416637,1036416695,1036516895,1036516905,1036516909,1036617158,1036617163,1036617187,1036617553,1036718555,1036918850,1036918858,1036919212,1037019460,1037119529,1037119539,1037119607,1037320079,1037320098,1037320318,1037420426,1037420593,1037521024,1037522022), 
                                outcomeConditionTypeConceptIds = "", 
                                maxOutcomeCount = 1,
                                exposureSchema = resultsSchema,
                                exposureTable = "MDCR_RivaWarfCER_cohort",
                                outcomeSchema = resultsSchema,
                                outcomeTable = "MDCR_RivaWarfCER_cohort",
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
                                useCovariateInteractionMonth = TRUE,
                                excludedCovariateConceptIds = excludedCovariates$concept_id, 
                                deleteCovariatesSmallCount = 100)
  
  #saveCohortData(cohortData,"OPTUMCohortDataNegControls4")
  
  cohortData <- loadCohortData("OPTUMCohortDataNegControls4") 
  outcomeCounts <- summary(cohortData)$outcomeCounts
  outcomeCounts <- outcomeCounts[outcomeCounts$eventCount != 0,]
  
  ps <- createPs(cohortData, 
                 checkSorting = FALSE,
                 outcomeConceptId = outcomeCounts$outcomeConceptId[1],
                 control = createControl(noise="quiet"))
  psVariance = attr(ps,"priorVariance")
  psVariance <- 0.007
  clusterExport(cluster,c("cohortData","psVariance"))
  runOutcome <- function(outcomeConceptId){
    library(CohortMethod)
    ps <- createPs(cohortData, outcomeConceptId = outcomeConceptId, prior=createPrior("laplace",psVariance,exclude = 0))
    save(ps,file = paste("analysis_1/ps_",outcomeConceptId,".rData",sep=""))
    
    psTrimmed <- trimByPsToEquipoise(ps)
    strata <- matchOnPs(psTrimmed, caliper = 0.25, caliperScale = "standardized",maxRatio=999)
    
    #balance <- computeCovariateBalance(strata, cohortData, outcomeConceptId = outcomeConceptId)
    #save(balance,file = paste("analysis_1/balance_",outcomeConceptId,".rData",sep=""))
    
    outcomeModel <- fitOutcomeModel(outcomeConceptId, 
                                    cohortData,
                                    strata,
                                    riskWindowStart = 0, 
                                    riskWindowEnd = 7, 
                                    addExposureDaysToEnd = TRUE,
                                    useCovariates = TRUE, 
                                    modelType = "cox", 
                                    stratifiedCox = TRUE,
                                    prior = createPrior("laplace",psVariance)
    )
    save(outcomeModel,file = paste("analysis_1/outcomeModel_",outcomeConceptId,".rData",sep=""))
  }
  parSapply(cluster,outcomeCounts$outcomeConceptId,runOutcome)
}

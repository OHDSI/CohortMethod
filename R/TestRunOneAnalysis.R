testRunOneAnalysis <- function(){
  library(CohortMethod)
  library(snowfall)
  setwd("c:/temp")
  options("fftempdir" = "c:/temp")
  sfInit(parallel=TRUE, cpus=32)
  
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT05"
  cdmSchema <- "CDM_optum"
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
  excludedCovariates <- querySql(conn,excludeSql)
  save(excludedCovariates,file="optumExcludedCovariates")
  
  sql <- "SELECT cohort_definition_id,count(subject_id) as num_persons FROM [Scratch].[dbo].[Optum_RivaWarfCER_cohort] group by cohort_definition_id order by cohort_definition_id"
  cohortIds <- querySql(conn,sql)
  cohortIds <- cohortIds$COHORT_DEFINITION_ID[cohortIds$COHORT_DEFINITION_ID > 2]
  
  cohortData <- getDbCohortData(connectionDetails,cdmSchema=cdmSchema,resultsSchema=resultsSchema,
                                targetDrugConceptId = 1,
                                comparatorDrugConceptId = 2, 
                                indicationConceptIds = "",
                                washoutWindow = 183, 
                                indicationLookbackWindow = 183,
                                studyStartDate = "", 
                                studyEndDate = "", 
                                exclusionConceptIds = "",
                                outcomeConceptIds = cohortIds,
                                outcomeConditionTypeConceptIds = "", 
                                maxOutcomeCount = 1,
                                exposureSchema = resultsSchema,
                                exposureTable = "Optum_RivaWarfCER_cohort",
                                outcomeSchema = resultsSchema,
                                outcomeTable = "Optum_RivaWarfCER_cohort",
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
                                excludedCovariateConceptIds = excludedCovariates$CONCEPT_ID, 
                                deleteCovariatesSmallCount = 100)
  
  saveCohortData(cohortData,"OPTUMCohortDataNegControlsNoInteract")
  
  cohortData <- loadCohortData("OPTUMCohortDataNegControlsNoInteract") 
  outcomeCounts <- summary(cohortData)$outcomeCounts
  outcomeCounts <- outcomeCounts[outcomeCounts$eventCount != 0,]
  
  ps <- createPs(cohortData, 
                 checkSorting = FALSE,
                 outcomeConceptId = outcomeCounts$outcomeConceptId[1],
                 control = createControl(noise="quiet",lowerLimit = 0.001, upperLimit = 1))
  psVariance = attr(ps,"priorVariance")
  psVariance <- 0.01

  sfExport("cohortData")
  sfExport("psVariance")
  runOutcome <- function(outcomeConceptId){
    if (file.exists(paste("analysis_1/outcomeModel_",outcomeConceptId,".rData",sep="")))
      return()
    
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
  dir.create("analysis_1")
  dummy <- sfClusterApplyLB((outcomeCounts$outcomeConceptId,runOutcome)
  
  #Reading all the estimates into one data frame:
  logRr <- c()
  conceptId <- c()
  ub <- c()
  lb <- c()
  for (outcomeConceptId in outcomeCounts$outcomeConceptId){
    file = paste("analysis_1/outcomeModel_",outcomeConceptId,".rData",sep="")
    if (file.exists(file)){
      load(file=paste("analysis_1/outcomeModel_",outcomeConceptId,".rData",sep=""))
      conceptId <- c(conceptId, outcomeConceptId)
      logRr <- c(logRr,coef(outcomeModel))
      lb <- c(lb,confint(outcomeModel)[1])
      ub <- c(ub,confint(outcomeModel)[2])
    }
  }
  data <- data.frame(conceptId = conceptId, logRr = logRr, lb = lb, ub = ub)
}


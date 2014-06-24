testCode <- function(){
  
  pw <- ""
  
  #Test on SQL Server
  #setwd("c:/temp")
  #connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
  #cdmSchema = "cdm4_sim"
  #cdmSchema = "CDM_Truven_MDCR"
  
  #Test on PostgreSQL
  setwd("c:/temp")
  connectionDetails <- createConnectionDetails(dbms="postgresql", server="localhost/ohdsi", user="postgres",password=pw)
  cdmSchema = "cdm4_sim"
  
  conn <- connect(connectionDetails)
  
  renderedSql <- loadRenderTranslateSql("CohortMethod.sql",
                                        packageName = "CohortMethod",
                                        dbms = connectionDetails$dbms,
                                        CDM_schema = cdmSchema)
  
  
  writeLines("Executing multiple queries. This could take a while")
  executeSql(conn,connectionDetails$dbms,renderedSql,TRUE)
  
  outcomeSql <-"SELECT * FROM #ccd_outcome_input_for_ps ORDER BY stratum_id, row_id"
  outcomeSql <- translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql
  
  covariateSql <-"SELECT * FROM #ccd_covariate_input_for_ps ORDER BY stratum_id, row_id, covariate_id"
  covariateSql <- translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql
  
  writeLines("Loading data for propensity model")
  ccdData <- dbGetCcdInput(conn,outcomeSql,covariateSql,modelType = "clr")
  
  writeLines("Fitting propensity model")
  ccdFit <- fitCcdModel(ccdData, prior = prior("normal",0.01))
  
}
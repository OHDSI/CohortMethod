
testCode <- function(){
  
  pw <- ""
  user <- "postgres"
  server <- "localhost/ohdsi"
  
  #Test on SQL Server
  #setwd("c:/temp")
  #connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
  #cdmSchema = "cdm4_sim"
  #cdmSchema = "CDM_Truven_MDCR"
  
  #Test on PostgreSQL
  setwd("c:/temp")
  connectionDetails <- createConnectionDetails(dbms="postgresql", server=server, user=user, password=pw)
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
  startQuery <- Sys.time()
  ccdData <- dbGetCcdInput(conn,outcomeSql,covariateSql,modelType = "lr",addIntercept=TRUE)
  ccdDelta <- Sys.time() - startQuery
  writeLines(paste("Load took", signif(ccdDelta,3), attr(ccdDelta,"units")))
  
  writeLines("Fitting propensity model")
  startQuery <- Sys.time()
  ccdFit <- fitCcdModel(ccdData, prior = prior("laplace",0.1))
  delta <- Sys.time() - startQuery
  writeLines(paste("Fitting took", signif(delta,3), attr(delta,"units")))
  #sum(coef(ccdFit)>0.1) 

  p <- predict(ccdFit)
  
  #Show preference score distributions:
  ySql <-"SELECT row_id,y FROM #ccd_outcome_input_for_ps"
  ySql <- translateSql(ySql,"sql server",connectionDetails$dbms)$sql
  y <- dbGetQuery(conn,ySql)
  colnames(y) <- tolower(colnames(y))
  p <- data.frame(pred = p, row_id = as.numeric(attr(p,"names")))
  #compute preference score:
  prop <- sum(y$y / nrow(y))
  interm <- exp(log(p$pred/(1-p$pred)) - log(prop/(1-prop)))
  p$pref <- interm / (interm+1)
  m <- merge(y,p,by=c("row_id"))
  
  library(ggplot2)
  ggplot(m, aes(x=pref,color=as.factor(y),group=as.factor(y),fill=as.factor(y))) + 
    geom_density() +
    scale_fill_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) +
    scale_color_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5)))
  
  
  dbDisconnect(conn)
  return(NULL)
  
  #to store temp tables:
  dbSendUpdate(conn,"CREATE TABLE temp_outcomes AS SELECT * FROM ccd_outcome_input_for_ps")
  dbSendUpdate(conn,"CREATE TABLE temp_covariates AS SELECT * FROM ccd_covariate_input_for_ps")
  
  #to restore temp tables:
  dbSendUpdate(conn,"SET SEARCH_PATH TO cdm4_sim")
  dbSendUpdate(conn,"CREATE TEMP TABLE ccd_outcome_input_for_ps AS SELECT * FROM temp_outcomes")
  dbSendUpdate(conn,"CREATE TEMP TABLE ccd_covariate_input_for_ps AS SELECT * FROM temp_covariates")
  
  #cross-validation example:
  ccdFit <- fitCcdModel(ccdData, 
                        prior = prior("laplace", useCrossValidation = TRUE),
                        control = control(cvType = "auto", cvRepetitions = 2, noiseLevel = "quiet"))
  
}

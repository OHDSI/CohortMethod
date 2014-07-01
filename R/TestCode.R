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
                                        CDM_schema = cdmSchema,
                                        results_schema = cdmSchema)
  
  
  writeLines("Executing multiple queries. This could take a while")
  executeSql(conn,connectionDetails$dbms,renderedSql,TRUE)
  
  outcomeSql <-"SELECT * FROM #ccd_outcome_input_for_ps ORDER BY stratum_id, row_id"
  outcomeSql <- translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql
  
  covariateSql <-"SELECT * FROM #ccd_covariate_input_for_ps ORDER BY stratum_id, row_id, covariate_id"
  covariateSql <- translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql
 
  writeLines("Loading data for propensity model")
  startQuery <- Sys.time()
  ccdData <- dbGetCcdInput(conn,outcomeSql,covariateSql,modelType = "lr",addIntercept=TRUE)
  delta <- Sys.time() - startQuery
  writeLines(paste("Load took", signif(delta,3), attr(delta,"units")))
  
  writeLines("Fitting propensity model")
  startQuery <- Sys.time()
  ccdFit <- fitCcdModel(ccdData, prior = prior("laplace",0.1))
  delta <- Sys.time() - startQuery
  writeLines(paste("Fitting took", signif(delta,3), attr(delta,"units")))
  #sum(coef(ccdFit)>0.1) 

  data <- predict(ccdFit)
  data <- data.frame(propensityScore = data, row_id = as.numeric(attr(data,"names")))
  
  #Merge with outcome data:
  ySql <-"SELECT row_id,y FROM #ccd_outcome_input_for_ps"
  ySql <- translateSql(ySql,"sql server",connectionDetails$dbms)$sql
  y <- dbGetQuery(conn,ySql)
  colnames(y) <- tolower(colnames(y))
  
  data <- merge(y,data,by=c("row_id"))
  
  
 # m <- Match(Tr=data$y, X=data$propensityScore, replace=FALSE)
  #MatchBalance(y~propensityScore, data=data, match.out=m)
  
  
  is.numeric(data$propensityScore)
  m_out <- matchit(y ~ propensityScore, data = data, method="nearest",distance=data$propensityScore)
  m_out2 <- matchit(y ~ propensityScore, data = data, method="nearest")
  plot(m_out)
  plot(m_out,type="jitter")
  
  plot(m_out,type="hist")
  head(m_out$match.matrix)
  head(m_out2$match.matrix)
  head(m_out$subclass)
  #Show preference score distributions:
  
  #compute preference score:
  prop <- sum(y$y / nrow(y))
  interm <- exp(log(p$pred/(1-p$pred)) - log(prop/(1-prop)))
  p$pref <- interm / (interm+1)
  
  
  library(ggplot2)
  ggplot(m, aes(x=pref,color=as.factor(y),group=as.factor(y),fill=as.factor(y))) + 
    geom_density() +
    scale_fill_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) +
    scale_color_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5)))
  
  
  dbDisconnect(conn)
  
  #to store temp tables:
  #dbSendUpdate(conn,"CREATE TABLE temp_outcomes AS SELECT * FROM ccd_outcome_input_for_ps")
  #dbSendUpdate(conn,"CREATE TABLE temp_covariates AS SELECT * FROM ccd_covariate_input_for_ps")
  
  #to restore temp tables:
  #dbSendUpdate(conn,"SET SEARCH_PATH TO cdm4_sim")
  #dbSendUpdate(conn,"CREATE TEMP TABLE ccd_outcome_input_for_ps AS SELECT * FROM temp_outcomes")
  #dbSendUpdate(conn,"CREATE TEMP TABLE ccd_covariate_input_for_ps AS SELECT * FROM temp_covariates")
}

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

executeSql <- function(conn, dbms, sql){
  sqlStatements = splitSql(sql)
  progressBar <- txtProgressBar(style=3)
  start <- Sys.time()
  for (i in 1:length(sqlStatements)){
    sqlStatement <- sqlStatements[i]
    #sink(paste("c:/temp/statement_",i,".sql",sep=""))
    #cat(sqlStatement)
    #sink()
    tryCatch ({   
      #startQuery <- Sys.time()
      dbSendUpdate(conn, sqlStatement)
      #delta <- Sys.time() - startQuery
      #writeLines(paste("Statement ",i,"took", delta, attr(delta,"units")))
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
    setTxtProgressBar(progressBar, i/length(sqlStatements))
  }
  close(progressBar)
  delta <- Sys.time() - start
  writeLines(paste("Analysis took", signif(delta,3), attr(delta,"units")))
}

#' @export
cohortMethod <- function(connectionDetails, cdmSchema){
  renderedSql <- loadRenderTranslateSql("CohortMethod.sql",
                                        packageName = "CohortMethod",
                                        dbms = connectionDetails$dbms,
                                        CDM_schema = cdmSchema)
  
  conn <- connect(connectionDetails)
  
  writeLines("Executing multiple queries. This could take a while")
  executeSql(conn,connectionDetails$dbms,renderedSql)
  
  outcomeSql <-"SELECT * FROM #ccd_outcome_input_for_ps ORDER BY stratum_id, row_id"
  outcomeSql <- translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql
  
  
  covariateSql <-"SELECT * FROM #ccd_covariate_input_for_ps ORDER BY stratum_id, row_id, covariate_id"
  covariateSql <- translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql
  
  writeLines("Loading data for propensity model")
  ccdData <- dbGetCCDInput(conn,outcomeSql,covariateSql,modelType = "clr")
  
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

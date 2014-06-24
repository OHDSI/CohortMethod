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

renderAndTranslate <- function(sqlFilename, packageName, dbms, ...){
  pathToSql <- system.file(paste("sql/",gsub(" ","_",dbms),sep=""), sqlFilename, package=packageName)
  mustTranslate <- !file.exists(pathToSql)
  if (mustTranslate) # If DBMS-specific code does not exists, load SQL Server code and translate after rendering
    pathToSql <- system.file(paste("sql/","sql_server",sep=""), sqlFilename, package=packageName)      
  parameterizedSql <- readChar(pathToSql,file.info(pathToSql)$size)  
  
  renderedSql <- renderSql(parameterizedSql[1], ...)$sql
  
  if (mustTranslate)
    renderedSql <- translateSql(renderedSql, "sql server", dbms)$sql
  
  renderedSql
}

#. @export
cohortMethod <- function(connectionDetails, cdmSchema){
  renderedSql <- renderAndTranslate("CohortMethod.sql",
                     packageName = "CohortMethod",
                     dbms = connectionDetails$dbms,
                     CDM_schema = cdmSchema)
  
  conn <- connect(connectionDetails)
  
  writeLines("Executing multiple queries. This could take a while")
  executeSql(conn,connectionDetails$dbms,renderedSql)
  writeLines("Done")
  
  outcomeSql <-"SELECT * FROM #ccd_outcome_input_for_ps ORDER BY stratum_id, row_id"
  outcomeSql <- translateSql(outcomeSql,"sql server",connectionDetails$dbms)$sql

  
  covariateSql <-"SELECT * FROM #ccd_covariate_input_for_ps ORDER BY stratum_id, row_id, covariate_id"
  covariateSql <- translateSql(covariateSql,"sql server",connectionDetails$dbms)$sql

  ccdData <- dbGetCCDInput(conn,outcomeSql,covariateSql,modelType = "clr")
  
  ccdFit <- fitCcdModel(ccdData, prior = prior("normal",0.01))
  #Todo: delete temp tables (for Oracle)
  dummy <- dbDisconnect(conn)
}

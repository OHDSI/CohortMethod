# @file InterfacecovarsToCcd.R
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

openResultSet <- function(conn, sql, batchSize) {
  .jcall(conn@jc,"V",method="setAutoCommit",FALSE)
  s <- .jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
  .jcall(s,"V",method="setFetchSize",as.integer(batchSize))
  r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery",as.character(sql)[1])
  md <- .jcall(r, "Ljava/sql/ResultSetMetaData;", "getMetaData", check=FALSE)
  new("JDBCResult", jr=r, md=md, stat=s, pull=.jnull())
}

lastRowNotHavingThisValue <- function(column, value){
  if (column[1] == value)
    return(0)
  for (i in length(column):1){
    if (column[i] != value)
      return(i)
  }
  return(0)
}

#' Get data from the database and insert it in a CCD data object
#'
#' @description
#' \code{dbGetCCDInput} loads data from the database using two queries, and inserts it into a CCD data object.
#' 
#' @param outcomeSql    A SQL select statement that returns a dataset of outcomes with predefined columns (see below).
#' @param covariateSql  A SQL select statement that returns a dataset of covariates with predefined columns (see below).
#' @param modelType		  CCD model type. Current supported types are "ls", "pr", "lr", "clr", "sccs", or "cox"
#' @param addIntercept  Add an intercept to the model?
#' @param useOffsetCovariate  Use the time variable in the model as an offset?
#' @param offsetAlreadyOnLogScale Is the time variable already on a log scale?
#' @param sortCovariates Do the covariates still need to be sorted?
#' @param batchSize			Number of rows to be read from each table at a time. Larger batch sizes lead to fewer calls to the 
#' database and should be more efficient, but may lead to out-of-memory errors.
#'
#' @details
#' These columns are expected in the outcome table:
#' \tabular{lll}{  
#'   \verb{stratum_id}		\tab(integer) \tab Stratum ID for conditional regression models \cr
#'   \verb{row_id}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{y}    \tab(real) \tab The outcome variable \cr
#'   \verb{time}    \tab(real) \tab For models that use time (e.g. Poisson or Cox regression) this contains time (e.g. number of days) \cr
#' }
#' The outcome table should be sorted by stratum_id and then by row_id
#' 
#' These columns are expected in the covariates table:
#' \tabular{lll}{  
#'   \verb{stratum_id}  	\tab(integer) \tab Stratum ID for conditional regression models \cr
#'   \verb{row_id}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{covariate_id}    \tab(integer) \tab A numeric identifier of a covariate  \cr
#'   \verb{covariate_value}    \tab(real) \tab The value of the specified covariate \cr
#' }
#' The covariate table should be sorted by stratum_id, row_id, and covariate_id
#' 
#' @return              
#' An object of type CCDModel
#' 
#' @examples \dontrun{
#'   connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com", schema="test")
#'   connection <- connect(connectionDetails)
#'   outcomeSql <- "SELECT * FROM outcomes ORDER BY stratum_id, row_id"
#'   covariateSql <-"SELECT * FROM covariates ORDER BY stratum_id, row_id, covariate_id"
#'   
#'   ccdData <- dbGetCCDInput(connection,outcomeSql,covariateSql,modelType = "clr")
#'   
#'   dbDisconnect(connection)
#'   
#'   ccdFit <- fitCcdModel(ccdData, prior = prior("normal",0.01))
#' }
#' @export
dbGetCCDInput <- function(connection, 
                          outcomeSql, 
                          covariateSql, 
                          modelType = "clr", 
                          addIntercept = FALSE,
                          useOffsetCovariate = NULL,
                          offsetAlreadyOnLogScale = FALSE,
                          sortCovariates = TRUE,
                          makeCovariatesDense = NULL,
                          batchSize = 100000){

  # Open resultSets:
  .jcall("java/lang/System",,"gc")
  resultSetOutcome <- openResultSet(connection, outcomeSql,batchSize)
  resultSetCovars <- openResultSet(connection, covariateSql,batchSize)
  
  exitFunction <- function(){
    dbClearResult(resultSetOutcome)
    dbClearResult(resultSetCovars)
    .jcall(conn@jc,"V",method="setAutoCommit",TRUE)
  }
  on.exit(exitFunction())
  
  # Construct empty CCD data object:
  dataPtr <- createSqlCcdData(modelType = modelType)
  
  #Fetch data in batches:
  doneCovars <- FALSE
  doneOutcome <- FALSE
  batchOutcome <- fetch(resultSetOutcome, batchSize)
  colnames(batchOutcome) <- toupper(colnames(batchOutcome))
  doneOutcome = (nrow(batchOutcome) != batchSize)
  
  lastUsedOutcome <- 0
  spillOverCovars <- NULL
  while (!doneCovars){
    #Get covars:
    batchCovars <- fetch(resultSetCovars, batchSize)
    doneCovars = (nrow(batchCovars) != batchSize)
    colnames(batchCovars) <- toupper(colnames(batchCovars))
    lastRowId <- batchCovars$ROW_ID[nrow(batchCovars)]
    lastStratumId <- batchCovars$STRATUM_ID[nrow(batchCovars)]
    
    endCompleteRow <- lastRowNotHavingThisValue(batchCovars$ROW_ID,lastRowId)
    endCompleteStratum <- lastRowNotHavingThisValue(batchCovars$STRATUM_ID,lastStratumId)
    if (endCompleteStratum > endCompleteRow)
      endCompleteRow = endCompleteStratum
    
    if (endCompleteRow == 0){ #Entire batch is about 1 row
      if (!is.null(spillOverCovars)){
        if (spillOverCovars$ROW_ID[1] == batchCovars$ROW_ID[1] & spillOverCovars$STRATUM_ID[1] == batchCovars$STRATUM_ID[1]){ #SpilloverCovars contains info on same row
          spillOverCovars <- rbind(spillOverCovars,batchCovars)
          covarsToCcd <- NULL
        } else { #SplilloverCovars contains covars for a different row
          covarsToCcd <- spillOverCovars
          spillOverCovars <- batchCovars
        }
      } else {
        spillOverCovars <- batchCovars
      }
    } else { #Batch is about different rows (so at least one is complete)
      if (!is.null(spillOverCovars)){
        covarsToCcd <- rbind(spillOverCovars,batchCovars[1:endCompleteRow,])      
      } else {
        covarsToCcd <- batchCovars[1:endCompleteRow,]
      }
      spillOverCovars <- batchCovars[(endCompleteRow+1):nrow(batchCovars),]
    }    
    
    #Get matching outcomes:
    if (!is.null(covarsToCcd)){ # There is a complete row
      completeRowId = covarsToCcd$ROW_ID[nrow(covarsToCcd)]
      completeStratumId = covarsToCcd$STRATUM_ID[nrow(covarsToCcd)]
      endCompleteRowInOutcome <- which(batchOutcome$ROW_ID == completeRowId & batchOutcome$STRATUM_ID == completeStratumId)
      while (length(endCompleteRowInOutcome) == 0 & !doneOutcome){
        if (lastUsedOutcome == nrow(batchOutcome)){
          batchOutcome <- fetch(resultSetOutcome, batchSize)
          colnames(batchOutcome) <- toupper(colnames(batchOutcome))
          doneOutcome = (nrow(batchOutcome) != batchSize)
        } else {      
          newBatchOutcome <- fetch(resultSetOutcome, batchSize)
          colnames(newBatchOutcome) <- toupper(colnames(newBatchOutcome))
          doneOutcome = (nrow(newBatchOutcome) != batchSize)
          batchOutcome <- rbind(batchOutcome[(lastUsedOutcome+1):nrow(batchOutcome),],newBatchOutcome)          
        }
        lastUsedOutcome = 0
        endCompleteRowInOutcome <- which(batchOutcome$ROW_ID == completeRowId & batchOutcome$STRATUM_ID == completeStratumId)
        if (nrow(batchOutcome) > 3*batchSize){
          stop(paste("No matching outcome found for stratum_id =",completeStratumId,"row_id =",completeRowId))
        }
      }
      if (min(covarsToCcd$ROW_ID %in% batchOutcome$ROW_ID) == 0)
        stop("Not all row_ids in covars matched to outcomes")
      
      #Append to CCD:
      appendSqlCcdData(dataPtr,
                       batchOutcome$STRATUM_ID[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       batchOutcome$ROW_ID[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       batchOutcome$Y[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       batchOutcome$TIME[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       covarsToCcd$ROW_ID,
                       covarsToCcd$COVARIATE_ID,
                       covarsToCcd$COVARIATE_VALUE
                       )
      
      lastUsedOutcome = endCompleteRowInOutcome
    }
  }
  #End of covar batches, add spillover to CCD:
  covarsToCcd <- spillOverCovars
  
  completeRowId = covarsToCcd$ROW_ID[nrow(covarsToCcd)]
  completeStratumId = covarsToCcd$STRATUM_ID[nrow(covarsToCcd)]
  endCompleteRowInOutcome <- which(batchOutcome$ROW_ID == completeRowId & batchOutcome$STRATUM_ID == completeStratumId)
  while (length(endCompleteRowInOutcome) == 0 & !doneOutcome){
    if (lastUsedOutcome == nrow(batchOutcome)){
      batchOutcome <- fetch(resultSetOutcome, batchSize)
      colnames(batchOutcome) <- toupper(colnames(batchOutcome))
      doneOutcome = (nrow(batchOutcome) != batchSize)
    } else {      
      newBatchOutcome <- fetch(resultSetOutcome, batchSize)
      colnames(newBatchOutcome) <- toupper(colnames(newBatchOutcome))
      doneOutcome = (nrow(newBatchOutcome) != batchSize)
      batchOutcome <- rbind(batchOutcome[(lastUsedOutcome+1):nrow(batchOutcome),],newBatchOutcome)          
    }
    lastUsedOutcome = 0
    endCompleteRowInOutcome <- which(batchOutcome$ROW_ID == completeRowId & batchOutcome$STRATUM_ID == completeStratumId)
    if (nrow(batchOutcome) > 3*batchSize){
      stop(paste("No matching outcome found for stratum_id =",completeStratumId,"row_id =",completeRowId))
    }
  }
  
  #Append to CCD:
  appendSqlCcdData(dataPtr,
                   batchOutcome$STRATUM_ID[(lastUsedOutcome+1):endCompleteRowInOutcome],
                   batchOutcome$ROW_ID[(lastUsedOutcome+1):endCompleteRowInOutcome],
                   batchOutcome$Y[(lastUsedOutcome+1):endCompleteRowInOutcome],
                   batchOutcome$TIME[(lastUsedOutcome+1):endCompleteRowInOutcome],
                   covarsToCcd$ROW_ID,
                   covarsToCcd$COVARIATE_ID,
                   covarsToCcd$COVARIATE_VALUE
  )
  
  lastUsedOutcome = endCompleteRowInOutcome
  
  #Add any outcomes that are left (without matching covar information):
  if (lastUsedOutcome < nrow(batchOutcome)){
    appendSqlCcdData(dataPtr,
                     batchOutcome$STRATUM_ID[(lastUsedOutcome+1):nrow(batchOutcome)],
                     batchOutcome$ROW_ID[(lastUsedOutcome+1):nrow(batchOutcome)],
                     batchOutcome$Y[(lastUsedOutcome+1):nrow(batchOutcome)],
                     batchOutcome$TIME[(lastUsedOutcome+1):nrow(batchOutcome)],
                     c(),
                     c(),
                     c())
  }
  while (!doneOutcome){
    batchOutcome <- fetch(resultSetOutcome, batchSize)
    colnames(batchOutcome) <- toupper(colnames(batchOutcome))
    doneOutcome = (nrow(newBatchOutcome) != batchSize)
    
    appendSqlCcdData(dataPtr,
                     batchOutcome$STRATUM_ID,
                     batchOutcome$ROW_ID,
                     batchOutcome$Y,
                     batchOutcome$TIME,
                     c(),
                     c(),
                     c())
  }
  finalizeSqlCcdData(dataPtr)
  dataPtr
}

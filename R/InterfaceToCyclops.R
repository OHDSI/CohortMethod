# @file InterfacecovarsToCyclops.R
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
  rs <- new.env()
  assign("resultSet",new("JDBCResult", jr=r, md=md, stat=s, pull=.jnull()),envir=rs)
  assign("batchSize",batchSize,envir=rs)
  assign("done",FALSE,envir=rs)
  rs
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

constructCyclopsDataFromBatchableSources <- function(resultSetOutcome,
                                                     resultSetCovariate,
                                                     getOutcomeBatch,
                                                     getCovariateBatch,
                                                     isDone,
                                                     modelType = "lr", 
                                                     addIntercept = TRUE,
                                                     offsetAlreadyOnLogScale = FALSE,
                                                     sortCovariates = TRUE,
                                                     makeCovariatesDense = NULL){
  if ((modelType == "clr" | modelType == "cpr") & addIntercept){
    warning("Intercepts are not allowed in conditional models, removing intercept",call.=FALSE)
    addIntercept = FALSE
  }
  
  # Construct empty Cyclops data object:
  dataPtr <- createSqlCyclopsData(modelType = modelType)
  
  #Fetch data in batches:
  batchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
  
  lastUsedOutcome <- 0
  spillOverCovars <- NULL
  while (!isDone(resultSetCovariate)){
    #Get covars:
    batchCovars <- getCovariateBatch(resultSetCovariate,modelType)
    lastRowId <- batchCovars$ROW_ID[nrow(batchCovars)]
    endCompleteRow <- lastRowNotHavingThisValue(batchCovars$ROW_ID,lastRowId)
    
    if (endCompleteRow == 0){ #Entire batch is about 1 row
      if (!is.null(spillOverCovars)){
        if (spillOverCovars$ROW_ID[1] == batchCovars$ROW_ID[1]){ #SpilloverCovars contains info on same row
          spillOverCovars <- rbind(spillOverCovars,batchCovars)
          covarsToCyclops <- NULL
        } else { #SplilloverCovars contains covars for a different row
          covarsToCyclops <- spillOverCovars
          spillOverCovars <- batchCovars
        }
      } else {
        spillOverCovars <- batchCovars
      }
    } else { #Batch is about different rows (so at least one is complete)
      if (!is.null(spillOverCovars)){
        covarsToCyclops <- rbind(spillOverCovars,batchCovars[1:endCompleteRow,])      
      } else {
        covarsToCyclops <- batchCovars[1:endCompleteRow,]
      }
      spillOverCovars <- batchCovars[(endCompleteRow+1):nrow(batchCovars),]
    }    
    
    #Get matching outcomes:
    if (!is.null(covarsToCyclops)){ # There is a complete row
      completeRowId = covarsToCyclops$ROW_ID[nrow(covarsToCyclops)]
      endCompleteRowInOutcome <- which(batchOutcome$ROW_ID == completeRowId)
      while (length(endCompleteRowInOutcome) == 0 & !isDone(resultSetOutcome)){
        if (lastUsedOutcome == nrow(batchOutcome)){
          batchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
        } else {      
          newBatchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
          batchOutcome <- rbind(batchOutcome[(lastUsedOutcome+1):nrow(batchOutcome),],newBatchOutcome)          
        }
        lastUsedOutcome = 0
        endCompleteRowInOutcome <- which(batchOutcome$ROW_ID == completeRowId)
      }
      #Append to Cyclops:
      appendSqlCyclopsData(dataPtr,
                           batchOutcome$STRATUM_ID[(lastUsedOutcome+1):endCompleteRowInOutcome],
                           batchOutcome$ROW_ID[(lastUsedOutcome+1):endCompleteRowInOutcome],
                           batchOutcome$Y[(lastUsedOutcome+1):endCompleteRowInOutcome],
                           batchOutcome$TIME[(lastUsedOutcome+1):endCompleteRowInOutcome],
                           covarsToCyclops$ROW_ID,
                           covarsToCyclops$COVARIATE_ID,
                           covarsToCyclops$COVARIATE_VALUE
      )
      
      lastUsedOutcome = endCompleteRowInOutcome
    }
  }
  #End of covar batches, add spillover to Cyclops:
  covarsToCyclops <- spillOverCovars
  
  completeRowId = covarsToCyclops$ROW_ID[nrow(covarsToCyclops)]
  endCompleteRowInOutcome <- which(batchOutcome$ROW_ID == completeRowId)
  while (length(endCompleteRowInOutcome) == 0 & !isDone(resultSetOutcome)){
    if (lastUsedOutcome == nrow(batchOutcome)){
      batchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
    } else {      
      batchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
      batchOutcome <- rbind(batchOutcome[(lastUsedOutcome+1):nrow(batchOutcome),],newBatchOutcome)          
    }
    lastUsedOutcome = 0
    endCompleteRowInOutcome <- which(batchOutcome$ROW_ID == completeRowId)
  }
  
  #Append to Cyclops:
  appendSqlCyclopsData(dataPtr,
                       batchOutcome$STRATUM_ID[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       batchOutcome$ROW_ID[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       batchOutcome$Y[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       batchOutcome$TIME[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       covarsToCyclops$ROW_ID,
                       covarsToCyclops$COVARIATE_ID,
                       covarsToCyclops$COVARIATE_VALUE
  )
  
  lastUsedOutcome = endCompleteRowInOutcome
  
  #Add any outcomes that are left (without matching covar information):
  if (lastUsedOutcome < nrow(batchOutcome)){
    appendSqlCyclopsData(dataPtr,
                         batchOutcome$STRATUM_ID[(lastUsedOutcome+1):nrow(batchOutcome)],
                         batchOutcome$ROW_ID[(lastUsedOutcome+1):nrow(batchOutcome)],
                         batchOutcome$Y[(lastUsedOutcome+1):nrow(batchOutcome)],
                         batchOutcome$TIME[(lastUsedOutcome+1):nrow(batchOutcome)],
                         as.numeric(c()),
                         as.numeric(c()),
                         as.numeric(c()))
  }
  while (!isDone(resultSetOutcome)){
    batchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
    colnames(batchOutcome) <- toupper(colnames(batchOutcome))
    
    appendSqlCyclopsData(dataPtr,
                         batchOutcome$STRATUM_ID,
                         batchOutcome$ROW_ID,
                         batchOutcome$Y,
                         batchOutcome$TIME,
                         as.numeric(c()),
                         as.numeric(c()),
                         as.numeric(c()))
  }
  if (modelType == "pr" | modelType == "cpr") 
    useOffsetCovariate = -1 
  else 
    useOffsetCovariate = NULL
  
  if (modelType != "cox"){
    finalizeSqlCyclopsData(dataPtr,
                           addIntercept = addIntercept,
                           useOffsetCovariate = useOffsetCovariate,
                           offsetAlreadyOnLogScale = offsetAlreadyOnLogScale,
                           sortCovariates = sortCovariates,
                           makeCovariatesDense = makeCovariatesDense)
  }
  dataPtr
}

#' Get data from the database and insert it in a Cyclops data object
#'
#' @description
#' \code{dbCreateCyclopsData} loads data from the database using two queries, and inserts it into a Cyclops data object.
#' 
#' @param connection    A connection to a database (see \code{DatabaseConnector} package).
#' @param outcomeSql    A SQL select statement that returns a dataset of outcomes with predefined columns (see below).
#' @param covariateSql  A SQL select statement that returns a dataset of covariates with predefined columns (see below).
#' @param modelType		  Cyclops model type. Current supported types are "ls", "pr", "lr", "clr", "sccs", or "cox"
#' @param addIntercept  Add an intercept to the model?
#' @param useOffsetCovariate  Use the time variable in the model as an offset?
#' @param offsetAlreadyOnLogScale Is the time variable already on a log scale?
#' @param sortCovariates Do the covariates still need to be sorted?
#' @param batchSize			Number of rows to be read from each table at a time. Larger batch sizes lead to fewer calls to the 
#' database and should be more efficient, but may lead to out-of-memory errors.
#'
#' @details
#' These columns are expected in the outcome object:
#' \tabular{lll}{  
#'   \verb{stratum_id}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{row_id}    \tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{y}    \tab(real) \tab The outcome variable \cr
#'   \verb{time}    \tab(real) \tab For models that use time (e.g. Poisson or Cox regression) this contains time (e.g. number of days) \cr
#' }
#' The outcome table should be sorted by stratum_id and then row_id
#' 
#' These columns are expected in the covariates object:
#' \tabular{lll}{  
#'   \verb{stratum_id}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{row_id}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{covariate_id}    \tab(integer) \tab A numeric identifier of a covariate  \cr
#'   \verb{covariate_value}    \tab(real) \tab The value of the specified covariate \cr
#' }
#' The covariate table should be sorted by stratum_id, row_id and covariate_id
#'  
#' @return              
#' An object of type CyclopsModel
#' 
#' @examples \dontrun{
#'   connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com", schema="test")
#'   connection <- connect(connectionDetails)
#'   outcomeSql <- "SELECT * FROM outcomes ORDER BY row_id"
#'   covariateSql <-"SELECT * FROM covariates ORDER BY row_id, covariate_id"
#'   
#'   cyclopsData <- dbGetCyclopsInput(connection,outcomeSql,covariateSql,modelType = "clr")
#'   
#'   dbDisconnect(connection)
#'   
#'   cyclopsFit <- fitCyclopsModel(cyclopsData, prior = prior("normal",0.01))
#' }
#' @export
dbCreateCyclopsData <- function(connection, 
                                outcomeSql, 
                                covariateSql, 
                                modelType = "lr", 
                                addIntercept = TRUE,
                                offsetAlreadyOnLogScale = FALSE,
                                sortCovariates = TRUE,
                                makeCovariatesDense = NULL,
                                batchSize = 100000){
  
  # Open resultSets:
  .jcall("java/lang/System",,"gc")
  
  resultSetOutcome <- openResultSet(connection, outcomeSql,batchSize)
  
  resultSetCovars <- openResultSet(connection, covariateSql,batchSize)
  
  exitFunction <- function(){
    dbClearResult(get("resultSet",envir=resultSetOutcome))
    dbClearResult(get("resultSet",envir=resultSetCovars))
    .jcall(conn@jc,"V",method="setAutoCommit",TRUE)
  }
  on.exit(exitFunction())
  
  getOutcomeBatch <- function(resultSet, modelType){
    batchOutcome <- fetch(get("resultSet",envir=resultSet), get("batchSize",envir=resultSet))
    if (nrow(batchOutcome) != get("batchSize",envir=resultSet))
      assign("done",TRUE,envir=resultSet)
    colnames(batchOutcome) <- toupper(colnames(batchOutcome))  
    if (modelType == "pr" | modelType == "cpr"| modelType == "cox")
      if (any(batchOutcome$TIME <= 0))
        stop("Time cannot be non-positive",call.=FALSE)
    if (modelType == "lr" | modelType == "pr")
      batchOutcome$STRATUM_ID = batchOutcome$ROW_ID
    if (modelType == "lr" | modelType == "clr")
      batchOutcome$TIME = 0
    batchOutcome
  }
  
  getCovariateBatch <- function(resultSet, modelType){
    batchCovariate <- fetch(get("resultSet",envir=resultSet), get("batchSize",envir=resultSet))
    if (nrow(batchCovariate) != get("batchSize",envir=resultSet))
      assign("done",TRUE,envir=resultSet)
    colnames(batchCovariate) <- toupper(colnames(batchCovariate))  
    batchCovariate
  }
  
  isDone <- function(resultSet){
    get("done",envir=resultSet)
  }
  
  constructCyclopsDataFromBatchableSources(resultSetOutcome,
                                           resultSetCovars,
                                           getOutcomeBatch,
                                           getCovariateBatch,
                                           isDone,
                                           modelType, 
                                           addIntercept,
                                           offsetAlreadyOnLogScale,
                                           sortCovariates,
                                           makeCovariatesDense
  )
  
  
}


#' Convert data from two ffdf objects into a CyclopsData object
#'
#' @description
#' \code{createCyclopsData.ffdf} loads data from two ffdf objects, and inserts it into a Cyclops data object.
#' 
#' @param outcomes    A ffdf object containing the outcomes with predefined columns (see below).
#' @param covariateSql  A ffdf object containing the covariates with predefined columns (see below).
#' @param modelType  	  Cyclops model type. Current supported types are "pr", "cpr", lr", "clr", "sccs", or "cox"
#' @param addIntercept  Add an intercept to the model?
#' @param useOffsetCovariate  Use the time variable in the model as an offset?
#' @param offsetAlreadyOnLogScale Is the time variable already on a log scale?
#' @param sortCovariates Do the covariates still need to be sorted?
#' @param batchBytes			Number of bytes to be read from the ffdf objects at a time. Larger batch sizes lead to fewer conversion calls
#' and should be more efficient, but may lead to out-of-memory errors.
#'
#' @details
#' These columns are expected in the outcome object:
#' \tabular{lll}{  
#'   \verb{stratum_id}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{row_id}    \tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{y}    \tab(real) \tab The outcome variable \cr
#'   \verb{time}    \tab(real) \tab For models that use time (e.g. Poisson or Cox regression) this contains time (e.g. number of days) \cr
#' }
#' The outcome table should be sorted by stratum_id and then row_id
#' 
#' These columns are expected in the covariates object:
#' \tabular{lll}{  
#'   \verb{stratum_id}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{row_id}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{covariate_id}    \tab(integer) \tab A numeric identifier of a covariate  \cr
#'   \verb{covariate_value}    \tab(real) \tab The value of the specified covariate \cr
#' }
#' The covariate table should be sorted by stratum_id, row_id and covariate_id
#'  
#' @return              
#' An object of type CyclopsModel
#' 
#' @examples \dontrun{
#'   connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com", schema="test")
#'   connection <- connect(connectionDetails)
#'   
#'   outcomes <- dbGetQuery.ffdf(conn,"SELECT * FROM outcomes ORDER BY row_id")
#'   covariates <- dbGetQuery.ffdf(conn,"SELECT * FROM covariates ORDER BY row_id, covariate_id")
#'   
#'   cyclopsData <- dbGetCyclopsInput(connection,outcomes,covariates,modelType = "lr")
#'   
#'   dbDisconnect(connection)
#'   
#'   cyclopsFit <- fitCyclopsModel(cyclopsData, prior = prior("normal",0.01))
#' }
#' @export
createCyclopsData.ffdf <- function(outcomes, 
                                   covariates,
                                   modelType = "lr", 
                                   addIntercept = TRUE,
                                   offsetAlreadyOnLogScale = FALSE,
                                   sortCovariates = TRUE,
                                   makeCovariatesDense = NULL){
  colnames(outcomes) <- toupper(colnames(outcomes))  
  colnames(covariates) <- toupper(colnames(covariates))    
  resultSetOutcome <- new.env()
  assign("data",outcomes,envir=resultSetOutcome)
  assign("chunks",chunk(outcomes),envir=resultSetOutcome)
  assign("cursor",1,envir=resultSetOutcome)
  resultSetCovariate <- new.env()
  assign("data",covariates,envir=resultSetCovariate)
  assign("chunks",chunk(covariates),envir=resultSetCovariate)
  assign("cursor",1,envir=resultSetCovariate)
  
  getOutcomeBatch <- function(resultSetOutcome, modelType){
    data <- get("data",envir=resultSetOutcome)
    chunks <- get("chunks",envir=resultSetOutcome)
    cursor <- get("cursor",envir=resultSetOutcome)
    batchOutcome <- data[chunks[[cursor]],]
    assign("cursor",cursor+1,envir=resultSetOutcome)
    if (modelType == "pr" | modelType == "cpr"| modelType == "cox")
      if (any(batchOutcome$TIME <= 0))
        stop("Time cannot be non-positive",call.=FALSE)
    if (modelType == "lr" | modelType == "pr" | (modelType == "cox" & is.null(batchOutcome$STRATUM_ID)))
      batchOutcome$STRATUM_ID = batchOutcome$ROW_ID
    if (modelType == "lr" | modelType == "clr")
      batchOutcome$TIME = 0
    batchOutcome
  }
  
  getCovariateBatch <- function(resultSetCovariate, modelType){
    data <- get("data",envir=resultSetCovariate)
    chunks <- get("chunks",envir=resultSetCovariate)
    cursor <- get("cursor",envir=resultSetCovariate)
    batchCovariate <- data[chunks[[cursor]],]
    assign("cursor",cursor+1,envir=resultSetCovariate)
    batchCovariate
  }
  
  isDone <- function(resultSet){
    chunks <- get("chunks",envir=resultSet)
    cursor <- get("cursor",envir=resultSet)
    cursor > length(chunks)
  }
  
  constructCyclopsDataFromBatchableSources(resultSetOutcome,
                                           resultSetCovariate,
                                           getOutcomeBatch,
                                           getCovariateBatch,
                                           isDone,
                                           modelType, 
                                           addIntercept,
                                           offsetAlreadyOnLogScale,
                                           sortCovariates,
                                           makeCovariatesDense)
}

#' Convert data from data frames into a CyclopsData object
#'
#' @description
#' \code{createCyclopsData} loads data from two data frames, and inserts it into a Cyclops data object.
#' 
#' @param outcomes    A data frame containing the outcomes with predefined columns (see below).
#' @param covariateSql  A data frame containing the covariates with predefined columns (see below).
#' @param modelType      Cyclops model type. Current supported types are "ls", "pr", "lr", "clr", "sccs", or "cox"
#' @param addIntercept  Add an intercept to the model?
#' @param useOffsetCovariate  Use the time variable in the model as an offset?
#' @param offsetAlreadyOnLogScale Is the time variable already on a log scale?
#' @param sortCovariates Do the covariates still need to be sorted?
#'
#' @details
#' These columns are expected in the outcome object:
#' \tabular{lll}{  
#'   \verb{stratum_id}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{row_id}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{y}    \tab(real) \tab The outcome variable \cr
#'   \verb{time}    \tab(real) \tab For models that use time (e.g. Poisson or Cox regression) this contains time (e.g. number of days) \cr
#' }
#' The outcome table should be sorted by stratum_id and then row_id
#' 
#' These columns are expected in the covariates object:
#' \tabular{lll}{  
#'   \verb{stratum_id}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{row_id}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{covariate_id}    \tab(integer) \tab A numeric identifier of a covariate  \cr
#'   \verb{covariate_value}    \tab(real) \tab The value of the specified covariate \cr
#' }
#' The covariate table should be sorted by stratum_id, row_id and covariate_id
#' 
#' @return              
#' An object of type CyclopsModel
#' 
#' @examples \dontrun{
#'   connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com", schema="test")
#'   connection <- connect(connectionDetails)
#'   
#'   outcomes <- dbGetQuery(conn,"SELECT * FROM outcomes ORDER BY row_id")
#'   covariates <- dbGetQuery(conn,"SELECT * FROM covariates ORDER BY row_id, covariate_id")
#'   
#'   cyclopsData <- dbGetCyclopsInput(connection,outcomes,covariates,modelType = "lr")
#'   
#'   dbDisconnect(connection)
#'   
#'   cyclopsFit <- fitCyclopsModel(cyclopsData, prior = prior("normal",0.01))
#' }
#' @export
createCyclopsData <- function(outcomes, 
                              covariates,
                              modelType = "lr", 
                              addIntercept = TRUE,
                              offsetAlreadyOnLogScale = FALSE,
                              sortCovariates = TRUE,
                              makeCovariatesDense = NULL){
  colnames(outcomes) <- toupper(colnames(outcomes))
  colnames(covariates) <- toupper(colnames(covariates))
  
  if ((modelType == "clr" | modelType == "cpr") & addIntercept){
    warning("Intercepts are not allowed in conditional models, removing intercept",call.=FALSE)
    addIntercept = FALSE
  }
  if (modelType == "pr" | modelType == "cpr"| modelType == "cox") {
    if (any(batchOutcome$TIME <= 0))
      stop("Time cannot be non-positive",call.=FALSE)
    useOffsetCovariate = -1 
  } else {
    useOffsetCovariate = NULL  
  }
  if (modelType == "lr" | modelType == "pr")
    outcomes$STRATUM_ID = outcomes$ROW_ID
  if (modelType == "lr" | modelType == "clr")
    outcomes$TIME = 0
  
  dataPtr <- createSqlCyclopsData(modelType = modelType)
  
  appendSqlCyclopsData(dataPtr,
                       outcomes$STRATUM_ID,
                       outcomes$ROW_ID,
                       outcomes$Y,
                       outcomes$TIME,
                       covariates$ROW_ID,
                       covariates$COVARIATE_ID,
                       covariates$COVARIATE_VALUE
  )
  
  finalizeSqlCyclopsData(dataPtr,
                         addIntercept = addIntercept,
                         useOffsetCovariate = useOffsetCovariate,
                         offsetAlreadyOnLogScale = offsetAlreadyOnLogScale,
                         sortCovariates = sortCovariates,
                         makeCovariatesDense = makeCovariatesDense)
  
  dataPtr
}



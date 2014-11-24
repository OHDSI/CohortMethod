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

#' Check if data is sorted by one or more columns
#'
#' @description
#' \code{isSorted} checks wether data is sorted by one or more specified columns.
#' 
#' @param data            Either a data.frame of ffdf object.
#' @param columnNames     Vector of one or more column names.
#' @param ascending       Logical vector indicating the data should be sorted ascending or descending 
#' according the specified columns.
#'
#' @details
#' This function currently only supports checking for sorting on numeric values.
#' 
#' @return              
#' True or false
#' 
#' @examples 
#' x <- data.frame(a = runif(1000),b = runif(1000))
#' x <- round(x,digits=2)
#' isSorted(x,c("a","b"))
#'  
#' x <- x[order(x$a,x$b),]
#' isSorted(x,c("a","b"))
#'  
#' x <- x[order(x$a,-x$b),]
#' isSorted(x,c("a","b"),c(TRUE,FALSE))
#' @export
isSorted <- function(data,columnNames,ascending=rep(TRUE,length(columnNames))){
  UseMethod("isSorted") 
}

isSorted.data.frame <- function(data,columnNames,ascending=rep(TRUE,length(columnNames))){
  return(.isSorted(data,columnNames,ascending))
}

isSorted.ffdf <- function(data,columnNames,ascending=rep(TRUE,length(columnNames))){
  if (nrow(data)>100000){ #If data is big, first check on a small subset. If that aready fails, we're done
    if (!isSorted(data[ri(1,1000),,drop=FALSE],columnNames,ascending))
      return(FALSE)
  }
  chunks <- chunk(data)
  for (i in chunk(data))
    if (!isSorted(data[i,,drop=FALSE],columnNames,ascending))
      return(FALSE)
  return(TRUE)
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
                           makeCovariatesDense = makeCovariatesDense)
  }
  return(dataPtr)
}

#' Convert data from two ffdf objects into a CyclopsData object
#'
#' @description
#' \code{createCyclopsData.ffdf} loads data from two ffdf objects, and inserts it into a Cyclops data object.
#' 
#' @param outcomes    A ffdf object containing the outcomes with predefined columns (see below).
#' @param covariates  A ffdf object containing the covariates with predefined columns (see below).
#' @param modelType  	  Cyclops model type. Current supported types are "pr", "cpr", lr", "clr", or "cox"
#' @param addIntercept  Add an intercept to the model?
#' @param useOffsetCovariate  Use the time variable in the model as an offset?
#' @param offsetAlreadyOnLogScale Is the time variable already on a log scale?
#' @param checkSorting  Check if the data is sorted appropriately, and if not, sort. 
#'
#' @details
#' These columns are expected in the outcome object:
#' \tabular{lll}{  
#'   \verb{stratum_id}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{row_id}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{y}    \tab(real) \tab The outcome variable \cr
#'   \verb{time}    \tab(real) \tab For models that use time (e.g. Poisson or Cox regression) this contains time (e.g. number of days) \cr
#' }
#' The outcome table should be sorted by stratum_id (if present) and then row_id except for Cox regression when
#' the table should be sorted by stratum_id (if present), -time, y, and row_id.
#' 
#' These columns are expected in the covariates object:
#' \tabular{lll}{  
#'   \verb{stratum_id}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{row_id}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{covariate_id}    \tab(integer) \tab A numeric identifier of a covariate  \cr
#'   \verb{covariate_value}    \tab(real) \tab The value of the specified covariate \cr
#' }
#' The covariate table should be sorted by stratum_id (if present), row_id and covariate_id except for Cox regression when
#' the table should be sorted by stratum_id (if present), -time, y, and row_id.
#'  
#' @return              
#' An object of type CyclopsModel
#' 
#' @examples 
#' #Convert infert dataset to Cyclops format:
#' covariates <- data.frame(stratum_id = rep(infert$stratum,2),
#'                          row_id = rep(1:nrow(infert),2),
#'                          covariate_id = rep(1:2,each=nrow(infert)),
#'                          covariate_value = c(infert$spontaneous,infert$induced))
#' outcomes <- data.frame(stratum_id = infert$stratum,
#'                        row_id = 1:nrow(infert),
#'                        y = infert$case)
#' #Make sparse:
#' covariates <- covariates[covariates$covariate_value != 0,]
#' 
#' #Sort:
#' covariates <- covariates[order(covariates$stratum_id,covariates$row_id,covariates$covariate_id),]
#' outcomes <- outcomes[order(outcomes$stratum_id,outcomes$row_id),]
#' 
#' #Convert to ffdf:
#' covariates <- as.ffdf(covariates)
#' outcomes <- as.ffdf(outcomes)
#'
#' #Create Cyclops data object:
#' cyclopsData <- createCyclopsData.ffdf(outcomes,covariates,modelType = "clr",addIntercept = FALSE)
#' 
#' #Fit model:
#' fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
#' 
#' 
#' @export
createCyclopsData.ffdf <- function(outcomes, 
                                   covariates,
                                   modelType = "lr", 
                                   addIntercept = TRUE,
                                   offsetAlreadyOnLogScale = FALSE,
                                   makeCovariatesDense = NULL,
                                   checkSorting = TRUE){
  colnames(outcomes) <- toupper(colnames(outcomes))  
  colnames(covariates) <- toupper(colnames(covariates))    
  
  if (checkSorting){
    if (modelType == "lr" | modelType == "pr"){
      if (!isSorted(outcomes,c("ROW_ID"))){
        writeLines("Sorting outcomes by row_id")
        rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
        outcomes <- outcomes[ffdforder(outcomes[c("ROW_ID")]),]
      }
      if (!isSorted(covariates,c("ROW_ID"))){
        writeLines("Sorting covariates by row_id")
        rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
        covariates <- covariates[ffdforder(covariates[c("ROW_ID")]),]
      }  
    }
    if (modelType == "clr" | modelType == "cpr"){
      if (!isSorted(outcomes,c("STRATUM_ID","ROW_ID"))){
        writeLines("Sorting outcomes by stratum_id and row_id")
        rownames(outcomes) <- NULL #Needs to be null or the ordering of ffdf will fail
        outcomes <- outcomes[ffdforder(outcomes[c("STRATUM_ID","ROW_ID")]),]
      }
      if (!isSorted(covariates,c("STRATUM_ID","ROW_ID"))){
        writeLines("Sorting covariates by stratum_id and row_id")
        rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
        covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","ROW_ID")]),]
      }      
    }
    if (modelType == "cox"){
      if (!isSorted(outcomes,c("STRATUM_ID","TIME","Y","ROW_ID"),c(TRUE,FALSE,TRUE,TRUE))){
        writeLines("Sorting outcomes by stratum_id, time (descending), y, and row_id")
        rownames(outcomes) <- NULL #Needs to be null or the ordering of ffdf will fail
        outcomes$MINTIME = 0-outcomes$TIME
        outcomes <- outcomes[ffdforder(outcomes[c("STRATUM_ID","MINTIME","Y","ROW_ID")]),]
      }
      if (is.null(covariates$TIME) | is.null(covariates$Y)){ # If time or y not present, add to check if sorted
        covariates$TIME = NULL
        covariates$Y = NULL
        covariates <- merge(covariates,outcomes,by=c("STRATUM_ID","ROW_ID"))
      }
      if (!isSorted(covariates,c("STRATUM_ID","TIME","Y","ROW_ID"),c(TRUE,FALSE,TRUE,TRUE))){
        writeLines("Sorting covariates by stratum_id, time (descending), y, and row_id")
        rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
        covariates$MINTIME = 0-covariates$TIME
        covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","MINTIME","Y","ROW_ID")]),]
      }      
    }
  }
  
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
    if (modelType == "lr" | modelType == "pr")
      batchOutcome$STRATUM_ID = batchOutcome$ROW_ID
    if (modelType == "cox" & is.null(batchOutcome$STRATUM_ID))
      batchOutcome$STRATUM_ID = 0
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
  
  result <- constructCyclopsDataFromBatchableSources(resultSetOutcome,
                                                     resultSetCovariate,
                                                     getOutcomeBatch,
                                                     getCovariateBatch,
                                                     isDone,
                                                     modelType, 
                                                     addIntercept,
                                                     offsetAlreadyOnLogScale,
                                                     makeCovariatesDense)
  return(result)
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
#' @param checkSorting  Check if the data is sorted appropriately, and if not, sort.
#'
#' @details
#' These columns are expected in the outcome object:
#' \tabular{lll}{  
#'   \verb{stratum_id}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{row_id}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{y}    \tab(real) \tab The outcome variable \cr
#'   \verb{time}    \tab(real) \tab For models that use time (e.g. Poisson or Cox regression) this contains time (e.g. number of days) \cr
#' }
#' The outcome table should be sorted by stratum_id (if present) and then row_id except for Cox regression when
#' the table should be sorted by stratum_id (if present), -time, y, and row_id.
#' 
#' These columns are expected in the covariates object:
#' \tabular{lll}{  
#'   \verb{stratum_id}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{row_id}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{covariate_id}    \tab(integer) \tab A numeric identifier of a covariate  \cr
#'   \verb{covariate_value}    \tab(real) \tab The value of the specified covariate \cr
#' }
#' The covariate table should be sorted by stratum_id (if present), row_id and covariate_id except for Cox regression when
#' the table should be sorted by stratum_id (if present), -time, y, and row_id.
#' 
#' @return              
#' An object of type CyclopsModel
#' 
#' @examples
#' #Convert infert dataset to Cyclops format:
#' covariates <- data.frame(stratum_id = rep(infert$stratum,2),
#'                          row_id = rep(1:nrow(infert),2),
#'                          covariate_id = rep(1:2,each=nrow(infert)),
#'                          covariate_value = c(infert$spontaneous,infert$induced))
#' outcomes <- data.frame(stratum_id = infert$stratum,
#'                        row_id = 1:nrow(infert),
#'                        y = infert$case)
#' #Make sparse:
#' covariates <- covariates[covariates$covariate_value != 0,]
#' 
#' #Sort:
#' covariates <- covariates[order(covariates$stratum_id,covariates$row_id,covariates$covariate_id),]
#' outcomes <- outcomes[order(outcomes$stratum_id,outcomes$row_id),]
#'
#' #Create Cyclops data object:
#' cyclopsData <- createCyclopsData(outcomes,covariates,modelType = "clr",addIntercept = FALSE)
#' 
#' #Fit model:
#' fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
#' 
#' @export
createCyclopsData <- function(outcomes, 
                              covariates,
                              modelType = "lr", 
                              addIntercept = TRUE,
                              offsetAlreadyOnLogScale = FALSE,
                              makeCovariatesDense = NULL,
                              checkSorting = TRUE){
  colnames(outcomes) <- toupper(colnames(outcomes))
  colnames(covariates) <- toupper(colnames(covariates))
  
  if ((modelType == "clr" | modelType == "cpr") & addIntercept){
    warning("Intercepts are not allowed in conditional models, removing intercept",call.=FALSE)
    addIntercept = FALSE
  }
  if (modelType == "pr" | modelType == "cpr") 
    if (any(outcomes$TIME <= 0))
      stop("Time cannot be non-positive",call.=FALSE)
  if (modelType == "cox" & is.null(outcomes$STRATUM_ID))
    outcomes$STRATUM_ID = 0	
  if (modelType == "lr" | modelType == "clr")
    outcomes$TIME = 0
  
  if (checkSorting){
    if (modelType == "lr" | modelType == "pr"){
      if (!isSorted(outcomes,c("ROW_ID"))){
        writeLines("Sorting outcomes by row_id")
        outcomes <- outcomes[order(outcomes$ROW_ID),]
      }
      if (!isSorted(covariates,c("ROW_ID"))){
        writeLines("Sorting covariates by row_id")
        covariates <- covariates[order(covariates$ROW_ID),]
      }   
    }
    
    if (modelType == "clr" | modelType == "cpr"){
      if (!isSorted(outcomes,c("STRATUM_ID","ROW_ID"))){
        writeLines("Sorting outcomes by stratum_id and row_id")
        outcomes <- outcomes[order(outcomes$STRATUM_ID,outcomes$ROW_ID),]
      }
      if (!isSorted(covariates,c("STRATUM_ID","ROW_ID"))){
        writeLines("Sorting covariates by stratum_id and row_id")
        covariates <- covariates[order(covariates$STRATUM_ID,covariates$ROW_ID),]
      }      
    }
    if (modelType == "cox"){
      if (!isSorted(outcomes,c("STRATUM_ID","TIME","Y","ROW_ID"),c(TRUE,FALSE,TRUE,TRUE))){
        writeLines("Sorting outcomes by stratum_id, time (descending), y, and row_id")
        outcomes <- outcomes[order(outcomes$STRATUM_ID,-outcomes$TIME,outcomes$Y,outcomes$ROW_ID),]
      }
      if (is.null(covariates$TIME) | is.null(covariates$Y)){ # If time or y not present, add to check if sorted
        covariates$TIME = NULL
        covariates$Y = NULL
        covariates <- merge(covariates,outcomes,by=c("STRATUM_ID","ROW_ID"))
      }
      if (!isSorted(covariates,c("STRATUM_ID","TIME","Y","ROW_ID"),c(TRUE,FALSE,TRUE,TRUE))){
        writeLines("Sorting covariates by stratum_id, time (descending), y, and row_id")
        covariates <- covariates[order(covariates$STRATUM_ID,-covariates$TIME,covariates$Y,covariates$ROW_ID),]
      }      
    }
  }
  
  dataPtr <- createSqlCyclopsData(modelType = modelType)
  
  if (modelType == "lr" | modelType == "pr"){
    appendSqlCyclopsData(dataPtr,
                         outcomes$ROW_ID,
                         outcomes$ROW_ID,
                         outcomes$Y,
                         outcomes$TIME,
                         covariates$ROW_ID,
                         covariates$COVARIATE_ID,
                         covariates$COVARIATE_VALUE
    )
  } else {
    appendSqlCyclopsData(dataPtr,
                         outcomes$STRATUM_ID,
                         outcomes$ROW_ID,
                         outcomes$Y,
                         outcomes$TIME,
                         covariates$ROW_ID,
                         covariates$COVARIATE_ID,
                         covariates$COVARIATE_VALUE
    )
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
                           makeCovariatesDense = makeCovariatesDense)
  }
  return(dataPtr)
}

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
    lastRowId <- batchCovars$rowId[nrow(batchCovars)]
    endCompleteRow <- lastRowNotHavingThisValue(batchCovars$rowId,lastRowId)
    
    if (endCompleteRow == 0){ #Entire batch is about 1 row
      if (!is.null(spillOverCovars)){
        if (spillOverCovars$rowId[1] == batchCovars$rowId[1]){ #SpilloverCovars contains info on same row
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
      completeRowId = covarsToCyclops$rowId[nrow(covarsToCyclops)]
      endCompleteRowInOutcome <- which(batchOutcome$rowId == completeRowId)
      while (length(endCompleteRowInOutcome) == 0 & !isDone(resultSetOutcome)){
        if (lastUsedOutcome == nrow(batchOutcome)){
          batchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
        } else {      
          newBatchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
          batchOutcome <- rbind(batchOutcome[(lastUsedOutcome+1):nrow(batchOutcome),],newBatchOutcome)          
        }
        lastUsedOutcome = 0
        endCompleteRowInOutcome <- which(batchOutcome$rowId == completeRowId)
      }
      #Append to Cyclops:
      appendSqlCyclopsData(dataPtr,
                           batchOutcome$stratumId[(lastUsedOutcome+1):endCompleteRowInOutcome],
                           batchOutcome$rowId[(lastUsedOutcome+1):endCompleteRowInOutcome],
                           batchOutcome$y[(lastUsedOutcome+1):endCompleteRowInOutcome],
                           batchOutcome$time[(lastUsedOutcome+1):endCompleteRowInOutcome],
                           covarsToCyclops$rowId,
                           covarsToCyclops$covariateId,
                           covarsToCyclops$covariateValue
      )
      
      lastUsedOutcome = endCompleteRowInOutcome
    }
  }
  #End of covar batches, add spillover to Cyclops:
  covarsToCyclops <- spillOverCovars
  
  completeRowId = covarsToCyclops$rowId[nrow(covarsToCyclops)]
  endCompleteRowInOutcome <- which(batchOutcome$rowId == completeRowId)
  while (length(endCompleteRowInOutcome) == 0 & !isDone(resultSetOutcome)){
    if (lastUsedOutcome == nrow(batchOutcome)){
      batchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
    } else {      
      batchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
      batchOutcome <- rbind(batchOutcome[(lastUsedOutcome+1):nrow(batchOutcome),],newBatchOutcome)          
    }
    lastUsedOutcome = 0
    endCompleteRowInOutcome <- which(batchOutcome$rowId == completeRowId)
  }
  
  #Append to Cyclops:
  appendSqlCyclopsData(dataPtr,
                       batchOutcome$stratumId[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       batchOutcome$rowId[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       batchOutcome$y[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       batchOutcome$time[(lastUsedOutcome+1):endCompleteRowInOutcome],
                       covarsToCyclops$rowId,
                       covarsToCyclops$covariateId,
                       covarsToCyclops$covariateValue
  )
  
  lastUsedOutcome = endCompleteRowInOutcome
  
  #Add any outcomes that are left (without matching covar information):
  if (lastUsedOutcome < nrow(batchOutcome)){
    appendSqlCyclopsData(dataPtr,
                         batchOutcome$stratumId[(lastUsedOutcome+1):nrow(batchOutcome)],
                         batchOutcome$rowId[(lastUsedOutcome+1):nrow(batchOutcome)],
                         batchOutcome$y[(lastUsedOutcome+1):nrow(batchOutcome)],
                         batchOutcome$time[(lastUsedOutcome+1):nrow(batchOutcome)],
                         as.numeric(c()),
                         as.numeric(c()),
                         as.numeric(c()))
  }
  while (!isDone(resultSetOutcome)){
    batchOutcome <- getOutcomeBatch(resultSetOutcome,modelType)
    
    appendSqlCyclopsData(dataPtr,
                         batchOutcome$stratumId,
                         batchOutcome$rowId,
                         batchOutcome$y,
                         batchOutcome$time,
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

#' Convert data from two data frames or ffdf objects into a CyclopsData object
#'
#' @description
#' \code{convertToCyclopsDataObject} loads data from two data frames or ffdf objects, and inserts it into a Cyclops data object.
#' 
#' @param outcomes    A data frame or ffdf object containing the outcomes with predefined columns (see below).
#' @param covariates  A data frame or ffdf object containing the covariates with predefined columns (see below).
#' @param modelType  	  Cyclops model type. Current supported types are "pr", "cpr", lr", "clr", or "cox"
#' @param addIntercept  Add an intercept to the model?
#' @param useOffsetCovariate  Use the time variable in the model as an offset?
#' @param offsetAlreadyOnLogScale Is the time variable already on a log scale?
#' @param checkSorting  Check if the data is sorted appropriately, and if not, sort. 
#' @param quiet         If true, (warning) messages are surpressed.
#'
#' @details
#' These columns are expected in the outcome object:
#' \tabular{lll}{  
#'   \verb{stratumId}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{rowId}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{y}    \tab(real) \tab The outcome variable \cr
#'   \verb{time}    \tab(real) \tab For models that use time (e.g. Poisson or Cox regression) this contains time (e.g. number of days) \cr
#' }
#' 
#' These columns are expected in the covariates object:
#' \tabular{lll}{  
#'   \verb{stratumId}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{rowId}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{covariateId}    \tab(integer) \tab A numeric identifier of a covariate  \cr
#'   \verb{covariateValue}    \tab(real) \tab The value of the specified covariate \cr
#' }
#' 
#' Note: If checkSorting is turned off, the outcome table should be sorted by stratumId (if present) 
#' and then rowId except for Cox regression when the table should be sorted by 
#' stratumId (if present), -time, y, and rowId. The covariate table should be sorted by stratumId 
#' (if present), rowId and covariateId except for Cox regression when the table should be sorted by 
#' stratumId (if present), -time, y, and rowId.
#'  
#' @return              
#' An object of type cyclopsData
#' 
#' @examples 
#' #Convert infert dataset to Cyclops format:
#' covariates <- data.frame(stratumId = rep(infert$stratum,2),
#'                          rowId = rep(1:nrow(infert),2),
#'                          covariateId = rep(1:2,each=nrow(infert)),
#'                          covariateValue = c(infert$spontaneous,infert$induced))
#' outcomes <- data.frame(stratumId = infert$stratum,
#'                        rowId = 1:nrow(infert),
#'                        y = infert$case)
#' #Make sparse:
#' covariates <- covariates[covariates$covariateValue != 0,]
#' 
#' #Create Cyclops data object:
#' cyclopsData <- convertToCyclopsDataObject(outcomes,covariates,modelType = "clr",addIntercept = FALSE)
#' 
#' #Fit model:
#' fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
#' 
#' @export
convertToCyclopsDataObject <- function(outcomes, 
                                       covariates,
                                       modelType = "lr", 
                                       addIntercept = TRUE,
                                       offsetAlreadyOnLogScale = FALSE,
                                       makeCovariatesDense = NULL,
                                       checkSorting = TRUE,
                                       quiet = FALSE){
  UseMethod("convertToCyclopsDataObject") 
}

convertToCyclopsDataObject.ffdf <- function(outcomes, 
                                            covariates,
                                            modelType = "lr", 
                                            addIntercept = TRUE,
                                            offsetAlreadyOnLogScale = FALSE,
                                            makeCovariatesDense = NULL,
                                            checkSorting = TRUE,
                                            quiet = FALSE){
  if (checkSorting){
    if (modelType == "lr" | modelType == "pr"){
      if (!isSorted(outcomes,c("rowId"))){
        if(!quiet)
          writeLines("Sorting outcomes by rowId")
        rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
        outcomes <- outcomes[ffdforder(outcomes[c("rowId")]),]
      }
      if (!isSorted(covariates,c("rowId"))){
        if(!quiet)
          writeLines("Sorting covariates by rowId")
        rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
        covariates <- covariates[ffdforder(covariates[c("rowId")]),]
      }  
    }
    if (modelType == "clr" | modelType == "cpr"){
      if (!isSorted(outcomes,c("stratumId","rowId"))){
        if(!quiet)
          writeLines("Sorting outcomes by stratumId and rowId")
        rownames(outcomes) <- NULL #Needs to be null or the ordering of ffdf will fail
        outcomes <- outcomes[ffdforder(outcomes[c("stratumId","rowId")]),]
      }
      if (!isSorted(covariates,c("stratumId","rowId"))){
        if(!quiet)
          writeLines("Sorting covariates by stratumId and rowId")
        rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
        covariates <- covariates[ffdforder(covariates[c("stratumId","rowId")]),]
      }      
    }
    if (modelType == "cox"){
      if (is.null(outcomes$stratumId)){
        outcomes$stratumId = 0  
        covariates$stratumId = 0
      }
      if (!isSorted(outcomes,c("stratumId","time","y","rowId"),c(TRUE,FALSE,TRUE,TRUE))){
        if(!quiet)
          writeLines("Sorting outcomes by stratumId, time (descending), y, and rowId")
        rownames(outcomes) <- NULL #Needs to be null or the ordering of ffdf will fail
        outcomes$minTime = 0-outcomes$time
        outcomes <- outcomes[ffdforder(outcomes[c("stratumId","minTime","y","rowId")]),]
      }
      if (is.null(covariates$time) | is.null(covariates$y)){ # If time or y not present, add to check if sorted
        covariates$time = NULL
        covariates$y = NULL
        covariates <- merge(covariates,outcomes,by=c("stratumId","rowId"))
      }
      if (!isSorted(covariates,c("stratumId","time","y","rowId"),c(TRUE,FALSE,TRUE,TRUE))){
        if(!quiet)
          writeLines("Sorting covariates by stratumId, time (descending), y, and rowId")
        rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
        covariates$minTime = 0-covariates$time
        covariates <- covariates[ffdforder(covariates[c("stratumId","minTime","y","rowId")]),]
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
      if (any(batchOutcome$time <= 0))
        stop("time cannot be non-positive",call.=FALSE)
    if (modelType == "lr" | modelType == "pr")
      batchOutcome$stratumId = batchOutcome$rowId
    if (modelType == "cox" & is.null(batchOutcome$stratumId))
      batchOutcome$stratumId = 0
    if (modelType == "lr" | modelType == "clr")
      batchOutcome$time = 0
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

convertToCyclopsDataObject.data.frame <- function(outcomes, 
                                                  covariates,
                                                  modelType = "lr", 
                                                  addIntercept = TRUE,
                                                  offsetAlreadyOnLogScale = FALSE,
                                                  makeCovariatesDense = NULL,
                                                  checkSorting = TRUE,
                                                  quiet = FALSE){
  if ((modelType == "clr" | modelType == "cpr") & addIntercept){
    if(!quiet)
      warning("Intercepts are not allowed in conditional models, removing intercept",call.=FALSE)
    addIntercept = FALSE
  }
  if (modelType == "pr" | modelType == "cpr") 
    if (any(outcomes$time <= 0))
      stop("time cannot be non-positive",call.=FALSE)
  if (modelType == "cox" & is.null(outcomes$stratumId))
    outcomes$stratumId = 0	
  if (modelType == "lr" | modelType == "clr")
    outcomes$time = 0
  
  if (checkSorting){
    if (modelType == "lr" | modelType == "pr"){
      if (!isSorted(outcomes,c("rowId"))){
        if(!quiet)
          writeLines("Sorting outcomes by rowId")
        outcomes <- outcomes[order(outcomes$rowId),]
      }
      if (!isSorted(covariates,c("rowId"))){
        if(!quiet)
          writeLines("Sorting covariates by rowId")
        covariates <- covariates[order(covariates$rowId),]
      }   
    }
    
    if (modelType == "clr" | modelType == "cpr"){
      if (!isSorted(outcomes,c("stratumId","rowId"))){
        if(!quiet)
          writeLines("Sorting outcomes by stratumId and rowId")
        outcomes <- outcomes[order(outcomes$stratumId,outcomes$rowId),]
      }
      if (!isSorted(covariates,c("stratumId","rowId"))){
        if(!quiet)
          writeLines("Sorting covariates by stratumId and rowId")
        covariates <- covariates[order(covariates$stratumId,covariates$rowId),]
      }      
    }
    if (modelType == "cox"){
      if (!isSorted(outcomes,c("stratumId","time","y","rowId"),c(TRUE,FALSE,TRUE,TRUE))){
        if(!quiet)
          writeLines("Sorting outcomes by stratumId, time (descending), y, and rowId")
        outcomes <- outcomes[order(outcomes$stratumId,-outcomes$time,outcomes$y,outcomes$rowId),]
      }
      if (is.null(covariates$time) | is.null(covariates$y)){ # If time or y not present, add to check if sorted
        covariates$time = NULL
        covariates$y = NULL
        covariates <- merge(covariates,outcomes,by=c("stratumId","rowId"))
      }
      if (!isSorted(covariates,c("stratumId","time","y","rowId"),c(TRUE,FALSE,TRUE,TRUE))){
        if(!quiet)
          writeLines("Sorting covariates by stratumId, time (descending), y, and rowId")
        covariates <- covariates[order(covariates$stratumId,-covariates$time,covariates$y,covariates$rowId),]
      }      
    }
  }
  
  dataPtr <- createSqlCyclopsData(modelType = modelType)
  
  if (modelType == "lr" | modelType == "pr"){
    appendSqlCyclopsData(dataPtr,
                         outcomes$rowId,
                         outcomes$rowId,
                         outcomes$y,
                         outcomes$time,
                         covariates$rowId,
                         covariates$covariateId,
                         covariates$covariateValue
    )
  } else {
    appendSqlCyclopsData(dataPtr,
                         outcomes$stratumId,
                         outcomes$rowId,
                         outcomes$y,
                         outcomes$time,
                         covariates$rowId,
                         covariates$covariateId,
                         covariates$covariateValue
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

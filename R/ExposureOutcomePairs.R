# @file ExposureOutcomePairs.R
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


createExposureOutcomePair <- function (targetDrugConceptId,
                                       comparatorDrugConceptId,
                                       outcomeConceptId, 
                                       indicationConceptIds,
                                       exclusionConceptIds,   
                                       excludedCovariateConceptIds) {
  
  #First: get the default values:
  exposureOutcomePair <- list()
  for (name in names(formals(createExposureOutcomePair))){
    exposureOutcomePair[[name]] = get(name)
  }
  
  #Next: overwrite defaults with actual values if specified:
  #values <- as.list(match.call())
  #Note: need this funky code to make sure parameters are stored as values, not symbols:
  values <- c(list(as.character(match.call()[[1]])),lapply(as.list(match.call())[-1],function(x) eval(x,envir=sys.frame(-3))))
  for (name in names(values)){
    if (name %in% names(exposureOutcomePair))
      exposureOutcomePair[[name]] = values[[name]]
  }
  
  class(exposureOutcomePair) <- "exposureOutcomePair"
  return(exposureOutcomePair)
}


appendToExposureOutcomePairs <- function(exposureOutcomePair,exposureOutcomePairs = NULL){
  stopifnot(class(exposureOutcomePair) == "exposureOutcomePair")
  if (is.null(exposureOutcomePairs)){
    exposureOutcomePairs = list()
    class(exposureOutcomePairs) <- "exposureOutcomePairs"
  }
  exposureOutcomePairs[[length(exposureOutcomePairs)+1]] <- exposureOutcomePair
  return(exposureOutcomePairs)
}

#' @export
writeExposureOutcomePairsToFile <- function(exposureOutcomePairs, file){
  stopifnot(class(exposureOutcomePairs) == "exposureOutcomePairs")
  
  #Convert exposureOutcomePairs to a data.frame, converting any nested vectors into semicolon-delimited strings:
  f <- exposureOutcomePairs
  d <- data.frame()
  for (row in 1:length(f)){
    class(f[[row]]) <- "list"
    for (column in 1:length(f[[row]])){
      if ((class(f[[row]][[column]]) == "numeric") && (length(f[[row]][[column]]) > 1))
        f[[row]][[column]] = paste(f[[row]][[column]],collapse=";")
    }
    d <- rbind(d,as.data.frame(f[[row]]))
  }
  
  write.csv(d,file=file, row.names=FALSE)
}

readExposureOutcomePairsFromFile <- function(file){
  d <- read.csv(file)
  d[is.na(d)] <- ""
  exposureOutcomePairs <- list()
  for (row in 1:nrow(d)){
    exposureOutcomePair <- as.list(d[row,])
    for (column in c("indicationConceptIds","exclusionConceptIds","excludedCovariateConceptId")){
      exposureOutcomePair[[column]] <- as.numeric(unlist(strsplit(as.character(d[row,column]),";")))
    }
    class(exposureOutcomePairs) = "exposureOutcomePairs"
    exposureOutcomePairs[[length(exposureOutcomePairs)+1]] <- exposureOutcomePair
  }
  class(exposureOutcomePairs) <- "exposureOutcomePairs"
  return(exposureOutcomePairs)
}

# @file HelperFunctions.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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

#' Check is CohortMethod and its dependencies are correctly installed
#'
#' @details
#' This function checks whether CohortMethod and its dependencies are correctly installed. This will
#' check the database connectivity, large scale regression engine (Cyclops), and large data object
#' handling (ff).
#'
#' @param connectionDetails   An R object of type\cr\code{connectionDetails} created using the function
#'                            \code{createConnectionDetails} in the \code{DatabaseConnector} package.
#'
#' @export
checkCmInstallation <- function(connectionDetails) {
  ParallelLogger::logInfo("Checking database connectivity")
  conn <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::disconnect(conn)
  ParallelLogger::logInfo("- Ok")

  ParallelLogger::logInfo("\nChecking large scale regression engine")
  counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
  outcome <- gl(3, 1, 9)
  treatment <- gl(3, 3)
  cyclopsData <- Cyclops::createCyclopsData(counts ~ outcome + treatment, modelType = "pr")
  cyclopsFit <- fitCyclopsModel(cyclopsData)
  if (length(coef(cyclopsFit)) != 5)
    stop("Error fitting regression model")
  ParallelLogger::logInfo("- Ok")

  ParallelLogger::logInfo("\nChecking support for large data objects")
  x <- Andromeda::andromeda(test = data.frame(a = 1:100, b = "test"))
  if (x$test %>% count() %>% pull() != 100)
    stop("Error creating large data object")
  ParallelLogger::logInfo("- Ok")

  ParallelLogger::logInfo("\nCohortMethod is correctly installed")
  ParallelLogger::logInfo(paste0("\nResponse code: ", round(pi * 123456)))
}

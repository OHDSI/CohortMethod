# Copyright 2025 Observational Health Data Sciences and Informatics
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

#' Cohort Method Data
#'
#' @description
#' `CohortMethodData` is an S4 class that inherits from [CoviarateData][FeatureExtraction::CovariateData], which in turn inherits from [Andromeda][Andromeda::Andromeda]. It contains information on the cohorts, their
#' outcomes, and baseline covariates. Information about multiple outcomes can be captured at once for
#' efficiency reasons.
#'
#' A `CohortMethodData` is typically created using [getDbCohortMethodData()], can only be saved using
#' [saveCohortMethodData()], and loaded using [loadCohortMethodData()].
#'
#' @name CohortMethodData-class
#' @aliases CohortMethodData
NULL

#' CohortMethodData class.
#'
#' @export
#' @import FeatureExtraction
setClass("CohortMethodData", contains = "CovariateData")

#' Save the cohort method data to file
#'
#' @description
#' Saves an object of type [CohortMethodData] to a file.
#'
#' @param cohortMethodData   An object of type [CohortMethodData] as generated using
#'                           [getDbCohortMethodData()].
#' @param file               The name of the file where the data will be written. If the file already
#'                           exists it will be overwritten.
#'
#' @return
#' Returns no output.
#'
#' @export
saveCohortMethodData <- function(cohortMethodData, file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(cohortMethodData, "CohortMethodData", add = errorMessages)
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  Andromeda::saveAndromeda(cohortMethodData, file)
  writeLines("To use this CohortMethodData object, you will have to load it from file (using loadCohortMethodData).")
}

#' Load the cohort method data from a file
#'
#' @description
#' Loads an object of type [CohortMethodData] from a file in the file system.
#'
#' @param file       The name of the file containing the data.
#'
#' @return
#' An object of class [CohortMethodData].
#'
#' @export
loadCohortMethodData <- function(file) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(file, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!file.exists(file)) {
    stop(sprintf("Cannot find file '%s'", file))
  }
  if (file.info(file)$isdir) {
    stop(sprintf("'%s' is a folder, but should be a file", file))
  }
  cohortMethodData <- Andromeda::loadAndromeda(file)
  class(cohortMethodData) <- "CohortMethodData"
  attr(class(cohortMethodData), "package") <- "CohortMethod"
  return(cohortMethodData)
}

# show()
#' @param object  An object of type `CohortMethodData`.
#'
#' @export
#' @rdname CohortMethodData-class
setMethod("show", "CohortMethodData", function(object) {
  metaData <- attr(object, "metaData")
  cli::cat_line(pillar::style_subtle("# CohortMethodData object"))
  cli::cat_line("")
  cli::cat_line(paste("Target cohort ID:", metaData$targetId))
  cli::cat_line(paste("Comparator cohort ID:", metaData$comparatorId))
  cli::cat_line(paste(
    "Outcome cohort ID(s):",
    paste(metaData$outcomeIds, collapse = ",")
  ))
  cli::cat_line("")
  cli::cat_line(pillar::style_subtle("Inherits from CovariateData:"))
  class(object) <- "CovariateData"
  attr(class(object), "package") <- "FeatureExtraction"
  show(object)
})


# summary()
#' @param object  An object of type `CohortMethodData`.
#'
#' @export
#' @rdname CohortMethodData-class
setMethod("summary", "CohortMethodData", function(object) {
  if (!Andromeda::isValidAndromeda(object)) {
    stop("Object is not valid. Probably the Andromeda object was closed.")
  }
  cohorts <- object$cohorts |>
    collect()
  metaData <- attr(object, "metaData")
  targetPersons <- length(unique(cohorts$personSeqId[cohorts$treatment == 1]))
  comparatorPersons <- length(unique(cohorts$personSeqId[cohorts$treatment == 0]))
  outcomeCounts <- data.frame(
    outcomeId = metaData$outcomeIds,
    eventCount = 0,
    personCount = 0
  )
  outcomes <- object$outcomes |>
    collect()
  for (i in 1:nrow(outcomeCounts)) {
    outcomeCounts$eventCount[i] <- sum(outcomes$outcomeId == metaData$outcomeIds[i])
    outcomeCounts$personCount[i] <- length(unique(outcomes$rowId[outcomes$outcomeId == metaData$outcomeIds[i]]))
  }
  result <- list(
    metaData = metaData,
    targetPersons = targetPersons,
    comparatorPersons = comparatorPersons,
    outcomeCounts = outcomeCounts,
    covariateCount = nrow_temp(object$covariateRef),
    covariateValueCount = nrow_temp(object$covariates)
  )
  class(result) <- "summary.CohortMethodData"
  return(result)
})

#' @export
print.summary.CohortMethodData <- function(x, ...) {
  writeLines("CohortMethodData object summary")
  writeLines("")
  writeLines(paste("Target cohort ID:", x$metaData$targetId))
  writeLines(paste("Comparator cohort ID:", x$metaData$comparatorId))
  writeLines(paste("Outcome cohort ID(s):", x$metaData$outcomeIds, collapse = ","))
  writeLines("")
  writeLines(paste("Target persons:", paste(x$targetPersons)))
  writeLines(paste("Comparator persons:", paste(x$comparatorPersons)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- x$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeId
  outcomeCounts$outcomeId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Person count")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Covariates:")
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:", x$covariateValueCount))
}

#' Check whether an object is a CohortMethodData object
#'
#' @param x  The object to check.
#'
#' @return
#' A logical value.
#'
#' @export
isCohortMethodData <- function(x) {
  return(inherits(x, "CohortMethodData"))
}

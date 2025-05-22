# @file HelperFunctions.R
#
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
  errorMessages <- checkmate::makeAssertCollection()
  if (is(connectionDetails, "connectionDetails")) {
    checkmate::assertClass(connectionDetails, "connectionDetails", add = errorMessages)
  } else {
    checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  }
  checkmate::reportAssertions(collection = errorMessages)

  message("Checking database connectivity")
  conn <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::disconnect(conn)
  message("- Ok")

  message("\nChecking large scale regression engine")
  counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
  outcome <- gl(3, 1, 9)
  treatment <- gl(3, 3)
  cyclopsData <- Cyclops::createCyclopsData(counts ~ outcome + treatment, modelType = "pr")
  cyclopsFit <- fitCyclopsModel(cyclopsData)
  if (length(coef(cyclopsFit)) != 5) {
    stop("Error fitting regression model")
  }
  message("- Ok")

  message("\nChecking support for large data objects")
  x <- Andromeda::andromeda(test = data.frame(a = 1:100, b = "test"))
  if (nrow_temp(x$test) != 100) {
    stop("Error creating large data object")
  }
  message("- Ok")

  message("\nCohortMethod is correctly installed")
  message(paste0("\nResponse code: ", round(pi * 123456)))
}

.assertCovariateId <- function(covariateId, len = NULL, min.len = NULL, null.ok = FALSE, add = NULL) {
  checkmate::assertNumeric(covariateId, null.ok = null.ok, len = len, min.len = 1, add = add)
  if (!is.null(covariateId)) {
    message <- sprintf(
      "Variable '%s' is a (64-bit) integer",
      paste0(deparse(eval.parent(substitute(substitute(covariateId))), width.cutoff = 500L), collapse = "\n")
    )
    checkmate::assertTRUE(all(covariateId == round(covariateId)), .var.name = message, add = add)
  }
}

nrow_temp <- function(x) {
  if (inherits(x, "tbl_dbi")) {
    return(x |> count() |> pull())
  } else {
    return(nrow(x))
  }
}

ensureInstalled <- function(pkgs) {
  notInstalled <- pkgs[!(pkgs %in% rownames(installed.packages()))]

  if (interactive() & length(notInstalled) > 0) {
    message("Package(s): ", paste(notInstalled, collapse = ", "), " not installed")
    if (!isTRUE(utils::askYesNo("Would you like to install them?"))) {
      return(invisible(NULL))
    }
  }
  for (package in notInstalled) {
    install.packages(package)
  }
}

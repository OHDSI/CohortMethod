% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Viewer.R
\name{uploadExportedResults}
\alias{uploadExportedResults}
\title{Upload exported results to a database}
\usage{
uploadExportedResults(
  connectionDetails,
  databaseSchema,
  append = FALSE,
  exportFolder,
  cohorts
)
}
\arguments{
\item{connectionDetails}{An R object of type \code{connectionDetails} created using the
\code{\link[DatabaseConnector:createConnectionDetails]{DatabaseConnector::createConnectionDetails()}} function.}

\item{databaseSchema}{The name of the database schema where the results will be
written.}

\item{append}{Append the results to existing tables? Can be used for
uploading results from multiple databases into a single
results schema.}

\item{exportFolder}{The folder containing the CSV files to upload, as generated
using the \code{\link[=exportToCsv]{exportToCsv()}} function.}

\item{cohorts}{A data frame describing the cohorts used in the study. Should
include the target, comparator, and outcome of interest cohorts.
The data frame should at least have a \code{cohortId} and \code{cohortName}
columns.}
}
\value{
Does not return anything. Is called for the side-effect of having the results uploaded
to the server.
}
\description{
Upload exported results to a database
}

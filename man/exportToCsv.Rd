% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Export.R
\name{exportToCsv}
\alias{exportToCsv}
\title{Export cohort method results to CSV files}
\usage{
exportToCsv(
  outputFolder,
  exportFolder = file.path(outputFolder, "export"),
  databaseId,
  minCellCount = 5,
  maxCores = 1,
  cmDiagnosticThresholds = createCmDiagnosticThresholds()
)
}
\arguments{
\item{outputFolder}{The folder where runCmAnalyses() generated all results.}

\item{exportFolder}{The folder where the CSV files will written.}

\item{databaseId}{A unique ID for the database. This will be appended to
most tables.}

\item{minCellCount}{To preserve privacy: the minimum number of subjects contributing
to a count before it can be included in the results. If the
count is below this threshold, it will be set to \code{-minCellCount}.}

\item{maxCores}{How many parallel cores should be used?}

\item{cmDiagnosticThresholds}{An object of type \code{CmDiagnosticThresholds} as created using
\code{\link[=createCmDiagnosticThresholds]{createCmDiagnosticThresholds()}}.}
}
\value{
Does not return anything. Is called for the side-effect of populating the \code{exportFolder}
with CSV files.
}
\description{
Export cohort method results to CSV files
}
\details{
This requires that \code{\link[=runCmAnalyses]{runCmAnalyses()}} has been executed first. It exports
all the results in the \code{outputFolder} to CSV files for sharing with other
sites.
}

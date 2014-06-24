\name{dbGetCcdInput}
\alias{dbGetCcdInput}
\title{Get data from the database and insert it in a CCD data object}
\usage{
dbGetCcdInput(connection, outcomeSql, covariateSql, modelType = "clr",
  addIntercept = FALSE, useOffsetCovariate = NULL,
  offsetAlreadyOnLogScale = FALSE, sortCovariates = TRUE,
  makeCovariatesDense = NULL, batchSize = 1e+05)
}
\arguments{
  \item{outcomeSql}{A SQL select statement that returns a
  dataset of outcomes with predefined columns (see below).}

  \item{covariateSql}{A SQL select statement that returns a
  dataset of covariates with predefined columns (see
  below).}

  \item{modelType}{CCD model type. Current supported types
  are "ls", "pr", "lr", "clr", "sccs", or "cox"}

  \item{addIntercept}{Add an intercept to the model?}

  \item{useOffsetCovariate}{Use the time variable in the
  model as an offset?}

  \item{offsetAlreadyOnLogScale}{Is the time variable
  already on a log scale?}

  \item{sortCovariates}{Do the covariates still need to be
  sorted?}

  \item{batchSize}{Number of rows to be read from each
  table at a time. Larger batch sizes lead to fewer calls
  to the database and should be more efficient, but may
  lead to out-of-memory errors.}
}
\value{
An object of type CcdModel
}
\description{
\code{dbGetCcdInput} loads data from the database using two
queries, and inserts it into a CCD data object.
}
\details{
These columns are expected in the outcome table:
\tabular{lll}{ \verb{stratum_id} \tab(integer) \tab Stratum
ID for conditional regression models \cr \verb{row_id}
\tab(integer) \tab Row ID is used to link multiple
covariates (x) to a single outcome (y) \cr \verb{y}
\tab(real) \tab The outcome variable \cr \verb{time}
\tab(real) \tab For models that use time (e.g. Poisson or
Cox regression) this contains time (e.g. number of days)
\cr } The outcome table should be sorted by stratum_id and
then by row_id

These columns are expected in the covariates table:
\tabular{lll}{ \verb{stratum_id} \tab(integer) \tab Stratum
ID for conditional regression models \cr \verb{row_id}
\tab(integer) \tab Row ID is used to link multiple
covariates (x) to a single outcome (y) \cr
\verb{covariate_id} \tab(integer) \tab A numeric identifier
of a covariate \cr \verb{covariate_value} \tab(real) \tab
The value of the specified covariate \cr } The covariate
table should be sorted by stratum_id, row_id, and
covariate_id
}
\examples{
\dontrun{
  connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com", schema="test")
  connection <- connect(connectionDetails)
  outcomeSql <- "SELECT * FROM outcomes ORDER BY stratum_id, row_id"
  covariateSql <-"SELECT * FROM covariates ORDER BY stratum_id, row_id, covariate_id"

  ccdData <- dbGetCcdInput(connection,outcomeSql,covariateSql,modelType = "clr")

  dbDisconnect(connection)

  ccdFit <- fitCcdModel(ccdData, prior = prior("normal",0.01))
}
}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateArgFunctions.R
\name{createMatchOnPsArgs}
\alias{createMatchOnPsArgs}
\title{Create a parameter object for the function matchOnPs}
\usage{
createMatchOnPsArgs(
  caliper = 0.2,
  caliperScale = "standardized logit",
  maxRatio = 1,
  allowReverseMatch = FALSE,
  stratificationColumns = c()
)
}
\arguments{
\item{caliper}{The caliper for matching. A caliper is the distance which is acceptable for any match. Observations which are outside of the caliper are dropped. A caliper of 0 means no caliper is used.}

\item{caliperScale}{The scale on which the caliper is defined. Three scales are supported: caliperScale = 'propensity score', caliperScale = 'standardized', or caliperScale = 'standardized logit'. On the standardized scale, the caliper is interpreted in standard deviations of the propensity score distribution. 'standardized logit' is similar, except that the propensity score is transformed to the logit scale because the PS is more likely to be normally distributed on that scale (Austin, 2011).}

\item{maxRatio}{The maximum number of persons in the comparator arm to be matched to each person in the treatment arm. A maxRatio of 0 means no maximum: all comparators will be assigned to a target person.}

\item{allowReverseMatch}{Allows n-to-1 matching if target arm is larger}

\item{stratificationColumns}{Names or numbers of one or more columns in the data data.frame on which subjects should be stratified prior to matching. No persons will be matched with persons outside of the strata identified by the values in these columns.}
}
\description{
Create a parameter object for the function matchOnPs
}
\details{
Create an object defining the parameter values.
}

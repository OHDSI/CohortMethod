library(CohortMethod)
library(testthat)

test_that("filterAndTidyCovariates applies includeCovariateIds and excludeCovariateIds", {
  # Regression test: the exclusion filter used to use includeCovariateIds.
  covariates <- tibble(
    rowId = c(1:2, 1:3, 1:4, 1:5),
    covariateId = rep(c(1001, 2001, 3001, 4001), 2:5),
    covariateValue = 1
  )
  cohortMethodData <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = tibble(
      covariateId = c(1001, 2001, 3001, 4001),
      covariateName = c("a", "b", "c", "d"),
      analysisId = 1,
      conceptId = 1:4
    ),
    analysisRef = tibble(
      analysisId = 1,
      analysisName = "test",
      domainId = "test",
      startDay = 0,
      endDay = 0,
      isBinary = "Y",
      missingMeansZero = "Y"
    )
  )
  attr(cohortMethodData, "metaData") <- list()
  on.exit(close(cohortMethodData))

  getCovariateIds <- function(includeCovariateIds, excludeCovariateIds) {
    covariateData <- filterAndTidyCovariates(
      cohortMethodData = cohortMethodData,
      includeRowIds = 1:10,
      includeCovariateIds = includeCovariateIds,
      excludeCovariateIds = excludeCovariateIds
    )
    on.exit(close(covariateData))
    covariateData$covariates |>
      distinct(.data$covariateId) |>
      pull() |>
      sort()
  }
  expect_equal(getCovariateIds(c(), c()), c(1001, 2001, 3001, 4001))
  expect_equal(getCovariateIds(c(), c(2001)), c(1001, 3001, 4001))
  expect_equal(getCovariateIds(c(1001, 2001, 3001), c()), c(1001, 2001, 3001))
  expect_equal(getCovariateIds(c(1001, 2001, 3001), c(2001)), c(1001, 3001))
})

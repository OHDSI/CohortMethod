library(CohortMethod)
library(testthat)

test_that("createPs keeps the rows of one subject in one cross-validation fold", {
  skip_if(packageVersion("Cyclops") < "4.0.1", "subjects in logistic regression need Cyclops 4.0.1")
  data("cohortMethodDataSimulationProfile", package = "CohortMethod", envir = environment())
  set.seed(1)
  cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = 400)
  on.exit(close(cohortMethodData))
  population <- dplyr::collect(cohortMethodData$cohorts)
  # The cross-validation unit that Cyclops reports. Treatment in the simulated data barely depends on
  # the covariates, so cross-validation picks the null model; its warning does not matter here.
  selectorType <- function(population) {
    createPsArgs <- createCreatePsArgs(
      errorOnHighCorrelation = FALSE,
      control = Cyclops::createControl(noiseLevel = "quiet", cvType = "auto", fold = 5, cvRepetitions = 1, seed = 1)
    )
    output <- capture.output(withCallingHandlers(
      suppressMessages(createPs(cohortMethodData, population, createPsArgs)),
      warning = function(w) if (grepl("All coefficients", conditionMessage(w))) invokeRestart("muffleWarning")
    ))
    return(sub("^Using cross-validation selector type ", "", grep("^Using cross-validation selector type", output, value = TRUE)))
  }
  expect_equal(selectorType(population), "byRow")
  # Pairs of rows as one subject each, as the copies of a person in a bootstrap sample:
  population$subjectId <- (rank(population$rowId) + 1) %/% 2
  expect_equal(selectorType(population), "bySubject")
})

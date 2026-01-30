library(testthat)
library(CohortMethod)

set.seed(1234)
data(cohortMethodDataSimulationProfile)
sampleSize <- 1
tmpCmd <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = sampleSize)

cohortRow <- tmpCmd$cohorts |>
  collect()

outcomeRow <- tmpCmd$outcomes |>
  collect()

test_that("createStudyPop: removeSubjectsWithPriorOutcome", {
  tmpCmd$cohorts <- cohortRow
  outcomes <- outcomeRow
  outcomes$rowId[1] <- 1
  outcomes$daysToEvent[1] <- -10
  outcomes$outcomeId[1] <- 123
  tmpCmd$outcomes <- outcomes

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE)
  )
  expect_equal(nrow(sp), 1)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE)
  )
  expect_equal(nrow(sp), 0)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 999,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE)
  )
  expect_equal(nrow(sp), 1)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = TRUE,
                                                                priorOutcomeLookback = 9)
  )
  expect_equal(nrow(sp), 1)
})

test_that("createStudyPop: minDaysAtRisk", {
  cohorts <- cohortRow
  cohorts$rowId[1] <- 1
  cohorts$daysToCohortEnd[1] <- 10
  cohorts$daysToObsEnd[1] <- 10
  tmpCmd$cohorts <- cohorts

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 999,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(minDaysAtRisk = 1,
                                                                endAnchor = "cohort end")
  )
  expect_equal(nrow(sp), 1)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 999,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(minDaysAtRisk = 20,
                                                                endAnchor = "cohort end")
  )
  expect_equal(nrow(sp), 0)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 999,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(minDaysAtRisk = 11,
                                                                endAnchor = "cohort end")
  )
  expect_equal(nrow(sp), 1)

})

test_that("createStudyPop: maxDaysAtRisk", {
  cohorts <- cohortRow
  cohorts$rowId[1] <- 1
  cohorts$daysToCohortEnd[1] <- 10
  cohorts$daysToObsEnd[1] <- 10
  tmpCmd$cohorts <- cohorts

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 999,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(endAnchor = "cohort end")
  )
  expect_equal(sp$riskEnd, 10)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 999,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(maxDaysAtRisk = 5,
                                                                endAnchor = "cohort end")
  )
  expect_equal(sp$riskEnd, 5)
})

test_that("createStudyPop: risk window definition", {
  cohorts <- cohortRow
  cohorts$rowId[1] <- 1
  cohorts$daysToCohortEnd[1] <- 10
  cohorts$daysToObsEnd[1] <- 20
  tmpCmd$cohorts <- cohorts

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                startAnchor = "cohort start",
                                                                riskWindowStart = 0,
                                                                endAnchor = "cohort end",
                                                                riskWindowEnd = 0)
  )
  expect_equal(sp$timeAtRisk, 11)

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                startAnchor = "cohort start",
                                                                riskWindowStart = 1,
                                                                endAnchor = "cohort end",
                                                                riskWindowEnd = 0)
  )
  expect_equal(sp$timeAtRisk, 10)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                startAnchor = "cohort start",
                                                                riskWindowStart = 0,
                                                                endAnchor = "cohort end",
                                                                riskWindowEnd = 9999)
  )
  expect_equal(sp$timeAtRisk, 21)
})

test_that("createStudyPop: censor at new risk window start", {
  cohorts <- bind_rows(cohortRow, cohortRow, cohortRow)
  cohorts$rowId <- c(1, 2, 3)
  cohorts$treatment <- c(1, 0, 1)
  cohorts$personSeqId <- c(1, 1, 2)
  cohorts$cohortStartDate <- as.Date(c("2000-01-01", "2000-02-01", "2000-01-01"))
  cohorts$daysToCohortEnd <- c(100, 100, 100)
  cohorts$daysToObsEnd <- c(1000, 1000, 1000)
  tmpCmd$cohorts <- cohorts

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 0,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(censorAtNewRiskWindow = TRUE)
  )
  expect_equal(sp$timeAtRisk, c(31, 101, 101))
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 0,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(censorAtNewRiskWindow = FALSE)
  )
  expect_equal(sp$timeAtRisk, c(101, 101, 101))
})

test_that("createStudyPop: outcomes", {
  outcomes <- outcomeRow
  outcomes$rowId[1] <- 1
  outcomes$daysToEvent[1] <- 15
  outcomes$outcomeId[1] <- 123
  cohorts <- cohortRow
  cohorts$rowId[1] <- 1
  cohorts$daysToCohortEnd[1] <- 10
  cohorts$daysToObsEnd[1] <- 20
  tmpCmd$outcomes <- outcomes
  tmpCmd$cohorts <- cohorts

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(riskWindowEnd = 999)
  )
  expect_equal(sp$outcomeCount, 1)
  expect_equal(sp$survivalTime, 16)
  expect_equal(sp$daysToEvent, 15)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    createStudyPopulationArgs = createCreateStudyPopulationArgs(riskWindowEnd = 0,
                                                                endAnchor = "cohort end")
  )
  expect_equal(sp$outcomeCount, 0)
  expect_equal(sp$survivalTime, 11)
})

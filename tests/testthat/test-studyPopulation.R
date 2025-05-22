library("testthat")
library("CohortMethod")

set.seed(1234)
data(cohortMethodDataSimulationProfile)
sampleSize <- 1
tmpCmd <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = sampleSize)

cohortRow <- tmpCmd$cohorts |>
  collect()

outcomeRow <- tmpCmd$outcomes |>
  collect()

test_that("createStudyPop: washout period", {
  tmpCmd$cohorts <- cohortRow |>
    mutate(rowId = 1, daysFromObsStart = 170)

  sp <- createStudyPopulation(cohortMethodData = tmpCmd, washoutPeriod = 160)
  expect_equal(nrow(sp), 1)
  sp <- createStudyPopulation(cohortMethodData = tmpCmd, washoutPeriod = 180)
  expect_equal(nrow(sp), 0)
})

test_that("createStudyPop: firstExposureOnly", {
  tmpCmd$cohorts <- bind_rows(
    cohortRow |>
      mutate(rowId = 1, daysFromObsStart = 200),
    cohortRow |>
      mutate(rowId = 1, daysFromObsStart = 210)
  )

  sp <- createStudyPopulation(cohortMethodData = tmpCmd, firstExposureOnly = FALSE)
  expect_equal(nrow(sp), 2)
  sp <- createStudyPopulation(cohortMethodData = tmpCmd, firstExposureOnly = TRUE)
  expect_equal(nrow(sp), 1)
})

test_that("createStudyPop: removeDuplicateSubjects = 'remove all'", {
  tmpCmd$cohorts <- bind_rows(
    cohortRow |>
      mutate(rowId = 1, treatment = 0),
    cohortRow |>
      mutate(rowId = 1, treatment = 1)
  )

  sp <- createStudyPopulation(cohortMethodData = tmpCmd, removeDuplicateSubjects = "keep all")
  expect_equal(nrow(sp), 2)
  sp <- createStudyPopulation(cohortMethodData = tmpCmd, removeDuplicateSubjects = "remove all")
  expect_equal(nrow(sp), 0)
})

test_that("createStudyPop: removeDuplicateSubjects = 'keep first'", {
  cohorts <- bind_rows(cohortRow, cohortRow, cohortRow)
  cohorts$rowId <- c(1, 2, 3)
  cohorts$treatment <- c(1, 0, 1)
  cohorts$personSeqId <- c(1, 1, 2)
  cohorts$cohortStartDate <- as.Date(c("2000-01-01", "2001-02-01", "2000-01-01"))
  tmpCmd$cohorts <- cohorts

  sp <- createStudyPopulation(cohortMethodData = tmpCmd, removeDuplicateSubjects = "keep first")
  expect_equal(sp$rowId, c(1, 3))
  sp <- createStudyPopulation(cohortMethodData = tmpCmd, removeDuplicateSubjects = "keep all")
  expect_equal(sp$rowId, c(1, 2, 3))
})

test_that("createStudyPop: removeDuplicateSubjects = 'keep first' removing ties", {
  cohorts <- bind_rows(cohortRow, cohortRow, cohortRow, cohortRow)
  cohorts$rowId <- c(1, 2, 3, 4)
  cohorts$treatment <- c(1, 0, 1, 0)
  cohorts$personSeqId <- c(1, 1, 2, 2)
  cohorts$cohortStartDate <- as.Date(c("2000-01-01", "2001-02-01", "2000-01-01", "2000-01-01"))
  tmpCmd$cohorts <- cohorts

  sp <- createStudyPopulation(cohortMethodData = tmpCmd, removeDuplicateSubjects = "keep first")
  expect_equal(sp$rowId, c(1))
  sp <- createStudyPopulation(cohortMethodData = tmpCmd, removeDuplicateSubjects = "keep all")
  expect_equal(sp$rowId, c(1, 2, 3, 4))
})

test_that("createStudyPop: restrictToCommonPeriod", {
  cohorts <- bind_rows(cohortRow, cohortRow, cohortRow, cohortRow, cohortRow, cohortRow)
  cohorts$rowId <- c(1, 2, 3, 4, 5, 6)
  cohorts$treatment <- c(1, 1, 1, 0, 0, 0)
  cohorts$cohortStartDate <- c("2000-01-01", "2000-09-01", "2000-08-01", "2000-04-01", "2000-02-01", "2000-10-01")
  tmpCmd$cohorts <- cohorts

  sp <- createStudyPopulation(cohortMethodData = tmpCmd, restrictToCommonPeriod = FALSE)
  expect_equal(nrow(sp), 6)
  sp <- createStudyPopulation(cohortMethodData = tmpCmd, restrictToCommonPeriod = TRUE)
  expect_equal(nrow(sp), 4)
  expect_equal(sp$rowId, c(2, 3, 4, 5))
})

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
    removeSubjectsWithPriorOutcome = FALSE
  )
  expect_equal(nrow(sp), 1)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    removeSubjectsWithPriorOutcome = TRUE
  )
  expect_equal(nrow(sp), 0)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 999,
    removeSubjectsWithPriorOutcome = TRUE
  )
  expect_equal(nrow(sp), 1)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 9
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
    minDaysAtRisk = 1,
    endAnchor = "cohort end"
  )
  expect_equal(nrow(sp), 1)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    minDaysAtRisk = 20,
    endAnchor = "cohort end"
  )
  expect_equal(nrow(sp), 0)
})

test_that("createStudyPop: maxDaysAtRisk", {
  cohorts <- cohortRow
  cohorts$rowId[1] <- 1
  cohorts$daysToCohortEnd[1] <- 10
  cohorts$daysToObsEnd[1] <- 10
  tmpCmd$cohorts <- cohorts

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    endAnchor = "cohort end"
  )
  expect_equal(sp$riskEnd, 10)
  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    maxDaysAtRisk = 5,
    endAnchor = "cohort end"
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
    removeSubjectsWithPriorOutcome = FALSE,
    startAnchor = "cohort start",
    riskWindowStart = 0,
    endAnchor = "cohort end",
    riskWindowEnd = 0
  )
  expect_equal(sp$timeAtRisk, 11)

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    removeSubjectsWithPriorOutcome = FALSE,
    startAnchor = "cohort start",
    riskWindowStart = 1,
    endAnchor = "cohort end",
    riskWindowEnd = 0
  )
  expect_equal(sp$timeAtRisk, 10)

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    removeSubjectsWithPriorOutcome = FALSE,
    startAnchor = "cohort start",
    riskWindowStart = 0,
    endAnchor = "cohort start",
    riskWindowEnd = 9999
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

  sp <- createStudyPopulation(cohortMethodData = tmpCmd, outcomeId = 0, censorAtNewRiskWindow = TRUE)
  expect_equal(sp$timeAtRisk, c(31, 101, 101))
  sp <- createStudyPopulation(cohortMethodData = tmpCmd, outcomeId = 0, censorAtNewRiskWindow = FALSE)
  expect_equal(sp$timeAtRisk, c(101, 101, 101))
})

test_that("createStudyPop: keep first when only 1 person overall", {
  cohorts <- bind_rows(cohortRow)
  cohorts$rowId <- c(1)
  cohorts$treatment <- c(0)
  cohorts$personSeqId <- c(1)
  cohorts$cohortStartDate <- as.Date(c("2000-01-01"))
  cohorts$daysToCohortEnd <- c(100)
  cohorts$daysToObsEnd <- c(1000)
  tmpCmd$cohorts <- cohorts

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    censorAtNewRiskWindow = TRUE,
    removeDuplicateSubjects = "keep first"
  )
  expect_equal(nrow(sp), 1)
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

  sp <- createStudyPopulation(cohortMethodData = tmpCmd, outcomeId = 123, riskWindowEnd = 999)
  expect_equal(sp$outcomeCount, 1)
  expect_equal(sp$survivalTime, 16)
  expect_equal(sp$daysToEvent, 15)

  sp <- createStudyPopulation(
    cohortMethodData = tmpCmd,
    outcomeId = 123,
    riskWindowEnd = 0,
    endAnchor = "cohort end"
  )
  expect_equal(sp$outcomeCount, 0)
  expect_equal(sp$survivalTime, 11)
})

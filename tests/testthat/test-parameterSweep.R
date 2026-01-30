library(CohortMethod)
library(testthat)

# This is a broad, shallow sweep of all functionality. It checks whether the code produces an output
# (and does not throw an error) under a wide range of parameter settings
set.seed(1234)
data(cohortMethodDataSimulationProfile)
sampleSize <- 1000
cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = sampleSize)

test_that("cohortMethodData functions", {
  expect_output(print(cohortMethodData), "CohortMethodData object.*")
  s <- summary(cohortMethodData)
  expect_s3_class(s, "summary.CohortMethodData")
  expect_equal(s$targetPersons + s$comparatorPersons, sampleSize)
  expect_output(print(s), "CohortMethodData object summary.*")

  file <- tempfile()
  cmd1 <- Andromeda::copyAndromeda(cohortMethodData)
  attr(cmd1, "metaData") <- attr(cohortMethodData, "metaData")
  class(cmd1) <- "CohortMethodData"
  saveCohortMethodData(cmd1, file)
  cmd2 <- loadCohortMethodData(file)
  expect_identical(collect(cohortMethodData$cohorts), collect(cmd2$cohorts))
  expect_identical(collect(cohortMethodData$outcomes), collect(cmd2$outcomes))
  expect_equal(collect(cohortMethodData$covariates), collect(cmd2$covariates))
  expect_equal(collect(cohortMethodData$covariateRef), collect(cmd2$covariateRef))
  expect_equal(collect(cohortMethodData$analysisRef), collect(cmd2$analysisRef))
  expect_equivalent(attr(cohortMethodData, "metaData"), attr(cmd2, "metaData"))
  close(cmd2)
  unlink(file, force = TRUE)
})

test_that("Create study population functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    createStudyPopulationArgs = createCreateStudyPopulationArgs(
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1
                                    ))
  expect_true(all(studyPop$timeAtRisk > 0))
  peopleWithPriorOutcomes <- cohortMethodData$outcomes |>
    filter(outcomeId == 194133 & daysToEvent < 0) |>
    distinct(rowId) |>
    pull()
  expect_false(any(peopleWithPriorOutcomes %in% studyPop$rowId))

  aTable <- getAttritionTable(studyPop)
  expect_s3_class(aTable, "data.frame")

  plot <- plotTimeToEvent(cohortMethodData,
                          outcomeId = 194133
  )
  expect_s3_class(plot, "ggplot")

  plot <- plotFollowUpDistribution(studyPop)
  expect_s3_class(plot, "ggplot")

  mdrr <- computeMdrr(studyPop)
  expect_s3_class(mdrr, "data.frame")
})

test_that("Propensity score functions", {
  # No population:
  ps <- createPs(cohortMethodData = cohortMethodData,
                 createPsArgs = createCreatePsArgs(
                   prior = createPrior("laplace", 0.1, exclude = 0)
                 ))
  expect_lt(0.65, computePsAuc(ps)[1])

  # With population:
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    createStudyPopulationArgs = createCreateStudyPopulationArgs(
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1
                                    ))
  ps <- createPs(cohortMethodData = cohortMethodData,
                 population = studyPop,
                 createPsArgs = createCreatePsArgs(
                   prior = createPrior("laplace", 0.1, exclude = 0)
                 ))
  expect_lt(0.65, computePsAuc(ps)[1])

  # Filtering of covariates:
  ps <- createPs(cohortMethodData = cohortMethodData,
                 population = studyPop,
                 createPsArgs = createCreatePsArgs(
                   prior = createPrior("laplace", 0.1, exclude = 0),
                   excludeCovariateIds = c(8507001, 8532001)
                 ))
  model <- getPsModel(ps, cohortMethodData)
  expect_s3_class(model, "data.frame")
  expect_true(all(!c(8507001, 8532001) %in% model$covariateId))

  for (scale in c("preference", "propensity")) {
    for (type in c("density", "histogram")) {
      p <- plotPs(ps, scale = scale, type = type)
      expect_s3_class(p, "ggplot")
    }
  }
  p <- plotPs(ps, showCountsLabel = TRUE, showEquipoiseLabel = TRUE, showAucLabel = TRUE)
  expect_s3_class(p, "ggplot")

  psTrimmed <- trimByPs(ps, trimByPsArgs = createTrimByPsArgs(trimFraction = 0.05))
  expect_s3_class(psTrimmed, "data.frame")

  psTrimmed <- trimByPs(ps, trimByPsArgs = createTrimByPsArgs(equipoiseBounds = c(0.3, 0.7)))
  expect_s3_class(psTrimmed, "data.frame")

  psTrimmed <- trimByPs(ps, trimByPsArgs = createTrimByPsArgs(maxWeight = 10))
  expect_s3_class(psTrimmed, "data.frame")

  for (scale in c("preference", "propensity")) {
    for (type in c("density", "histogram")) {
      p <- plotPs(psTrimmed, ps, scale = scale, type = type)
      expect_s3_class(p, "ggplot")
    }
  }

  equipoise <- computeEquipoise(ps)
  expect_gt(equipoise, 0.5)

  for (numberOfStrata in c(2, 5, 10)) {
    strata <- stratifyByPs(psTrimmed,
                           stratifyByPsArgs = createStratifyByPsArgs(
                             numberOfStrata = numberOfStrata
                           ))
    expect_s3_class(strata, "data.frame")
  }

  for (numberOfStrata in c(2, 5, 10)) {
    # age + sex
    strata <- stratifyByPs(psTrimmed,
                           cohortMethodData = cohortMethodData,
                           stratifyByPsArgs = createStratifyByPsArgs(
                             numberOfStrata = numberOfStrata,
                             stratificationCovariateIds = c(0:27 * 1000 + 3, 8532001)
                           ))

    expect_s3_class(strata, "data.frame")
  }

  for (caliper in c(0, 0.25)) {
    for (caliperScale in c("propensity score", "standardized", "standardized logit")) {
      for (maxRatio in c(0, 1, 3)) {
        for (covariateIds in list(NULL, c(11:27, 8507)))
          strata <- matchOnPs(psTrimmed,
                              cohortMethodData = cohortMethodData,
                              matchOnPsArgs = createMatchOnPsArgs(
                                caliper = caliper,
                                caliperScale = caliperScale,
                                maxRatio = maxRatio,
                                matchCovariateIds = covariateIds
                              ))
        expect_s3_class(strata, "data.frame")
      }
    }
  }
})

test_that("Balance functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    createStudyPopulationArgs = createCreateStudyPopulationArgs(
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1
                                    ))
  ps <- createPs(cohortMethodData = cohortMethodData,
                 population = studyPop,
                 createPsArgs = createCreatePsArgs(
                   prior = createPrior("laplace", 0.1, exclude = 0)
                 ))
  strata <- matchOnPs(population = ps,
                      matchOnPsArgs = createMatchOnPsArgs(
                        caliper = 0.25,
                        caliperScale = "standardized",
                        maxRatio = 1))
  balance <- computeCovariateBalance(population = strata,
                                     cohortMethodData = cohortMethodData,
                                     computeCovariateBalanceArgs = createComputeCovariateBalanceArgs())
  expect_s3_class(balance, "data.frame")

  p <- plotCovariateBalanceScatterPlot(balance)
  expect_s3_class(p, "ggplot")

  p <- plotCovariateBalanceOfTopVariables(balance)
  expect_s3_class(p, "ggplot")

  p <- plotCovariatePrevalence(balance)
  expect_s3_class(p, "ggplot")

  table1 <- createCmTable1(balance)
  expect_s3_class(table1, "data.frame")

  covariateIds <- 0:20 * 1000 + 3
  balance <- computeCovariateBalance(population = strata,
                                     cohortMethodData = cohortMethodData,
                                     computeCovariateBalanceArgs = createComputeCovariateBalanceArgs(
                                       covariateFilter = covariateIds
                                     ))
  expect_s3_class(balance, "data.frame")
  expect_true(all(balance$covariateId %in% covariateIds))

  # Test sampling for balance:
  expect_no_error({
    balanceSampled <- computeCovariateBalance(strata,
                                              cohortMethodData,
                                              computeCovariateBalanceArgs = createComputeCovariateBalanceArgs(
                                                maxCohortSize = 100
                                              ))

  })
  expect_s3_class(balanceSampled, "data.frame")

  # Test balance for subgroup
  expect_no_error({
    balanceSubgroup <- computeCovariateBalance(strata,
                                               cohortMethodData,
                                               computeCovariateBalanceArgs = createComputeCovariateBalanceArgs(
                                                 subgroupCovariateId = 8532001 # Female
                                               ))

  })
  expect_s3_class(balanceSubgroup, "data.frame")
  expect_true(nrow(balanceSubgroup) != nrow(balance))

  table <- getGeneralizabilityTable(balance)
  expect_s3_class(table, "data.frame")
})

test_that("Outcome functions", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    createStudyPopulationArgs = createCreateStudyPopulationArgs(
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1
                                    ))
  ps <- createPs(cohortMethodData = cohortMethodData,
                 population = studyPop,
                 createPsArgs = createCreatePsArgs(
                   prior = createPrior("laplace", 0.1, exclude = 0)
                 ))
  strata <- matchOnPs(population = ps,
                      matchOnPsArgs = createMatchOnPsArgs(
                        caliper = 0.25,
                        caliperScale = "standardized",
                        maxRatio = 1))

  lbs <- c()
  # params <- c()
  for (modelType in c("logistic", "poisson", "cox")) {
    for (stratified in c(TRUE, FALSE)) {
      for (useCovariates in c(TRUE, FALSE)) {
        for (bootstrapCi in if (useCovariates) FALSE else TRUE) {
          writeLines(paste(
            "modelType:",
            modelType,
            ",stratified:",
            stratified,
            ",useCovariates:",
            useCovariates,
            ",bootstrapCI:",
            bootstrapCi
          ))

          outcomeModel <- fitOutcomeModel(
            population = strata,
            cohortMethodData = cohortMethodData,
            fitOutcomeModelArgs = createFitOutcomeModelArgs(
              modelType = modelType,
              stratified = stratified,
              useCovariates = useCovariates,
              bootstrapCi = bootstrapCi,
              prior = createPrior("laplace", 0.1)
            )
          )
          expect_s3_class(outcomeModel, "OutcomeModel")
          lbs <- c(lbs, confint(outcomeModel)[1])
          # params <-
          # c(params,paste('type:',type,',stratified:',stratified,',useCovariates:',useCovariates,',addExposureDaysToEnd:',addExposureDaysToEnd))
        }
      }
    }
  }
  writeLines("IPTW")
  for (estimator in c("att", "ate", "ato")) {
    ps <- createPs(cohortMethodData = cohortMethodData,
                   population = studyPop,
                   createPsArgs = createCreatePsArgs(
                     prior = createPrior("laplace", 0.1, exclude = 0),
                     estimator = estimator
                   ))
    for (modelType in c("logistic", "poisson", "cox")) {
      outcomeModel <- fitOutcomeModel(
        population = ps,
        cohortMethodData = cohortMethodData,
        fitOutcomeModelArgs = createFitOutcomeModelArgs(
          modelType = modelType,
          stratified = FALSE,
          useCovariates = FALSE,
          inversePtWeighting = TRUE
        )
      )
      expect_s3_class(outcomeModel, "OutcomeModel")
      lbs <- c(lbs, confint(outcomeModel)[1])
    }
  }

  # results <- data.frame(logRr = logRrs, param = params) results <- results[order(results$logRr),]
  # results

  # All analyses are fundamentally different, so should have no duplicate values at full precision:
  expect_equal(length(unique(lbs)), length(lbs))
})


test_that("Functions on outcome model", {
  studyPop <- createStudyPopulation(cohortMethodData,
                                    outcomeId = 194133,
                                    createStudyPopulationArgs = createCreateStudyPopulationArgs(
                                      removeSubjectsWithPriorOutcome = TRUE,
                                      minDaysAtRisk = 1
                                    ))
  ps <- createPs(cohortMethodData = cohortMethodData,
                 population = studyPop,
                 createPsArgs = createCreatePsArgs(
                   prior = createPrior("laplace", 0.1, exclude = 0)
                 ))
  strata <- matchOnPs(population = ps,
                      matchOnPsArgs = createMatchOnPsArgs(
                        caliper = 0.25,
                        caliperScale = "standardized",
                        maxRatio = 1))
  outcomeModel <- fitOutcomeModel(
    population = strata,
    cohortMethodData = cohortMethodData,
    fitOutcomeModelArgs = createFitOutcomeModelArgs(
      modelType = "cox",
      stratified = TRUE,
      useCovariates = TRUE,
      prior = createPrior("laplace", 0.1)
    )
  )

  expect_output(print(outcomeModel), "Model type: cox.*")

  p <- plotKaplanMeier(strata)
  expect_s3_class(p, "grob")

  p <- drawAttritionDiagram(outcomeModel)
  expect_s3_class(p, "ggplot")

  cf <- coef(outcomeModel)
  ci <- confint(outcomeModel)
  expect_gt(cf, ci[1])
  expect_lt(cf, ci[2])

  fullOutcomeModel <- getOutcomeModel(outcomeModel, cohortMethodData)
  expect_s3_class(fullOutcomeModel, "data.frame")
})

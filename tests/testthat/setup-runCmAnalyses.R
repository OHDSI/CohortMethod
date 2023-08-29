# Setup ----
library(CohortMethod)

outputFolder <- tempfile(pattern = "cmData")

covarSettings <- createDefaultCovariateSettings(addDescendantsToExclude = TRUE)

getDbCmDataArgs <- createGetDbCohortMethodDataArgs(
  washoutPeriod = 183,
  firstExposureOnly = TRUE,
  removeDuplicateSubjects = "remove all",
  covariateSettings = covarSettings
)

createPsArgs <- createCreatePsArgs(
  prior = createPrior("laplace", variance = 0.01),
  estimator = "att"
)

matchOnPsArgs <- createMatchOnPsArgs(maxRatio = 100)

computeSharedCovBalArgs <- createComputeCovariateBalanceArgs()

computeCovBalArgs <- createComputeCovariateBalanceArgs(
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)

truncateIptwArgs <- createTruncateIptwArgs(maxWeight = 10)

tcos1 <- createTargetComparatorOutcomes(
  targetId = 1,
  comparatorId = 2,
  outcomes = list(
    createOutcome(
      outcomeId = 3,
      priorOutcomeLookback = 30
    ),
    createOutcome(
      outcomeId = 4,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  ),
  excludedCovariateConceptIds = c(1118084, 1124300)
)
# Empty cohorts:
tcos2 <- createTargetComparatorOutcomes(
  targetId = 998,
  comparatorId = 999,
  outcomes = list(
    createOutcome(
      outcomeId = 3,
      priorOutcomeLookback = 30
    ),
    createOutcome(
      outcomeId = 4,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  )
)

targetComparatorOutcomesList <- list(tcos1, tcos2)

analysesToExclude <- data.frame(
  targetId = c(998, 998),
  analysisId = c(3, 4)
)

createStudyPopArgs1 <- createCreateStudyPopulationArgs(
  removeSubjectsWithPriorOutcome = TRUE,
  firstExposureOnly = TRUE,
  restrictToCommonPeriod = TRUE,
  removeDuplicateSubjects = "remove all",
  washoutPeriod = 183,
  censorAtNewRiskWindow = TRUE,
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 30,
  endAnchor = "cohort end"
)

createStudyPopArgs2 <- createCreateStudyPopulationArgs(
  removeSubjectsWithPriorOutcome = TRUE,
  firstExposureOnly = TRUE,
  restrictToCommonPeriod = TRUE,
  removeDuplicateSubjects = "keep first",
  washoutPeriod = 183,
  censorAtNewRiskWindow = TRUE,
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 30,
  endAnchor = "cohort end"
)

# Clean-up ----
withr::defer({
  unlink(outputFolder)
})

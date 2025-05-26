# The code here uses simulations to verify the computed variances of the SDM lead to (near) nominal
# type-1 error. These are too computationally expensive to run as unit tests

library(dplyr)

# Simulation under the null, continuous, stratification, sparse covariates -------------------------
simulateOne <- function(seed) {
  print(seed)
  set.seed(seed)

  threshold <- 0

  nStrata <- 10
  n1 <- sample.int(9, nStrata, replace = TRUE) + 1
  n0 <- sample.int(9, nStrata, replace = TRUE) + 1
  ratio <- n1/n0
  proportion <- 1/(1+exp(1-ratio)) # Make proportion function of ratio to make problem non-trivial

  # Create population data:
  stratumId1 <- do.call(c, lapply(seq_len(nStrata), function(x) rep(x, n1[x])))
  covariateValue1 <- rbinom(sum(n1), 1, proportion[stratumId1])
  stratumId0 <- do.call(c, lapply(seq_len(nStrata), function(x) rep(x, n0[x])))
  covariateValue0 <- rbinom(sum(n0), 1, proportion[stratumId0])
  data <- tibble(
    rowId = seq_len(sum(n1) + sum(n0)),
    stratumId = c(stratumId1, stratumId0),
    covariateId = 1,
    covariateValue = c(covariateValue1, covariateValue0),
    treatment = c(rep(1, sum(n1)), rep(0, sum(n0)))
  )
  cohortMethodData <- Andromeda::andromeda()
  cohortMethodData$covariates <- data |>
    filter(covariateValue != 0) |>
    select("rowId", "covariateId", "covariateValue")
  cohortMethodData$cohorts <- data |>
    select("rowId", "stratumId", "treatment")

  balance <- CohortMethod:::computeMeansPerGroup(
    cohorts = cohortMethodData$cohorts,
    cohortMethodData = cohortMethodData,
    covariateFilter = NULL
  )
  t <- (abs(balance$stdDiff) - threshold)/sqrt(balance$sdmVariance)
  p <- pnorm(t, lower.tail = FALSE)
  return(p)
}
cluster <- ParallelLogger::makeCluster(10)
ParallelLogger::clusterRequire(cluster, "dplyr")
ps <- ParallelLogger::clusterApply(cluster, 1:1000, simulateOne)
ParallelLogger::stopCluster(cluster)

ps <- unlist(ps)
mean(ps<0.05/2, na.rm = TRUE)
# [1] 0.078


# Simulation under the null, continuous, no stratification, sparse ---------------------------------
simulateOne <- function(seed) {
  threshold <- 0

  set.seed(seed)
  n1 <- sample.int(90, 1) + 10
  n0 <- sample.int(90, 1) + 10
  trueMean <- runif(1, 0.1, 5)
  trueSd <- 1
  x1 <- rnorm(n1, mean = trueMean, sd = trueSd)
  x0 <- rnorm(n0, mean = trueMean, sd = trueSd)

  data <- tibble(
    rowId = seq_len(sum(n1) + sum(n0)),
    covariateId = 1,
    covariateValue = c(x1, x0),
    treatment = c(rep(1, sum(n1)), rep(0, sum(n0)))
  )
  cohortMethodData <- Andromeda::andromeda()
  cohortMethodData$covariates <- data |>
    filter(covariateValue != 0) |>
    select("rowId", "covariateId", "covariateValue")
  cohortMethodData$cohorts <- data |>
    select("rowId", "treatment")

  balance <- CohortMethod:::computeMeansPerGroup(
    cohorts = cohortMethodData$cohorts,
    cohortMethodData = cohortMethodData,
    covariateFilter = NULL
  )
  t <- (abs(balance$stdDiff) - threshold)/sqrt(balance$sdmVariance)
  p <- pnorm(t, lower.tail = FALSE)
  return(p)
}
cluster <- ParallelLogger::makeCluster(10)
ParallelLogger::clusterRequire(cluster, "dplyr")
ps <- ParallelLogger::clusterApply(cluster, 1:1000, simulateOne)
ParallelLogger::stopCluster(cluster)

ps <- unlist(ps)
mean(ps<0.05/2, na.rm = TRUE)
# [1] 0.059


# Simulation under the null, continuous, IPTW, sparse ----------------------------------------------
simulateOne <- function(seed) {
  threshold <- 0

  set.seed(seed)
  n1 <- sample.int(90, 1) + 10
  n0 <- sample.int(90, 1) + 10
  trueMean <- runif(1, 0.1, 5)
  trueSd <- 1
  x1 <- rnorm(n1, mean = trueMean, sd = trueSd)
  x0 <- rnorm(n0, mean = trueMean, sd = trueSd)

  data <- tibble(
    rowId = seq_len(sum(n1) + sum(n0)),
    covariateId = 1,
    covariateValue = c(x1, x0),
    treatment = c(rep(1, sum(n1)), rep(0, sum(n0)))
  ) |> mutate(
    propensityScore = case_when(
      treatment == 1 ~ 0.6 + 0.1 * covariateValue,
      treatment == 0 ~ 0.4 - 0.1 * covariateValue
    )
  ) |>
    mutate(propensityScore = pmin(pmax(propensityScore, 0.01), 0.99)) |>
    mutate(iptw = if_else(treatment == 1,
                          (n1 / (n1 + n0)) / propensityScore,
                          (n0 / (n1 + n0)) / (1 - propensityScore)))
  cohortMethodData <- Andromeda::andromeda()
  cohortMethodData$covariates <- data |>
    filter(covariateValue != 0) |>
    select("rowId", "covariateId", "covariateValue")
  cohortMethodData$cohorts <- data |>
    select("rowId", "treatment", "iptw")

  balance <- CohortMethod:::computeMeansPerGroup(
    cohorts = cohortMethodData$cohorts,
    cohortMethodData = cohortMethodData,
    covariateFilter = NULL
  )
  t <- (abs(balance$stdDiff) - threshold)/sqrt(balance$sdmVariance)
  p <- pnorm(t, lower.tail = FALSE)
  return(p)
}
cluster <- ParallelLogger::makeCluster(10)
ParallelLogger::clusterRequire(cluster, "dplyr")
ps <- ParallelLogger::clusterApply(cluster, 1:1000, simulateOne)
ParallelLogger::stopCluster(cluster)

ps <- unlist(ps)
mean(ps<0.05/2, na.rm = TRUE)
# [1] 0.067


# Explore new balance metric with real data ------------------------------------
# Assumes SingleStudyVignetteDataFetch.R has been executed
library(CohortMethod)
library(dplyr)
options(andromedaTempFolder = "e:/andromedaTemp")
folder <- "e:/temp/cohortMethodVignette"

cohortMethodData <- loadCohortMethodData(file.path(folder, "cohortMethodData.zip"))
studyPop <- readRDS(file.path(folder, "studyPop.rds"))
ps <- readRDS(file.path(folder, "ps.rds"))

matchedPop <- matchOnPs(ps, caliper = 0.25, caliperScale = "standardized", maxRatio = 100)
balance <- computeCovariateBalance(matchedPop, cohortMethodData, threshold = 0.1, alpha = 0.05)
plotCovariateBalanceScatterPlot(balance, showUnbalanced = TRUE)

balanceIptw <- computeCovariateBalance(ps, cohortMethodData, threshold = 0.1, alpha = 0.05)
plotCovariateBalanceScatterPlot(balanceIptw, showUnbalanced = TRUE)

balanceUnadjusted <- computeCovariateBalance(studyPop, cohortMethodData, threshold = 0.1, alpha = 0.05)
plotCovariateBalanceScatterPlot(balanceUnadjusted, showUnbalanced = TRUE)

ageCovariateIds <- covariateIds <- 0:22 * 1000 + 3
sexCovariateId <-  8532 * 1000 + 1
psAgeSex <- createPs(
  cohortMethodData = cohortMethodData,
  population = studyPop,
  prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
  control = createControl(
    cvType = "auto",
    startingVariance = 0.01,
    noiseLevel = "quiet",
    tolerance = 2e-07,
    cvRepetitions = 1,
    threads = 10
  ),
  includeCovariateIds = c(ageCovariateIds, sexCovariateId)
)

matchedPopAgeSex <- matchOnPs(psAgeSex, caliper = 0.25, caliperScale = "standardized", maxRatio = 100)
balanceAgeSex <- computeCovariateBalance(matchedPopAgeSex, cohortMethodData, threshold = 0.1, alpha = 0.05)
plotCovariateBalanceScatterPlot(balanceAgeSex, showUnbalanced = TRUE)

balanceIptwAgeSex <- computeCovariateBalance(psAgeSex, cohortMethodData, threshold = 0.1, alpha = 0.05)
plotCovariateBalanceScatterPlot(balanceIptwAgeSex, showUnbalanced = TRUE)




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
  p <- (1 - pnorm(t))*2
  return(p)
}
cluster <- ParallelLogger::makeCluster(10)
ParallelLogger::clusterRequire(cluster, "dplyr")
ps <- ParallelLogger::clusterApply(cluster, 1:1000, simulateOne)
ParallelLogger::stopCluster(cluster)

ps <- unlist(ps)
mean(ps<0.05, na.rm = TRUE)
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
  p <- (1 - pnorm(t))*2
  return(p)
}
cluster <- ParallelLogger::makeCluster(10)
ParallelLogger::clusterRequire(cluster, "dplyr")
ps <- ParallelLogger::clusterApply(cluster, 1:1000, simulateOne)
ParallelLogger::stopCluster(cluster)

ps <- unlist(ps)
mean(ps<0.05, na.rm = TRUE)
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
  p <- (1 - pnorm(t))*2
  return(p)
}
cluster <- ParallelLogger::makeCluster(10)
ParallelLogger::clusterRequire(cluster, "dplyr")
ps <- ParallelLogger::clusterApply(cluster, 1:1000, simulateOne)
ParallelLogger::stopCluster(cluster)

ps <- unlist(ps)
mean(ps<0.05, na.rm = TRUE)
# [1] 0.067

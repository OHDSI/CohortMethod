# Some simulations to explore the idea of using PS stratification in combination with weighting.
# This allows estimation of the ATT while avoiding the observed poor performance of IPTW.

library(CohortMethod)
library(survival)
library(ParallelLogger)
options(andromedaTempFolder = "d:/andromedaTemp")

# Simulation --------------------------------------------------------------------------------------
settings <- list(
  n = 10000,
  pTreatmentIntercept = 0.2,
  psSd = 3, # Propensity score SD
  hazardIntercept = 0.001,
  confoundingStrength = 4,
  effectModificationStrength = 4,
  psNoise = 0
)

runOneSimulation <- function(seed, settings, strategy = "unadjusted") {
    # Introduce confounding by having baseline hazard correlate with PS:
  baselineHazard <- function(ps) {
    return(settings$hazardIntercept + ps * settings$hazardIntercept * settings$confoundingStrength)
  }
  # Introduce effect modification by having hazard ratio correlate with PS:
  hazardRatio <- function(ps) {
    return(1 + ps * settings$effectModificationStrength)
  }
  logistic <- function(x) {
    return(1/(1+exp(-x)))
  }
  logit <- function(x) {
    return(log(x / (1 - x)))
  }
  propensityScore <- logistic(log(settings$pTreatmentIntercept) + rnorm(settings$n, sd = settings$psSd))
  population <- tibble(rowId = 1:settings$n,
                       treatment = as.integer(runif(settings$n, 0,1) < propensityScore),
                       propensityScore = propensityScore)
  # plotPs(population, showEquiposeLabel = TRUE, showAucLabel = TRUE)
  # mean(population$treatment)
  population$hr <- if_else(population$treatment == 1, hazardRatio(population$propensityScore), 1)
  population$hazard <- baselineHazard(population$propensityScore) * population$hr
  timeToEvent <- round(rexp(settings$n, population$hazard))
  timeToCensor <- round(rexp(settings$n, 0.01))
  population$survivalTime <- timeToEvent
  population$survivalTime[timeToEvent > timeToCensor] <- timeToCensor[timeToEvent > timeToCensor]
  population$y <- 1
  population$y[timeToEvent > timeToCensor] <- 0

  # Add some noise to PS:
  population$propensityScore <- logistic(logit(population$propensityScore) + rnorm(settings$n, sd = settings$psNoise))

  if (strategy == "unadjusted") {
    cyclopsData <- createCyclopsData(Surv(survivalTime, y) ~ treatment,
                                     data = population,
                                     modelType = "cox")
    fit <- fitCyclopsModel(cyclopsData)
    ci <- confint(fit, "treatment")[2:3]
  } else if (strategy == "statification") {
    strataPop <- stratifyByPs(population, numberOfStrata = 10)
    cyclopsData <- createCyclopsData(Surv(survivalTime, y) ~ treatment + strata(stratumId),
                                     data = strataPop,
                                     modelType = "cox")
    fit <- fitCyclopsModel(cyclopsData)
    ci <- confint(fit, "treatment")[2:3]
  } else if (strategy == "1-on-1 matching") {
    matchedPop <- matchOnPs(population, maxRatio = 1)
    cyclopsData <- createCyclopsData(Surv(survivalTime, y) ~ treatment,
                                     data = matchedPop,
                                     modelType = "cox")
    fit <- fitCyclopsModel(cyclopsData)
    ci <- confint(fit, "treatment")[2:3]
  } else if (strategy == "variable ratio matching") {
    matchedPop <- matchOnPs(population, maxRatio = 100)
    cyclopsData <- createCyclopsData(Surv(survivalTime, y) ~ treatment + strata(stratumId),
                                     data = matchedPop,
                                     modelType = "cox")
    fit <- fitCyclopsModel(cyclopsData)
    ci <- confint(fit, "treatment")[2:3]
  } else if (strategy == "weighting") {
    population$weights <- ifelse(population$treatment == 1,
                                 mean(population$treatment == 1),
                                 mean(population$treatment == 0) * population$propensityScore / (1 - population$propensityScore))
    fit <- ipwCoxCSV::ipwCoxInd(data = as.data.frame(population),
                         indA = "treatment",
                         indX = c("propensityScore"),
                         indStatus = "y",
                         indTime = "survivalTime")
    ci <- log(fit[2, 4:5])
    # cyclopsData <- createCyclopsData(Surv(survivalTime, y) ~ treatment,
    #                                  data = population,
    #                                  modelType = "cox")
    # fit <- fitCyclopsModel(cyclopsData, weights = population$weights)
  } else if (strategy == "weighted stratification") {
    strataPop <- stratifyByPs(population, numberOfStrata = 10)
    counts <- strataPop |>
      group_by(stratumId, treatment) |>
      summarize(subjects = n(), outcomes = sum(y), .groups = "drop") |>
      ungroup()
    strataPop <- strataPop |>
      group_by(stratumId) |>
      mutate(weight = mean(treatment)) |>
      ungroup()
    cyclopsData <- createCyclopsData(Surv(survivalTime, y) ~ treatment + strata(stratumId),
                                     data = strataPop,
                                     modelType = "cox")
    fit <- fitCyclopsModel(cyclopsData, weights = strataPop$weight)
    ci <- confint(fit, "treatment")[2:3]
  } else {
    stop("Unknown strategy: ", strategy)
  }

  logTrueAtt <- mean(log(population$hr[population$treatment == 1]))
  coverage <- logTrueAtt >= ci[1] & logTrueAtt <= ci[2]
}

cluster <- makeCluster(20)
clusterRequire(cluster, "survival")
clusterRequire(cluster, "Cyclops")
clusterRequire(cluster, "CohortMethod")

x <- clusterApply(cluster, 1:1000, runOneSimulation, settings = settings, strategy = "unadjusted")
mean(do.call(c, x))
# 0

x <- clusterApply(cluster, 1:1000, runOneSimulation, settings = settings, strategy = "statification")
mean(do.call(c, x))
# 0.019

x <- clusterApply(cluster, 1:1000, runOneSimulation, settings = settings, strategy = "1-on-1 matching")
mean(do.call(c, x))
# 0.011

x <- clusterApply(cluster, 1:1000, runOneSimulation, settings = settings, strategy = "variable ratio matching")
mean(do.call(c, x))
# 0.014

x <- clusterApply(cluster, 1:1000, runOneSimulation, settings = settings, strategy = "weighting")
mean(do.call(c, x))
# 0.008

x <- clusterApply(cluster, 1:1000, runOneSimulation, settings = settings, strategy = "weighted stratification")
mean(do.call(c, x), na.rm = TRUE)
# 0.98

results <- tibble(pT = seq(0.05, 0.95, by = 0.05),
                  coverage = 0)

for (i in seq_len(nrow(results))) {
  settings$pTreatmentIntercept = results$pT[i]
  x <- clusterApply(cluster, 1:1000, runOneSimulation, settings = settings, strategy = "weighted stratification")
  results$coverage[i] <-   mean(do.call(c, x), na.rm = TRUE)
  writeLines(sprintf("pT = %0.2f, coverage = %0.2f", settings$pTreatmentIntercept, results$coverage[i]))
}
plot(results$pT, results$coverage)

stopCluster(cluster)

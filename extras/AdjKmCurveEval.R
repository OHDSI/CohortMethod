library(CohortMethod)
options(andromedaTempFolder = "d:/andromedaTemp")

# Simulation --------------------------------------------------------------------------------------
n <- 10000
hr <- 1.1
pT <- 0.3 # Probability of treatment

# Introduce confounding by having baseline hazard correlate with PS:
baselineHazard <- function(ps) {
  return(0.001 + ps*0.004)
}

runOneSimulation <- function(x, strategy = "match") {
  propensityScore <- logistic(log(pT) + rnorm(n, sd = 2))
  population <- tibble(rowId = 1:n,
                       treatment = as.integer(runif(n, 0,1) < propensityScore),
                       propensityScore = propensityScore)
  population$hazard <- baselineHazard(population$propensityScore) * (1 + (hr - 1)*population$treatment)
  timeToEvent <- round(rexp(n, population$hazard))
  timeToCensor <- round(rexp(n, 0.01))
  population$survivalTime <- timeToEvent
  population$survivalTime[timeToEvent > timeToCensor] <- timeToCensor[timeToEvent > timeToCensor]
  population$y <- 1
  population$y[timeToEvent > timeToCensor] <- 0

  # Adjustment strategy
  if (strategy == "match") {
    population <- matchOnPs(population , maxRatio = 100)
  } else {
    population <- stratifyByPs(population, numberOfStrata = 10)
  }

  # Code from plotKaplanMeier:
  population$stratumSizeT <- 1
  strataSizesT <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 1,], sum)
  strataSizesC <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 0,], sum)
  colnames(strataSizesC)[2] <- "stratumSizeC"
  weights <- merge(strataSizesT, strataSizesC)
  weights$weight <- weights$stratumSizeT / weights$stratumSizeC
  population <- merge(population, weights[, c("stratumId", "weight")])
  population$weight[population$treatment == 1] <- 1
  idx <- population$treatment == 1
  survTarget <- CohortMethod:::adjustedKm(weight = population$weight[idx],
                                          time = population$survivalTime[idx],
                                          y = population$y[idx])
  idx <- population$treatment == 0
  survComparator <- CohortMethod:::adjustedKm(weight = population$weight[idx],
                                              time = population$survivalTime[idx],
                                              y = population$y[idx])

  # Compute coverage:
  computeCoverage <- function(surv, trueHazards) {
    surv$upper <- surv$s^exp(qnorm(1 - 0.025)/log(surv$s)*sqrt(surv$var)/surv$s)
    surv$lower <- surv$s^exp(qnorm(0.025)/log(surv$s)*sqrt(surv$var)/surv$s)

    coverage <- rep(NA, nrow(surv))
    for (i in 1:nrow(surv)) {
      trueS <- (1 - trueHazards) ^ surv$time[i]
      trueS <- mean(trueS)
      coverage[i] <- trueS >= surv$lower[i] & trueS <= surv$upper[i]
    }
    return(mean(coverage))
  }
  cutoff <- quantile(population$survivalTime, 0.9)
  survTarget <- survTarget[survTarget$time <= cutoff, ]
  coverageT <- computeCoverage(survTarget, population$hazard[population$treatment == 1])

  survComparator <- survComparator[survComparator$time <= cutoff, ]
  coverageC <- computeCoverage(survComparator, population$hazard[population$treatment == 1] / hr)
  return(data.frame(coverageT = coverageT, coverageC = coverageC))
}

x <- plyr::llply(1:100, runOneSimulation, strategy = "match", .progress = "text")
x <- do.call("rbind", x)
mean(x$coverageT)
mean(x$coverageC)

x <- plyr::llply(1:100, runOneSimulation, strategy = "stratify", .progress = "text")
x <- do.call("rbind", x)
mean(x$coverageT)
mean(x$coverageC)

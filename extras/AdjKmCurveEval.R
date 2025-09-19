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

logistic <- function(x) {
  1 / (1 + exp(-x))
}

runOneSimulation <- function(x, strategy = "variable ratio matching") {
  propensityScore <- logistic(log(pT) + rnorm(n, sd = 2))
  population <- tibble(rowId = 1:n,
                       treatment = as.integer(runif(n, 0,1) < propensityScore),
                       propensityScore = propensityScore)
  population$hazard <- baselineHazard(population$propensityScore) * (1 + (hr - 1)*population$treatment)
  timeToEvent <- round(rexp(n, population$hazard))
  timeToCensor <- round(rexp(n, 0.01))
  population$survivalTime <- timeToEvent
  population$survivalTime[timeToEvent > timeToCensor] <- timeToCensor[timeToEvent > timeToCensor]
  population$outcomeCount <- 1
  population$outcomeCount[timeToEvent > timeToCensor] <- 0

  # Adjustment strategy
  if (strategy == "variable ratio matching") {
    population <- matchOnPs(population , maxRatio = 100)
  } else if (strategy == "1-on-1 matching") {
    population <- matchOnPs(population , maxRatio = 1)
  } else if (strategy == "stratification") {
    population <- stratifyByPs(population, numberOfStrata = 10)
  } else if (strategy == "no adjustment") {
    # Do nothing
  } else {
    stop("Unknown strategy: ", strategy)
  }

  data <- CohortMethod:::prepareKaplanMeier(population)

  # Compute coverage:
  computeCoverage <- function(surv, trueHazards) {
    coverage <- rep(NA, nrow(surv))
    for (i in 1:nrow(surv)) {
      trueS <- (1 - trueHazards) ^ surv$time[i]
      trueS <- mean(trueS)
      coverage[i] <- trueS >= surv$lower[i] & trueS <= surv$upper[i]
    }
    return(mean(coverage))
  }
  coverageT <- computeCoverage(surv = data[data$treatment == 1, ],
                               trueHazards = population$hazard[population$treatment == 1])
  coverageC <- computeCoverage(surv = data[data$treatment == 0, ],
                               trueHazards = population$hazard[population$treatment == 1] / hr)
  return(data.frame(coverageT = coverageT, coverageC = coverageC))
}

x <- plyr::llply(1:100, runOneSimulation, strategy = "variable ratio matching", .progress = "text")
x <- do.call("rbind", x)
writeLines(sprintf("Coverage target: %0.2f, comparator: %0.2f", mean(x$coverageT), mean(x$coverageC)))
# Coverage target: 0.95, comparator: 0.91

x <- plyr::llply(1:100, runOneSimulation, strategy = "1-on-1 matching", .progress = "text")
x <- do.call("rbind", x)
writeLines(sprintf("Coverage target: %0.2f, comparator: %0.2f", mean(x$coverageT), mean(x$coverageC)))
# Coverage target: 0.95, comparator: 0.95

x <- plyr::llply(1:100, runOneSimulation, strategy = "stratification", .progress = "text")
x <- do.call("rbind", x)
writeLines(sprintf("Coverage target: %0.2f, comparator: %0.2f", mean(x$coverageT), mean(x$coverageC)))
# Coverage target: 0.94, comparator: 0.89

x <- plyr::llply(1:100, runOneSimulation, strategy = "no adjustment", .progress = "text")
x <- do.call("rbind", x)
writeLines(sprintf("Coverage target: %0.2f, comparator: %0.2f", mean(x$coverageT), mean(x$coverageC)))
# Coverage target: 0.94, comparator: 0.01

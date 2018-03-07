library(CohortMethod)
options(fftempdir = "c:/fftemp")

# Generate plots based on vignette data ----------------------------------------------------------
ps <- readRDS('c:/temp/ps.rds')
#ps <- ps[sample.int(nrow(ps), 100000), ]
population <- matchOnPs(ps, maxRatio = 1)
plotKaplanMeier(population, treatmentLabel = "Celecoxib", comparatorLabel = "Diclofenac", fileName = "c:/temp/KM_1_to_1.png")
population <- matchOnPs(ps, maxRatio = 100)
plotKaplanMeier(population, treatmentLabel = "Celecoxib", comparatorLabel = "Diclofenac", fileName = "c:/temp/KM_var_ratio.png")
population <- stratifyByPs(ps, numberOfStrata = 5)
plotKaplanMeier(population,  treatmentLabel = "Celecoxib", comparatorLabel = "Diclofenac", fileName = "c:/temp/KM_5_strata.png")
population <- stratifyByPs(ps, numberOfStrata = 10)
plotKaplanMeier(population,  treatmentLabel = "Celecoxib", comparatorLabel = "Diclofenac", fileName = "c:/temp/KM_10_strata.png")

# Simulation --------------------------------------------------------------------------------------
n <- 10000
nStrata <- 10
hr <- 2

# Introduce confounding by having baseline hazard correlate with proportion in target cohort:
baselineHazards <- seq(0.001, 0.01, length.out = nStrata)
targetProportions <- seq(0.1, 0.9, length.out = nStrata)

targetCounts <- round(targetProportions*n/nStrata)
population <- data.frame(stratumId = rep(1:nStrata, each = n / nStrata),
                         treatment = 0)
for (i in 1:nStrata) {
  start <- 1 + (i-1)*n/nStrata
  population$treatment[start:(start+targetCounts[i])] <- 1
}
hazard <- baselineHazards[population$stratumId] * (1+(hr-1)*population$treatment)
timeToEvent <- round(rexp(n, hazard))
timeToCensor <- round(rexp(n, 0.01))
population$survivalTime <- timeToEvent
population$survivalTime[timeToEvent > timeToCensor] <- timeToCensor[timeToEvent > timeToCensor]
population$y <- 1
population$y[timeToEvent > timeToCensor] <- 0


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
# survTarget$strata <- treatmentLabel
idx <- population$treatment == 0
survComparator <- CohortMethod:::adjustedKm(weight = population$weight[idx],
                             time = population$survivalTime[idx],
                             y = population$y[idx])
# survComparator$strata <- comparatorLabel


# Compute coverage:


computeCoverage <- function(surv, trueHazards) {
  surv$upper <- surv$s^exp(qnorm(1-0.025)/log(surv$s)*sqrt(surv$var)/surv$s)
  surv$lower <- surv$s^exp(qnorm(0.025)/log(surv$s)*sqrt(surv$var)/surv$s)

  coverage <- rep(NA, nrow(surv))
  for (i in 1:nrow(surv)) {
    trueS <- (1-trueHazards) ^ surv$time[i]
    trueS <- mean(trueS)
    coverage[i] <- trueS >= surv$lower[i] & trueS <= surv$upper[i]
  }
  return(mean(coverage))
}
cutoff <- quantile(population$survivalTime, 0.9)
survTarget <- survTarget[survTarget$time <= cutoff, ]
computeCoverage(survTarget, hazard[population$treatment == 1])
survComparator <- survComparator[survComparator$time <= cutoff, ]
computeCoverage(survComparator, hazard[population$treatment == 1] / hr)



library(Cyclops)
library(survival)
library(ipwCoxCSV)
library(EmpiricalCalibration)
library(dplyr)
settings = list(
  n = 1000,
  nSimulations = 1000,
  baselineHazard = 0.001,
  censorHazard = 0.01,
  hr = 4, # True hazard ratio
  z = 1 # Confounding magnitude
)

runSimulation <- function(seed, settings, method) {
  set.seed(seed)
  probTreatment <- runif(settings$n) # The true propensity score
  treatment <- runif(settings$n) < probTreatment
  hazard <- settings$baselineHazard * ifelse(treatment, settings$hr, 1) * (1 + settings$z * probTreatment)
  timeToEvent <- rexp(settings$n, hazard)
  timeToCensor <- rexp(settings$n, settings$censorHazard)
  status <- timeToEvent < timeToCensor
  time <- ifelse(status, timeToEvent, timeToCensor)

  if (method == "unadjusted") {
    cyclopsData <- createCyclopsData(Surv(time, status) ~ treatment, modelType = "cox")
    fit <- fitCyclopsModel(cyclopsData)
    ci <- confint(fit, 1)
    estimate <- data.frame(logRr = coef(fit),
                           seLogRr = (ci[3] - ci[2])/(2 * qnorm(0.975)),
                           ci95Lb = exp(ci[2]),
                           ci95Ub = exp(ci[3]))
    return(estimate)
  } else if (method == "Cyclops") {
    # Stabilized IPTW for ATE:
    iptw <- ifelse(treatment,
                   mean(treatment == 1) / probTreatment,
                   mean(treatment == 0) / (1 - probTreatment))
    cyclopsData <- createCyclopsData(Surv(time, status) ~ treatment, weights = iptw, modelType = "cox")
    fit <- fitCyclopsModel(cyclopsData)
    ci <- confint(fit, 1)
    estimate <- data.frame(logRr = coef(fit),
                           seLogRr = (ci[3] - ci[2])/(2 * qnorm(0.975)),
                           ci95Lb = exp(ci[2]),
                           ci95Ub = exp(ci[3]))
    return(estimate)
  } else if (method == "ipwCoxCSV") {
    # Computes stabilized IPTW for ATE internally:
    fit <- ipwCoxInd(data = data.frame(treatment = treatment,
                                       probTreatment = probTreatment,
                                       status = status,
                                       time = time),
                     indA = "treatment",
                     indX = "probTreatment",
                     indStatus = "status",
                     indTime = "time",
                     ties = "breslow")
    estimate <- data.frame(logRr = fit[2, 1],
                           seLogRr = fit[2, 2],
                           ci95Lb = fit[2, 4],
                           ci95Ub = fit[2, 5])
    return(estimate)
  } else if (method == "bootstrap") {
    iptw <- ifelse(treatment,
                   mean(treatment == 1) / probTreatment,
                   mean(treatment == 0) / (1 - probTreatment))
    data <- data.frame(
      time = time,
      status = status,
      treatment = treatment,
      iptw = iptw
    )
    computeHr <- function(dummy, data, sample) {
      if (sample) {
        indices <- sample.int(n = nrow(data), size = nrow(data), replace = TRUE)
        sampleData <- data[indices, ]
      } else {
        sampleData <- data
      }
      cyclopsData <- createCyclopsData(Surv(time, status) ~ treatment, weights = sampleData$iptw, modelType = "cox", data = sampleData)
      fit <- fitCyclopsModel(cyclopsData)
      value <- coef(fit)
      names(value) <- NULL
      return(value)
    }
    bootstrap <- sapply(seq_len(200), computeHr, data = data, sample = TRUE)
    logRr <- computeHr(NULL, data, FALSE)
    seLogRr <- sqrt(var(bootstrap))
    # Derive CI directly from bootstrap sample:
    # ci <- quantile(bootstrap, c(0.025, 0.975))
    # Use Austin' method: assume normality:
    ci <- logRr + qnorm(c(0.025, 0.975)) * seLogRr
    estimate <- data.frame(logRr = logRr,
                           seLogRr = seLogRr,
                           ci95Lb = exp(ci[1]),
                           ci95Ub = exp(ci[2]))
    return(estimate)
  } else if (method == "package") {
    iptw <- ifelse(treatment,
                   mean(treatment == 1) / probTreatment,
                   mean(treatment == 0) / (1 - probTreatment))

    population <- tibble(
      rowId = seq_len(settings$n),
      personSeqId = seq_len(settings$n),
      timeAtRisk = time,
      survivalTime = time,
      outcomeCount = status,
      treatment = treatment,
      iptw = iptw
    )
    model <- CohortMethod::fitOutcomeModel(
      population = population,
      modelType = "cox",
      inversePtWeighting = TRUE,
      bootstrapCi = TRUE,
      profileBounds = NULL)
    estimate <- model$outcomeModelTreatmentEstimate |>
      transmute(
        logRr,
        seLogRr,
        ci95Lb = exp(logLb95),
        ci95Ub = exp(logUb95))
    return(estimate)
  } else {
    stop("Unknown method ", method)
  }
}

cluster <- ParallelLogger::makeCluster(10)
ParallelLogger::clusterRequire(cluster, "dplyr")
ParallelLogger::clusterRequire(cluster, "Cyclops")
ParallelLogger::clusterRequire(cluster, "survival")
ParallelLogger::clusterRequire(cluster, "ipwCoxCSV")

estimates0 <- ParallelLogger::clusterApply(cluster, 1:settings$nSimulations, runSimulation, settings = settings, method = "unadjusted")
estimates0 <- do.call(rbind, estimates0)
message(sprintf("Coverage = %0.1f%%", 100 * mean(estimates0$ci95Lb < settings$hr & settings$hr < estimates0$ci95Ub)))
# Coverage = 68.1%
# plotCalibrationEffect(estimates0$logRr, estimates0$seLogRr, showExpectedAbsoluteSystematicError = TRUE, showCis = TRUE)
computeExpectedAbsoluteSystematicError(fitNull(estimates0$logRr - log(settings$hr), estimates0$seLogRr))
# [1] 0.2085251

estimates1 <- ParallelLogger::clusterApply(cluster, 1:settings$nSimulations, runSimulation, settings = settings, method = "Cyclops")
estimates1 <- do.call(rbind, estimates1)
message(sprintf("Coverage = %0.1f%%", 100 * mean(estimates1$ci95Lb < settings$hr & settings$hr < estimates1$ci95Ub)))
# Coverage = 79.2%
# plotCalibrationEffect(estimates1$logRr, estimates1$seLogRr, showExpectedAbsoluteSystematicError = TRUE, showCis = TRUE)
computeExpectedAbsoluteSystematicError(fitNull(estimates1$logRr - log(settings$hr), estimates1$seLogRr))
# [1] 0.1540369

estimates2 <- ParallelLogger::clusterApply(cluster, 1:settings$nSimulations, runSimulation, settings = settings, method = "ipwCoxCSV")
estimates2 <- do.call(rbind, estimates2)
message(sprintf("Coverage = %0.1f%%", 100 * mean(estimates2$ci95Lb < settings$hr & settings$hr < estimates2$ci95Ub)))
# Coverage = 94.4%
# plotCalibrationEffect(estimates2$logRr, estimates2$seLogRr, showExpectedAbsoluteSystematicError = TRUE, showCis = TRUE)
computeExpectedAbsoluteSystematicError(fitNull(estimates2$logRr - log(settings$hr), estimates2$seLogRr))
# [1] 0.05088311

estimates3 <- ParallelLogger::clusterApply(cluster, 1:settings$nSimulations, runSimulation, settings = settings, method = "bootstrap")
estimates3 <- do.call(rbind, estimates3)
message(sprintf("Coverage = %0.1f%%", 100 * mean(estimates3$ci95Lb < settings$hr & settings$hr < estimates3$ci95Ub)))
# Coverage = 93.1%
# plotCalibrationEffect(estimates3$logRr, estimates3$seLogRr, showExpectedAbsoluteSystematicError = TRUE, showCis = TRUE)
computeExpectedAbsoluteSystematicError(fitNull(estimates3$logRr - log(settings$hr), estimates3$seLogRr))
# [1] 0.07142416

estimates4 <- ParallelLogger::clusterApply(cluster, 1:settings$nSimulations, runSimulation, settings = settings, method = "package")
estimates4 <- do.call(rbind, estimates4)
message(sprintf("Coverage = %0.1f%%", 100 * mean(estimates4$ci95Lb < settings$hr & settings$hr < estimates4$ci95Ub)))
# Coverage = 93.1%
# plotCalibrationEffect(estimates4$logRr, estimates4$seLogRr, showExpectedAbsoluteSystematicError = TRUE, showCis = TRUE)
computeExpectedAbsoluteSystematicError(fitNull(estimates4$logRr - log(settings$hr), estimates4$seLogRr))
# [1] 0.07142416

ParallelLogger::stopCluster(cluster)

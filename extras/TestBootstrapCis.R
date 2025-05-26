library(Cyclops)
library(survival)
library(ipwCoxCSV)
library(EmpiricalCalibration)
library(dplyr)
n <- 1000
nSimulations <- 10000
baselineHazard <- 0.001
censorHazard <- 0.01
hr <- 1 # True hazard ratio
z <- 0.2 # Confounding magnitude

runSimulation <- function(seed, method) {
  set.seed(seed)
  probTreatment <- runif(n) # The true propensity score
  treatment <- runif(n) < probTreatment
  hazard <- baselineHazard * ifelse(treatment, hr, 1) * (1 + z * probTreatment)
  timeToEvent <- rexp(n, hazard)
  timeToCensor <- rexp(n, censorHazard)
  status <- timeToEvent < timeToCensor
  time <- ifelse(status, timeToEvent, timeToCensor)

  if (method == "Cyclops") {
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
        data <- data[indices, ]
      }
      cyclopsData <- createCyclopsData(Surv(time, status) ~ treatment, weights = iptw, modelType = "cox", data = data)
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
    population <- tibble(
      rowId = seq_len(n),
      personSeqId = seq_len(n),
      timeAtRisk = time,
      survivalTime = time,
      outcomeCount = status,
      treatment = treatment,
      iptw = iptw
    )
    model <- fitOutcomeModel(
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

estimates1 <- plyr::llply(1:nSimulations, runSimulation, method = "Cyclops", .progress = "text")
estimates1 <- do.call(rbind, estimates1)
message(sprintf("Coverage = %0.1f%%", 100 * mean(estimates1$ci95Lb < hr & hr < estimates1$ci95Ub)))
# Coverage = 79.4%
plotCalibrationEffect(estimates1$logRr, estimates1$seLogRr, showExpectedAbsoluteSystematicError = TRUE, showCis = TRUE)
computeExpectedAbsoluteSystematicError(fitNull(estimates1$logRr, estimates1$seLogRr))
# [1] 0.193238

estimates2 <- plyr::llply(1:nSimulations, runSimulation, method = "ipwCoxCSV", .progress = "text")
estimates2 <- do.call(rbind, estimates2)
message(sprintf("Coverage = %0.1f%%", 100 * mean(estimates2$ci95Lb < hr & hr < estimates2$ci95Ub)))
# Coverage = 93.2%
plotCalibrationEffect(estimates2$logRr, estimates2$seLogRr, showExpectedAbsoluteSystematicError = TRUE, showCis = TRUE)
computeExpectedAbsoluteSystematicError(fitNull(estimates2$logRr, estimates2$seLogRr))
# [1] 0.07301792

estimates3 <- plyr::llply(1:nSimulations, runSimulation, method = "bootstrap", .progress = "text")
estimates3 <- do.call(rbind, estimates3)
message(sprintf("Coverage = %0.1f%%", 100 * mean(estimates3$ci95Lb < hr & hr < estimates3$ci95Ub)))
# Coverage = 94.8%
plotCalibrationEffect(estimates3$logRr, estimates3$seLogRr, showExpectedAbsoluteSystematicError = TRUE, showCis = TRUE)
computeExpectedAbsoluteSystematicError(fitNull(estimates3$logRr, estimates3$seLogRr))
# [1] 0.0737213

estimates4 <- plyr::llply(1:nSimulations, runSimulation, method = "package", .progress = "text")
estimates4 <- do.call(rbind, estimates4)
message(sprintf("Coverage = %0.1f%%", 100 * mean(estimates4$ci95Lb < hr & hr < estimates4$ci95Ub)))
# Coverage = 92.9%
plotCalibrationEffect(estimates4$logRr, estimates4$seLogRr, showExpectedAbsoluteSystematicError = TRUE, showCis = TRUE)
computeExpectedAbsoluteSystematicError(fitNull(estimates4$logRr, estimates4$seLogRr))
# [1] 0.07170155


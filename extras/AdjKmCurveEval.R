library(CohortMethod)
options(fftempdir = "c:/fftemp")

ps <- readRDS('c:/temp/ps.rds')
#ps <- ps[sample.int(nrow(ps), 100000), ]
population <- matchOnPs(ps, maxRatio = 1)

system.time(
  plotKaplanMeier(population, treatmentLabel = "Celecoxib", comparatorLabel = "Diclofenac", fileName = "c:/temp/KM_1_to_1.png")
)

population <- matchOnPs(ps, maxRatio = 100)

system.time(
plotKaplanMeier(population, treatmentLabel = "Celecoxib", comparatorLabel = "Diclofenac", fileName = "c:/temp/KM_var_ratio.png")
)


population <- stratifyByPs(ps, numberOfStrata = 5)
plotKaplanMeier(population,  treatmentLabel = "Celecoxib", comparatorLabel = "Diclofenac", fileName = "c:/temp/KM_5_strata.png")


population <- stratifyByPs(ps, numberOfStrata = 10)
plotKaplanMeier(population,  treatmentLabel = "Celecoxib", comparatorLabel = "Diclofenac", fileName = "c:/temp/KM_10_strata.png")


om <- fitOutcomeModel(population, modelType = "cox", stratified = TRUE, useCovariates = FALSE)
om



#
population$stratumId <- NULL
plotKaplanMeier(population)

population$y <- 0
population$y[population$outcomeCount != 0] <- 1
nBootstrap <- 100
idx <- population$treatment == 0
survComparator <- CohortMethod:::adjustedKm(stratumId = population$stratumId[idx],
                                        time = population$survivalTime[idx],
                                        y = population$y[idx],
                                        nBootstrap = nBootstrap)

survComparator$upper <- survComparator$s^exp(qnorm(1-0.025)/log(survComparator$s)*sqrt(survComparator$var)/survComparator$s)
survComparator$lower <- survComparator$s^exp(qnorm(0.025)/log(survComparator$s)*sqrt(survComparator$var)/survComparator$s)
survComparator$lower2 <- survComparator$s^exp(qnorm(0.025)/log(survComparator$s)*sqrt(survComparator$var2)/survComparator$s)
saveRDS(survComparator, "c:/temp/surv.rds")
old <- survComparator

survComparator$var[1:100]
survComparator$var2[1:100]
survComparator$lower[1:100]
survComparator$lower2[1:100]


library(survey)

population$y <- 0
population$y[population$outcomeCount != 0] <- 1
design<-svydesign(id=~rowId, strata=~treatment, data=pop, weights = pop$w)
sv <- svykm(survival::Surv(survivalTime, y) ~ treatment, design = design, se=TRUE)
plot(sv[[2]],col="purple")
confint(sv[[2]], parm=365*(1:5))
quantile(sv[[1]], ci=TRUE)
sv[[2]]$time[1:100]
sv[[2]]$surv[1:100]
survComparator$s[1:100]


library(survival)
addWeights <- function(arm) {
  strataSizes <- aggregate(rowId ~ stratumId, arm, length)
  colnames(strataSizes)[2] <- "stratumSize"
  data <- merge(arm, strataSizes)
  data$w <- 1/data$stratumSize
  return(data)
}
population$y <- 0
population$y[population$outcomeCount != 0] <- 1
popTarget <- addWeights(population[population$treatment == 1, ])
popComparator <- addWeights(population[population$treatment == 0, ])
pop <- rbind(popTarget, popComparator)
sv <- survfit.formula(survival::Surv(survivalTime, y) ~ treatment, pop, conf.int = TRUE, weights = w, error = "tsiatis")

data <- data.frame(time = sv$time,
                   n.censor = sv$n.censor,
                   s = sv$surv,
                   strata = summary(sv, censored = T)$strata,
                   upper = sv$upper,
                   lower = sv$lower)
data$s[data$strata =="treatment=0"][1:100]
survComparator$s[1:100]

data$upper[data$strata =="treatment=0"][1:100]
survComparator$upper[1:100]


# MacKenzie IPTW -----------
obj.glm <- glm((PROC1) ~ AGE + MALE, familybinomial)
PS <- obj.glm$fit #PSPropensity Score (for CABG)
wt.cabg <- ifelse(PROC1, 1/PS, 0)
wt.ptca <- ifelse(PROC2, 1/(1-PS), 0)
IW.cabg <-survfit(Surv(years,MRS) ~ 1, weightwt.cabg)
IW.ptca <-survfit(Surv(years,MRS) ~ 1, weightwt.ptca)
plot(c(0,max(years)), c(0,1), type = "n")
lines(IW.cabg$time, IW.cabg$surv, lty1, type = "s")
lines(IW.ptca$time, IW.ptca$surv, lty2, type = "s")

# IPWsurvival package ---------------------------------
Pr0 <- glm(ecd ~ 1, family = binomial(link="logit"), data=DIVAT)$fitted.values[1]
Pr1 <- glm(ecd ~ age + hla + retransplant, data=DIVAT,
           family=binomial(link = "logit"))$fitted.values
W <- (DIVAT$ecd==1) * (1/Pr1) + (DIVAT$ecd==0) * (1)/(1-Pr1)
res.akm <- adjusted.KM(times=DIVAT$times, failures=DIVAT$failures,
                       variable=DIVAT$ecd, weights=W)
lines(res.akm$times[res.akm$variable==1], res.akm$survival[res.akm$variable==1],
      type="s",col=2,lwd=2)
lines(res.akm$times[res.akm$variable==0], res.akm$survival[res.akm$variable==0],
      type="s",col=1,lwd=2)

# Xie et al -----------------------------------------------------
n <- nrow(survComparator)
survComparator$delta[2:n] <- survComparator$s[2:n] / survComparator$s[1:(n-1)]
survComparator$delta[1] <- survComparator$s[1]

tail(pop)

survComparator$var[1:100]

aggregate(rowId ~ stratumId, population[idx, ], length)

x1 <- survComparator
x3 <- survComparator
x100 <- survComparator
x1$s[1:100]
x3$s[1:100]
x100$s[1:100]

x1$var[1:100]
x3$var[1:100]
x100$var[1:100]

survTarget$strata <- treatmentLabel

idx <- population$treatment == 0
survComparator <- CohortMethod:::adjustedKm(stratumId = population$stratumId[idx],
                                            time = population$survivalTime[idx],
                                            y = population$y[idx],
                                            nBootstrap = nBootstrap)
survComparator$strata <- comparatorLabel
data <- rbind(survTarget, survComparator)
data$upper <- data$s^exp(qnorm(1-0.025)/log(data$s)*sqrt(data$var)/data$s)
data$lower <- data$s^exp(qnorm(0.025)/log(data$s)*sqrt(data$var)/data$s)

# Mean S = 0.887269, sumV = 0.00433726, var = 0
(1/(10-1)) * 0.00433726
x <- c(0.91947, 0.896167, 0.916631, 0.93138, 0.903896, 0.881231, 0.858551, 0.912155, 0.901072, 0.882927)
meanX <- mean(x)
(1/(10-1)) *sum((x-meanX)^2)

surv$strata <- "Target"
ggplot2::ggplot(surv, ggplot2::aes(x = time,
                                   y = s,
                                   color = strata,
                                   fill = strata)) +
  ggplot2::geom_step(size = 1)


sv <- survival::survfit(survival::Surv(survivalTime, y) ~ treatment, population, conf.int = TRUE)
idx <- (summary(sv, censored = T)$strata == "treatment=1")

data <- data.frame(time = sv$time[idx],
                   n.risk = sv$n.risk[idx],
                   n.event = sv$n.event[idx],
                   n.censor = sv$n.censor[idx],
                   surv = sv$surv[idx],
                   strata = summary(sv, censored = T)$strata[idx],
                   upper = sv$upper[idx],
                   lower = sv$lower[idx])

ggplot2::ggplot(data, ggplot2::aes(x = time,
                                   y = s,
                                   color = strata,
                                   fill = strata,
                                   ymin = lower,
                                   ymax = upper)) +
  ggplot2::geom_ribbon(color = rgb(0, 0, 0, alpha = 0), alpha=0.3) +
  ggplot2::geom_step(size = 1)


fitOutcomeModel(population, modelType = "cox", stratified = TRUE, useCovariates = FALSE)

adjustedKm <- function(population) {
  adjustedKmArm <- function(arm) {
    strataSizes <- aggregate(rowId ~ stratumId, arm, length)
    colnames(strataSizes)[2] <- "stratumSize"
    data <- merge(arm, strataSizes)
    data$w <- 1/data$stratumSize

    arm$y <- as.integer(arm$y)
    arm$survivalTime <- as.integer(arm$survivalTime)
    arm$stratumId <- as.integer(arm$stratumId)
    data <- split(arm[, c("survivalTime", "y")], arm$stratumId)
    names(data)[2]

    fun2 <- function(time, subset) {
      events <- sum(subset$y[subset$survivalTime == time])
      atRisk <- sum(subset$survivalTime >= time)
      weight <- 1/nrow(subset)
      return(c(events*weight, atRisk*weight))
    }

    fun <- function(time) {
      system.time(
      x <- sapply(data, fun2, time = time)
      )
      return(1 - (sum(x[1, ]) / sum(x[2, ])))
    }
    times <- unique(arm$survivalTime)
    times <- times[order(times)]
    rates <- sapply(times, fun)


    fun2 <- function(subset, times) {
      events <- rep(0, length(times))
      agg <- aggregate(y ~ survivalTime, subset, sum)
      events[match(agg$survivalTime, times)] <- agg$y
      atRisk <- rep(
      atRisk <- 1#sum(subset$survivalTime >= time)
      weight <- 1/nrow(subset)
      return(c(events*weight, atRisk*weight))
    }


    strataSizes <- aggregate(rowId ~ stratumId, arm, length)
    colnames(strataSizes)[2] <- "stratumSize"

    events <- aggregate(y ~ stratumId + survivalTime, arm, sum)
    arm$censored <- 1
    censored <- aggregate(censored ~ stratumId + survivalTime, arm, sum)
    censored <- censored[order(-censored$survivalTime), ]
    censored$atRisk <- ave(censored$censored, censored$stratumId, FUN=cumsum)
    data <- merge(events, censored)
    data <- merge(data, strataSizes)
    data$w <- 1/data$stratumSize
    fun <- function(subset) {
      1 - (sum(subset$y * subset$w) / sum(subset$atRisk * subset$w))
    }
    result <- plyr::ddply(data, "survivalTime", fun)
    result <- result[order(result$survivalTime), ]
    result$surv <- cumprod(result$V1)

    fullGrid <- expand.grid(unique(data$stratumId), unique(data$survivalTime))
  }
  target <- population[population$treatment == 1, ]
}


# Generalization to all stratified data ----------------------------------------------------------
population$stratumSizeT <- 1
strataSizesT <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 1,], sum)
strataSizesC <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 0,], sum)
colnames(strataSizesC)[2] <- "stratumSizeC"
weights <- merge(strataSizesT, strataSizesC)
weights$stratumSize <- weights$stratumSizeC + weights$stratumSizeC
mOverN <- sum(population$treatment == 1) / nrow(population)
weights$weight <- mOverN / (weights$stratumSizeT / weights$stratumSize)



weights$weight <- weights$stratumSizeT / weights$stratumSizeC
population <- merge(population, weights[, c("stratumId", "weight")])
population$weight[population$treatment == 1] <- 1

survComparator <- CohortMethod:::adjustedKm(weight = population$weight[idx],
                                            time = population$survivalTime[idx],
                                            y = population$y[idx])
survComparator$upper <- survComparator$s^exp(qnorm(1-0.025)/log(survComparator$s)*sqrt(survComparator$var)/survComparator$s)
survComparator$lower <- survComparator$s^exp(qnorm(0.025)/log(survComparator$s)*sqrt(survComparator$var)/survComparator$s)

survComparator$s[1:100]
data$surv[1:100]
old$s[1:100]
survComparator$lower[1:100]
data$lower[1:100]
old$lower[1:100]





population$size <- 1
sizes <- aggregate(size ~ stratumId + treatment, population, sum)

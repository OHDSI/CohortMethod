# Some code to evaluate if the Cox model supports probabilistic outcomes
library(Cyclops)
library(survival)

# Settings
n <- 10000
pA <- 0.5
yIntercept <- log(0.01)
yA <- log(2)
hY2 <- 0.02
hCensor <- 0.02

# Simulation
a <- runif(n) < pA
hY <- exp(yIntercept + a * yA)
tY <- rexp(n, hY)
tY2 <- rexp(n, hY2)
pY <- runif(n)
tY <- ifelse(runif(n) < pY, tY, tY2)
tCensor <- rexp(n, hCensor)

t <- pmin(tY, tCensor)
y <- tY <= tCensor
cd <- createCyclopsData(Surv(t, y) ~ a, modelType = "cox")
fit <- fitCyclopsModel(cd, weights = pY)
exp(coef(fit))

# Some R code to test whether sampling doesn't distort balance computation

library(CohortMethod)
library(dplyr)
options(andromedaTempFolder = "s:/andromedaTemp")

cohortMethodData <- loadCohortMethodData('s:/temp/cohortMethodVignette/cohortMethodData.zip')
ps <- readRDS('s:/temp/cohortMethodVignette/ps.rds')

# Matched ---------------------------------------
matchedPop <- matchOnPs(ps, caliper = 0.25, caliperScale = "standardized", maxRatio = 100)

system.time(
  balance <- computeCovariateBalance(matchedPop, cohortMethodData)
)
# Computing covariate balance took 2.52 mins
# user  system elapsed
# 129.90   11.81  151.20

system.time(
  balance2 <- computeCovariateBalance(matchedPop, cohortMethodData, maxCohortSize = 10000)
)
# user  system elapsed
# 49.89    4.90   56.78

joined <- balance |>
  select("covariateId", old = "afterMatchingStdDiff") |>
  inner_join(balance2 |>
               select("covariateId", new = "afterMatchingStdDiff"))
plot(joined$old, joined$new)

min(balance$afterMatchingStdDiff, na.rm = TRUE)
# [1] -0.03841681
min(balance2$afterMatchingStdDiff, na.rm = TRUE)
# [1] -0.05833007
max(balance$afterMatchingStdDiff, na.rm = TRUE)
# [1] 0.03778678
max(balance2$afterMatchingStdDiff, na.rm = TRUE)
# [1] 0.05928888

# Stratified ---------------------------------------
matchedPop <- stratifyByPs(ps, numberOfStrata = 5)

system.time(
  balance <- computeCovariateBalance(matchedPop, cohortMethodData)
)
# Computing covariate balance took 3.17 mins
#    user  system elapsed
 # 157.82   23.44  190.06

system.time(
  balance2 <- computeCovariateBalance(matchedPop, cohortMethodData, maxCohortSize = 10000)
)
# Computing covariate balance took 59.8 secs
#    user  system elapsed
#   51.09    4.51   59.84

joined <- balance |>
  select(covariateId, old = afterMatchingStdDiff) |>
  inner_join(balance2 |>
               select("covariateId", new = "afterMatchingStdDiff"))
plot(joined$old, joined$new)

min(balance$afterMatchingStdDiff, na.rm = TRUE)
# [1] -0.07032446
min(balance2$afterMatchingStdDiff, na.rm = TRUE)
# [1] -0.06093756
max(balance$afterMatchingStdDiff, na.rm = TRUE)
# [1] 0.09543712
max(balance2$afterMatchingStdDiff, na.rm = TRUE)
# [1] 0.09344089

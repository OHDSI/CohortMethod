# Some code for testing different approaches to measuring covariate balance

library(CohortMethod)
library(RItools)
options(fftempdir = "s:/fftemp")

outputFolder = "s:/temp/cohortMethodVignette2"

omr <- readRDS(file.path(outputFolder, "outcomeModelReference.rds"))

cmData <- loadCohortMethodData(file.path(outputFolder, omr$cohortMethodDataFolder[1]))

strataPop <- readRDS(file.path(outputFolder, omr$strataFile[omr$outcomeId == 192671 & omr$analysisId == 3]))

# sampleSize <- 100

computeMetrics <- function(sampleSize) {
  writeLines(paste("Sample size =", sampleSize))
  population <- strataPop[sample.int(nrow(strataPop), sampleSize), ]
  covariates <- cmData$covariates[ffbase::`%in%`(cmData$covariates$rowId, population$rowId), ]
  covariates <- FeatureExtraction::tidyCovariateData(covariates = covariates,
                                                     populationSize = nrow(population),
                                                     minFraction = 0.01,
                                                     normalize = FALSE,
                                                     removeRedundancy = FALSE)$covariates

  dummyCmData <- cmData
  dummyCmData$cohorts <- population
  dummyCmData$covariates <- covariates

  covariates <- ff::as.ram(covariates)
  covariateIds <- unique(covariates$covariateId)
  rowIds <- unique(covariates$rowId)
  ncovars <- length(covariateIds)
  nrows <- length(rowIds)
  writeLines(paste("Number of covariates =", ncovars))

  covariates$covariateSeqIds <- match(covariates$covariateId, covariateIds)
  covariates$rowSeqIds <- match(covariates$rowId, rowIds)
  population$rowSeqIds <- match(population$rowId, rowIds)

  m <- matrix(0, nrows, ncovars)
  for (i in 1:nrow(covariates)) {
    m[covariates$rowSeqIds[i], covariates$covariateSeqIds[i]] <- covariates$covariateValue[i]
  }
  data <- as.data.frame(m)

  data$rowSeqIds <- 1:nrows
  data <- merge(data, population[, c("rowSeqIds", "treatment", "stratumId")])
  # formula <- as.formula(paste(c("treatment ~ strata(stratumId)", paste("V", 1:ncovars, sep = "")), collapse = " + "))
  # x <- xBalance(formula,	data = data,	report	= c("chisquare.test"))

  fmla <- formula(paste("treatment ~", paste(paste("V", 1:ncovars, sep = ""), collapse = " + ")))
  strataColumn <- "stratumId"
  x <- newXbalance(fmla,	strataColumn, data = data, report	= c("chisquare.test"))


  # x$overall

  bal <- computeCovariateBalance(population, dummyCmData)

  # max(abs(bal$beforeMatchingStdDiff), na.rm = TRUE)
  # max(abs(bal$afterMatchingStdDiff), na.rm = TRUE)
  # sum(abs(bal$beforeMatchingStdDiff) > 0.1, na.rm = TRUE)
  # sum(abs(bal$afterMatchingStdDiff) > 0.1, na.rm = TRUE)
  # mean(population$treatment)

  rndPop <- population
  rndPop$treatment <- runif(nrow(population)) < mean(population$treatment)
  dataRnd <- data
  dataRnd$treatment <- NULL
  dataRnd <- merge(dataRnd, rndPop[, c("rowSeqIds", "treatment")])

  balRnd <- computeCovariateBalance(rndPop, dummyCmData)
  xRnd <- newXbalance(fmla,	strataColumn, data = dataRnd, report	= c("chisquare.test"))


  result <- dplyr::tibble(sampleSize = sampleSize,
                          type = c("unadjusted", "stratified", "randomized"),
                          p = c(x$overall$p.value, xRnd$overall$p.value[2]),
                          maxAbsStdDiff = c(max(abs(bal$beforeMatchingStdDiff), na.rm = TRUE),
                                            max(abs(bal$afterMatchingStdDiff), na.rm = TRUE),
                                            max(abs(balRnd$afterMatchingStdDiff), na.rm = TRUE)),
                          unbalCovs = c(sum(abs(bal$beforeMatchingStdDiff) > 0.1, na.rm = TRUE),
                                        sum(abs(bal$afterMatchingStdDiff) > 0.1, na.rm = TRUE),
                                        sum(abs(balRnd$afterMatchingStdDiff) > 0.1, na.rm = TRUE)))
  print(result)


  return(result)
}
sampleSizes <- c(100, 200, 400, 800, 1600, 3200, 6400)
results <- lapply(sampleSizes, computeMetrics)
results <- do.call(rbind, results)
results

saveRDS(results, "s:/temp/covBalanceTestResults.rds")

library(ggplot2)
ggplot(results, aes(x = sampleSize, y = p, group = type, color = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous("Sample size (target + comparator)") +
  scale_y_log10("Chi2 p for balance")
ggsave("s:/temp/bal1.png")



ggplot(results, aes(x = sampleSize, y = unbalCovs, group = type, color = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous("Sample size (target + comparator)") +
  scale_y_continuous("Number of covariates with std. diff > 0.1")

ggsave("s:/temp/bal2.png")


x <- formula


t <- terms(x, specials = "strata", data = data)
strata <- rownames(attr(t, "factors"))[attr(t, "specials")$strata]
if (length(strata) > 0) {
  x <- update(terms(x, data = data), as.formula(paste("~ . - ",
                                                      paste(strata, collapse = "-"))))
  return(list(newx = x, strata = gsub("\\)", "",
                                      gsub("strata\\(", "", strata))))
}

newXbalance <- function(fmla, strataColumn = NULL , data, report = c("std.diffs",
                                                                     "z.scores", "adj.means", "adj.mean.diffs",
                                                                     "adj.mean.diffs.null.sd", "chisquare.test", "p.values",
                                                                     "all")[1:2], stratum.weights = RItools:::harmonic, na.rm = FALSE,
                        covariate.scaling = NULL, normalize.weights = TRUE, impfn = median,
                        post.alignment.transform = NULL) {
  if (!is.null(strataColumn)) {
    strata <- list()
    for (i in paste("~", strataColumn)) {
      strata <- c(strata, list(formula(i)))
    }
    names(strata) <- strataColumn
    strata <- c(list(Unadj = NULL), strata)
  }
  valid.for.report <- c("adj.means", "adj.mean.diffs",
                        "adj.mean.diffs.null.sd", "chisquare.test",
                        "std.diffs", "z.scores", "p.values",
                        "all")
  report.good <- charmatch(report, valid.for.report, -1)
  if (any(report.good == -1)) {
    stop(paste("Invalid option(s) for report:", paste(report[report.good ==
                                                               -1], collapse = ", ")))
  }
  if (any(report.good == 0)) {
    stop(paste("Option(s) for report match multiple possible values:",
               paste(report[report.good == 0], collapse = ", ")))
  }
  report <- valid.for.report[report.good]
  if (is.null(strata))
    warning("Passing NULL as a 'strata=' argument is depracated;\n for balance w/o stratification pass 'list(nostrat=NULL)' instead.\n (Or did you mean to pass a non-NULL 'strata=' argument? Then check for typos.)")
  if (is.list(strata) && !is.data.frame(strata) && !all(sapply(strata,
                                                               function(x) (is.null(x) | inherits(x, "formula")))))
    stop("For balance against multiple alternative stratifications,\n please make 'strata' either a data frame or a list containing formulas or NULL entries.")
  if ("all" %in% report)
    report <- c("adj.means", "adj.mean.diffs",
                "adj.mean.diffs.null.sd", "chisquare.test",
                "std.diffs", "z.scores", "p.values")
  if (na.rm == TRUE) {
    tfmla <- terms.formula(fmla, data = data, keep.order = TRUE)
  } else {
    data <- RItools:::naImpute(fmla, data, impfn)
    tfmla <- attr(data, "terms")
  }
  if (!attr(tfmla, "response") > 0)
    stop("fmla must specify a treatment group variable")
  zz <- eval(tfmla[[2]], data, parent.frame())
  zzname <- deparse(tfmla[[2]])
  if (!is.numeric(zz) & !is.logical(zz))
    stop("LHS of fmla should be logical or numeric")
  if (any(is.na(zz)))
    stop("NAs on LHS of fmla not allowed.")
  mm1 <- RItools:::xBalance.makeMM(tfmla, data)
  if (is.null(strata))
    ss.df <- data.frame(unstrat = factor(numeric(length(zz))))
  if (is.factor(strata) & length(strata) != length(zz))
    stop("length of strata doesn't match dim of data")
  if (is.factor(strata))
    ss.df <- data.frame(strat = factor(strata))
  if (is.data.frame(strata))
    ss.df <- as.data.frame(lapply(strata, factor))
  if (is.list(strata) & !is.data.frame(strata)) {
    pfr <- parent.frame()
    ss.df <- lapply(strata, function(fmla) {
      if (is.null(fmla))
        factor(numeric(length(zz)))
      else {
        ss <- eval(attr(terms(fmla), "variables"),
                   data, pfr)
        if (length(ss) - 1)
          interaction(ss, drop = TRUE)
        else factor(ss[[1]])
      }
    })
    ss.df <- as.data.frame(ss.df)
  }
  if (any(ss.rm <- !sapply(ss.df, nlevels))) {
    if (length(ss.df) == 1)
      stop("'strata=' variable contains no strata.  Perhaps it evaluates to NAs?")
    if (all(ss.rm))
      stop("'strata=' variables contain no strata.  Perhaps they all evaluate to NAs?")
    ss.rm.nms <- if (is.null(names(ss.df)))
      which(ss.rm)
    else names(ss.df)[ss.rm]
    ss.rm.nms <- paste(ss.rm.nms, collapse = " ,")
    warning(paste("Removing the following strata entries, which contained no strata.\n(Perhaps they evaluate to NAs?)\n",
                  ss.rm.nms))
    ss.df <- ss.df[!ss.rm]
  }
  gs.df <- RItools:::xBalance.find.goodstrats(ss.df, zz, mm1)
  swt.ls <- RItools:::xBalance.make.stratwts(stratum.weights, ss.df,
                                             gs.df, zz, data, normalize.weights)
  s.p <- if (is.null(covariate.scaling)) {
    RItools:::xBalance.makepooledsd(zz, mm1, dim(mm1)[1])
  }
  else 1
  RES <- lapply(names(ss.df), function(nm) {
    RItools:::xBalanceEngine(factor(ss.df[gs.df[[nm]], nm]), zz[gs.df[[nm]]],
                             mm1[gs.df[[nm]], , drop = FALSE], report, swt.ls[[nm]],
                             s.p, normalize.weights, zzname, post.alignment.transform)
  })
  names(RES) <- names(ss.df)
  ans <- list()
  ans$results <- array(dim = c(vars = nrow(RES[[1]][["dfr"]]),
                               stat = ncol(RES[[1]][["dfr"]]), strata = length(RES)),
                       dimnames = list(vars = rownames(RES[[1]][["dfr"]]),
                                       stat = colnames(RES[[1]][["dfr"]]), strata = names(RES)))
  attr(ans$results, "originals") <- attr(mm1, "originals")
  for (i in names(RES)) {
    ans$results[, , i] <- as.matrix(RES[[i]][["dfr"]])
  }
  attr(ans, "fmla") <- formula(tfmla)
  if ("chisquare.test" %in% report) {
    ans$overall <- data.frame(chisquare = numeric(length(RES)),
                              df = numeric(length(RES)), p.value = numeric(length(RES)),
                              row.names = names(RES))
    for (nn in names(RES)) {
      ans$overall[nn, "chisquare"] <- RES[[nn]]$chisq["chisquare"]
      ans$overall[nn, "df"] <- RES[[nn]]$chisq["df"]
      ans$overall[nn, "p.value"] <- pchisq(RES[[nn]]$chisq["chisquare"],
                                           df = RES[[nn]]$chisq["df"], lower.tail = FALSE)
    }
    attr(ans$overall, "tcov") <- lapply(RES, function(r) {
      r$tcov
    })
  }
  class(ans) <- c("xbal", "list")
  ans
}

# Code for testing generalizability metrics ------------------------------------
# saveRDS(population, "d:/temp/studyPop.rds")
# saveCohortMethodData(cohortMethodData, "d:/temp/cmData.zip")


population <- readRDS("d:/temp/studyPop.rds")
cohortMethodData <- loadCohortMethodData("d:/temp/cmData.zip")
cohorts <- cohortMethodData$cohorts |>
  collect()

bal <- computeCovariateBalance(population, cohortMethodData) |>
  arrange(covariateId)


tPlusCBefore <- cohortMethodData$cohorts |>
  collect() |>
  select("rowId") |>
  mutate(treatment = 1)
tPlusCAfter <- population |>
  # mutate(stratumId = stratumId + treatment * (1 + max(population$stratumId))) |>
  select("rowId", "stratumId") |>
  mutate(treatment = 0)
adjustedCohorts <- bind_rows(tPlusCBefore, tPlusCAfter)
# cohortMethodData$adjustedCohorts <- adjustedCohorts
# adjustedCohorts <- cohortMethodData$adjustedCohorts
dummyBal <- CohortMethod:::computeMeansPerGroup(cohorts = adjustedCohorts, cohortMethodData, NULL) |>
  arrange(covariateId)

# Compute mean before the hard way:
cohortMethodData$cohorts |>
  left_join(cohortMethodData$covariates |>
              filter(covariateId == 1007),
            by = join_by("rowId")) |>
  mutate(covariateValue = if_else(is.na(covariateValue), 0, covariateValue)) |>
  summarise(mean(covariateValue),
            sd(covariateValue))
# Using the dummy cov balance:
dummyBal |>
  select(meanTarget, sdTarget) |>
  head(10)
# Compute mean in before using computeCovariateBalance output:
# Using insight that "The exact pooled variance is the mean of the variances
# plus the variance of the means of the component data sets." from
# https://arxiv.org/ftp/arxiv/papers/1007/1007.1012.pdf
bal |>
  mutate(meanBefore = beforeMatchingMeanTarget * mean(cohorts$treatment) + beforeMatchingMeanComparator * mean(!cohorts$treatment)) |>
  mutate(beforeVarTarget = beforeMatchingSdTarget^2,
         beforeVarComparator = beforeMatchingSdComparator^2) |>
  mutate(meanVar = beforeVarTarget * mean(cohorts$treatment) + beforeVarComparator * mean(!cohorts$treatment),
         varOfMeans = (beforeMatchingMeanTarget-meanBefore)^2 * mean(cohorts$treatment) + (beforeMatchingMeanComparator-meanBefore)^2 * mean(!cohorts$treatment)) |>
  mutate(sdBefore = sqrt(meanVar + varOfMeans)) |>
  select(meanBefore, sdBefore) |>
  head(10)
bal  |>
  select("beforeMatchingMean", "beforeMatchingSd") |>
  head(10)

# Same for after matching:
dummyBal |>
  select(meanComparator, sdComparator) |>
  head(10)
bal |>
  select("afterMatchingMean", "afterMatchingSd") |>
  head(10)

# Implementing George's covariate balance ----------------------------------------------------------
library(dplyr)

# Simulation under the null, binary, no stratification
threshold <- 0 # Normally 0.1, but then it is hard to figure what correct type 1 error is

simulateOne <- function(seed) {
  set.seed(seed)
  n1 <- sample.int(90, 1) + 10
  n0 <- sample.int(90, 1) + 10
  proportion <- runif(1, 0.1, 0.9)
  s1 <- rbinom(1, n1, proportion)
  s0 <- rbinom(1, n0, proportion)

  # George's algorithm:
  p1 <- s1/n1
  p0 <- s0/n0
  var1 <- sqrt(p1 * (1-p1))
  var0 <- sqrt(p0 * (1-p0))
  sd <- sqrt((var1^2 + var0^2) / 2)
  sdm <- (p1 - p0) / sd
  varSdm <- (n1 + n0) / (n1*n0) + (sdm^2) / (2*(n1 + n0 - 2))
  t <- (abs(sdm) - threshold)/sqrt(varSdm)
  p <- (1 - pnorm(t))*2
  return(p)
}
ps <- sapply(1:10000, simulateOne)
mean(ps<0.05, na.rm = TRUE)
# [1] 0.05290529 # Slightly higher than nominal, likely because of discrete counts


# Simulation under the null, continuous, stratification
threshold <- 0 # Normally 0.1, but then it is hard to figure what correct type 1 error is

simulateOne <- function(seed) {
  set.seed(seed)
  nStrata <- 10
  n1 <- sample.int(9, nStrata, replace = TRUE) + 1
  n0 <- sample.int(9, nStrata, replace = TRUE) + 1
  ratio <- n1/n0
  proportion <- 1/(1+exp(1-ratio)) # Make proportion function of ratio to make problem non-trivial

  # Create population data:
  stratumId1 <- do.call(c, lapply(seq_len(nStrata), function(x) rep(x, n1[x])))
  covariateValue1 <- rbinom(sum(n1), 1, proportion[stratumId1])
  w1 <- 1/n1
  w1 <- w1[stratumId1]
  w1 <- w1 / sum(w1) # Normalization
  sumW1 <- 1 # After normalization

  stratumId0 <- do.call(c, lapply(seq_len(nStrata), function(x) rep(x, n0[x])))
  covariateValue0 <- rbinom(sum(n0), 1, proportion[stratumId0])
  w0 <- 1/n0
  w0 <- w0[stratumId0]
  w0 <- w0 / sum(w0) # Normalization
  sumW0 <- 1 # After normalization

  # Compute SDM using CohortMethod approach:
  mean1 <- sum(w1 * covariateValue1)
  sumSqr1 <- sum(w1 * covariateValue1^2)
  sumWSqr1 <- sum(w1 ^ 2)
  sd1 <- sqrt(abs(sumSqr1 - mean1^2) * sumW1 / (sumW1^2 - sumWSqr1))

  mean0 <- sum(w0 * covariateValue0)
  sumSqr0 <- sum(w0 * covariateValue0^2)
  sumWSqr0 <- sum(w0 ^ 2)
  sd0 <- sqrt(abs(sumSqr0 - mean0^2) * sumW0 / (sumW0^2 - sumWSqr0))

  sd <- sqrt((sd1^2 + sd0^2) / 2)
  sdm <- (mean1 - mean0) / sd

  # Computing the variance of the SDM, taking stratification into account:
  data <- tibble(
    stratumId = c(stratumId1, stratumId0),
    covariateValue = c(covariateValue1, covariateValue0),
    treatment = c(rep(1, length(stratumId1)), rep(0, length(stratumId0)))
  )
  strataSizes <- data |>
    group_by(.data$stratumId) |>
    summarise(size = n())
  numerator <- data |>
    group_by(.data$treatment, .data$stratumId) |>
    summarise(s = if_else(n() == 1, 0, var(.data$covariateValue) / n()), .groups = "drop") |>
    inner_join(strataSizes, by = join_by("stratumId")) |>
    mutate(s = s * ((size / nrow(data)) ^ 2)) |>
    summarise(sum(s)) |>
    pull()
  varSdm <- numerator / sd^2

  t <- (abs(sdm) - threshold)/sqrt(varSdm)
  p <- (1 - pnorm(t))*2
  return(p)
}
ps <- sapply(1:1000, simulateOne)
mean(ps<0.05, na.rm = TRUE)
# [1] 0.078

# Simulation under the null, continuous, stratification, sparse covariates
threshold <- 0

simulateOne <- function(seed) {
  set.seed(seed)
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
  covariates <- data |>
    filter(covariateValue != 0) |>
    select("rowId", "covariateId", "covariateValue")
  cohorts <- data |>
    select("rowId", "stratumId", "treatment")

  # Compute SDM using CohortMethod approach:
  stratumSize <- cohorts |>
    group_by(.data$stratumId, .data$treatment) |>
    count() |>
    ungroup() |>
    collect()
  sumW <- 1
  w <- stratumSize |>
    mutate(weight = 1 / .data$n) |>
    inner_join(cohorts, by = c("stratumId", "treatment"), copy = TRUE) |>
    select("rowId", "treatment", "weight")
  wSum <- w |>
    group_by(.data$treatment) |>
    summarize(wSum = sum(.data$weight, na.rm = TRUE)) |>
    ungroup()
  w <- w |>
    inner_join(wSum, by = "treatment") |>
    mutate(weight = .data$weight / .data$wSum) |>
    select("rowId", "treatment", "weight")
  sumWSqr <- w |>
    group_by(.data$treatment) |>
    summarise(sumWSqr = sum(.data$weight^2, na.rm = TRUE))
  result <- covariates |>
    inner_join(w, by = c("rowId")) |>
    group_by(.data$covariateId, .data$treatment) |>
    summarise(
      sum = sum(as.numeric(.data$covariateValue), na.rm = TRUE),
      mean = sum(.data$weight * as.numeric(.data$covariateValue), na.rm = TRUE),
      sumSqr = sum(.data$weight * as.numeric(.data$covariateValue)^2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    inner_join(sumWSqr, join_by("treatment")) |>
    mutate(sd = sqrt(abs(.data$sumSqr - .data$mean^2) * sumW / (sumW^2 - .data$sumWSqr))) |>
    ungroup() |>
    select("covariateId", "treatment", "sum", "mean", "sd") |>
    collect()

  sd <- sqrt((sum(result$sd^2)) / 2)
  sdm <- (result$mean[result$treatment == 1] - result$mean[result$treatment == 0]) / sd

  # Compute variance of SDM using sparse data in efficient manner:
  totalStratumSize <- stratumSize |>
    group_by(.data$stratumId) |>
    summarise(nInStratum = sum(.data$n))
  nTotal <- sum(totalStratumSize$nInStratum)

  variances <- covariates |>
    inner_join(cohorts, by = join_by("rowId")) |>
    group_by(.data$treatment, .data$stratumId, .data$covariateId) |>
    summarise(
      sumX = sum(.data$covariateValue),
      sumXsqr = sum(.data$covariateValue * .data$covariateValue),
      .groups = "drop"
    ) |>
    inner_join(stratumSize, by = join_by("treatment", "stratumId")) |>
    mutate(
      sumX = coalesce(sumX, 0),
      sumXsqr = coalesce(sumXsqr, 0)
    ) |>
    mutate(
      sumSqrDiffs = sumXsqr - (sumX * sumX) / n,
      variance = case_when(
        n <= 1 ~ 0.0,
        TRUE ~ sumSqrDiffs / (n - 1)
      )
    )
  numerator <- variances |>
    inner_join(totalStratumSize, by = join_by("stratumId")) |>
    mutate(s = (variance / n) * ((nInStratum / nTotal) ^ 2)) |>
    summarise(sum(s)) |>
    pull()
  varSdm <- numerator / sd^2
  t <- (abs(sdm) - threshold)/sqrt(varSdm)
  p <- (1 - pnorm(t))*2
  return(p)
}
ps <- sapply(1:1000, simulateOne)
mean(ps<0.05, na.rm = TRUE)
# [1] 0.078

# Simulation under the null, continuous, stratification, sparse covariates, use code implemented in package
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

# Simulation under the null, continuous, no stratification
threshold <- 0

simulateOne <- function(seed) {
  set.seed(seed)
  n1 <- sample.int(90, 1) + 10
  n0 <- sample.int(90, 1) + 10
  trueMean <- runif(1, 0.1, 5)
  trueSd <- 1
  x1 <- rnorm(n1, mean = trueMean, sd = trueSd)
  x0 <- rnorm(n0, mean = trueMean, sd = trueSd)

  mean1 <- mean(x1)
  mean0 <- mean(x0)
  sd1 <- sd(x1)
  sd0 <- sd(x0)
  sd <- sqrt((sd1 ^ 2 + sd0^2) / 2)
  sdm <- (mean1 - mean0) / sd
  varSdm <- (n1 + n0) / (n1*n0) + (sdm^2) / (2*(n1 + n0 - 2))
  t <- (abs(sdm) - threshold)/sqrt(varSdm)
  p <- pnorm(t, lower.tail = FALSE)
  return(p)
}
ps <- sapply(1:10000, simulateOne)
mean(ps<0.05/2, na.rm = TRUE) # Divide by 2 because we take absolute value
# [1] 0.053 #

# Simulation under the null, continuous, no stratification, sparse, use code in package
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

# Simulation under the null, continuous, IPTW, sparse, use code in package
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

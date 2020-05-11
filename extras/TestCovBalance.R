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


  result <- tibble::tibble(sampleSize = sampleSize,
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

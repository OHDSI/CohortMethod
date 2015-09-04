library("testthat")
library("ff")
library("ffbase")

test_that("getDimensionTable", {
  dimensions = c()
  expect_that(getDimensionTable(dimensions), equals(NULL))

  dimensions = c("banana")
  expect_that(getDimensionTable(dimensions), throws_error())

  dimensions = c("conditionICD9", "drug", "procedureCPT4")
  dimensionTable = getDimensionTable(dimensions)
  expect_that(dimensionTable$conditionICD9$analysisId, equals(104))
  expect_that(dimensionTable$drug$analysisId, equals(505))
  expect_that(dimensionTable$procedureCPT4$analysisId, equals(703))
})

test_that("getDimensionAnalysisId", {
  dimensions = c("conditionICD9")
  analysisId = getDimensionAnalysisId(getDimensionTable(dimensions)[[1]])
  expect_that(analysisId, equals(104))
})

test_that("getDimensionSql", {
  dimensions = c("conditionICD9")
  sql = getDimensionSql(getDimensionTable(dimensions)[[1]])
  expect_that(sql, is_a("character"))
})

test_that("getConceptId", {
  x1 = ffdf(analysisId = as.ff(1), conceptId = as.ff(100))
  x2 = ffdf(analysisId = as.ff(1), conceptId = as.ff(101))
  x3 = ffdf(analysisId = as.ff(2), conceptId = as.ff(102))
  x4 = ffdf(analysisId = as.ff(1), conceptId = as.ff(103))
  x = combineFunction(list(x1, x2, x3, x4), ffdfrbind.fill)
  y = list(covariateRef = x)
  expect_that(getConceptId(1, y), equals(ff(vmode="double", initdata = c(100, 101, 103))))
  expect_that(getConceptId(3, y), equals(ff(vmode="double", initdata = c(-1))))
})

test_that("getCovariateId", {
  x1 = ffdf(analysisId = as.ff(1), covariateId = as.ff(100))
  x2 = ffdf(analysisId = as.ff(1), covariateId = as.ff(101))
  x3 = ffdf(analysisId = as.ff(2), covariateId = as.ff(102))
  x4 = ffdf(analysisId = as.ff(1), covariateId = as.ff(103))
  x = combineFunction(list(x1, x2, x3, x4), ffdfrbind.fill)
  y = list(covariateRef = x)
  expect_that(getCovariateId(1, y), equals(ff(vmode="double", initdata = c(100, 101, 103))))
})

test_that("covariateIdToFactor", {
  codes = ffdf(SOURCE_CODE = ff(vmode = "double", c(1,2,3)), TARGET_CONCEPT_ID = ff(vmode = "double", c(101, 102, 103)))
  result = covariateIdToFactor(codes)[[1]]
  expect_that(result$SOURCE_CODE, is_equivalent_to(ff(vmode = "integer", factor(c("1", "2", "3")))))

  codes = ffdf(SOURCE_CODE = ff(vmode = "integer", factor(c("a", "b", "c"))), TARGET_CONCEPT_ID = ff(vmode = "double", c(101, 102, 103)))
  result = covariateIdToFactor(codes)[[1]]
  expect_that(result$SOURCE_CODE, equals(codes$SOURCE_CODE))

  codes = data.frame(X = c(), Y = c())
  result = covariateIdToFactor(codes)[[1]]
  expect_that(result, equals(NULL))
})

test_that("deletePredefinedCodes", {
  sourceCodes = ff(vmode="integer", factor(1:10))
  targetCodes = ff(vmode="double", 101:110)

  deletedSourceCodes = ff(vmode="integer", factor(c(2,3,4,6,7,8,9,10)))
  deletedTargetCodes = ff(vmode="double", c(102,103,104,106,107,108,109,110))
  codes = ffdf(SOURCE_CODE=sourceCodes, TARGET_CONCEPT_ID=targetCodes)
  result = deletePredefinedCodes(codes, c(101,105))[[1]]
  expect_that(result$SOURCE_CODE, is_equivalent_to(deletedSourceCodes))
  expect_that(result$TARGET_CONCEPT_ID, equals(deletedTargetCodes))

  result = deletePredefinedCodes(codes, c())[[1]]
  expect_that(result, equals(codes))

  result = deletePredefinedCodes(codes, 101:110)[[1]]
  expect_that(result, equals(NULL))
})

test_that("truncateRawCodes", {
  sourceCodes = ff(vmode = "integer", factor(11:13))
  targetCodes = ff(vmode = "double", 111:113)
  codes = ffdf(SOURCE_CODE = sourceCodes, TARGET_CONCEPT_ID = targetCodes)


  dimensionInfo = list(truncate = "")
  result = truncateRawCodes(codes, dimensionInfo)[[1]]
  expect_that(result$SOURCE_CODE, equals(sourceCodes))

  dimensionInfo = list(truncate = "function(x){return(gsub(\"^.\", \"banana\", x))}")
  result = truncateRawCodes(codes, dimensionInfo)[[1]]
  expect_that(result$SOURCE_CODE, is_equivalent_to(ff(vmode = "integer", initdata = factor(c("banana1", "banana2", "banana3")))))

  result = truncateRawCodes(NULL, dimensionInfo)[[1]]
  expect_that(result, equals(NULL))
})

test_that("deleteRepeatCodes", {
  sourceCodes = ff(vmode = "integer", factor(c("1", "1", "2", "2", "3", "4")))
  targetCodes = ff(vmode = "double", c(1, 2, 3, 3, 1, 4))
  codes = ffdf(SOURCE_CODE = sourceCodes, TARGET_CONCEPT_ID = targetCodes)

  deletedSourceCodes = ff(vmode = "integer", factor(c(1, 2, 4)))
  deletedTargetCodes = ff(vmode = "double", c(2,3,4))
  deletedCodes = ffdf(SOURCE_CODE = deletedSourceCodes, TARGET_CONCEPT_ID = deletedTargetCodes)

  result = deleteRepeatCodes(codes)[[1]]
  expect_that(result$SOURCE_CODE, is_equivalent_to(deletedCodes$SOURCE_CODE))
  expect_that(result$TARGET_CONCEPT_ID, equals(deletedCodes$TARGET_CONCEPT_ID))

  result = deleteRepeatCodes(NULL)[[1]]
  expect_that(result, equals(NULL))
})

test_that("combineData",{
  #   rowId     covariateId     treatment     outcome
  #   1         apple           1             1
  #   8         banana          1             0
  #   9         apple           1             1
  #   10        apple           0             0
  #   -----------------------------------------------
  #   4         grape           0             0
  #   5         orange          0             1
  #   6         orange          0             0
  #   11        orange          0             1

  codes1 = ffdf(SOURCE_CODE = ff(vmode = "integer", factor(c("apple", "banana"))),
                TARGET_CONCEPT_ID = ff(vmode = "double", c(1, 2)))
  codes2 = ffdf(SOURCE_CODE = ff(vmode = "integer", factor(c("grape", "orange"))),
                TARGET_CONCEPT_ID = ff(vmode = "double", c(3, 4)))
  codes = list(codes1, codes2)

  covariateRef = ffdf(covariateId = ff(vmode = "double", c(101, 102, 103, 104, 105, 106)),
                      conceptId = ff(vmode = "double", c(1, 2, 3, 4, 5, 6)))
  covariates = ffdf(rowId = ff(vmode = "double", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)),
                    covariateId = ff(vmode = "double", c(101, 105, 106, 103, 104, 104, 105, 102, 101, 101, 104, 105)),
                    covariateValue = ff(vmode = "double", initdata = 1, length = 12))
  treatments = ffdf(rowId = ff(vmode = "double", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)),
                 treatment = ff(vmode = "double", c(1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0)))
  outcomes = ffdf(rowId = ff(vmode = "double", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)),
                  outcome = ff(vmode = "double", c(2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0)))
  cohortData = list(cohorts = treatments, outcomes = outcomes, covariates = covariates, covariateRef = covariateRef)

  dimCovariateId = list(ff(vmode = "double", c(101, 102)), ff(vmode = "double", c(103, 104)))

  result = combineData(cohortData, codes, dimCovariateId)
  result1 = result[[1]]
  result2 = result[[2]]

  expect_that(result1$rowId, equals(ff(vmode = "double", c(1, 8, 9, 10))))
  expect_that(result1$covariateId, is_equivalent_to(ff(vmode = "integer", factor(c("apple", "banana", "apple", "apple")))))
  expect_that(result1$treatment, equals(ff(vmode = "double", c(1, 1, 1, 0))))
  expect_that(result1$outcome, equals(ff(vmode = "double", c(1, 0, 1, 0))))

  expect_that(result2$rowId, equals(ff(vmode = "double", c(4, 5, 6, 11))))
  expect_that(result2$covariateId, is_equivalent_to(ff(vmode = "integer", factor(c("grape", "orange", "orange", "orange")))))
  expect_that(result2$treatment, equals(ff(vmode = "double", c(0, 0, 0, 0))))
  expect_that(result2$outcome, equals(ff(vmode = "double", c(0, 1, 0, 1))))

  dimCovariateId = list(ff(vmode = "double", c(101, 102)), ff(vmode = "double", c(103)))
  result = combineData(cohortData, codes, dimCovariateId)
  result2 = result[[2]]

  expect_that(result2$covariateId, is_equivalent_to(ff(vmode = "integer", factor(c("grape")))))

  codesN = list(codes1, NULL)
  result = combineData(cohortData, codesN, dimCovariateId)
  expect_that(result[[2]], equals(NULL))

  result = combineData(cohortData, codes, NULL)
  expect_that(result, equals(NULL))
})

test_that("removeRareCodes",{
  # removeRareCodes <- function(data, lowPopCutoff, dimensionCutoff=NULL)
  # Covariate   Unique-People
  #     1             8
  #     2             2
  #     3             5
  #     4             3
  #     5             2
  #     6             4
  #     7             3
  rowId = ff(vmode="double",               c(1,2,3,4,5,6,7,8,1,2,1,2,3,4,5,1,2,3,7,8,8,8,1,2,3,4,8,9,9,10,1,2,3,4))
  covariateId = ff(vmode="integer", factor(c(1,1,1,1,1,1,1,1,2,2,3,3,3,3,3,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8)))
  treatment = ff(vmode="double", initdata=0, length=34)
  outcome = ff(vmode="double", initdata=0, length=34)
  data = ffdf(rowId=rowId, covariateId=covariateId, treatment=treatment, outcome=outcome)

  result = removeRareCodes(data, 100, 8)[[1]]
  expect_that(result, equals(NULL))

  result = removeRareCodes(data, 3, 8)[[1]]
  expect_that(length(unique(result$covariateId)), equals(5))
  expect_that(unique(result$covariateId), is_equivalent_to(ff(vmode="double", c(3,4,6,7,8))))

  result = removeRareCodes(data, 3, 2)[[1]]
  expect_that(length(unique(result$covariateId)), equals(2))
  expect_that(unique(result$covariateId), is_equivalent_to(ff(vmode="double", c(3,6))))

  result = removeRareCodes(data, 3, 4)[[1]]
  expect_that(length(unique(result$covariateId)), equals(4))
  expect_that(unique(result$covariateId), is_equivalent_to(ff(vmode="double", c(3,6,7,8))))

  result = removeRareCodes(NULL, 3, 3)[[1]]
  expect_that(result, equals(NULL))
})

test_that("expandCovariates", {
  #   Covariate1: (1, 1, 1, 2, 3, 5, 6)
  #     median = 2
  #     3rd quartile = 4
  rowId = ff(vmode = "double", c(1, 2, 3, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7))
  covariateId = ff(vmode = "integer", initdata = factor("banana"), length = 19)
  treatment = ff(vmode = "double", length = 19)
  outcome = ff(vmode = "double", length = 19)
  data = ffdf(rowId = rowId, covariateId = covariateId, treatment = treatment, outcome = outcome)

  result = expandCovariates(data)[[1]]
  expect_that(length(unique(result$covariateId)), equals(3))
  expect_that(result$rowId[ffwhich(result, result$covariateId == "banana_sporadic"),], is_equivalent_to(ff(vmode = "double", c(5,6,7))))
  expect_that(result$rowId[ffwhich(result, result$covariateId == "banana_frequent"),], is_equivalent_to(ff(vmode = "double", c(6,7))))

  result = expandCovariates(NULL)[[1]]
  expect_that(result, equals(NULL))

  #   Covariate1: (1, 1, 1, 2)
  #     median = 1
  #     3rd quartile = 1
  rowId = ff(vmode = "double", c(1, 2, 3, 4, 4))
  covariateId = ff(vmode = "integer", initdata = factor("banana"), length = 5)
  treatment = ff(vmode = "double", length = 5)
  outcome = ff(vmode = "double", length = 5)
  data = ffdf(rowId = rowId, covariateId = covariateId, treatment = treatment, outcome = outcome)

  result = expandCovariates(data)[[1]]
  expect_that(length(unique(result$covariateId)), equals(1))
  expect_that(result$covariateId, is_equivalent_to(ff(vmode = "integer", factor(c("banana")))))
})

test_that("calculateRanks", {
  #   Covariate1:
  #                   Outcome       No-Outcome      Treatment     No-Treatment
  #   Covariate         2               3               1             4
  #   No-Covariate      1               3               2             2
  #
  #       RR  = 1.6
  #       PC1 = 0.3333333
  #       PC0 = 0.6666667 -> 0.3333333
  #       Bias = 1
  #       biasRank = 0
  #       RRce = 1
  #       expRank = 0
  #
  #   Covariate2:
  #                   Outcome       No-Outcome      Treatment     No-Treatment
  #   Covariate         1               4               2             3
  #   No-Covariate      2               2               1             3
  #
  #       RR  = 0.4
  #       PC1 = 0.6666667 -> 0.3333333
  #       PC0 = 0.5
  #       Bias = 0.8571428
  #       biasRank = 0.1541507
  #       RRce = 0.6666667
  #       expRank = 0.4054651
  covariates = ffdf(rowId = ff(vmode = "double", c(1, 2, 3, 4, 5, 1, 6, 7, 8, 9)),
                    covariateId = ff(vmode = "integer", factor(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))),
                    treatment = ff(vmode = "double", c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0)),
                    outcome = ff(vmode = "double", c(0, 1, 1, 0, 0, 0, 0, 1, 0, 0)))
  cohorts = ffdf(rowId = ff(vmode = "double", c(1, 2, 3, 4, 5, 6, 7, 8, 9)),
                 treatment = ff(vmode = "double", c(0, 0, 1, 0, 0, 0, 1, 1, 0)))
  outcomes = ffdf(rowId = ff(vmode = "double", c(2, 3, 7)),
                  outcome = ff(vmode = "double", c(1, 1, 1)))
  cohortData = list(cohorts = cohorts, outcomes = outcomes)

  result = calculateRanks(covariates, cohortData, 0)[[1]]
  expect_that(result$covariateId, is_equivalent_to(ff(vmode = "integer", factor(c(2,1)))))
  expect_that(result$biasRank, equals(ff(vmode = "double", c(0.1541507, 0))))
  expect_that(result$expRank, equals(ff(vmode = "double", c(0.4054651, 0))))



  #   Covariate1:
  #                   Outcome       No-Outcome      Treatment     No-Treatment
  #   Covariate         0               2               1             1
  #   No-Covariate      1               3               1             3
  #
  #       RR  = 0
  #       RR (fudge = 0.01) = 0.01975272
  #       PC1 = 0.5
  #       PC0 = 0.25
  #       Bias (fudge=0) = NA
  #       Bias (fudge=0.01) = 1.925409
  #       abs(log(bias)) = 0.6551384
  #       RRce = 2
  #       expRank = 0.6931472
  covariates = ffdf(rowId = ff(vmode = "double", c(1, 2)),
                    covariateId = ff(vmode = "integer", factor(c(1, 1))),
                    treatment = ff(vmode = "double", c(1, 0)),
                    outcome = ff(vmode = "double", c(0, 0)))
  cohorts = ffdf(rowId = ff(vmode = "double", c(1, 2, 3, 4, 5, 6)),
                 treatment = ff(vmode = "double", c(1, 0, 1, 0, 0, 0)))
  outcomes = ffdf(rowId = ff(vmode = "double", c(4)),
                  outcome = ff(vmode = "double", c(1)))
  cohortData = list(cohorts = cohorts, outcomes = outcomes)

  result = calculateRanks(covariates, cohortData, 0)[[1]]
  expect_that(result$covariateId, is_equivalent_to(ff(vmode = "integer", factor(c(1)))))
  expect_that(result$biasRank, equals(ff(vmode = "double", c(NA))))
  expect_that(result$expRank, equals(ff(vmode = "double", c(0.6931472))))

  result = calculateRanks(covariates, cohortData, 0.01)[[1]]
  expect_that(result$covariateId, is_equivalent_to(ff(vmode = "integer", factor(c(1)))))
  expect_that(result$biasRank, equals(ff(vmode = "double", c(0.6551384))))
  expect_that(result$expRank, equals(ff(vmode = "double", c(0.6931472))))
})

test_that("removeLowRanks", {
  #   Covariate       biasRank    expRank     PC1       PC0
  #       1           10          1           0.6       0.4
  #       2           6           2           0         0
  #       3           5           2           0         0
  #  ----------------------
  #       4           7           1           0.6       0.4
  #       5           6           1           0.8       0
  #       6           2           1           0.6       0.5

# removeLowRank <- function(data, rankings, rankCutoff, useExpRank) {
  rowId1 = ff(vmode = "double", length = 3)
  covariateId1 = ff(vmode = "integer", initdata = factor(c(1, 2, 3)))
  treatment1 = ff(vmode = "double", length = 3)
  outcome1 = ff(vmode = "double", length = 3)
  data1 = ffdf(rowId = rowId1, covariateId = covariateId1, treatment = treatment1, outcome = outcome1)
  rowId2 = ff(vmode = "double", length = 3)
  covariateId2 = ff(vmode = "integer", initdata = factor(c(4, 5, 6)))
  treatment2 = ff(vmode = "double", length = 3)
  outcome2 = ff(vmode = "double", length = 3)
  data2 = ffdf(rowId = rowId2, covariateId = covariateId2, treatment = treatment2, outcome = outcome2)
  data = list(data1, data2)

  bias1 = ff(vmode = "double", c(10, 6, 5))
  exp1 = ff(vmode = "double", c(1, 2, 2))
  pc11 = ff(vmode = "double", c(0.6, 0, 0))
  pc10 = ff(vmode = "double", c(0.4, 0, 0))
  ranks1 = ffdf(covariateId = covariateId1, biasRank = bias1, expRank = exp1, PC1 = pc11, PC0 = pc10)
  bias2 = ff(vmode = "double", c(7, 6, 2))
  exp2 = ff(vmode = "double", c(1, 1, 1))
  pc21 = ff(vmode = "double", c(0.6, 0.8, 0.6))
  pc20 = ff(vmode = "double", c(0.4, 0, 0.5))
  ranks2 = ffdf(covariateId = covariateId2, biasRank = bias2, expRank = exp2, PC1 = pc21, PC0 = pc20)
  ranks = list(ranks1, ranks2)

  result = removeLowRank(data, ranks, 1, FALSE)
  expect_that(result[[1]]$covariateId[], equals(factor("1")))
  expect_that(result[[2]], equals(NULL))

  result = removeLowRank(data, ranks, 2, FALSE)
  expect_that(result[[1]]$covariateId[], equals(factor("1")))
  expect_that(result[[2]]$covariateId[], equals(factor("4")))

  result = removeLowRank(data, ranks, 3, FALSE)
  expect_that(result[[1]]$covariateId[], equals(factor(c("1", "2"))))
  expect_that(result[[2]]$covariateId[], equals(factor(c("4"))))

  ranks[[1]]$biasRank = ff(vmode = "double", c(1, 2, 1))
  ranks[[2]]$biasRank = ff(vmode = "double", c(1, 1, 1))

  result = removeLowRank(data, ranks, 1, TRUE)
  expect_that(result[[1]]$covariateId[], equals(factor(c("2"))))
  expect_that(result[[2]]$covariateId[], equals(NULL))

  result = removeLowRank(data, ranks, 3, TRUE)
  expect_that(result[[1]]$covariateId[], equals(factor(c("2","3"))))
  expect_that(result[[2]]$covariateId[], equals(factor(c("5"))))

  result = removeLowRank(data, ranks, 4, TRUE)
  expect_that(result[[1]]$covariateId[], equals(factor(c("2","3"))))
  expect_that(result[[2]]$covariateId[], equals(factor(c("5","6"))))

  result = removeLowRank(data, ranks, 5, TRUE)
  expect_that(result[[1]]$covariateId[], equals(factor(c("1","2","3"))))
  expect_that(result[[2]]$covariateId[], equals(factor(c("5","6"))))

  result = removeLowRank(data, ranks, 8, TRUE)
  expect_that(result[[1]]$covariateId[], equals(factor(c("1","2","3"))))
  expect_that(result[[2]]$covariateId[], equals(factor(c("4","5","6"))))

  result = removeLowRank(data, NULL, 5, TRUE)[[1]]
  expect_that(result, equals(NULL))
})

test_that("combineFunction", {
  x = data.frame(a = 1, b = 2)
  y = data.frame(a = 3, b = 4)
  z = data.frame(a = 5, b = 6)

  result = combineFunction(list(x,y,z), rbind)
  expect_that(result, equals(data.frame(a = c(1,3,5), b = c(2,4,6))))

  result = combineFunction(list(NULL, x, y), rbind)
  expect_that(result, equals(data.frame(a = c(1,3), b = c(2,4))))

  result = combineFunction(list(NULL), rbind)
  expect_that(result, equals(NULL))

  result = combineFunction(list(x), rbind)
  expect_that(result, equals(x))

  f = function(a,b){return(a+b)}
  result = combineFunction(list(1,2,3,4), f)
  expect_that(result, equals(10))

  result = combineFunction(list(NULL, 1, 2, NULL), f)
  expect_that(result, equals(3))
})

test_that("combineWithOtherCovariates", {
  rowId = ff(vmode = "double", 1:2)
  covariateIdN = ff(vmode = "integer", factor(c(1,2)))
  covariateIdD = ff(vmode = "double", c(3,4))
  covariateIdP = ff(vmode = "double", c(5,6))
  treatment = ff(vmode = "double", length = 2)
  outcome = ff(vmode = "double", length = 2)
  covariateValue = ff(vmode = "double", initdata = 1, length = 2)

  newCovariates = ffdf(rowId = rowId, covariateId = covariateIdN, treatment = treatment, outcome = outcome)
  demCovariates = ffdf(rowId = rowId, covariateId = covariateIdD, covariateValue = covariateValue)
  preCovariates = ffdf(rowId = rowId, covariateId = covariateIdP, covariateValue = covariateValue)

  result = combineWithOtherCovariates(newCovariates, demCovariates, preCovariates)
  expect_that(result$covariateId[1], is_a("factor"))
  expect_that(result$covariateId, is_equivalent_to(ff(vmode = "integer", initdata = factor(c(1,2,3,4,5,6)))))

  result = combineWithOtherCovariates(newCovariates, NULL, NULL)
  expect_that(result$rowId, equals(newCovariates$rowId))
  expect_that(result$covariateId, equals(newCovariates$covariateId))

  result = combineWithOtherCovariates(newCovariates, NULL, preCovariates)
  expect_that(result$covariateId, is_equivalent_to(ff(vmode = "integer", initdata = factor(c(1,2,5,6)))))

  result = combineWithOtherCovariates(NULL, demCovariates, preCovariates)
  expect_that(result$covariateId[1], is_a("factor"))
  expect_that(result$covariateId, is_equivalent_to(ff(vmode = "integer", initdata = factor(3,4,5,6))))

  result = combineWithOtherCovariates(NULL, NULL, NULL)
  expect_that(result, equals(NULL))
})

test_that("combineWithOtherCovariateRef", {
#   New-Index       Old-Index
#       3               103
#       4               104
#       5               105
#       6               106
  covariateIdN = ff(vmode = "double", c(1,2))
  covariateNameN = ff(vmode = "integer", factor(c("apple", "banana")))
  covariateIdD = ff(vmode = "double", c(103, 104))
  covariateNameD = ff(vmode = "integer", factor(c("cherry", "grape")))
  covariateIdP = ff(vmode = "double", c(105, 106))
  covariateNameP = ff(vmode = "integer", factor(c("mango", "orange")))

  covariateIndex = ffdf(covariateId = ff(vmode = "double", c(103, 104, 105, 106)), index = ff(vmode = "double", c(3,4,5,6)))
  newCovariates = ffdf(covariateId = covariateIdN, covariateName = covariateNameN)
  demCovariates = ffdf(covariateId = covariateIdD, covariateName = covariateNameD)
  preCovariates = ffdf(covariateId = covariateIdP, covariateName = covariateNameP)

  result = combineWithOtherCovariateRef(newCovariates, demCovariates, preCovariates, covariateIndex)
  expect_that(result$covariateId, is_equivalent_to(ff(vmode = "double", c(1,2,3,4,5,6))))

  result = combineWithOtherCovariateRef(newCovariates, NULL, NULL, covariateIndex)
  expect_that(result$covariateId, equals(newCovariates$covariateId))

  result = combineWithOtherCovariateRef(newCovariates, NULL, preCovariates, covariateIndex)
  expect_that(result$covariateId, is_equivalent_to(ff(vmode = "double", c(1,2,5,6))))

  result = combineWithOtherCovariateRef(NULL, demCovariates, preCovariates, covariateIndex)
  expect_that(result$covariateId, is_equivalent_to(ff(vmode = "double", c(3,4,5,6))))

  result = combineWithOtherCovariateRef(NULL, NULL, NULL, covariateIndex)
  expect_that(result$covariateId, equals(NULL))
})

test_that("createNewCovRef", {
  covariateIdIndex = ffdf(covariateId = ff(vmode = "integer", factor(c("1", "1_sporadic", "1_frequent", "2", "2_sporadic", "2_frequent"))),
                          index = ff(vmode = "double", c(101, 102, 103, 104, 105, 106)))
  dimensionInfo = data.frame(analysisId = 1,
                             truncate = "yes",
                             truncatedNames = "data.frame(code = c(\"1\",\"2\"), name = c(\"apple\", \"banana\"))", stringsAsFactors = FALSE)
  data = ffdf(covariateId = ff(vmode = "integer", factor(c("1", "1_sporadic", "2", "2_frequent"))))

  result = createNewCovRef(data, dimensionInfo, NULL, covariateIdIndex, NULL)
  expect_that(result$covariateId, equals(ff(vmode = "double", c(101, 102, 104, 106))))
  expect_that(result$covariateName, equals(ff(vmode = "integer", factor(c("(once) apple", "(sporadic) apple", "(once) banana", "(frequent) banana")))))
  expect_that(result$analysisId, equals(ff(vmode = "double", c(1, 1, 1, 1))))
  expect_that(result$conceptId, equals(ff(vmode = "double", c(0, 0, 0, 0))))


  covariateIdIndex = ffdf(covariateId = ff(vmode = "integer", factor(c("1", "1_sporadic", "1_frequent", "2", "2_sporadic", "2_frequent"))),
                          index = ff(vmode = "double", c(101, 102, 103, 104, 105, 106)))
  dimensionInfo = data.frame(analysisId = 2, truncate = "", truncatedNames = "")
  rawCodes = ffdf(SOURCE_CODE = ff(vmode = "integer", factor(c("1", "2"))),
                  TARGET_CONCEPT_ID = ff(vmode = "double", c(1, 2)))
  cohortData = list(covariateRef = ffdf(conceptId = ff(vmode = "double", c(1, 2)),
                                        covariateName = ff(vmode = "integer", factor(c("grape", "orange"))),
                                        analysisId = ff(vmode = "double", c(2, 2))))
  data = ffdf(covariateId = ff(vmode = "integer", factor(c("1", "1_frequent", "2", "2_sporadic"))))

  result = createNewCovRef(data, dimensionInfo, rawCodes, covariateIdIndex, cohortData)
  expect_that(result$covariateId, equals(ff(vmode = "double", c(101, 103, 104, 105))))
  expect_that(result$covariateName, equals(ff(vmode = "integer", factor(c("(once) grape", "(frequent) grape", "(once) orange", "(sporadic) orange")))))
  expect_that(result$analysisId, equals(ff(vmode = "double", c(2, 2, 2, 2))))
  expect_that(result$conceptId, equals(ff(vmode = "double", c(1, 1, 2, 2))))
})

test_that("createCovariateIndex", {
  covariateId = ff(vmode = "integer", factor(c(1, 4, 9, 16, 25)))
  covariates = ffdf(covariateId = covariateId)

  result = createCovariateIndex(covariates)
  expect_that(result$covariateId, equals(covariateId))
  expect_that(result$index, equals(ff(vmode = "double", c(1,2,3,4,5))))

  result = createCovariateIndex(NULL)
  expect_that(result, equals(NULL))
})

test_that("convertCovariateId", {
  covariateId = ff(vmode = "integer", factor(c(1, 4, 9, 16, 25)))
  covariates = ffdf(covariateId = covariateId)
  index = ffdf(covariateId = covariateId, index = ff(vmode = "double", c(1,2,3,4,5)))

  result = convertCovariateId(covariates, index)
  expect_that(result$covariateId[1], is_a("numeric"))
  expect_that(result$covariateId, equals(ff(vmode = "double", c(1,2,3,4,5))))

  result = convertCovariateId(NULL, NULL)
  expect_that(result, equals(NULL))
})

test_that("getDemographicsCovariateRef", {
#   CovariateId       AnalysisId
#         1                 1
#         2                 1
#         3                 2
#         4                 2
#         5                 3
#         6                 4
  covariateId = ff(vmode = "double", c(1,2,3,4,5,6))
  analysisId = ff(vmode = "double", c(1,1,2,2,3,4))
  covariateName = ff(vmode = "integer", factor(c("a", "b", "c", "d", "e", "f")))
  covariateRef = ffdf(covariateId = covariateId, analysisId = analysisId, covariateName = covariateName)

  demographicsAnalysisIds = c(1,3)
  result = getDemographicsCovariateRef(list(covariateRef = covariateRef), demographicsAnalysisIds)
  expect_that(result$covariateId, equals(ff(vmode = "double", c(1,2,5))))

  demographicsAnalysisIds = c()
  result = getDemographicsCovariateRef(list(covariateRef = covariateRef), demographicsAnalysisIds)
  expect_that(result, equals(NULL))
})

test_that("getDemographicsCovariates", {
  rowId = ff(vmode = "double", c(1,2,3,4,5))
  covariateId = ff(vmode = "double", c(1, 1, 2, 3, 4))
  covariates = ffdf(rowId = rowId, covariateId = covariateId)

  demographicsCovariateRef = ffdf(covariateId = as.ff(c(1,3)))
  result = getDemographicsCovariates(list(covariates = covariates), demographicsCovariateRef)
  expect_that(result$rowId, equals(ff(vmode = "double", c(1,2,4))))

  demographicsCovariateRef = NULL
  result = getDemographicsCovariates(list(covariates = covariates), demographicsCovariateRef)
  expect_that(result, equals(NULL))
})

test_that("getPredefinedCovariateRef", {
  #   CovariateId       ConceptId
  #         1                 1
  #         2                 1
  #         3                 2
  #         4                 2
  #         5                 3
  #         6                 4
  covariateId = ff(vmode = "double", c(1,2,3,4,5,6))
  conceptId = ff(vmode = "double", c(1,1,2,2,3,4))
  covariateName = ff(vmode = "integer", factor(c("a", "b", "c", "d", "e", "f")))
  covariateRef = ffdf(covariateId = covariateId, conceptId = conceptId, covariateName = covariateName)
  cohortData = list(covariateRef = covariateRef)

  predefinedConceptIds = c(1,3)
  result = getPredefinedCovariateRef(cohortData, predefinedConceptIds)
  expect_that(result$covariateId, equals(ff(vmode = "double", c(1,2,5))))

  predefinedConceptIds = c()
  result = getPredefinedCovariateRef(cohortData, predefinedConceptIds)
  expect_that(result, equals(NULL))
})

test_that("getPredefinedCovariates", {
  rowId = ff(vmode = "double", c(1,2,3,4,5))
  covariateId = ff(vmode = "double", c(1, 1, 2, 3, 4))
  covariates = ffdf(rowId = rowId, covariateId = covariateId)

  predefinedCovariateRef = ffdf(covariateId = as.ff(c(1,3)))
  result = getPredefinedCovariates(list(covariates = covariates), predefinedCovariateRef)
  expect_that(result$rowId, equals(ff(vmode = "double", c(1,2,4))))

  predefinedCovariateRef = NULL
  result = getPredefinedCovariates(list(covariates = covariates), predefinedCovariateRef)
  expect_that(result, equals(NULL))
})

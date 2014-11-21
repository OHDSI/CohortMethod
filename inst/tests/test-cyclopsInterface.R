library("testthat")
library("survival")

test_that("Test data.frame to data for clr", {
  gold <- clogit(case ~ spontaneous + induced + strata(stratum), data=infert)    
  
  #Convert infert dataset to Cyclops format:
  covariates <- data.frame(stratum_id = rep(infert$stratum,2),
                           row_id = rep(1:nrow(infert),2),
                           covariate_id = rep(1:2,each=nrow(infert)),
                           covariate_value = c(infert$spontaneous,infert$induced))
  outcomes <- data.frame(stratum_id = infert$stratum,
                         row_id = 1:nrow(infert),
                         y = infert$case)
  #Make sparse:
  covariates <- covariates[covariates$covariate_value != 0,]
  
  #Sort:
  covariates <- covariates[order(covariates$stratum_id,covariates$row_id,covariates$covariate_id),]
  outcomes <- outcomes[order(outcomes$stratum_id,outcomes$row_id),]
  
  
  cyclopsData <- createCyclopsData(outcomes,covariates,modelType = "clr",addIntercept = FALSE)
  fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  tolerance <- 1E-4
  expect_equal(as.vector(coef(fit)), as.vector(coef(gold)), tolerance = tolerance)
})

test_that("Test ffdf to data for clr", {
  gold <- clogit(case ~ spontaneous + induced + strata(stratum), data=infert)    
  
  #Convert infert dataset to Cyclops format:
  covariates <- data.frame(stratum_id = rep(infert$stratum,2),
                           row_id = rep(1:nrow(infert),2),
                           covariate_id = rep(1:2,each=nrow(infert)),
                           covariate_value = c(infert$spontaneous,infert$induced))
  outcomes <- data.frame(stratum_id = infert$stratum,
                         row_id = 1:nrow(infert),
                         y = infert$case)
  #Make sparse:
  covariates <- covariates[covariates$covariate_value != 0,]
  
  #Sort:
  covariates <- covariates[order(covariates$stratum_id,covariates$row_id,covariates$covariate_id),]
  outcomes <- outcomes[order(outcomes$stratum_id,outcomes$row_id),]
  
  #convert to ffdf:
  covariates <- as.ffdf(covariates)
  outcomes <- as.ffdf(outcomes)
  
  cyclopsData <- createCyclopsData.ffdf(outcomes,covariates,modelType = "clr",addIntercept = FALSE)
  fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  tolerance <- 1E-4
  expect_equal(as.vector(coef(fit)), as.vector(coef(gold)), tolerance = tolerance)
})

test_that("Test data.frame to data for stratified cox", {
  # Create the simplest test data set
  test1 <- data.frame(time=c(4,3,1,1,2,2,3),
                      status=c(1,1,1,0,1,1,0),
                      x=c(0,2,1,1,1,0,0),
                      sex=c(0,0,0,0,1,1,1))
  
  gold <- coxph(Surv(time, status) ~ x + strata(sex), test1, method="breslow")
  
  cyclopsDataFormula <- createCyclopsDataFrame(Surv(time, status) ~ x + strata(sex), data=test1,modelType="cox")
  fitFormula <- fitCyclopsModel(cyclopsDataFormula)
  
  #Convert to data.frames for Cyclops:
  covariates <- data.frame(stratum_id = test1$sex,
                           row_id = 1:nrow(test1),
                           covariate_id = rep(1,nrow(test1)),
                           covariate_value = test1$x,
                           time = test1$time,
                           y = test1$status)
  outcomes <- data.frame(stratum_id = test1$sex,
                         row_id = 1:nrow(test1),
                         y = test1$status,
                         time = test1$time)
  
  #Make sparse:
  covariates <- covariates[covariates$covariate_value != 0,]

  #Sort:
  covariates <- covariates[order(covariates$stratum_id,-covariates$time,covariates$y,covariates$row_id),]
  outcomes <- outcomes[order(outcomes$stratum_id,-outcomes$time,outcomes$y,outcomes$row_id),]
  
  cyclopsData <- createCyclopsData(outcomes,covariates,modelType = "cox")
  
  cyclopsFitStrat <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  tolerance <- 1E-4
  expect_equal(as.vector(coef(fit)), as.vector(coef(fitFormula)), tolerance = tolerance)
  expect_equal(as.vector(coef(fit)), as.vector(coef(gold)), tolerance = tolerance)  
})

test_that("Test data.frame to data for stratified cox using lung dataset ", { 
  test <- lung
  test[is.na(test)] <- 0 # Don't want to bother with missing values
 
  gold <- coxph(Surv(time, status) ~ age + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss + strata(sex), test, method="breslow")
  
  cyclopsDataFormula <- createCyclopsDataFrame(Surv(time, status) ~ age + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss + strata(sex), data=test,modelType="cox")
  fitFormula <- fitCyclopsModel(cyclopsDataFormula)
    
  #Convert to data.frames for Cyclops:
  nCovars = 6
  covariates <- data.frame(stratum_id = rep(test$sex,nCovars),
                           row_id = rep(1:nrow(test),nCovars),
                           covariate_id = rep(1:nCovars,each = nrow(test)),
                           covariate_value = c(test$age,test$ph.ecog,test$ph.karno,test$pat.karno,test$meal.cal,test$wt.loss),
                           time = rep(test$time,nCovars),
                           y = rep(test$status-1,nCovars))
  outcomes <- data.frame(stratum_id = test$sex,
                         row_id = 1:nrow(test),
                         y = test$status-1,
                         time = test$time)
  
  #Make sparse:
  covariates <- covariates[covariates$covariate_value != 0,]
  
  #Sort:
  covariates <- covariates[order(covariates$stratum_id,-covariates$time,covariates$y,covariates$row_id),]
  outcomes <- outcomes[order(outcomes$stratum_id,-outcomes$time,outcomes$y,outcomes$row_id),]
  
  cyclopsData <- createCyclopsData(outcomes,covariates,modelType = "cox")
  fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  tolerance <- 1E-4
  expect_equal(as.vector(coef(fit)), as.vector(coef(fitFormula)), tolerance = tolerance)
  expect_equal(as.vector(coef(fit)), as.vector(coef(gold)), tolerance = tolerance)
})

test_that("Test ffdf to data for stratified cox", {
  # Create the simplest test data set
  test1 <- data.frame(time=c(4,3,1,1,2,2,3),
                      status=c(1,1,1,0,1,1,0),
                      x=c(0,2,1,1,1,0,0),
                      sex=c(0,0,0,0,1,1,1))
  
  gold <- coxph(Surv(time, status) ~ x + strata(sex), test1, method="breslow")
  
  cyclopsDataFormula <- createCyclopsDataFrame(Surv(time, status) ~ x + strata(sex), data=test1,modelType="cox")
  fitFormula <- fitCyclopsModel(cyclopsDataFormula)
  
  #Convert to data.frames for Cyclops:
  covariates <- data.frame(stratum_id = test1$sex,
                           row_id = 1:nrow(test1),
                           covariate_id = rep(1,nrow(test1)),
                           covariate_value = test1$x,
                           time = test1$time,
                           y = test1$status)
  outcomes <- data.frame(stratum_id = test1$sex,
                         row_id = 1:nrow(test1),
                         y = test1$status,
                         time = test1$time)
  
  #Make sparse:
  covariates <- covariates[covariates$covariate_value != 0,]
  
  #Sort:
  covariates <- covariates[order(covariates$stratum_id,-covariates$time,covariates$y,covariates$row_id),]
  outcomes <- outcomes[order(outcomes$stratum_id,-outcomes$time,outcomes$y,outcomes$row_id),]
  
  #Convert to ffdf:
  covariates <- as.ffdf(covariates)
  outcomes <- as.ffdf(outcomes)
  
  cyclopsData <- createCyclopsData.ffdf(outcomes,covariates,modelType = "cox")
  
  cyclopsFitStrat <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  tolerance <- 1E-4
  expect_equal(as.vector(coef(fit)), as.vector(coef(fitFormula)), tolerance = tolerance)
  expect_equal(as.vector(coef(fit)), as.vector(coef(gold)), tolerance = tolerance)  
})

test_that("Test ffdf to data for stratified cox using lung dataset", { 
  test <- lung
  test[is.na(test)] <- 0 # Don't want to bother with missing values
  
  gold <- coxph(Surv(time, status) ~ age + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss + strata(sex), test, method="breslow")
  
  cyclopsDataFormula <- createCyclopsDataFrame(Surv(time, status) ~ age + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss + strata(sex), data=test,modelType="cox")
  fitFormula <- fitCyclopsModel(cyclopsDataFormula)
  
  #Convert to data.frames for Cyclops:
  nCovars = 6
  covariates <- data.frame(stratum_id = rep(test$sex,nCovars),
                           row_id = rep(1:nrow(test),nCovars),
                           covariate_id = rep(1:nCovars,each = nrow(test)),
                           covariate_value = c(test$age,test$ph.ecog,test$ph.karno,test$pat.karno,test$meal.cal,test$wt.loss),
                           time = rep(test$time,nCovars),
                           y = rep(test$status-1,nCovars))
  outcomes <- data.frame(stratum_id = test$sex,
                         row_id = 1:nrow(test),
                         y = test$status-1,
                         time = test$time)
  
  #Make sparse:
  covariates <- covariates[covariates$covariate_value != 0,]
  
  #Sort:
  covariates <- covariates[order(covariates$stratum_id,-covariates$time,covariates$y,covariates$row_id),]
  outcomes <- outcomes[order(outcomes$stratum_id,-outcomes$time,outcomes$y,outcomes$row_id),]
  
  #Convert to ffdf:
  covariates <- as.ffdf(covariates)
  outcomes <- as.ffdf(outcomes)
  
  cyclopsData <- createCyclopsData.ffdf(outcomes,covariates,modelType = "cox")
  fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  tolerance <- 1E-4
  expect_equal(as.vector(coef(fit)), as.vector(coef(fitFormula)), tolerance = tolerance)
  expect_equal(as.vector(coef(fit)), as.vector(coef(gold)), tolerance = tolerance)
})
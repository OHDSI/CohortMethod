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
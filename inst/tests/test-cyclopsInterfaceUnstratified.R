library("testthat")
library("survival")

test_that("Test data.frame to data for lr", {
  gold <- glm(case ~ spontaneous + induced, data=infert, family="binomial")    
  
  #Convert infert dataset to Cyclops format:
  covariates <- data.frame(row_id = rep(1:nrow(infert),2),
                           covariate_id = rep(1:2,each=nrow(infert)),
                           covariate_value = c(infert$spontaneous,infert$induced))
  outcomes <- data.frame(row_id = 1:nrow(infert),
                         y = infert$case)
  #Make sparse:
  covariates <- covariates[covariates$covariate_value != 0,]
  
  #Sort:
  covariates <- covariates[order(covariates$row_id,covariates$covariate_id),]
  outcomes <- outcomes[order(outcomes$row_id),]
  
  
  cyclopsData <- createCyclopsData(outcomes,covariates,modelType = "lr",addIntercept = TRUE)
  fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  cyclopsDataFfdf <- createCyclopsData.ffdf(as.ffdf(outcomes),as.ffdf(covariates),modelType = "lr",addIntercept = TRUE)
  fitFfdf <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  tolerance <- 1E-4
  expect_equal(as.vector(coef(fit)), as.vector(coef(gold)), tolerance = tolerance)
  expect_equal(as.vector(coef(fitFfdf)), as.vector(coef(gold)), tolerance = tolerance)
})

test_that("Test unstratified cox using lung dataset ", { 
  test <- lung
  test[is.na(test)] <- 0 # Don't want to bother with missing values
  
  gold <- coxph(Surv(time, status) ~ age + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, test, method="breslow")
  
  cyclopsDataFormula <- createCyclopsDataFrame(Surv(time, status) ~ age + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, data=test,modelType="cox")
  fitFormula <- fitCyclopsModel(cyclopsDataFormula)
  
  #Convert to data.frames for Cyclops:
  nCovars = 6
  covariates <- data.frame(stratum_id = 0,
                           row_id = rep(1:nrow(test),nCovars),
                           covariate_id = rep(1:nCovars,each = nrow(test)),
                           covariate_value = c(test$age,test$ph.ecog,test$ph.karno,test$pat.karno,test$meal.cal,test$wt.loss),
                           time = rep(test$time,nCovars),
                           y = rep(test$status-1,nCovars))
  outcomes <- data.frame(stratum_id = 0,
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
  
  cyclopsDataFfdf <- createCyclopsData.ffdf(as.ffdf(outcomes),as.ffdf(covariates),modelType = "cox")
  fitFfdf <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  
  tolerance <- 1E-4
  expect_equal(as.vector(coef(fit)), as.vector(coef(fitFormula)), tolerance = tolerance)
  expect_equal(as.vector(coef(fit)), as.vector(coef(gold)), tolerance = tolerance)
  expect_equal(as.vector(coef(fitFfdf)), as.vector(coef(gold)), tolerance = tolerance)
})


test_that("Test poisson regression", { 
  sim <- simulateData(nstrata = 1, nrows = 10000, ncovars = 2, eCovarsPerRow = 0.5, effectSizeSd = 1,model = "poisson")
  covariates <- sim$covariates
  outcomes <- sim$outcomes
  
  #Convert to data format for gnm:
  ncovars <- max(covariates$covariate_id)
  nrows <- nrow(outcomes)
  m <- matrix(0,nrows,ncovars)
  for (i in 1:nrow(covariates)){
    m[covariates$row_id[i],covariates$covariate_id[i]] <- 1
  }
  data <- as.data.frame(m)
  
  data$row_id <- 1:nrow(data)
  data <- merge(data,outcomes)
  data <- data[order(data$stratum_id,data$row_id),]
  formula <- as.formula(paste(c("y ~ V1",paste("V",2:ncovars,sep="")),collapse=" + "))
  
  gold <- gnm(formula, family=poisson, offset=log(time), data = data)
  
  cyclopsDataFormula <- createCyclopsDataFrame(formula, offset=log(time), data=data,modelType="pr")
  fitFormula <- fitCyclopsModel(cyclopsDataFormula)
  
  #Sort:
  covariates <- covariates[order(covariates$row_id),]
  outcomes <- outcomes[order(outcomes$row_id),]
  
  cyclopsData <- createCyclopsData(outcomes,covariates,modelType = "pr",addIntercept = TRUE)
  fit <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  cyclopsDataFfdf <- createCyclopsData.ffdf(as.ffdf(outcomes),as.ffdf(covariates),modelType = "pr",addIntercept = TRUE)
  fitFfdf <- fitCyclopsModel(cyclopsData,prior = prior("none"))  
  
  
  tolerance <- 1E-4
  expect_equal(as.vector(sort(coef(fit))), as.vector(sort(coef(fitFormula))), tolerance = tolerance)
  expect_equal(as.vector(sort(coef(fit))), as.vector(sort(coef(gold))), tolerance = tolerance)
  expect_equal(as.vector(sort(coef(fitFfdf))), as.vector(sort(coef(gold))), tolerance = tolerance)
})


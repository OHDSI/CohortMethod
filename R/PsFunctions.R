# @file PsFunctions.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# @author Observational Health Data Sciences and Informatics
# @author Patrick Ryan
# @author Marc Suchard
# @author Martijn Schuemie



#' Create propensity scores
#'
#' @description
#' \code{psCreate} creates propensity scores
#' 
#' @param cohortData        An object of type \code{cohortData} as generated using \code{dbGetCohortData}
#' @param prior             The prior used to fit the model
#'
#' @details
#' todo
#'  
#' @examples 
#' #todo
#' 
#' @export
psCreate <- function(cohortData, prior = prior("laplace", useCrossValidation = TRUE)){
  if (cohortData$useFf){
    cohortData$cohorts$Y <- cohortData$cohorts$TREATMENT
    cyclopsData <- createCyclopsData.ffdf(cohortData$cohorts,cohortData$covariates,modelType="lr")
    ps <- as.ram(cohortData$cohorts[,c("Y","ROW_ID")])
    cohortData$cohorts$y <- NULL
  } else {
    ps <- cohortData$cohorts
    colnames(ps)[colnames(ps) == "TREATMENT"] = "Y"
    cyclopsData <- createCyclopsData(ps,cohortData$covariates,modelType="lr")
  }
  cyclopsFit <- fitCyclopsModel(cyclopsData, 
                        prior = prior,
                        control = control(cvType = "auto", cvRepetitions = 2, noiseLevel = "quiet"))
  pred <- predict(cyclopsFit)

  colnames(ps)[colnames(ps) == "Y"] <- "treatment"
  data <- data.frame(PROPENSITY_SCORE = pred, ROW_ID = as.numeric(attr(pred,"names")))
  data <- merge(data,ps,by="ROW_ID")
  attr(data,"coefficients") <- coef(cyclopsFit)
  data
}

#' Show the propensity model
#'
#' @description
#' \code{psShowModel} shows the propensity score model
#' 
#' @param propensityScore       An object of type \code{cohortData} as generated using \code{dbGetCohortData}
#' @param connectionDetails     An R object of type ConnectionDetail (details for the function that contains server info, database type, optionally username/password, port)
#' @param cdmSchema	        		Database schema that contains the vocabulary
#'
#' @details
#' Shows the coefficients and names of the covariates with non-zero coefficients
#'  
#' @examples 
#' #todo
#' 
#' @export
psShowModel <- function(propensityScore, connectionDetails, cdmSchema){
  
  cfs <- attr(propensityScore,"coefficients")
  cfs <- cfs[cfs != 0]
  attr(cfs,"names")[1] <- 0
  cfs <- data.frame(coefficient = cfs, id = as.numeric(attr(cfs,"names")))
  
  cfs$name <- ""
  
  cfs$name[cfs$id == 0] <- "(Intercept)"
  for (age in 1:11){
    cfs$name[cfs$id == age] <- paste("Age between",(age-1)*10,"and",((age)*10)-1)
  }
  cfs$name[cfs$id == 20] <- "Number of distinct conditions in last 6mo"
  cfs$name[cfs$id == 21] <- "Number of distinct drugs in last 6mo"
  cfs$name[cfs$id == 22] <- "Number of distinct procedures in last 6mo"
  cfs$name[cfs$id == 23] <- "Number of distinct outpatient visits in last 6mo"
  cfs$name[cfs$id == 24] <- "Number of distinct inpatient visits in last 6mo"
  cfs$name[cfs$id == 25] <- "Number of distinct ER visits in last 6mo"
  
  #Get concept names from server:
  conceptIds <- cfs$id[cfs$id > 25 & cfs$id < 2000000000]
  conceptIds <- c(conceptIds, (cfs$id[cfs$id > 2000000000] - 2000000000))
  conceptIds <- unique(conceptIds)
  
  connectionDetails$schema = cdmSchema
  conn <- connect(connectionDetails)
  sql <- "SELECT concept_id AS id,concept_name FROM concept WHERE concept_id IN (@conceptIds)"
  renderedSql <- renderSql(sql,conceptIds = conceptIds)$sql
  conceptNames <- dbGetQuery(conn,renderedSql)
  colnames(conceptNames) <- tolower(colnames(conceptNames))
  dummy <- dbDisconnect(conn)
  
  #Add concept names to explanation:
  cfs <- merge(cfs,conceptNames,by="id", all.x=TRUE)
  cfs$name[!is.na(cfs$concept_name)] <- cfs$concept_name[!is.na(cfs$concept_name)]
  cfs$concept_name <- NULL
  
  cfs$atc3Id <- NA
  cfs$atc3Id[cfs$id > 2000000000] <- cfs$id[cfs$id > 2000000000] - 2000000000
  cfs <- merge(cfs,conceptNames,by.x="atc3Id",by.y="id", all.x=TRUE)
  cfs$name[!is.na(cfs$concept_name)] <- paste("Number of drugs in ATC3 class",cfs$concept_name[!is.na(cfs$concept_name)])
  cfs$concept_name <- NULL
  cfs$atc3Id <- NULL 
  cfs <- cfs[order(-abs(cfs$coefficient)),]
  colnames(cfs)[colnames(cfs) == "id"] <- "covariate_id"
  cfs
}

#' Plot the propensity score distribution
#'
#' @description
#' \code{psPlot} shows the propensity (or preference) score distribution
#' 
#' @param data              A data frame with at least the two columns described below
#' @param scale             The scale of the graph. Two scales are supported: \code{
#' scale = "propensity"} or  \code{#' scale = "preference"}. The preference score scale is defined by Walker 
#' et al (2013). 
#'
#' @details
#' The data frame should have a least the following two columns:
#' \tabular{lll}{  
#'   \verb{treatment}          \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
#'   \verb{propensity_score}   \tab(real)    \tab Propensity score \cr
#' }
#'  
#' @examples 
#' treatment = rep(0:1, each = 100)
#' propensityScore = c(rnorm(100,mean=0.4, sd=0.25),rnorm(100,mean=0.6, sd=0.25))
#' data <- data.frame(treatment = treatment, propensityScore = propensityScore)
#' data <- data[data$propensityScore > 0 & data$propensityScore < 1,]
#' psPlot(data)
#' 
#' @references
#' Walker AM, Patrick AR, Lauer MS, Hornbrook MC, Marin MG, Platt R, Roger VL, Stang P, and Schneeweiss S.
#' (2013) A tool for assessing the feasibility of comparative effectiveness research, Comparative Effective 
#' Research, 3, 11-20
#' 
#' @export
psPlot <- function(data, scale = "preference"){
  colnames(data) <- toupper(colnames(data))
  if (scale == "preference") {
    proportion <- sum(data$TREATMENT) / nrow(data)
    x <- exp(log(data$PROPENSITY_SCORE/(1-data$PROPENSITY_SCORE)) - log(proportion/(1-proportion)))
    data$SCORE <- x / (x+1)
    label = "Preference score"
  } else {
    data$SCORE = data$PROPENSITY_SCORE
    label = "Propensity score"
  }
  data$GROUP <- "Treated"
  data$GROUP[data$TREATMENT == 0] <- "Comparator"
  
  ggplot(data, aes(x=SCORE,color=GROUP,group=GROUP,fill=GROUP)) + 
    geom_density() +
    scale_fill_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) +
    scale_color_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) + 
    scale_x_continuous(label,limits=c(0,1)) +
    scale_y_continuous("Density")
}

#' Compute the area under the ROC curve
#'
#' @description
#' \code{psAuc} shows the area under the ROC curve os the propensity score
#' 
#' @param data              A data frame with at least the two columns described below
#'
#' @details
#' The data frame should have a least the following two columns:
#' \tabular{lll}{  
#'   \verb{treatment}          \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
#'   \verb{propensity_score}   \tab(real)    \tab Propensity score \cr
#' }
#' 
#' @return
#' A data frame holding the AUC and its 95% confidence interval
#'  
#' @examples 
#' treatment = rep(0:1, each = 100)
#' propensityScore = c(rnorm(100,mean=0.4, sd=0.25),rnorm(100,mean=0.6, sd=0.25))
#' data <- data.frame(treatment = treatment, propensityScore = propensityScore)
#' data <- data[data$propensityScore > 0 & data$propensityScore < 1,]
#' psAuc(data)
#' 
#' @export
psAuc <- function(data){
  colnames(data) <- toupper(colnames(data))
  rocobj <- roc(data$TREATMENT,data$PROPENSITY_SCORE, algorithm=3)
  auc <- as.numeric(ci.auc(rocobj, method="delong"))
  data.frame(auc=auc[2],auc_lb95ci=auc[1],auc_lb95ci=auc[3])
}

#' Trim persons by propensity score
#'
#' @description
#' \code{psTrim} uses the provided propensity scores to trim subjects with extreme scores.
#' 
#' @param data              A data frame with the three columns described below#' @param method    	      Whether \code{matching} or \code{stratification} should be performed.
#' @param trimFraction      This fraction will be removed from each treatment group. In the treatment group, persons
#' with the highest propensity scores will be removed, in the comparator group person with the lowest scores will be removed.
#' 
#' @details
#' The data frame should have the following three columns:
#' \tabular{lll}{  
#'   \verb{row_id}             \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr
#'   \verb{treatment}  	       \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
#'   \verb{propensity_score}   \tab(real)    \tab Propensity score \cr
#' }
#' 
#' @return Returns a date frame with the same three columns as the input.
#' @examples 
#' rowId = 1:2000
#' treatment = rep(0:1, each = 1000)
#' propensityScore = c(runif(1000,min=0,max=1),runif(1000,min=0,max=1))
#' data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
#' result <- psTrim(data,0.05)
#' 
#' @export
psTrim <- function(data, trimFraction=0.05){
  colnames(data) <- toupper(colnames(data))
  cutoffTreated <- quantile(data$PROPENSITY_SCORE[data$TREATMENT == 1],1-trimFraction)
  cutoffComparator <- quantile(data$PROPENSITY_SCORE[data$TREATMENT == 0],trimFraction)
  data[(data$PROPENSITY_SCORE <= cutoffTreated & data$TREATMENT == 1) | (data$PROPENSITY_SCORE >= cutoffComparator & data$TREATMENT == 0),]
}


#' Match persons by propensity score
#'
#' @description
#' \code{psMatch} uses the provided propensity scores to match treated to comparator persons.
#' 
#' @param data              A data frame with the three columns described below
#' @param caliper		        The caliper for matching. A caliper is the distance which is acceptable for 
#' any match. Observations which are outside of the caliper are dropped. A caliper of 0 means no caliper is used.
#' @param caliperScale      The scale on which the caliper is defined. Two scales are supported: \code{caliperScale = "propensity score"}
#' or  \code{caliperScale = "standardized"}. On the standardized scale, the 
#' caliper is interpreted in standard deviations of the propensity score distribution.
#' @param maxRatio		    The maximum number of persons int the comparator arm to be matched to each person in the treatment arm. A 
#' maxRatio of 0 means no maximum: all comparators will be assigned to a treated person.
#' 
#' @details
#' The data frame should have the following three columns:
#' \tabular{lll}{  
#'   \verb{row_id}  	          \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr
#'   \verb{treatment}  	      \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
#'   \verb{propensity_score}   \tab(real)    \tab Propensity score \cr
#' }
#' 
#' This function implements the greedy variable-ratio matching algorithm described in Rassen et al (2012).
#' 
#' @return Returns a date frame with the same columns as the input data plus one extra column: stratumId.
#' Any rows that could not be matched are removed 
#' 
#' @examples 
#' rowId = 1:5
#' treatment = c(1,0,1,0,1)
#' propensityScore = c(0,0.1,0.3,0.4,1)
#' data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
#' result <- psMatch(data, caliper = 0, maxRatio = 1)
#' 
#' @references
#' Rassen JA, Shelat AA, Myers J, Glynn RJ, Rothman KJ, Schneeweiss S. (2012) One-to-many propensity score matching in 
#' cohort studies, Pharmacoepidemiology and Drug Safety, May, 21 Suppl 2:69-80.
#' 
#' @export
psMatch <- function(data, caliper = 0.25, caliperScale = "standardized", maxRatio = 1){
  colnames(data) <- toupper(colnames(data))
  data <- data[order(data$PROPENSITY_SCORE),]
  if (caliper <= 0)
    caliper = 9999
  else if (caliperScale == "standardized")
    caliper = caliper * sd(data$PROPENSITY_SCORE)
  if (maxRatio == 0) {
    maxRatio = 999
  } 
  
  result <- .Call('CohortMethod_matchOnPs', PACKAGE = 'CohortMethod', data$PROPENSITY_SCORE, data$TREATMENT, maxRatio, caliper)
  result$ROW_ID <- data$ROW_ID
  result[result$STRATUM_ID != -1,]
}

#' Stratify persons by propensity score
#'
#' @description
#' \code{psStratify} uses the provided propensity scores to stratify persons. 
#' 
#' @param data              A data frame with the three columns described below
#' @param numberOfStrata    How many strata? The boundaries of the strata are automatically defined to 
#' contain equal numbers of treated persons.
#' 
#' @details
#' The data frame should have the following three columns:
#' \tabular{lll}{  
#'   \verb{rowId}             \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr
#'   \verb{treatment}         \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
#'   \verb{propensityScore}   \tab(real)    \tab Propensity score \cr
#' }
#' 
#' @return Returns a date frame with the same columns as the input data plus one extra column: stratumId.
#' @examples 
#' rowId = 1:200
#' treatment = rep(0:1, each = 100)
#' propensityScore = c(runif(100,min=0,max=1),runif(100,min=0,max=1))
#' data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
#' result <- psStratify(data,5)
#' 
#' @export
psStratify <- function(data, numberOfStrata=5){
  colnames(data) <- toupper(colnames(data))
  strata <- quantile(data$PROPENSITY_SCORE[data$TREATMENT == 1],(1:(numberOfStrata-1))/numberOfStrata)
  data$stratumId <- cut(data$PROPENSITY_SCORE,breaks=c(0,strata,1), labels = (1:numberOfStrata)-1)
  data
}





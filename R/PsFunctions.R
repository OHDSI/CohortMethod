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

in.ff <- function(a,b){
  return(ffmatch(x=a, table=b, nomatch = 0L) > 0L)
}

#' Create propensity scores
#'
#' @description
#' \code{psCreate} creates propensity scores using a regularized logistic regression.
#' 
#' @param cohortData        An object of type \code{cohortData} as generated using \code{dbGetCohortData}.
#' @param outcomeConceptId  The concept ID of the outcome. Persons marked for removal for the outcome will be removed prior to
#' creating the propensity score model.
#' @param prior             The prior used to fit the model.
#'
#' @details
#' \code{psCreate} creates propensity scores using a regularized logistic regression.
#'  
#' @examples 
#' #todo
#' 
#' @export
psCreate <- function(cohortData, outcomeConceptId = NULL, prior = prior("laplace", useCrossValidation = TRUE)){
  if (is.null(outcomeConceptId)){
    cohortSubset <- cohortData$cohorts
    covariateSubset <- subset(cohortData$covariates,COVARIATE_ID != 1)
  } else {
    t <- in.ff(cohortData$cohorts$ROW_ID ,cohortData$exclude$ROW_ID[cohortData$exclude$OUTCOME_ID == outcomeConceptId])
    cohortSubset <- cohortData$cohort[ffwhich(t,t == FALSE),]
    t <- in.ff(cohortData$covariates$ROW_ID ,cohortData$exclude$ROW_ID[cohortData$exclude$OUTCOME_ID == outcomeConceptId])
    t <- t | cohortData$covariates$COVARIATE_ID == 1
    covariateSubset <- cohortData$covariates[ffwhich(t,t == FALSE),]
  }
  colnames(cohortSubset)[colnames(cohortSubset) == "TREATMENT"] <- "Y"
  cyclopsData <- createCyclopsData.ffdf(cohortSubset,covariateSubset,modelType="lr")
  ps <- as.ram(cohortSubset[,c("Y","ROW_ID")])
  cyclopsFit <- fitCyclopsModel(cyclopsData, 
                                prior = prior,
                                control = control(cvType = "auto", cvRepetitions = 2, noiseLevel = "quiet"))
  pred <- predict(cyclopsFit)
  
  colnames(ps)[colnames(ps) == "Y"] <- "treatment"
  data <- data.frame(PROPENSITY_SCORE = pred, ROW_ID = as.numeric(attr(pred,"names")))
  data <- merge(data,ps,by="ROW_ID")
  attr(data,"coefficients") <- coef(cyclopsFit)
  return(data)
}

#' Get the propensity model
#'
#' @description
#' \code{psGetModel} shows the propensity score model
#' 
#' @param propensityScore       The propensity scores as generated using the \code{psCreate} function.
#' @param cohortData            An object of type \code{cohortData} as generated using \code{dbGetCohortData}.
#'
#' @details
#' Shows the coefficients and names of the covariates with non-zero coefficients.
#'  
#' @examples 
#' #todo
#' 
#' @export
psGetModel <- function(propensityScore, cohortData){
  cfs <- attr(propensityScore,"coefficients")
  cfs <- cfs[cfs != 0]
  #attr(cfs,"names")[1] <- 0
  cfs <- data.frame(coefficient = cfs, id = as.numeric(attr(cfs,"names")))
  
  cfs <- merge(as.ffdf(cfs),cohortData$covariateRef,by.x="id",by.y="COVARIATE_ID")
  cfs <- as.ram(cfs[,c("coefficient","id","COVARIATE_NAME")])
  cfs <- cfs[order(-abs(cfs$coefficient)),]
  colnames(cfs) <- toupper(colnames(cfs))
  return(cfs)
}

computePreferenceScore <- function(data, unfilteredData = NULL){
  if (is.null(unfilteredData))
    proportion <- sum(data$TREATMENT) / nrow(data)
  else #Proportion may have changed, but we still want to use the old proportion
    proportion <- sum(unfilteredData$TREATMENT) / nrow(unfilteredData)
  x <- exp(log(data$PROPENSITY_SCORE/(1-data$PROPENSITY_SCORE)) - log(proportion/(1-proportion)))
  data$PREFERENCE_SCORE <- x / (x+1)
  return(data)
}

#' Plot the propensity score distribution
#'
#' @description
#' \code{psPlot} shows the propensity (or preference) score distribution
#' 
#' @param data              A data frame with at least the two columns described below
#' @param unfilteredData    To be used when computing preference scores on data from which subjects have
#' already been removed, e.g. through trimming and/or matching. This data frame should have the same 
#' structure as \code{data}.
#' @param scale             The scale of the graph. Two scales are supported: \code{
#' scale = "propensity"} or  \code{scale = "preference"}. The preference score scale is defined by Walker 
#' et al (2013). 
#' @param type              Type of plot. Two possible values: \code{type = "density"} or \code{type = "histogram"}
#' @param binWidth          For histograms, the width of the bins
#' @param fileName        Name of the file where the plot should be saved, for example 'plot.png'. See 
#' the function \code{ggsave} in the ggplot2 package for supported file formats.
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
#' propensity_score = c(rnorm(100,mean=0.4, sd=0.25),rnorm(100,mean=0.6, sd=0.25))
#' data <- data.frame(treatment = treatment, propensity_score = propensity_score)
#' data <- data[data$propensity_score > 0 & data$propensity_score < 1,]
#' psPlot(data)
#' 
#' @references
#' Walker AM, Patrick AR, Lauer MS, Hornbrook MC, Marin MG, Platt R, Roger VL, Stang P, and Schneeweiss S.
#' (2013) A tool for assessing the feasibility of comparative effectiveness research, Comparative Effective 
#' Research, 3, 11-20
#' 
#' @export
psPlot <- function(data, unfilteredData = NULL, scale = "preference", type = "density", binWidth = 0.05, fileName=NULL){
  require(ggplot2)
  colnames(data) <- toupper(colnames(data))
  if (!("TREATMENT" %in% colnames(data))) 
    stop("Missing column TREATMENT in data")
  if (!("PROPENSITY_SCORE" %in% colnames(data))) 
    stop("Missing column PROPENSITY_SCORE in data")
  if (!is.null(unfilteredData)){
    colnames(unfilteredData) <- toupper(colnames(unfilteredData))
    if (!("TREATMENT" %in% colnames(unfilteredData))) 
      stop("Missing column TREATMENT in unfilteredData")
    if (!("PROPENSITY_SCORE" %in% colnames(unfilteredData))) 
      stop("Missing column PROPENSITY_SCORE in unfilteredData")   
  }
  
  if (scale == "preference") {
    data <- computePreferenceScore(data,unfilteredData)
    data$SCORE <- data$PREFERENCE_SCORE
    label = "Preference score"
  } else {
    data$SCORE = data$PROPENSITY_SCORE
    label = "Propensity score"
  }
  data$GROUP <- "Treated"
  data$GROUP[data$TREATMENT == 0] <- "Comparator"
  if (type == "density"){
    plot = ggplot(data, aes(x=SCORE,color=GROUP,group=GROUP,fill=GROUP)) + 
      geom_density() +
      scale_fill_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) +
      scale_color_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) + 
      scale_x_continuous(label,limits=c(0,1)) +
      scale_y_continuous("Density")
  } else {
    plot = ggplot(data, aes(x=SCORE,color=GROUP,group=GROUP,fill=GROUP)) + 
      geom_histogram(binwidth = binWidth, position="identity") +
      scale_fill_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) +
      scale_color_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) + 
      scale_x_continuous(label,limits=c(0,1)) +
      scale_y_continuous("Number of subjects")
  }
  if (!is.null(fileName))
    ggsave(fileName,plot,width=5,height=3.5,dpi=400) 
  return(plot)
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
#' propensity_score = c(rnorm(100,mean=0.4, sd=0.25),rnorm(100,mean=0.6, sd=0.25))
#' data <- data.frame(treatment = treatment, propensity_score = propensity_score)
#' data <- data[data$propensity_score > 0 & data$propensity_score < 1,]
#' psAuc(data)
#' 
#' @export
psAuc <- function(data){
  require(pROC)
  colnames(data) <- toupper(colnames(data))
  if (!("TREATMENT" %in% colnames(data))) 
    stop("Missing column TREATMENT in data")
  if (!("PROPENSITY_SCORE" %in% colnames(data))) 
    stop("Missing column PROPENSITY_SCORE in data")
  
  rocobj <- roc(data$TREATMENT,data$PROPENSITY_SCORE, algorithm=3)
  auc <- as.numeric(ci.auc(rocobj, method="delong"))
  return(data.frame(auc=auc[2],auc_lb95ci=auc[1],auc_lb95ci=auc[3]))
}

#' Trim persons by propensity score
#'
#' @description
#' \code{psTrim} uses the provided propensity scores to trim subjects with extreme scores.
#' 
#' @param data              A data frame with the three columns described below
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
#' row_id = 1:2000
#' treatment = rep(0:1, each = 1000)
#' propensity_score = c(runif(1000,min=0,max=1),runif(1000,min=0,max=1))
#' data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
#' result <- psTrim(data,0.05)
#' 
#' @export
psTrim <- function(data, trimFraction=0.05){
  colnames(data) <- toupper(colnames(data))
  if (!("ROW_ID" %in% colnames(data))) 
    stop("Missing column ROW_ID in data")
  if (!("TREATMENT" %in% colnames(data))) 
    stop("Missing column TREATMENT in data")
  if (!("PROPENSITY_SCORE" %in% colnames(data))) 
    stop("Missing column PROPENSITY_SCORE in data")
  cutoffTreated <- quantile(data$PROPENSITY_SCORE[data$TREATMENT == 1],1-trimFraction)
  cutoffComparator <- quantile(data$PROPENSITY_SCORE[data$TREATMENT == 0],trimFraction)
  result <- data[(data$PROPENSITY_SCORE <= cutoffTreated & data$TREATMENT == 1) | (data$PROPENSITY_SCORE >= cutoffComparator & data$TREATMENT == 0),]
  return(result)
}

#' Keep only persons in clinical equipoise
#'
#' @description
#' \code{psTrimToEquipoise} uses the preference score to trim subjects that are not in clinical equipoise
#' 
#' @param data              A data frame with at least the three columns described below
#' @param bounds            The upper and lower bound on the preference score for keeping persons
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
#' row_id = 1:2000
#' treatment = rep(0:1, each = 1000)
#' propensity_score = c(runif(1000,min=0,max=1),runif(1000,min=0,max=1))
#' data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
#' result <- psTrimToEquipoise(data)
#'  
#' @references
#' Walker AM, Patrick AR, Lauer MS, Hornbrook MC, Marin MG, Platt R, Roger VL, Stang P, and Schneeweiss S.
#' (2013) A tool for assessing the feasibility of comparative effectiveness research, Comparative Effective 
#' Research, 3, 11-20
#' 
#' @export
psTrimToEquipoise <- function(data,bounds=c(0.25,0.75)){
  colnames(data) <- toupper(colnames(data))
  if (!("ROW_ID" %in% colnames(data))) 
    stop("Missing column ROW_ID in data")
  if (!("TREATMENT" %in% colnames(data))) 
    stop("Missing column TREATMENT in data")
  if (!("PROPENSITY_SCORE" %in% colnames(data))) 
    stop("Missing column PROPENSITY_SCORE in data")
  
  data <- computePreferenceScore(data)
  return(data[data$PREFERENCE_SCORE >= bounds[1] & data$PREFERENCE_SCORE <= bounds[2],])
}

#' Match persons by propensity score
#'
#' @description
#' \code{psMatch} uses the provided propensity scores to match treated to comparator persons.
#' 
#' @param data              A data frame with the three columns described below.
#' @param caliper		        The caliper for matching. A caliper is the distance which is acceptable for 
#' any match. Observations which are outside of the caliper are dropped. A caliper of 0 means no caliper is used.
#' @param caliperScale      The scale on which the caliper is defined. Two scales are supported: \code{caliperScale = "propensity score"}
#' or  \code{caliperScale = "standardized"}. On the standardized scale, the 
#' caliper is interpreted in standard deviations of the propensity score distribution.
#' @param maxRatio		    The maximum number of persons int the comparator arm to be matched to each person in the treatment arm. A 
#' maxRatio of 0 means no maximum: all comparators will be assigned to a treated person.
#' @param stratificationColumns   Names of one or more columns in the \code{data} data.frame on which subjects should be stratified prior to matching.
#' No persons will be matched with persons outside of the strata identified by the values in these columns.
#' 
#' @details
#' The data frame should have at least the following three columns:
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
#' row_id = 1:5
#' treatment = c(1,0,1,0,1)
#' propensity_score = c(0,0.1,0.3,0.4,1)
#' age_group =c(1,1,1,1,1) #everyone in the same age group, so will not influence the matching
#' data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score, age_group = age_group)
#' result <- psMatch(data, caliper = 0, maxRatio = 1, stratificationColumns = "age_group")
#' 
#' @references
#' Rassen JA, Shelat AA, Myers J, Glynn RJ, Rothman KJ, Schneeweiss S. (2012) One-to-many propensity score matching in 
#' cohort studies, Pharmacoepidemiology and Drug Safety, May, 21 Suppl 2:69-80.
#' 
#' @export
psMatch <- function(data, caliper = 0.25, caliperScale = "standardized", maxRatio = 1, stratificationColumns = c()){
  colnames(data) <- toupper(colnames(data))
  stratificationColumns <- toupper(stratificationColumns)
  if (!("ROW_ID" %in% colnames(data))) 
    stop("Missing column ROW_ID in data")
  if (!("TREATMENT" %in% colnames(data))) 
    stop("Missing column TREATMENT in data")
  if (!("PROPENSITY_SCORE" %in% colnames(data))) 
    stop("Missing column PROPENSITY_SCORE in data")
  
  data <- data[order(data$PROPENSITY_SCORE),]
  if (caliper <= 0){
    caliper = 9999
  } else if (caliperScale == "standardized")
    caliper = caliper * sd(data$PROPENSITY_SCORE)
  if (maxRatio == 0) {
    maxRatio = 999
  } 
  if (length(stratificationColumns) == 0) {
    result <- .Call('CohortMethod_matchOnPs', PACKAGE = 'CohortMethod', data$PROPENSITY_SCORE, data$TREATMENT, maxRatio, caliper)
    result$ROW_ID <- data$ROW_ID
    return(result[result$STRATUM_ID != -1,])
  } else {
    result <- data.frame()
    interactions <- interaction(data[,stratificationColumns])
    strata <- levels(interactions)
    for (i in 1:length(strata)){
      subset <- data[interactions == strata[i],]
      subResult <- .Call('CohortMethod_matchOnPs', PACKAGE = 'CohortMethod', subset$PROPENSITY_SCORE, subset$TREATMENT, maxRatio, caliper)
      subResult$ROW_ID <- subset$ROW_ID
      subResult[,stratificationColumns] <- subset[,stratificationColumns]
      subResult <- subResult[subResult$STRATUM_ID != -1,]
      if (nrow(result) != 0)
        subResult$STRATUM_ID = subResult$STRATUM_ID + max(result$STRATUM_ID) + 1
      result <- rbind(result,subResult)
    }
    return(result)
  }
}

#' Stratify persons by propensity score
#'
#' @description
#' \code{psStratify} uses the provided propensity scores to stratify persons. 
#' 
#' @param data              A data frame with the three columns described below
#' @param numberOfStrata    How many strata? The boundaries of the strata are automatically defined to 
#' contain equal numbers of treated persons.
#' @param stratificationColumns   Names of one or more columns in the \code{data} data.frame on which subjects should also be 
#' stratified in addition to stratification on propensity score.
#' 
#' @details
#' The data frame should have the following three columns:
#' \tabular{lll}{  
#'   \verb{row_id}              \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr
#'   \verb{treatment}           \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
#'   \verb{propensity_score}    \tab(real)    \tab Propensity score \cr
#' }
#' 
#' @return Returns a date frame with the same columns as the input data plus one extra column: stratumId.
#' @examples 
#' row_id = 1:200
#' treatment = rep(0:1, each = 100)
#' propensity_score = c(runif(100,min=0,max=1),runif(100,min=0,max=1))
#' data <- data.frame(row_id = row_id, treatment = treatment, propensity_score = propensity_score)
#' result <- psStratify(data,5)
#' 
#' @export
psStratify <- function(data, numberOfStrata=5, stratificationColumns = c()){
  colnames(data) <- toupper(colnames(data))
  stratificationColumns <- toupper(stratificationColumns)
  if (!("ROW_ID" %in% colnames(data))) 
    stop("Missing column ROW_ID in data")
  if (!("TREATMENT" %in% colnames(data))) 
    stop("Missing column TREATMENT in data")
  if (!("PROPENSITY_SCORE" %in% colnames(data))) 
    stop("Missing column PROPENSITY_SCORE in data")
  
  psStrata <- quantile(data$PROPENSITY_SCORE[data$TREATMENT == 1],(1:(numberOfStrata-1))/numberOfStrata)
  if (length(stratificationColumns) == 0) {
    data$STRATUM_ID <- as.integer(as.character(cut(data$PROPENSITY_SCORE,breaks=c(0,psStrata,1), labels = (1:numberOfStrata)-1)))
    return(data)
  } else {
    result <- data.frame()
    interactions <- interaction(data[,stratificationColumns])
    strata <- levels(interactions)
    for (i in 1:length(strata)){
      subset <- data[interactions == strata[i],]
      subset$STRATUM_ID <- as.integer(as.character(cut(subset$PROPENSITY_SCORE,breaks=c(0,psStrata,1), labels = (1:numberOfStrata)-1)))
      if (nrow(result) != 0)
        subset$STRATUM_ID = subset$STRATUM_ID + max(result$STRATUM_ID) + 1
      result <- rbind(result,subset)
    }
    return(result)
  }
}

quickSum <- function(data,squared=FALSE){
  result <- NULL
  for (i in chunk(data)){
    dataChunk <- data[i,]
    if (squared)
      x <- bySum(dataChunk$COVARIATE_VALUE^2,as.factor(dataChunk$COVARIATE_ID))
    else
      x <- bySum(dataChunk$COVARIATE_VALUE,as.factor(dataChunk$COVARIATE_ID))
    COVARIATE_ID <- attr(x,"dimnames")
    attributes(x) <- NULL
    r <- data.frame(value = x,COVARIATE_ID = COVARIATE_ID, stringsAsFactors=FALSE)
    colnames(r)[2] <- "COVARIATE_ID" #for some reason we lose the name when stringsAsFactors = FALSE
    if (is.null(result)){
      result <- r
      colnames(result)[colnames(result) == "value"] = "S"
    } else {
      result <- merge(result,r,all=TRUE)
      result$S[is.na(result$S)] = 0
      result$value[is.na(result$value)] = 0
      result$S = result$S + result$value
      result$value <- NULL
    }
  }
  if (squared)
    colnames(result)[colnames(result) == "S"] = "SUM_SQR"
  else
    colnames(result)[colnames(result) == "S"] = "SUM"
  
  result$COVARIATE_ID <- as.numeric(result$COVARIATE_ID)
  return(result)
}


computeMeansPerGroup <- function(cohorts, covariates){
  nOverall <- nrow(cohorts)
  nTreated <-  sum(cohorts$TREATMENT == 1)
  nComparator <- nOverall - nTreated
  
  t <- in.ff(covariates$ROW_ID,cohorts$ROW_ID[cohorts$TREATMENT == 1])
  treated <- quickSum(covariates[ffwhich(t,t == TRUE),])
  treated$MEAN_TREATED <- treated$SUM / nTreated
  colnames(treated)[colnames(treated) == "SUM"] <- "SUM_TREATED"
  
  t <- in.ff(covariates$ROW_ID,cohorts$ROW_ID[cohorts$TREATMENT == 0])
  comparator <- quickSum(covariates[ffwhich(t,t == TRUE),])
  comparator$MEAN_COMPARATOR <- comparator$SUM / nComparator
  colnames(comparator)[colnames(comparator) == "SUM"] <- "SUM_COMPARATOR"
  
  t <- in.ff(covariates$ROW_ID,cohorts$ROW_ID)
  overall <- quickSum(covariates[ffwhich(t,t == TRUE),])
  overallSqr <- quickSum(covariates[ffwhich(t,t == TRUE),],squared=TRUE)
  overall <- merge(overall,overallSqr)
  overall$SD <- sqrt((overall$SUM_SQR - (overall$SUM^2/nOverall))/nOverall)
  overall <- data.frame(COVARIATE_ID = overall$COVARIATE_ID,SD = overall$SD)
  
  result <- merge(treated,comparator)
  result <- merge(result,overall)
  return(result)
}


#' Compute covariate balance before and after matching and trimming
#'
#' @description
#' For every covariate, prevalence in treatment and comparator groups before and after matching/trimming are computed.
#' 
#' @param restrictedCohorts  A data frame containing the people that are remaining after matching and/or trimming.
#' @param cohortData        An object of type \code{cohortData} as generated using \code{dbGetCohortData}.
#' @param outcomeConceptId  The concept ID of the outcome. Persons marked for removal for the outcome will be removed 
#' when computing the balance before matching/trimming.
#' 
#' @details
#' The restrictedCohorts data frame should have at least the following three columns:
#' \tabular{lll}{  
#'   \verb{row_id}              \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr
#'   \verb{treatment}           \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
#' }
#' 
#' @return Returns a date frame describing the covariate balance before and after matching/trimming.
#' 
#' @export
psComputeCovariateBalance <- function (restrictedCohorts, cohortData, outcomeConceptId = NULL) {
  colnames(restrictedCohorts) <- toupper(colnames(restrictedCohorts))
  if (is.null(outcomeConceptId)){
    cohorts <- cohortData$cohorts
    covariates <- subset(cohortData$covariates,COVARIATE_ID != 1)
  } else {
    t <- in.ff(cohortData$cohorts$ROW_ID ,cohortData$exclude$ROW_ID[cohortData$exclude$OUTCOME_ID == outcomeConceptId])
    cohorts <- cohortData$cohort[ffwhich(t,t == FALSE),]
    t <- in.ff(cohortData$covariates$ROW_ID ,cohortData$exclude$ROW_ID[cohortData$exclude$OUTCOME_ID == outcomeConceptId])
    t <- t | cohortData$covariates$COVARIATE_ID == 1
    covariates <- cohortData$covariates[ffwhich(t,t == FALSE),]
  }
  
  beforeMatching <- computeMeansPerGroup(cohorts,covariates)
  afterMatching <- computeMeansPerGroup(as.ffdf(restrictedCohorts),covariates)
  
  colnames(beforeMatching)[colnames(beforeMatching) == "MEAN_TREATED"] <- "BEFORE_MATCHING_MEAN_TREATED"
  colnames(beforeMatching)[colnames(beforeMatching) == "MEAN_COMPARATOR"] <- "BEFORE_MATCHING_MEAN_COMPARATOR"
  colnames(beforeMatching)[colnames(beforeMatching) == "SUM_TREATED"] <- "BEFORE_MATCHING_SUM_TREATED"
  colnames(beforeMatching)[colnames(beforeMatching) == "SUM_COMPARATOR"] <- "BEFORE_MATCHING_SUM_COMPARATOR"
  colnames(beforeMatching)[colnames(beforeMatching) == "SD"] <- "BEFORE_MATCHING_SD"
  colnames(afterMatching)[colnames(afterMatching) == "MEAN_TREATED"] <- "AFTER_MATCHING_MEAN_TREATED"
  colnames(afterMatching)[colnames(afterMatching) == "MEAN_COMPARATOR"] <- "AFTER_MATCHING_MEAN_COMPARATOR"
  colnames(afterMatching)[colnames(afterMatching) == "SUM_TREATED"] <- "AFTER_MATCHING_SUM_TREATED"
  colnames(afterMatching)[colnames(afterMatching) == "SUM_COMPARATOR"] <- "AFTER_MATCHING_SUM_COMPARATOR"
  colnames(afterMatching)[colnames(afterMatching) == "SD"] <- "AFTER_MATCHING_SD"
  balance <- merge(beforeMatching,afterMatching)
  balance <- merge(balance,as.ram(cohortData$covariateRef))
  balance$BEFORE_MATCHING_STD_DIFF <- (balance$BEFORE_MATCHING_MEAN_TREATED-balance$BEFORE_MATCHING_MEAN_COMPARATOR)/balance$BEFORE_MATCHING_SD
  balance$AFTER_MATCHING_STD_DIFF <- (balance$AFTER_MATCHING_MEAN_TREATED-balance$AFTER_MATCHING_MEAN_COMPARATOR)/balance$AFTER_MATCHING_SD
  balance$BEFORE_MATCHING_STD_DIFF[balance$BEFORE_MATCHING_SD == 0] <- 0
  balance$AFTER_MATCHING_STD_DIFF[balance$BEFORE_MATCHING_SD == 0] <- 0
  balance <- balance[order(-abs(balance$BEFORE_MATCHING_STD_DIFF)),]
  return(balance)
}

#' Create a scatterplot of the covariate balance
#'
#' @description
#' Create a scatterplot of the covariate balance, showing all variables with balance before and after 
#' matching on the x and y axis respectively. Requires running \code{psComputeCovariateBalance} first.
#' 
#' @param balance  A data frame created by the \code{psComputeCovariateBalance} funcion.
#' @param fileName  Name of the file where the plot should be saved, for example 'plot.png'. See the 
#' function \code{ggsave} in the ggplot2 package for supported file formats.
#' 
#' @export
psPlotCovariateBalanceScatterPlot <- function(balance, fileName=NULL) {
  require(ggplot2)
  balance$BEFORE_MATCHING_STD_DIFF <- abs(balance$BEFORE_MATCHING_STD_DIFF)
  balance$AFTER_MATCHING_STD_DIFF <- abs(balance$AFTER_MATCHING_STD_DIFF)
  limits=c(0,max(c(balance$BEFORE_MATCHING_STD_DIFF,balance$AFTER_MATCHING_STD_DIFF)))
  plot <- ggplot(balance, aes(x=BEFORE_MATCHING_STD_DIFF,y=AFTER_MATCHING_STD_DIFF)) + 
    geom_point(color = rgb(0,0,0.8,alpha=0.3)) +
    geom_abline(a = 1) + 
    geom_hline(yintercept = 0) + 
    ggtitle("Standardized difference of mean") +
    scale_x_continuous("Before matching",limits=limits) +
    scale_y_continuous("After matching",limits=limits)
  if (!is.null(fileName))
    ggsave(fileName,plot,width=4,height=4,dpi=400) 
  return(plot)
}  

#' Plot variables with largest imbalance
#'
#' @description
#' Create a plot showing those variables having the largest imbalance before matching, and
#' those variables having the largest imbalance after matching. Requires running 
#' \code{psComputeCovariateBalance} first.
#' 
#' @param balance  A data frame created by the \code{psComputeCovariateBalance} funcion.
#' @param fileName  Name of the file where the plot should be saved, for example 'plot.png'. See the 
#' function \code{ggsave} in the ggplot2 package for supported file formats.
#' 
#' @export
psPlotCovariateBalanceTopVariables <- function(balance, n = 20, fileName=NULL) {
  require(ggplot2)
  topBefore <- balance[order(-abs(balance$BEFORE_MATCHING_STD_DIFF)),]
  topBefore <- topBefore[1:n,]
  topBefore$facet <- paste("Top",n,"before matching")
  topAfter <- balance[order(-abs(balance$AFTER_MATCHING_STD_DIFF)),]
  topAfter <- topAfter[1:n,]
  topAfter$facet <- paste("Top",n,"after matching")
  filtered <- rbind(topBefore,topAfter)
  
  data <- data.frame(covariate_id = rep(filtered$COVARIATE_ID,2),
                     covariate = rep(filtered$COVARIATE_NAME,2), 
                     difference = c(filtered$BEFORE_MATCHING_STD_DIFF,filtered$AFTER_MATCHING_STD_DIFF), 
                     group = rep(c("before matching","after matching"),each=nrow(filtered)),
                     facet = rep(filtered$facet,2),
                     rowId = rep(nrow(filtered):1,2))
  data$facet <- factor(data$facet, levels = rev(levels(data$facet)))
  data$group <- factor(data$group, levels = rev(levels(data$group)))
  plot <- ggplot(data, aes(x=difference,y=rowId,color=group,group=group,fill=group,shape=group)) + 
    geom_point() +
    geom_vline(xintercept = 0) + 
    scale_fill_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) +
    scale_color_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) +
    scale_x_continuous("Standardized difference of mean") +
    scale_y_continuous(breaks = nrow(filtered):1, labels = filtered$COVARIATE_NAME) +
    facet_grid(facet ~ ., scales="free", space="free") +
    theme (
      axis.text.y = element_text(size=7),
      axis.title.y = element_blank(),
      legend.position="top",
      legend.title = element_blank()
    )
  if (!is.null(fileName))
    ggsave(fileName,plot,width=10,height=max(2+n*0.2,5),dpi=400) 
  return(plot)
}


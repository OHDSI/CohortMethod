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

#' Plot the propensity score distribution
#'
#' @description
#' \code{psPlot} shows the propensity (or preference) score distribution
#' 
#' @param data              A data frame with at least the two columns described below
#' @param scale             The scale of the graph. Two scales are supported: \code{
#' scale = "propensity"} or  \code{#' scale = "preference"}. The preference score scale is defined by Walker 
#' et al (2013). 

#' @details
#' The data frame should have a least the following two columns:
#' \tabular{lll}{  
#'   \verb{treatment}          \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
#'   \verb{propensityScore}   \tab(real)    \tab Propensity score \cr
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
  if (scale == "preference") {
    proportion <- sum(data$treatment) / nrow(data)
    x <- exp(log(data$propensityScore/(1-data$propensityScore)) - log(proportion/(1-proportion)))
    data$score <- x / (x+1)
    label = "Preference score"
  } else {
    data$score = data$propensityScore
    label = "Propensity score"
  }
  data$Group <- "Treated"
  data$Group[data$treatment == 0] <- "Comparator"
  
  ggplot(data, aes(x=score,color=Group,group=Group,fill=Group)) + 
    geom_density() +
    scale_fill_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) +
    scale_color_manual(values=c(rgb(0.8,0,0,alpha=0.5),rgb(0,0,0.8,alpha=0.5))) + 
    scale_x_continuous(label) +
    scale_y_continuous("Density")
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
#'   \verb{rowId}             \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr
#'   \verb{treatment}  	      \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
#'   \verb{propensityScore}   \tab(real)    \tab Propensity score \cr
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
  cutoffTreated <- quantile(data$propensityScore[data$treatment == 1],1-trimFraction)
  cutoffComparator <- quantile(data$propensityScore[data$treatment == 0],trimFraction)
  data[(data$propensityScore <= cutoffTreated & data$treatment == 1) | (data$propensityScore >= cutoffComparator & data$treatment == 0),]
}


#' Match persons by propensity score
#'
#' @description
#' \code{psMatch} uses the provided propensity scores to match treated to comparator persons.
#' 
#' @param data              A data frame with the three columns described below
#' @param caliper		        The caliper for matching. A caliper is the distance which is acceptable for 
#' any match. Observations which are outside of the caliper are dropped. A caliper of 0 means no caliper is used.
#' @param caliperScale      The scale on which the caliper is defined. Two scales are supported: \code{
#' caliperScale = "propensity score"} or  \code{#' caliperScale = "standardized"}. On the standardized scale, the 
#' caliper is interpreted in standard deviations of the propensity score distribution.
#' @param maxRatio		    The maximum number of persons int the comparator arm to be matched to each person in the treatment arm.
#' 
#' @details
#' The data frame should have the following three columns:
#' \tabular{lll}{  
#'   \verb{rowId}  	          \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr
#'   \verb{treatment}  	      \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
#'   \verb{propensityScore}   \tab(real)    \tab Propensity score \cr
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
psMatch <- function(data, caliper = 0, caliperScale = "propensity score", maxRatio = 1){
  data <- data[order(data$propensityScore),]
  if (caliper <= 0)
    caliper = 9999
  else if (caliperScale == "standardized")
    caliper = caliper * sd(data$propensityScore)
  
  result <- .Call('CohortMethod_matchOnPs', PACKAGE = 'CohortMethod', data$propensityScore, data$treatment, data$rowId, maxRatio, caliper)
  result[result$stratumId != -1,]
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
  strata <- quantile(data$propensityScore[data$treatment == 1],(1:(numberOfStrata-1))/numberOfStrata)
  data$stratumId <- cut(data$propensityScore,breaks=c(0,strata,1), labels = (1:numberOfStrata)-1)
  data
}





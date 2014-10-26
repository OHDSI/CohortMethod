# @file CohortMethod.R
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

#' Create an outcome model, and compute the relative risk
#'
#' @description
#' \code{estimateEffect} creates an outcome model, and computes the relative risk
#' 
#' @param cohortData          An object of type \code{cohortData} as generated using \code{dbGetCohortData}
#' @param strata              A data frame specifying the strata. This data frame should have at least the following columns:
#' \code{RowId} and \code{StratumId}
#' @param riskWindowEnd       The maximum length (in days) of the risk window.
#' @param useCovariates       Whether to use the covariate matrix in the cohortData in the outcome model
#' @param modelType           The type of model to be fitted. See details for options
#' 
#'
#' @details
#' The data frame should have a least the following two columns:
#' \tabular{ll}{  
#'   \verb{lr}      \tab Logistic regression  \cr
#'   \verb{clr}     \tab Conditional logistic regression \cr
#'   \verb{cox}     \tab Cox regression (stratified or not, depending on whether \code{stata} is specified) \cr
#'   \verb{pr}      \tab Poisson regression  \cr
#'   \verb{cpr}     \tab Conditional Poisson regression \cr
#' }
#' 
#' @return
#' A data frame holding the effect estimate
#'  
#' @examples 
#' #todo
#' 
#' @export
estimateEffect <- function(outcomeConceptId,
                           cohortData,
                           strata=NULL, 
                           riskWindowStart = 0,
                           riskWindowEnd = 9999, 
                           addExposureDaysToEnd = FALSE,
                           useCovariates = TRUE, 
                           fitModel = TRUE,
                           getDemographics = FALSE,
                           modelType = "cox"){
  if (modelType != "cox")
    stop("Currently only Cox model is implemented")
  useStrata = (modelType == "clr" | modelType == "cpr" | (modelType == "cox" & !is.null(strata)))
  
  outcomes <- subset(cohortData$outcomes,OUTCOME_ID == as.double(outcomeConceptId))
  colnames(outcomes) <- toupper(colnames(outcomes))
  
  t <- in.ff(cohortData$cohorts$ROW_ID ,cohortData$exclude$ROW_ID[cohortData$exclude$OUTCOME_ID == outcomeConceptId])
  cohorts <- as.ram(cohortData$cohort[ffwhich(t,t == FALSE),])
  colnames(cohorts) <- toupper(colnames(cohorts))
  
  if (useStrata) {
    colnames(strata) <- toupper(colnames(strata))
    cohorts <- merge(strata,cohorts) #keeping only persons that have been matched
  }
  
  if (useCovariates){
    covariates <- cohortData$covariates
    colnames(covariates) <- toupper(colnames(covariates))  
  }
  #Censor outcomes outside of risk window:
  cohorts$TIME_TO_CENSOR <- riskWindowEnd
  if (addExposureDaysToEnd)
    cohorts$TIME_TO_CENSOR <- cohorts$TIME_TO_CENSOR + cohorts$TIME_TO_COHORT_END
  cohorts$TIME_TO_CENSOR[cohorts$TIME_TO_CENSOR > cohorts$TIME_TO_OBS_PERIOD_END] <-  cohorts$TIME_TO_OBS_PERIOD_END
  outcomes <- merge(outcomes,as.ffdf(cohorts))
  outcomes <- subset(outcomes, TIME_TO_EVENT >= riskWindowStart & TIME_TO_EVENT <= TIME_TO_CENSOR)  
  
  df <- NULL
  demo <- NULL
  
  if (modelType == "cox"){
    outcomes <- aggregate(TIME_TO_EVENT ~ ROW_ID,data=outcomes,min) #keep first outcome per person
    data <- merge(cohorts,outcomes, all.x=TRUE)
    data$Y <- 0
    data$Y[!is.na(data$TIME_TO_EVENT)] <- 1
    data$TIME <- data$TIME_TO_EVENT
    data$TIME[is.na(data$TIME)] <- data$TIME_TO_CENSOR[is.na(data$TIME)]
    data <- data[data$TIME > 0,]
    
    if (useCovariates) { 
      if (useStrata){
        data <- data[order(data$STRATUM_ID,-data$TIME,data$Y,data$ROW_ID),]
        covariates <- merge(covariates,as.ffdf(data[,c("ROW_ID","Y","TIME","STRATUM_ID")]))
        covariates$MINTIME <- 0-covariates$TIME
        covariates <- covariates[ffdforder(covariates[c("STRATUM_ID","MINTIME","Y","ROW_ID")]),]
        if (fitModel) {
          cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cox")
          fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1, exclude=1))  
          ci <- confint(fit,parm=1)
          df <- data.frame(LOGRR=coef(fit)[names(coef(fit)) == "1"], LOGLB95 = ci[2], LOGUB95 = ci[3])
        }
      } else {
        data <- data[order(data$ROW_ID,-data$TIME,data$Y),]
        covariates <- merge(covariates,as.ffdf(data[,c("ROW_ID","Y","TIME")]))
        covariates$MINTIME <- 0-covariates$TIME
        covariates <- covariates[ffdforder(covariates[c("ROW_ID","MINTIME","Y")]),]
        if (fitModel) {
          cyclopsData <- createCyclopsData.ffdf(as.ffdf(data),covariates,modelType="cox")
          fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1, exclude=1))  
          ci <- confint(fit,parm=1)
          df <- data.frame(LOGRR=coef(fit)[names(coef(fit)) == "1"], LOGLB95 = ci[2], LOGUB95 = ci[3])
        }
      }
      
    } else {# don't use covariates  
      if (useStrata){
        if (fitModel) {
          cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT + strata(STRATUM_ID),data=data, modelType = "cox")
          fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1, exclude=1))
          ci <- confint(fit,parm=1)
          df <- data.frame(LOGRR=coef(fit)[names(coef(fit)) == "TREATMENT"], LOGLB95 = ci[2], LOGUB95 = ci[3])
        }
      } else {
        if (fitModel) {
          cyclopsData <- createCyclopsDataFrame(Surv(TIME, Y) ~ TREATMENT,data=data, modelType = "cox")
          fit <- fitCyclopsModel(cyclopsData, prior=prior("laplace",0.1, exclude=1))
          ci <- confint(fit,parm=1)
          df <- data.frame(LOGRR=coef(fit)[names(coef(fit)) == "TREATMENT"], LOGLB95 = ci[2], LOGUB95 = ci[3])        
        }
      }
    }
    if (getDemographics) {
      demo <- data      
    }
  }
  return(list(estimates = df, demographics = demo))
}

getSummaryStatistics <- function(data,
                     modelType = "cox",
                     plot = FALSE,
                     timeBy = 1000) {
  labels <- c("Comparator", "Treated")
  
  patientTable <- table(data$TREATMENT)
  eventTable <- table(data$TREATMENT, data$Y)
  timeTable <- aggregate(TIME ~ TREATMENT, FUN = sum, data = data)[,2]
  
  tmp <- matrix(0, nrow=3, ncol=2)
  tmp[1,] <- patientTable
  tmp[2,] <- eventTable[,2]
  tmp[3,] <- timeTable
  
  km <- NULL
  
  if (plot && modelType == "cox") {
    km <- .ggkm(survfit(Surv(TIME, Y) ~ TREATMENT, data), 
                pval = FALSE, 
                timeby = timeBy, 
                ystratalabs=c("Comparator","Treated"), return = TRUE)  
  }
  
  return (list(info = data.frame(Comparator = tmp[,1], 
                                 Treated = tmp[,2],
                                 row.names = c("Patients","Events","Time")),
               plot = km))
}


#' Create a Kaplan-Meier plot using ggplot2
#'
#' @param sfit: a survfit object
#' @param table: logical: Create a table graphic below the K-M plot, indicating at-risk numbers?
#' @param returns logical: if TRUE, return an arrangeGrob object
#' @param xlabs: x-axis label
#' @param ylabs: y-axis label
#' @param ystratalabs: The strata labels. Default = levels(summary(sfit)$strata)
#' @param ystrataname: The legend name. Default = "Strata"
#' @param timeby numeric: control the granularity along the time-axis
#' @param main plot title
#' @param pval logical: add the pvalue to the plot?
#' @param marks logical: should censoring marks be added?
#' @param shape: what shape should the censoring marks be, default is a vertical line
#' @param legend logical: should a legend be added to the plot?
#' 
#' @return a ggplot is made. if return=TRUE, then an arrangeGlob object
#' is returned
#' @author Abhijit Dasgupta with contributions by Gil Tomas
#' \url{http://statbandit.wordpress.com/2011/03/08/an-enhanced-kaplan-meier-plot/}
#' slight adjustment to cope with none strata calls (e.g. Surv(time,event)~1), 
#' option to remove the legend and also draw marks at censoring locations by Nadieh Bremer
#' 
#' @examples
#'  library(survival)
#'  data(colon)
#'  fit <- survfit(Surv(time,status)~rx, data=colon)
#'  ggkm(fit, timeby=500)

.ggkm <- function(sfit,
                 table = TRUE,
                 returns = FALSE,
                 xlabs = "Time",
                 ylabs = "Survival Probability",
                 xlims = c(0,max(sfit$time)),
                 ylims = c(0,1),
                 ystratalabs = NULL,
                 ystrataname = NULL,
                 timeby = 100,
                 main = "Kaplan-Meier Plot",
                 pval = TRUE,
                 marks = FALSE,
                 shape = 3,
                 legend = TRUE,
                 subs = NULL,
                 ...) {
  
  #############
  # libraries #
  #############
  
  #Check if the following packages have been installed. If not, install them
  if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
  if (!"survival" %in% installed.packages()) install.packages("survival")
  if (!"gridExtra" %in% installed.packages()) install.packages("gridExtra")
  if (!"reshape" %in% installed.packages()) install.packages("reshape")
  
  suppressPackageStartupMessages(library(ggplot2, warn.conflicts=FALSE))
  suppressPackageStartupMessages(library(survival, warn.conflicts=FALSE))
  suppressPackageStartupMessages(library(gridExtra, warn.conflicts=FALSE))
  suppressPackageStartupMessages(library(reshape, warn.conflicts=FALSE))
  
  #################################
  # sorting the use of subsetting #
  #################################
  
  times <- seq(0, max(sfit$time), by = timeby)
  
  if(is.null(subs)){
    if(length(levels(summary(sfit)$strata)) == 0) {
      subs1 <- 1
      subs2 <- 1:length(summary(sfit,censored=T)$time)
      subs3 <- 1:length(summary(sfit,times = times,extend = TRUE)$time)
    } else {
      subs1 <- 1:length(levels(summary(sfit)$strata))
      subs2 <- 1:length(summary(sfit,censored=T)$strata)
      subs3 <- 1:length(summary(sfit,times = times,extend = TRUE)$strata)
    }
  } else{
    for(i in 1:length(subs)){
      if(i==1){
        ssvar <- paste("(?=.*\\b=",subs[i],sep="")
      }
      if(i==length(subs)){
        ssvar <- paste(ssvar,"\\b)(?=.*\\b=",subs[i],"\\b)",sep="")
      }
      if(!i %in% c(1, length(subs))){
        ssvar <- paste(ssvar,"\\b)(?=.*\\b=",subs[i],sep="")
      }
      if(i==1 & i==length(subs)){
        ssvar <- paste("(?=.*\\b=",subs[i],"\\b)",sep="")
      }
    }
    subs1 <- which(regexpr(ssvar,levels(summary(sfit)$strata), perl=T)!=-1)
    subs2 <- which(regexpr(ssvar,summary(sfit,censored=T)$strata, perl=T)!=-1)
    subs3 <- which(regexpr(ssvar,summary(sfit,times = times,extend = TRUE)$strata, perl=T)!=-1)
  }
  
  if(!is.null(subs)) pval <- FALSE
  
  ##################################
  # data manipulation pre-plotting #
  ##################################
  
  if(length(levels(summary(sfit)$strata)) == 0) {
    #[subs1]
    if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","","All"))
  } else {
    #[subs1]
    if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","",names(sfit$strata)))
  }
  
  if(is.null(ystrataname)) ystrataname <- "Strata"
  m <- max(nchar(ystratalabs))
  times <- seq(0, max(sfit$time), by = timeby)
  
  if(length(levels(summary(sfit)$strata)) == 0) {
    Factor <- factor(rep("All",length(subs2)))
  } else {
    Factor <- factor(summary(sfit, censored = T)$strata[subs2])
  }
  
  #Data to be used in the survival plot
  .df <- data.frame(
    time = sfit$time[subs2],
    n.risk = sfit$n.risk[subs2],
    n.event = sfit$n.event[subs2],
    n.censor = sfit$n.censor[subs2],
    surv = sfit$surv[subs2],
    strata = Factor,
    upper = sfit$upper[subs2],
    lower = sfit$lower[subs2]
  )
  
  #Final changes to data for survival plot
  levels(.df$strata) <- ystratalabs
  zeros <- data.frame(time = 0, surv = 1,
                      strata = factor(ystratalabs, levels=levels(.df$strata)),
                      upper = 1, lower = 1)
  .df <- rbind.fill(zeros, .df)
  d <- length(levels(.df$strata))
  
  ###################################
  # specifying plot parameteres etc #
  ###################################
  
  p <- ggplot( .df, aes(time, surv)) +
    geom_step(aes(linetype = strata), size = 0.7) +
    theme_bw() +
    theme(axis.title.x = element_text(vjust = 0.5)) +
    scale_x_continuous(xlabs, breaks = times, limits = xlims) +
    scale_y_continuous(ylabs, limits = ylims) +
    theme(panel.grid.minor = element_blank()) +
    # MOVE LEGEND HERE BELOW [first is x dim, second is y dim]
    theme(legend.position = c(ifelse(m < 10, .85, .75),ifelse(d < 4, .85, .8))) +
    theme(legend.key = element_rect(colour = NA)) +
    theme(panel.border = element_blank()) +
    labs(linetype = ystrataname) +
    theme(plot.margin = unit(c(0, 1, .5,ifelse(m < 10, 1.5, 2.5)),"lines")) +
    ggtitle(main)
  
  #Removes the legend: 
  if(legend == FALSE) 
    p <- p + theme(legend.position="none")
  
  #Add censoring marks to the line:
  if(marks == TRUE)
    p <- p + geom_point(data = subset(.df, n.censor >= 1), aes(x = time, y = surv), shape = shape)
  
  ## Create a blank plot for place-holding
  blank.pic <- ggplot(.df, aes(time, surv)) +
    geom_blank() + theme_bw() +
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),panel.border = element_blank())
  
  #####################
  # p-value placement #
  #####################a
  
  if(length(levels(summary(sfit)$strata)) == 0) pval <- FALSE
  
  if(pval == TRUE) {
    sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
    pvalue <- pchisq(sdiff$chisq,length(sdiff$n) - 1,lower.tail = FALSE)
    pvaltxt <- ifelse(pvalue < 0.0001,"p < 0.0001",paste("p =", signif(pvalue, 3)))
    # MOVE P-VALUE LEGEND HERE BELOW [set x and y]
    p <- p + annotate("text",x = 150, y = 0.1,label = pvaltxt)
  }#if
  
  ###################################################
  # Create table graphic to include at-risk numbers #
  ###################################################
  
  if(length(levels(summary(sfit)$strata)) == 0) {
    Factor <- factor(rep("All",length(subs3)))
  } else {
    Factor <- factor(summary(sfit,times = times,extend = TRUE)$strata[subs3])
  }
  
  if(table) {
    risk.data <- data.frame(
      strata = Factor,
      time = summary(sfit,times = times,extend = TRUE)$time[subs3],
      n.risk = summary(sfit,times = times,extend = TRUE)$n.risk[subs3]
    )
    risk.data$strata <- factor(risk.data$strata, levels=rev(levels(risk.data$strata)))
    
    data.table <- ggplot(risk.data,aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +
      geom_text(size = 3.5) + theme_bw() +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)),
                       labels = rev(ystratalabs)) +
      scale_x_continuous("Numbers at risk", limits = xlims) +
      theme(axis.title.x = element_text(size = 10, vjust = 1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),axis.text.x = element_blank(),
            axis.ticks = element_blank(),axis.text.y = element_text(face = "bold",hjust = 1))
    
    data.table <- data.table +
      theme(legend.position = "none") + xlab(NULL) + ylab(NULL)
    
    # ADJUST POSITION OF TABLE FOR AT RISK
    data.table <- data.table +
      theme(plot.margin = unit(c(-1.5, 1, 0.1, ifelse(m < 10, 2.5, 3.5) - 0.15 * m), "lines"))
    
    #######################
    # Plotting the graphs #
    #######################
    
    grid.arrange(p, blank.pic, data.table, clip = FALSE, nrow = 3,
                 ncol = 1, heights = unit(c(2, .1, .25),c("null", "null", "null")))
    
    if(returns) {
      a <- arrangeGrob(p, blank.pic, data.table, clip = FALSE, nrow = 3,
                       ncol = 1, heights = unit(c(2, .1, .25), c("null", "null", "null")))
      return(a)
    }#if
  } else {
    if(returns) return(p)
  }#else
}

##################################################################################################
##################################################################################################
##################################################################################################

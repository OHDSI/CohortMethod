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

#' @export
createOutcomeModel <- function(cohortData,
                               strata=NULL, 
                               riskWindowEnd = 9999, 
                               useCovariates = FALSE, 
                               modelType = "cox"){
  outcomes <- cohortData$outcomes
  colnames(outcomes) <- toupper(colnames(outcomes))
  colnames(outcomes)[colnames(outcomes) == "ROW_ID"] <- "ROWID"
  colnames(strata) <- toupper(colnames(strata))
  cohorts <- cohortData$cohorts
  colnames(cohorts) <- toupper(colnames(cohorts))
  colnames(cohorts)[colnames(cohorts) == "ROW_ID"] <- "ROWID"
  if (modelType == "cox"){
    data <- merge(cohorts[,c("ROWID","TIME_TO_CENSOR","TREATMENT")],outcomes,by="ROWID",all.x=TRUE)
    data$Y[is.na(data$Y)] <- 0
    data$Y[data$Y != 0] <- 1
    data$TIME <- data$TIME_TO_OUTCOME
    data$TIME[is.na(data$TIME)] <- data$TIME_TO_CENSOR[is.na(data$TIME)]
    data$Y[data$TIME > riskWindowEnd] <- 0
    data$TIME[data$TIME > riskWindowEnd] <- riskWindowEnd
    data$TIME = data$TIME + 1
    data$START <- 0
    if (useCovariates) { # To implement: CCD cox regression (stratified and unstratified)
      
    } else {
      if (is.null(strata)){ # Unstratified Cox regression
        fit <- coxph( Surv(START, TIME, Y) ~ TREATMENT,data=data ) 
        fit
      } else { # Stratified Cox regression
        data <- merge(data,strata[,c("ROWID","STRATUMID")],by="ROWID")
        fit <- coxph( Surv(START, TIME, Y) ~ TREATMENT + strata(STRATUMID),data=data ) 
        fit
      }
    }
  }
}
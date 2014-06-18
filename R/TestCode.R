testCode <- function(){
  
  pw <- ""
  
  setwd("c:/temp")
  connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
  cdmSchema = "cdm4_sim"
  
  #Test on PostgreSQL
  setwd("c:/temp")
  connectionDetails <- createConnectionDetails(dbms="postgresql", server="localhost/ohdsi", user="postgres",password=pw)
  cdmSchema = "cdm4_sim"
  
  
  
  df <- data.frame(stratum_id = c(1,1,1,2,2,2), row_id = c(1,1,2,3,3,4), covariate_id = c(1,2,3,1,2,3), value = c(1,1,1,1,1,1))
  covariateInputForPs <- as.ffdf(df)  
  
  createRow <- function(data){
    covarString = paste(paste(data$covariate_id,data$value, sep=":"),collapse=" ")
    data.frame(stratum_id = data$stratum_id[1], row_id = data$row_id[1], covarString = covarString)
  }
  
  doDdply <- function(data){
    ddply(data, .(row_id), createRow)
    
  }
  #covariateInputForPs <- ffdfsort(covariateInputForPs,covariateInputForPs$rowid)
  
  
  covariatesForPS <- ffdfdply(covariateInputForPs, as.character(covariateInputForPs$row_id), doDdply, trace=TRUE) 
  
  
 by(df, as.character(df$row_id), createRow)
  
 
 ddply(df, .(row_id), createRow)
}
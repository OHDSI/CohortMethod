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
  ffd <- ffdf <- as.ffdf(df)  
  
  createRow <- function(data){
    paste(data$stratum_id[1],data$row_id[1],paste(paste(data$covariate_id,data$value, sep=":"),collapse=" "),sep=" ")
  }
  
  by(df, df$row_id, createRow) 
}
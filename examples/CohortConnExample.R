library(CohortMethod)

# Login info.
pw <- Sys.getenv("MYPGPASSWORD")
dbms <- "redshift"
user <- Sys.getenv("USER")
server <- "omop-datasets.cqlmv7nlakap.us-east-1.redshift.amazonaws.com/truven"
cdmSchema <- "mslr_cdm4"
port <- "5439"

cohortConn <- CohortConn$new(
                pw=pw,
                dbms=dbms,
                user=user,
                server=server,
                cdmSchema=cdmSchema,
                port=port
              )

cohortConn$connect()
cohortConn$disconnect()

library(CohortMethod)

# Login info.
pw <- Sys.getenv("MYPGPASSWORD")
dbms <- "redshift"
user <- Sys.getenv("USER")
server <- "omop-datasets.cqlmv7nlakap.us-east-1.redshift.amazonaws.com/truven"
cdmSchema <- "mslr_cdm4"
port <- "5439"

# Drug/condition info.
Erythromycin = 1746940
Amoxicillin = 1713332

rifaximin = 1735947
Lactulose = 987245

celecoxib = 1118084
rofecoxib = 1189754

Cyclosporine = 9010482
Tacrolimus = 950637

MyocardialInfarction = 35205189


# Begin analysis.
cohortConn <- CohortConn$new(
                pw=pw,
                dbms=dbms,
                user=user,
                server=server,
                cdmSchema=cdmSchema,
                port=port
              )

cohortConn$connect()

cohortConn$buildCohorts(Erythromycin, Amoxicillin, MyocardialInfarction)
cohortConn$getCohortSize()
cohortConn$balanceCohorts()

cohortConn$disconnect()

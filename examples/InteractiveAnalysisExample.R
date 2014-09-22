library(CohortMethod)

# Login info.
pw <- Sys.getenv("MYPGPASSWORD")
dbms <- "redshift"
user <- Sys.getenv("USER")
server <- "omop-datasets.cqlmv7nlakap.us-east-1.redshift.amazonaws.com/truven"
cdmSchema <- "mslr_cdm4"
port <- "5439"

Erythromycin = 1746940
Amoxicillin = 1713332

connectionDetails <- createConnectionDetails(
                        dbms=dbms,
                        server=server,
                        user=user,
                        password=pw,
                        schema=cdmSchema,
                        port=port)


conn <- connect(connectionDetails)

setSearchPath(conn)

buildCohorts(conn, Erythromycin, Amoxicillin)

getCohortSize(conn)

balanceCohorts(conn)

# Takes a long time.
buildCovariates(conn)

buildOutcomes(conn)

dbDisconnect(conn)

library(CohortMethod)
setwd("/tmp")

# Connection parameters.
connparams <- list(
    "pw" = Sys.getenv("MYPGPASSWORD"),
    "dbms" = "redshift",
    "user" = Sys.getenv("USER"),
    "server" =
    "omop-datasets.cqlmv7nlakap.us-east-1.redshift.amazonaws.com/truven",
    "port" = "5439"
)

# Model parameters.
dataparams <- list(
    "cdmSchema" = "ccae_cdm4",
    "resultsSchema" = "ccae_cdm4",
    "washoutWindow" = 365,
    "indicationLookbackWindow" = 365,
    "useCovariateDrugExposure" = TRUE,
    "useCovariateDrugEra" = TRUE,
    "useCovariateDrugGroup" = TRUE
)

# Drugs studied.
drugs <- list(
    "erythromycin" = 1746940,
    "amoxicillin" = 1713332,

    "rifaximin" = 1735947,
    "Lactulose" = 987245,

    "celecoxib" = 1118084,
    "rofecoxib" = 1189754,

#    "Cyclosporine" = 9010482,
#    "Tacrolimus" = 950637,

    "clopidogrel" = 1322184,
    "Metoprolol" = 1307046
)

# Conditions studied.
conditions <- list(
    "myocardialinfarction" = 35205189
)


# Actual analysis.
runanalysis <- function() {
    mylogger <- Mylogger(logfilename = "/tmp/analysis")
    
    mylogger$log(paste("New analysis:", Sys.time(), "\n"))

    mylogger$log("Drugs studied:")
    mylogger$log(listtostring(drugs))

    mylogger$log("Conditions studied:")
    mylogger$log(listtostring(conditions))

    mylogger$log("Data parameters:")
    mylogger$log(listtostring(dataparams))
    
    connectionDetails <- createConnectionDetails(
        dbms = connparams$dbms,
        server = connparams$server,
        user = connparams$user,
        password = connparams$pw,
        schema = dataparams$cdmSchema,
        port = connparams$port
    )

    drugnames <- names(drugs)
    numdrugs <- length(drugnames)

    numconditions <- length(conditions)
    
    for (i in 1:numdrugs) {
        for (j in 1:numconditions) {
            targetDrugConceptId = drugs[2*i - 1]
            comparatorDrugConceptId = drugs[2*i]
            indicationConceptIds = conditions$myocardialinfarction

            targetDrugConceptIdname = drugnames[2*i - 1]
            comparatorDrugConceptIdname = drugnames[2*i]

            output <- sprintf(
                "Analyzing %s and %s",
                targetDrugConceptIdname,
                comparatorDrugConceptIdname
            )

            print(output)
            mylogger$log(output)

            mylogger$log("Starting timer.")
            mylogger$getelapsedtime() # Reset time.

            result <- dbGetCohortData(
                connectionDetails,
                cdmSchema=dataparams$cdmSchema,
                resultsSchema=dataparams$cdmSchema,
                targetDrugConceptId = targetDrugConceptId,
                comparatorDrugConceptId = comparatorDrugConceptId,
                indicationConceptIds = indicationConceptIds,
                washoutWindow = dataparams$washoutWindow,
                indicationLookbackWindow = dataparams$indicationLookbackWindow,
                useCovariateDrugExposure = dataparams$useCovariateDrugExposure,
                useCovariateDrugEra = dataparams$useCovariateDrugEra,
                useCovariateDrugGroup = dataparams$useCovariateDrugGroup
            )

            cohortData = result$cohortData

            mylogger$log("Size of cohorts.")
            cohortSize = as.list.data.frame(result$cohortSize)
            mylogger$log(listtostring(cohortSize))

            mylogger$log("Number of covariates.")
            covariateSize = as.list.data.frame(result$covariateSize)
            mylogger$log(listtostring(covariateSize))

            output = sprintf(
                "Downloading data took: %s",
                mylogger$getelapsedtime()
            )
            mylogger$log(output)

            ps <- psCreate(cohortData, prior=prior("laplace",0.1))
            output = sprintf(
                "Computing propensity score took: %s",
                mylogger$getelapsedtime()
            )
            mylogger$log(output)

            auc <- as.list.data.frame(psAuc(ps))
            output = sprintf(
                "Computing AUC took: %s",
                mylogger$getelapsedtime()
            )
            mylogger$log(output)
            mylogger$log(listtostring(auc))
        }
    }


}


# Logger functionality.
getelapsedtime <- function() {
    newtime <- Sys.time()
    elapsedtime <- newtime - oldtime
    oldtime <<- newtime
    format(elapsedtime)
}

logger <- function(text) {
    f = file(logfilename, open="a")
    write(text, f)
    close(f)
}

Mylogger <- setRefClass(
                    "Mylogger",
                    fields=list(
                        oldtime = "POSIXt",
                        logfilename = "character"
                    ),
                    prototype=list(
                        connected = FALSE
                    ),
                    methods=list(
                        initialize = function(..., connected=FALSE) {
                            callSuper(..., oldtime=Sys.time(),
                                           logfilename="/tmp/log")
                        },
                        getelapsedtime = getelapsedtime,
                        log = logger
                    )
)

listtostring <- function(listinput) {
    keys <- attributes(listinput)$names
    outstring <- ""
    for (i in 1:length(keys)) {
        key = keys[i]
        value = listinput[key]
        key = toString(key)
        value = toString(value)
        outstring <- paste(
            outstring,
            toString(key),
            ":",
            toString(value),
            "\n")
    }
    return(outstring)
}


# Actually run the analysis.
runanalysis()

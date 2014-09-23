library(DatabaseConnector)

oldconnect = connect

connect <- function() {
  connectionDetails <- createConnectionDetails(
                          dbms=dbms,
                          server=server,
                          user=user,
                          password=pw,
                          schema=cdmSchema,
                          port=port)
  conn <<- oldconnect(connectionDetails)
  connected <<- TRUE
}

disconnect <- function() {
  dbDisconnect(conn)
  connected <<- FALSE
}

#' @export
CohortConn <- setRefClass(
                  "CohortConn",
                  fields=list(
                      pw = "character",
                      dbms = "character",
                      user = "character",
                      server = "character",
                      cdmSchema = "character",
                      port = "character",
                      connected = "logical",
                      conn = "ANY"
                  ),
                  prototype=list(
                      connected = FALSE
                  ),
                  methods=list(
                      initialize = function(..., connected=FALSE) {
                          callSuper(..., connected=FALSE)
                      },
                      connect = connect,
                      disconnect = disconnect
                  )
              )

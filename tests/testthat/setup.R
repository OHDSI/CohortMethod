library(Eunomia)
connectionDetails <- getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

withr::defer(testthat::teardown_env(), {
  # Remove the Eunomia database:
  unlink(connectionDetails$server())
})
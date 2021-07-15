library(Eunomia)
connectionDetails <- getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

withr::defer({
  # Remove the Eunomia database:
  unlink(connectionDetails$server())
}, testthat::teardown_env())
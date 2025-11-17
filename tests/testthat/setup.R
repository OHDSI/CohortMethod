library(CohortMethod)
library(testthat)

if (!isFALSE(tryCatch(find.package("Eunomia"), error = function(e) FALSE))) {

  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  Eunomia::createCohorts(connectionDetails)

  if (is_checking()) {
    withr::defer(
      {
        unlink(connectionDetails$server())
      },
      testthat::teardown_env()
    )
  }
}


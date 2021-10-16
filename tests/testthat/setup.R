library(Eunomia)
connectionDetails <- getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

withr::defer({
  # Remove the Eunomia database:
  unlink(connectionDetails$server())
  if (getOption("use.devtools.sql_shim", FALSE)){
    # Remove symbolic link to sql folder created when devtools::test loads helpers
    packageRoot <- normalizePath(system.file("..", package = "CohortMethod"))
    unlink(file.path(packageRoot, "sql"), recursive = FALSE)
  }
}, testthat::teardown_env())

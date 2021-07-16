# When devtools::load_all is run, create symbolic link for sql directory
# Allows testing with devtools::test
if (Sys.getenv("DEVTOOLS_LOAD") == "true") {
  print("setting sql folder symobolic link")
  packageRoot <- normalizePath(system.file("..", package = "CohortMethod"))
  # Create symbolic link so code can be used in devtools::test()
  R.utils::createLink(link = file.path(packageRoot, "sql"), system.file("sql", package = "CohortMethod"))
  options("use.devtools.sql_shim" = TRUE)
}

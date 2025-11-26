library(CohortMethod)
library(testthat)

if (Sys.getenv("CDM5_POSTGRESQL_SERVER") != "") {

  if (dir.exists(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))) {
    jdbcDriverFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
  } else {
    jdbcDriverFolder <- "~/.jdbcDrivers"
    dir.create(jdbcDriverFolder, showWarnings = FALSE)
    DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)
    withr::defer(
      {
        unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
      },
      testthat::teardown_env()
    )
  }

  postgresConnectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    user = Sys.getenv("CDM5_POSTGRESQL_USER"),
    password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
    server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
    pathToDriver = jdbcDriverFolder
  )

  postgresResultsDatabaseSchema <- paste0("r", Sys.getpid(), format(Sys.time(), "%s"), sample(1:100, 1))

  sqliteFile <- tempfile(fileext = ".sqlite")
  sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteFile
  )
  sqliteResultsDatabaseSchema <- "main"

  # Disabling DuckDB unit tests until SqlRender 1.19.4 has been released:
  # duckDbFile <- tempfile(fileext = ".duckdb")
  # duckDbConnectionDetails <- DatabaseConnector::createConnectionDetails(
  #   dbms = "duckdb",
  #   server = duckDbFile
  # )
  # duckDbResultsDatabaseSchema <- "main"

  withr::defer({
    connection <- DatabaseConnector::connect(connectionDetails = postgresConnectionDetails)
    sql <- "DROP SCHEMA IF EXISTS @resultsDatabaseSchema CASCADE;"
    DatabaseConnector::renderTranslateExecuteSql(
      sql = sql,
      resultsDatabaseSchema = postgresResultsDatabaseSchema,
      connection = connection
    )

    DatabaseConnector::disconnect(connection)
    unlink(sqliteFile, force = TRUE)
    # unlink(duckDbFile, force = TRUE)
  },
  testthat::teardown_env()
  )

  testCreateSchema <- function(connectionDetails, resultsDatabaseSchema) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
    if (!connectionDetails$dbms %in% c("sqlite", "duckdb")) {
      sql <- "CREATE SCHEMA @resultsDatabaseSchema;"
      DatabaseConnector::renderTranslateExecuteSql(
        sql = sql,
        resultsDatabaseSchema = resultsDatabaseSchema,
        connection = connection
      )
    }
    suppressWarnings(
      createResultsDataModel(
        connectionDetails = connectionDetails,
        databaseSchema = resultsDatabaseSchema,
        tablePrefix = ""
      )
    )
    specifications <- getResultsDataModelSpecifications()
    for (tableName in unique(specifications$tableName)) {
      expect_true(DatabaseConnector::existsTable(connection = connection,
                                                 databaseSchema = resultsDatabaseSchema,
                                                 tableName = tableName))
    }
    # Bad schema name
    expect_error(createResultsDataModel(
      connectionDetails = connectionDetails,
      databaseSchema = "non_existant_schema"
    ))
  }

  test_that("Create schema", {
    testCreateSchema(connectionDetails = postgresConnectionDetails,
                     resultsDatabaseSchema = postgresResultsDatabaseSchema)
    testCreateSchema(connectionDetails = sqliteConnectionDetails,
                     resultsDatabaseSchema = sqliteResultsDatabaseSchema)
    # testCreateSchema(connectionDetails = duckDbConnectionDetails,
    #                 resultsDatabaseSchema = duckDbResultsDatabaseSchema)
  })

  testUploadResults <- function(connectionDetails, resultsDatabaseSchema) {
    uploadResults(
      connectionDetails = connectionDetails,
      schema = resultsDatabaseSchema,
      zipFileName = system.file("Results_Eunomia.zip", package = "CohortMethod"),
      purgeSiteDataBeforeUploading = FALSE)

    # Check if there's data:
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))

    specifications <- getResultsDataModelSpecifications()
    for (tableName in unique(specifications$tableName)) {
      primaryKey <- specifications |>
        dplyr::filter(tableName == !!tableName &
                        primaryKey == "Yes") |>
        dplyr::select("columnName") |>
        dplyr::pull()

      if ("database_id" %in% primaryKey) {
        sql <- "SELECT COUNT(*) FROM @database_schema.@table_name WHERE database_id = '@database_id';"
        databaseIdCount <- DatabaseConnector::renderTranslateQuerySql(
          connection = connection,
          sql = sql,
          database_schema = resultsDatabaseSchema,
          table_name = tableName,
          database_id = "Eunomia"
        )[, 1]
        expect_true(databaseIdCount >= 0)
      }
    }
  }

  test_that("Results upload", {
    testUploadResults(connectionDetails = postgresConnectionDetails,
                      resultsDatabaseSchema = postgresResultsDatabaseSchema)
    testUploadResults(connectionDetails = sqliteConnectionDetails,
                      resultsDatabaseSchema = sqliteResultsDatabaseSchema)
    # testUploadResults(connectionDetails = duckDbConnectionDetails,
    #                  resultsDatabaseSchema = duckDbResultsDatabaseSchema)
  })

}

# Create the results data model tables on a database server.

Create the results data model tables on a database server.

## Usage

``` r
createResultsDataModel(
  connectionDetails = NULL,
  databaseSchema,
  tablePrefix = ""
)
```

## Arguments

- connectionDetails:

  DatabaseConnector connectionDetails instance
  @seealso[DatabaseConnector::createConnectionDetails](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)

- databaseSchema:

  The schema on the server where the tables will be created.

- tablePrefix:

  (Optional) string to insert before table names for database table
  names

## Details

Only PostgreSQL and SQLite servers are supported.

# Get database migrations instance

Returns ResultModelManager DataMigrationsManager instance.

## Usage

``` r
getDataMigrator(connectionDetails, databaseSchema, tablePrefix = "")
```

## Arguments

- connectionDetails:

  DatabaseConnector connection details object

- databaseSchema:

  String schema where database schema lives

- tablePrefix:

  (Optional) Use if a table prefix is used before table names (e.g.
  "cd\_")

## Value

Instance of ResultModelManager::DataMigrationManager that has interface
for converting existing data models

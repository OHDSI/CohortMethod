# Insert exported results into a SQLite database

Insert exported results into a SQLite database

## Usage

``` r
insertExportedResultsInSqlite(sqliteFileName, exportFolder, cohorts)
```

## Arguments

- sqliteFileName:

  The name of the SQLite file to store the results in. If the file does
  not exist it will be created.

- exportFolder:

  The folder containing the CSV files to upload, as generated using the
  [`exportToCsv()`](https://ohdsi.github.io/CohortMethod/reference/exportToCsv.md)
  function.

- cohorts:

  A data frame describing the cohorts used in the study. Should include
  the target, comparator, and outcome of interest cohorts. The data
  frame should at least have a `cohortDefinitionId` and `cohortName`
  columns.

## Value

Does not return anything. Called for the side effect of inserting data
into the SQLite database.

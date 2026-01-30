# Launch Shiny app using a SQLite database

Launch Shiny app using a SQLite database

## Usage

``` r
launchResultsViewerUsingSqlite(sqliteFileName)
```

## Arguments

- sqliteFileName:

  The name of the SQLite file where the results were stored using the
  [`insertExportedResultsInSqlite()`](https://ohdsi.github.io/CohortMethod/reference/insertExportedResultsInSqlite.md)
  function.

## Value

Does not return anything. Is called for the side-effect of launching the
Shiny app.

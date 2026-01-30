# Launch Shiny app using

Launch Shiny app using

## Usage

``` r
launchResultsViewer(connectionDetails, databaseSchema)
```

## Arguments

- connectionDetails:

  An R object of type `connectionDetails` created using the
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.

- databaseSchema:

  The name of the database schema where the results were written using
  [`uploadResults()`](https://ohdsi.github.io/CohortMethod/reference/uploadResults.md).

## Value

Does not return anything. Is called for the side-effect of launching the
Shiny app.

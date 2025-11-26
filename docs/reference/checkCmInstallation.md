# Check is CohortMethod and its dependencies are correctly installed

Check is CohortMethod and its dependencies are correctly installed

## Usage

``` r
checkCmInstallation(connectionDetails)
```

## Arguments

- connectionDetails:

  An R object of type  
  `connectionDetails` created using the function
  `createConnectionDetails` in the `DatabaseConnector` package.

## Details

This function checks whether CohortMethod and its dependencies are
correctly installed. This will check the database connectivity, large
scale regression engine (Cyclops), and large data object handling (ff).

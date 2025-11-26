# Save the cohort method data to file

Saves an object of type
[CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
to a file.

## Usage

``` r
saveCohortMethodData(cohortMethodData, file)
```

## Arguments

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).

- file:

  The name of the file where the data will be written. If the file
  already exists it will be overwritten.

## Value

Returns no output.

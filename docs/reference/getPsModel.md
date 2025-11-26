# Get the propensity model

Returns the coefficients and names of the covariates with non-zero
coefficients.

## Usage

``` r
getPsModel(propensityScore, cohortMethodData)
```

## Arguments

- propensityScore:

  The propensity scores as generated using the
  [`createPs()`](https://ohdsi.github.io/CohortMethod/reference/createPs.md)
  function.

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).

## Value

A tibble.

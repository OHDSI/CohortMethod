# Create simulation profile

Creates a profile based on the provided
[CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
object, which can be used to generate simulated data that has similar
characteristics.

## Usage

``` r
createCohortMethodDataSimulationProfile(cohortMethodData, minCellCount = 5)
```

## Arguments

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).

- minCellCount:

  If \> 0, will set to zero all low-prevalence covariates in the
  supplied simulation table in order to prevent identification of
  persons.

## Value

An object of type `CohortDataSimulationProfile`.

## Details

The output of this function is an object that can be used by the
[`simulateCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/simulateCohortMethodData.md)
function to generate a cohortMethodData object.

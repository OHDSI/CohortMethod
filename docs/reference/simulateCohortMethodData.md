# Generate simulated data

Creates a
[CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
object with simulated data.

## Usage

``` r
simulateCohortMethodData(profile, n = 10000)
```

## Arguments

- profile:

  An object of type `CohortMethodDataSimulationProfile` as generated
  using the
  [`createCohortMethodDataSimulationProfile()`](https://ohdsi.github.io/CohortMethod/reference/createCohortMethodDataSimulationProfile.md)
  function.

- n:

  The size of the population to be generated.

## Value

An object of type
[CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md).

## Details

This function generates simulated data that is in many ways similar to
the original data on which the simulation profile is based. The contains
same outcome, comparator, and outcome concept IDs, and the covariates
and their 1st order statistics should be comparable.

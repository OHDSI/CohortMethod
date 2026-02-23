# Create propensity scores

Creates propensity scores and inverse probability of treatment weights
(IPTW) using a regularized logistic regression.

## Usage

``` r
createPs(
  cohortMethodData,
  population = NULL,
  createPsArgs = createCreatePsArgs()
)
```

## Arguments

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).

- population:

  A data frame describing the population. This should at least have a
  `rowId` column corresponding to the `rowId` column in the
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  covariates object and a `treatment` column. If population is not
  specified, the full population in the
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  will be used.

- createPsArgs:

  And object of type `CreatePsArgs` as created by the
  [`createCreatePsArgs()`](https://ohdsi.github.io/CohortMethod/reference/createCreatePsArgs.md)
  function

## Details

IPTW estimates either the average treatment effect (ate) or average
treatment effect in the treated (att) using stabilized inverse
propensity scores (Xu et al. 2010).

## References

Xu S, Ross C, Raebel MA, Shetterly S, Blanchette C, Smith D. Use of
stabilized inverse propensity scores as weights to directly estimate
relative risk and its confidence intervals. Value Health.
2010;13(2):273-277. doi:10.1111/j.1524-4733.2009.00671.x

## Examples

``` r
data(cohortMethodDataSimulationProfile)
cohortMethodData <- simulateCohortMethodData(cohortMethodDataSimulationProfile, n = 1000)
#> Generating covariates
#> Generating treatment variable
#> Generating cohorts
#> Generating outcomes after index date
#> Generating outcomes before index date
ps <- createPs(cohortMethodData, createPsArgs = createCreatePsArgs())
#> Removing 1 redundant covariates
#> Removing 0 infrequent covariates
#> Normalizing covariates
#> Tidying covariates took 4.01 secs
#> Propensity model fitting finished with status OK
#> Creating propensity scores took 13.7 secs
```

# Stratify persons by propensity score

Use the provided propensity scores to stratify persons. Additional
stratification variables for stratifications can also be used.

## Usage

``` r
stratifyByPs(
  population,
  stratifyByPsArgs = createStratifyByPsArgs(),
  cohortMethodData = NULL
)
```

## Arguments

- population:

  A data frame with the three columns described below

- stratifyByPsArgs:

  An object of type `StratifyByPsArgs` as created by the
  [`createStratifyByPsArgs()`](https://ohdsi.github.io/CohortMethod/reference/createStratifyByPsArgs.md)
  function.

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).
  Needed when additionally matching on covariate IDs.

## Value

Returns a tibble with the same columns as the input data plus one extra
column: stratumId.

## Details

The data frame should have the following three columns:

- rowId (numeric): A unique identifier for each row (e.g. the person
  ID).

- treatment (integer): Column indicating whether the person is in the
  target (1) or comparator (0) group.

- propensityScore (numeric): Propensity score.

## Examples

``` r
rowId <- 1:200
treatment <- rep(0:1, each = 100)
propensityScore <- c(runif(100, min = 0, max = 1), runif(100, min = 0, max = 1))
data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
result <- stratifyByPs(data, createStratifyByPsArgs(numberOfStrata = 5))
```

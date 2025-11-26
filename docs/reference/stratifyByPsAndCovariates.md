# Stratify persons by propensity score and other covariates

Use the provided propensity scores and covariates to stratify persons.

## Usage

``` r
stratifyByPsAndCovariates(
  population,
  numberOfStrata = 5,
  baseSelection = "all",
  cohortMethodData,
  covariateIds
)
```

## Arguments

- population:

  A data frame with the three columns described below

- numberOfStrata:

  Into how many strata should the propensity score be divided? The
  boundaries of the strata are automatically defined to contain equal
  numbers of target persons.

- baseSelection:

  What is the base selection of subjects where the strata bounds are to
  be determined? Strata are defined as equally-sized strata inside this
  selection. Possible values are "all", "target", and "comparator".

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).

- covariateIds:

  One or more covariate IDs in the `cohortMethodData` object on which
  subjects should also be stratified.

## Value

Returns a date frame with the same columns as the input population plus
one extra column: stratumId.

## Details

The data frame should have the following three columns:

- rowId (numeric): A unique identifier for each row (e.g. the person
  ID).

- treatment (integer): Column indicating whether the person is in the
  target (1) or comparator (0) group.

- propensityScore (numeric): Propensity score.

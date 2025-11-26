# Get the outcome model

Get the full outcome model, so showing the betas of all variables
included in the outcome model, not just the treatment variable.

## Usage

``` r
getOutcomeModel(outcomeModel, cohortMethodData)
```

## Arguments

- outcomeModel:

  An object of type `OutcomeModel` as generated using he
  [`fitOutcomeModel()`](https://ohdsi.github.io/CohortMethod/reference/fitOutcomeModel.md)
  function.

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).

## Value

A tibble.

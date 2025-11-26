# Create a parameter object for the function stratifyByPsAndCovariates

Create a parameter object for the function stratifyByPsAndCovariates

## Usage

``` r
createStratifyByPsAndCovariatesArgs(
  numberOfStrata = 5,
  baseSelection = "all",
  covariateIds
)
```

## Arguments

- numberOfStrata:

  Into how many strata should the propensity score be divided? The
  boundaries of the strata are automatically defined to contain equal
  numbers of target persons.

- baseSelection:

  What is the base selection of subjects where the strata bounds are to
  be determined? Strata are defined as equally-sized strata inside this
  selection. Possible values are "all", "target", and "comparator".

- covariateIds:

  One or more covariate IDs in the cohortMethodData object on which
  subjects should also be stratified.

## Details

Create an object defining the parameter values.

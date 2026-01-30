# Create a parameter object for the function [`stratifyByPs()`](https://ohdsi.github.io/CohortMethod/reference/stratifyByPs.md)

Create a parameter object for the function
[`stratifyByPs()`](https://ohdsi.github.io/CohortMethod/reference/stratifyByPs.md)

## Usage

``` r
createStratifyByPsArgs(
  numberOfStrata = 10,
  baseSelection = "all",
  stratificationColumns = c(),
  stratificationCovariateIds = c()
)
```

## Arguments

- numberOfStrata:

  How many strata? The boundaries of the strata are automatically
  defined to contain equal numbers of target persons.

- baseSelection:

  What is the base selection of subjects where the strata bounds are to
  be determined? Strata are defined as equally-sized strata inside this
  selection. Possible values are "all", "target", and "comparator".

- stratificationColumns:

  Names or numbers of one or more columns in the `data` data.frame on
  which subjects should be stratified prior to matching. No persons will
  be matched with persons outside of the strata identified by the values
  in these columns.

- stratificationCovariateIds:

  One or more covariate IDs in the `cohortMethodData` object on which
  subjects should also be stratified.

## Value

An object of type `StratifyByPsArgs`.

## Details

Create an object defining the parameter values.

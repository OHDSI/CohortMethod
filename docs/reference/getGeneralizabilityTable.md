# Get information on generalizability

to assess generalizability we compare the distribution of covariates
before and after any (propensity score) adjustments. We compute the
standardized difference of mean as our metric of generalizability.
(Lipton et al., 2017)

Depending on our target estimand, we need to consider a different base
population for generalizability. For example, if we aim to estimate the
average treatment effect in the treated (ATT), our base population
should be the target population, meaning we should consider the
covariate distribution before and after PS adjustment in the target
population only. By default this function will attempt to select the
right base population based on what operations have been performed on
the population. For example, if PS matching has been performed we assume
the target estimand is the ATT, and the target population is selected as
base.

Requires running
[`computeCovariateBalance()`](https://ohdsi.github.io/CohortMethod/reference/computeCovariateBalance.md)\`
first.

## Usage

``` r
getGeneralizabilityTable(balance, baseSelection = "auto")
```

## Arguments

- balance:

  A data frame created by the `computeCovariateBalance` function.

- baseSelection:

  The selection of the population to consider for generalizability.
  Options are "auto", "target", "comparator", and "both". The "auto"
  option will attempt to use the balance meta-data to pick the most
  appropriate population based on the target estimator.

## Value

A tibble with the following columns:

- covariateId: The ID of the covariate. Can be linked to the
  `covariates` and `covariateRef` tables in the `CohortMethodData`
  object.

- covariateName: The name of the covariate.

- beforeMatchingMean: The mean covariate value before any (propensity
  score) adjustment.

- afterMatchingMean: The mean covariate value after any (propensity
  score) adjustment.

- stdDiff: The standardized difference of means between before and after
  adjustment.

The tibble also has a 'baseSelection' attribute, documenting the base
population used to assess generalizability.

## References

Tipton E, Hallberg K, Hedges LV, Chan W (2017) Implications of Small
Samples for Generalization: Adjustments and Rules of Thumb, Eval Rev.
Oct;41(5):472-505.

# Create a parameter object for the function [`matchOnPs()`](https://ohdsi.github.io/CohortMethod/reference/matchOnPs.md)

Create a parameter object for the function
[`matchOnPs()`](https://ohdsi.github.io/CohortMethod/reference/matchOnPs.md)

## Usage

``` r
createMatchOnPsArgs(
  caliper = 0.2,
  caliperScale = "standardized logit",
  maxRatio = 1,
  allowReverseMatch = FALSE,
  matchColumns = c(),
  matchCovariateIds = c()
)
```

## Arguments

- caliper:

  The caliper for matching. A caliper is the distance which is
  acceptable for any match. Observations which are outside of the
  caliper are dropped. A caliper of 0 means no caliper is used.

- caliperScale:

  The scale on which the caliper is defined. Three scales are supported:
  `caliperScale = 'propensity score'`, `caliperScale = 'standardized'`,
  or `caliperScale = 'standardized logit'`. On the standardized scale,
  the caliper is interpreted in standard deviations of the propensity
  score distribution. 'standardized logit' is similar, except that the
  propensity score is transformed to the logit scale because the PS is
  more likely to be normally distributed on that scale (Austin, 2011).

- maxRatio:

  The maximum number of persons in the comparator arm to be matched to
  each person in the treatment arm. A `maxRatio` of 0 means no maximum:
  all comparators will be assigned to a target person.

- allowReverseMatch:

  Allows n-to-1 matching if target arm is larger

- matchColumns:

  Names or numbers of one or more columns in the `data` data.frame on
  which subjects should be stratified prior to matching. No persons will
  be matched with persons outside of the strata identified by the values
  in these columns.

- matchCovariateIds:

  One or more covariate IDs in the `cohortMethodData` object on which
  subjects should be also matched.

## Value

An object of type `MatchOnPsArgs`.

## Details

Create an object defining the parameter values.

## References

Austin, PC. (2011) Optimal caliper widths for propensity-score matching
when estimating differences in means and differences in proportions in
observational studies, Pharmaceutical statistics, March, 10(2):150-161.

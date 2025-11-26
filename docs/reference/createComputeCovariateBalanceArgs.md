# Create a parameter object for the function [`computeCovariateBalance()`](https://ohdsi.github.io/CohortMethod/reference/computeCovariateBalance.md)

Create a parameter object for the function
[`computeCovariateBalance()`](https://ohdsi.github.io/CohortMethod/reference/computeCovariateBalance.md)

## Usage

``` r
createComputeCovariateBalanceArgs(
  subgroupCovariateId = NULL,
  maxCohortSize = 250000,
  covariateFilter = NULL,
  threshold = 0.1,
  alpha = 0.05
)
```

## Arguments

- subgroupCovariateId:

  Optional: a covariate ID of a binary covariate that indicates a
  subgroup of interest. Both the before and after populations will be
  restricted to this subgroup before computing covariate balance.

- maxCohortSize:

  If the target or comparator cohort are larger than this number, they
  will be downsampled before computing covariate balance to save time.
  Setting this number to 0 means no downsampling will be applied.

- covariateFilter:

  Determines the covariates for which to compute covariate balance.
  Either a vector of covariate IDs, or a table 1 specifications object
  as generated for example using
  [`FeatureExtraction::getDefaultTable1Specifications()`](https://rdrr.io/pkg/FeatureExtraction/man/getDefaultTable1Specifications.html).
  If `covariateFilter = NULL`, balance will be computed for all
  variables found in the data.

- threshold:

  Threshold value for the absolute value of the standardized difference
  of means (ASDM). If the ASDM exceeds this threshold it will be marked
  as unbalanced. (Hripcsak et al. 2025)

- alpha:

  The family-wise alpha for testing whether the absolute value of the
  standardized difference of means is greater than the threshold. If not
  provided, any value greater than the threshold will be marked as
  unbalanced.

## Value

An object of type `ComputeCovariateBalanceArgs`.

## Details

Create an object defining the parameter values.

## References

Hripcsak G, Zhang L, Chen Y, Li K, Suchard MA, Ryan PB, Schuemie MJ,
Assessing Covariate Balance with Small Sample Sizes. Statistics in
Medicine 44, no. 18-19 (2025): e70212

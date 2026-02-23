# Compute covariate balance before and after PS adjustment

For every covariate, prevalence in treatment and comparator groups
before and after matching/trimming/weighting are computed. When variable
ratio matching was used the balance score will be corrected according
the method described in Austin et al (2008).

## Usage

``` r
computeCovariateBalance(
  population,
  cohortMethodData,
  computeCovariateBalanceArgs = createComputeCovariateBalanceArgs()
)
```

## Arguments

- population:

  A data frame containing the people that are remaining after PS
  adjustment.

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).

- computeCovariateBalanceArgs:

  Settings object as created by
  [`createComputeCovariateBalanceArgs()`](https://ohdsi.github.io/CohortMethod/reference/createComputeCovariateBalanceArgs.md).

## Value

Returns a tibble describing the covariate balance before and after PS
adjustment, with one row per covariate, with the same data as the
`covariateRef` table in the `CohortMethodData` object, and the following
additional columns:

- beforeMatchingMeanTarget: The (weighted) mean value in the target
  before PS adjustment.

- beforeMatchingMeanComparator: The (weighted) mean value in the
  comparator before PS adjustment.

- beforeMatchingSumTarget: The (weighted) sum value in the target before
  PS adjustment.

- beforeMatchingSumComparator: The (weighted) sum value in the
  comparator before PS adjustment.

- beforeMatchingSdTarget: The standard deviation of the value in the
  target before PS adjustment.

- beforeMatchingSdComparator: The standard deviation of the value in the
  comparator before PS adjustment.

- beforeMatchingMean: The mean of the value across target and comparator
  before PS adjustment.

- beforeMatchingSd: The standard deviation of the value across target
  and comparator before PS adjustment.

- beforeMatchingStdDiff: The standardized difference of means when
  comparing the target to the comparator before PS adjustment.

- beforeMatchingSdmVariance: The variance of the standardized difference
  of the means when comparing the target to the comparator before PS
  adjustment.

- beforeMatchingSdmP : The P-value for whether
  abs(beforeMatchingStdDiff) exceeds the threshold.

- beforeMatchingBalanced : TRUE if the covariate is considered balanced
  between the target and comparator before PS adjustment (depending on
  the threshold and alpha settings).

- afterMatchingMeanTarget: The (weighted) mean value in the target after
  PS adjustment.

- afterMatchingMeanComparator: The (weighted) mean value in the
  comparator after PS adjustment.

- afterMatchingSumTarget: The (weighted) sum value in the target after
  PS adjustment.

- afterMatchingSumComparator: The (weighted) sum value in the comparator
  after PS adjustment.

- afterMatchingSdTarget: The standard deviation of the value in the
  target after PS adjustment.

- afterMatchingSdComparator: The standard deviation of the value in the
  comparator after PS adjustment.

- afterMatchingMean: The mean of the value across target and comparator
  after PS adjustment.

- afterMatchingSd: The standard deviation of the value across target and
  comparator after PS adjustment.

- afterMatchingStdDiff: The standardized difference of means when
  comparing the target to the comparator after PS adjustment.

- afterMatchingSdmVariance: The variance of the standardized difference
  of the means when comparing the target to the comparator after PS
  adjustment.

- afteMatchingSdmP : The P-value for whether abs(beforeMatchingStdDiff)
  exceeds the threshold.

- afteMatchingBalanced : TRUE if the covariate is considered balanced
  between the target and comparator before PS adjustment (depending on
  the threshold and alpha settings).

- targetStdDiff: The standardized difference of means when comparing the
  target before PS adjustment to the target after PS adjustment.

- comparatorStdDiff: The standardized difference of means when comparing
  the comparator before PS adjustment to the comparator after PS
  adjustment. -targetComparatorStdDiff: The standardized difference of
  means when comparing the entire population before PS adjustment to the
  entire population after PS adjustment.

The 'beforeMatchingStdDiff' and 'afterMatchingStdDiff' columns inform on
the balance: are the target and comparator sufficiently similar in terms
of baseline covariates to allow for valid causal estimation?

The 'targetStdDiff', 'comparatorStdDiff', and 'targetComparatorStdDiff'
columns inform on the generalizability: are the cohorts after PS
adjustment sufficiently similar to the cohorts before adjustment to
allow generalizing the findings to the original cohorts?

## Details

The population data frame should have the following three columns:

- rowId (numeric): A unique identifier for each row (e.g. the person
  ID).

- treatment (integer): Column indicating whether the person is in the
  target (1) or comparator (0) group.

- propensityScore (numeric): Propensity score.

## References

Austin, PC (2008) Assessing balance in measured baseline covariates when
using many-to-one matching on the propensity-score. Pharmacoepidemiology
and Drug Safety, 17: 1218-1225.

Hripcsak G, Zhang L, Chen Y, Li K, Suchard MA, Ryan PB, Schuemie MJ
(2025) Assessing Covariate Balance with Small Sample Sizes. Stat Med.
2025 Aug;44(18-19):e70212.

# Create CohortMethod diagnostics thresholds

Threshold used when calling
[`exportToCsv()`](https://ohdsi.github.io/CohortMethod/reference/exportToCsv.md)
to determine if we pass or fail diagnostics.

## Usage

``` r
createCmDiagnosticThresholds(
  mdrrThreshold = 10,
  easeThreshold = 0.25,
  sdmThreshold = 0.1,
  sdmAlpha = NULL,
  equipoiseThreshold = 0.2,
  generalizabilitySdmThreshold = 999
)
```

## Arguments

- mdrrThreshold:

  What is the maximum allowed minimum detectable relative risk (MDRR)?

- easeThreshold:

  What is the maximum allowed expected absolute systematic error (EASE).

- sdmThreshold:

  What is the maximum allowed standardized difference of mean (SDM)? If
  any covariate has an SDM exceeding this threshold, the diagnostic will
  fail.

- sdmAlpha:

  What is the alpha for testing whether the absolute SDM exceeds
  `sdmThreshold`? If not provided, no significance testing will be
  performed and any absolute SDM greater than the threshold will be
  considered imbalance. Note that a Bonferroni adjustment will
  automatically be applied to adjust for the number of tests performed.

- equipoiseThreshold:

  What is the minimum required equipoise?

- generalizabilitySdmThreshold:

  What is the maximum allowed standardized difference of mean (SDM)when
  comparing the population before and after PS adjustments? If the SDM
  is greater than this value, the diagnostic will fail.

## Value

An object of type `CmDiagnosticThresholds`.

## Details

The `sdmThreshold` and `sdmAlpha` arguments are independent of the
`threshold` and `alpha` threshold provided to the
[`createComputeCovariateBalanceArgs()`](https://ohdsi.github.io/CohortMethod/reference/createComputeCovariateBalanceArgs.md)
function. The latter have no impact on blinding and diagnostics reported
in the export.

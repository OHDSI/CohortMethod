# Create a table 1

Creates a formatted table of cohort characteristics, to be included in
publications or reports.

## Usage

``` r
createCmTable1(
  balance,
  specifications = getDefaultCmTable1Specifications(),
  beforeTargetPopSize = NULL,
  beforeComparatorPopSize = NULL,
  afterTargetPopSize = NULL,
  afterComparatorPopSize = NULL,
  beforeLabel = "Before matching",
  afterLabel = "After matching",
  targetLabel = "Target",
  comparatorLabel = "Comparator",
  percentDigits = 1,
  stdDiffDigits = 2
)
```

## Arguments

- balance:

  A data frame created by the `computeCovariateBalance` function.

- specifications:

  Specifications of which covariates to display, and how.

- beforeTargetPopSize:

  The number of people in the target cohort before
  matching/stratification/trimming, to mention in the table header. If
  not provide, no number will be included in the header.

- beforeComparatorPopSize:

  The number of people in the comparator cohort before
  matching/stratification/trimming, to mention in the table header. If
  not provide, no number will be included in the header.

- afterTargetPopSize:

  The number of people in the target cohort after
  matching/stratification/trimming, to mention in the table header. If
  not provide, no number will be included in the header.

- afterComparatorPopSize:

  The number of people in the comparator cohort after
  matching/stratification/trimming, to mention in the table header. If
  not provide, no number will be included in the header.

- beforeLabel:

  Label for identifying columns before matching / stratification /
  trimming.

- afterLabel:

  Label for identifying columns after matching / stratification /
  trimming.

- targetLabel:

  Label for identifying columns of the target cohort.

- comparatorLabel:

  Label for identifying columns of the comparator cohort.

- percentDigits:

  Number of digits to be used for percentages.

- stdDiffDigits:

  Number of digits to be used for the standardized differences.

## Value

A data frame with the formatted table 1.

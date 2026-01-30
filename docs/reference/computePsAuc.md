# Compute the area under the ROC curve

Compute the area under the ROC curve of the propensity score.

## Usage

``` r
computePsAuc(data, confidenceIntervals = FALSE, maxRows = 1e+05)
```

## Arguments

- data:

  A data frame with at least the two columns described below

- confidenceIntervals:

  Compute 95 percent confidence intervals (computationally expensive for
  large data sets)

- maxRows:

  Maximum number of rows to use. If the number of rows is larger, a
  random sample will be taken. This can increase speed, with minor cost
  to precision. Set to 0 to use all data.

## Value

A tibble holding the AUC and its 95 percent confidence interval

## Details

The data frame should have a least the following two columns:

- treatment (integer): Column indicating whether the person is in the
  target (1) or comparator (0) group.

- propensityScore (numeric): Propensity score.

## Examples

``` r
treatment <- rep(0:1, each = 100)
propensityScore <- c(rnorm(100, mean = 0.4, sd = 0.25), rnorm(100, mean = 0.6, sd = 0.25))
data <- data.frame(treatment = treatment, propensityScore = propensityScore)
data <- data[data$propensityScore > 0 & data$propensityScore < 1, ]
computePsAuc(data)
#> [1] 0.6716202
```

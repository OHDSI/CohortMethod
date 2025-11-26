# Truncate IPTW values

Set the inverse probability of treatment weights (IPTW) to the
user-specified threshold if it exceeds said threshold.

## Usage

``` r
truncateIptw(population, truncateIptwArgs = createTruncateIptwArgs())
```

## Arguments

- population:

  A data frame with at least the two columns described in the details.

- truncateIptwArgs:

  An object of type `TruncateIptwArgs` as created by the
  [`createTruncateIptwArgs()`](https://ohdsi.github.io/CohortMethod/reference/createTruncateIptwArgs.md)
  function.

## Value

Returns a tibble with the same columns as the input.

## Details

The data frame should have the following two columns:

- treatment (integer): Column indicating whether the person is in the
  target (1) or comparator (0) group.

- iptw (numeric): Propensity score.

## Examples

``` r
rowId <- 1:2000
treatment <- rep(0:1, each = 1000)
iptw <- 1 / c(runif(1000, min = 0, max = 1), runif(1000, min = 0, max = 1))
data <- data.frame(rowId = rowId, treatment = treatment, iptw = iptw)
result <- truncateIptw(data)
#> Truncating 192 (9.6%) IPTW values
```

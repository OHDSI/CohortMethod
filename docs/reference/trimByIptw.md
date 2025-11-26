# Remove subjects with a high IPTW

Remove subjects having a weight higher than the user-specified
threshold.

## Usage

``` r
trimByIptw(population, maxWeight = 10)
```

## Arguments

- population:

  A data frame with at least the two columns described in the details

- maxWeight:

  The maximum allowed IPTW.

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
result <- trimByIptw(data)
#> Population size after trimming is 1771
```

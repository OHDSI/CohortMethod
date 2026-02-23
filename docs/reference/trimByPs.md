# Trim persons by propensity score

Use the provided propensity scores to trim subjects with extreme scores
or weights.

## Usage

``` r
trimByPs(population, trimByPsArgs = createTrimByPsArgs(trimFraction = 0.05))
```

## Arguments

- population:

  A data frame with the three columns described below

- trimByPsArgs:

  An object of type `trimByPsArgs` as created by the
  [`createTrimByPsArgs()`](https://ohdsi.github.io/CohortMethod/reference/createTrimByPsArgs.md)
  function.

## Value

Returns a tibble with the same three columns as the input.

## Details

The data frame should have the following three columns:

- rowId (numeric): A unique identifier for each row (e.g. the person
  ID).

- treatment (integer): Column indicating whether the person is in the
  target (1) or comparator (0) group.

- propensityScore (numeric): Propensity score.

## Examples

``` r
rowId <- 1:2000
treatment <- rep(0:1, each = 1000)
propensityScore <- c(runif(1000, min = 0, max = 1), runif(1000, min = 0, max = 1))
iptw <- ifelse(treatment == 1,
               mean(treatment == 1) / propensityScore,
               mean(treatment == 0) / (1 - propensityScore))
data <- data.frame(rowId = rowId,
                   treatment = treatment,
                   propensityScore = propensityScore,
                   iptw = iptw)
result1 <- trimByPs(data, createTrimByPsArgs(trimFraction = 0.05))
#> Trimming removed 96 (9.6%) rows from the target, 99 (9.9%) rows from the comparator in total.
result2 <- trimByPs(data, createTrimByPsArgs(equipoiseBounds = c(0.3, 0.7)))
#> Trimming removed 597 (59.7%) rows from the target, 592 (59.2%) rows from the comparator in total.
result3 <- trimByPs(data, createTrimByPsArgs(maxWeight = 10))
#> Trimming removed 51 (5.1%) rows from the target, 49 (4.9%) rows from the comparator in total.
```

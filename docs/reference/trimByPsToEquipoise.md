# Keep only persons in clinical equipoise

Use the preference score to trim subjects that are not in clinical
equipoise

## Usage

``` r
trimByPsToEquipoise(population, bounds = c(0.3, 0.7))
```

## Arguments

- population:

  A data frame with at least the three columns described below.

- bounds:

  The upper and lower bound on the preference score for keeping persons.

## Value

Returns a tibble with the same three columns as the input.

## Details

The data frame should have the following three columns:

- rowId (numeric): A unique identifier for each row (e.g. the person
  ID).

- treatment (integer): Column indicating whether the person is in the
  target (1) or comparator (0) group.

- propensityScore (numeric): Propensity score.

## References

Walker AM, Patrick AR, Lauer MS, Hornbrook MC, Marin MG, Platt R, Roger
VL, Stang P, and Schneeweiss S. (2013) A tool for assessing the
feasibility of comparative effectiveness research, Comparative Effective
Research, 3, 11-20

## Examples

``` r
rowId <- 1:2000
treatment <- rep(0:1, each = 1000)
propensityScore <- c(runif(1000, min = 0, max = 1), runif(1000, min = 0, max = 1))
data <- data.frame(rowId = rowId, treatment = treatment, propensityScore = propensityScore)
result <- trimByPsToEquipoise(data)
#> Population size after trimming is 829
```

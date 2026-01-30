# Match persons by propensity score

Use the provided propensity scores to match target to comparator
persons.

## Usage

``` r
matchOnPs(
  population,
  matchOnPsArgs = createMatchOnPsArgs(),
  cohortMethodData = NULL
)
```

## Arguments

- population:

  A data frame with the three columns described below.

- matchOnPsArgs:

  An object of type `MatchOnPsArgs` as created by the
  [`createMatchOnPsArgs()`](https://ohdsi.github.io/CohortMethod/reference/createMatchOnPsArgs.md)
  function.

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).
  Needed when additionally matching on covariate IDs.

## Value

Returns a date frame with the same columns as the input data plus one
extra column: stratumId. Any rows that could not be matched are removed

## Details

The data frame should have the following three columns:

- rowId (numeric): A unique identifier for each row (e.g. the person
  ID).

- treatment (integer): Column indicating whether the person is in the
  target (1) or comparator (0) group.

- propensityScore (numeric): Propensity score.

The default caliper (0.2 on the standardized logit scale) is the one
recommended by Austin (2011).

## References

Rassen JA, Shelat AA, Myers J, Glynn RJ, Rothman KJ, Schneeweiss S.
(2012) One-to-many propensity score matching in cohort studies,
Pharmacoepidemiology and Drug Safety, May, 21 Suppl 2:69-80.

Austin, PC. (2011) Optimal caliper widths for propensity-score matching
when estimating differences in means and differences in proportions in
observational studies, Pharmaceutical statistics, March, 10(2):150-161.

## Examples

``` r
rowId <- 1:5
treatment <- c(1, 0, 1, 0, 1)
propensityScore <- c(0, 0.1, 0.3, 0.4, 1)
age_group <- c(1, 1, 1, 1, 1)
data <- data.frame(
  rowId = rowId,
  treatment = treatment,
  propensityScore = propensityScore,
  age_group = age_group
)
result <- matchOnPs(data, createMatchOnPsArgs(
  caliper = 0,
  maxRatio = 1,
  matchColumns = "age_group")
)
#> Population size after matching is 4
```

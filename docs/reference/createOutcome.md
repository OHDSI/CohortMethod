# Create outcome definition

Create outcome definition

## Usage

``` r
createOutcome(
  outcomeId,
  outcomeOfInterest = TRUE,
  trueEffectSize = NA,
  priorOutcomeLookback = NULL,
  riskWindowStart = NULL,
  startAnchor = NULL,
  riskWindowEnd = NULL,
  endAnchor = NULL
)
```

## Arguments

- outcomeId:

  An integer used to identify the outcome in the outcome cohort table.

- outcomeOfInterest:

  Is this an outcome of interest? If not, creation of non-essential
  files will be skipped, including outcome=specific covariate balance
  files. This could be helpful to speed up analyses with many controls,
  for which we're only interested in the effect size estimate.

- trueEffectSize:

  For negative and positive controls: the known true effect size. To be
  used for empirical calibration. Negative controls have
  `trueEffectSize = 1`. If the true effect size is unknown, use
  `trueEffectSize = NA`

- priorOutcomeLookback:

  How many days should we look back when identifying prior. outcomes?

- riskWindowStart:

  The start of the risk window (in days) relative to the `startAnchor`.

- startAnchor:

  The anchor point for the start of the risk window. Can be
  `"cohort start"` or `"cohort end"`.

- riskWindowEnd:

  The end of the risk window (in days) relative to the `endAnchor`.

- endAnchor:

  The anchor point for the end of the risk window. Can be
  `"cohort start"` or `"cohort end"`.

## Value

An object of type `Outcome`, to be used in
[`createTargetComparatorOutcomes()`](https://ohdsi.github.io/CohortMethod/reference/createTargetComparatorOutcomes.md).

## Details

Any settings here that are not `NULL` will override any values set in
[`createCreateStudyPopulationArgs()`](https://ohdsi.github.io/CohortMethod/reference/createCreateStudyPopulationArgs.md).

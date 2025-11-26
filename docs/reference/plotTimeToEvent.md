# Plot time-to-event

Plot time-to-event

## Usage

``` r
plotTimeToEvent(
  cohortMethodData,
  population = NULL,
  outcomeId = NULL,
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 0,
  endAnchor = "cohort end",
  censorAtNewRiskWindow = FALSE,
  periodLength = 7,
  numberOfPeriods = 52,
  highlightExposedEvents = TRUE,
  includePostIndexTime = TRUE,
  showFittedLines = TRUE,
  targetLabel = "Target",
  comparatorLabel = "Comparator",
  title = NULL,
  fileName = NULL
)
```

## Arguments

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).

- population:

  If specified, this population will be used as the starting point
  instead of the cohorts in the `cohortMethodData` object.

- outcomeId:

  The ID of the outcome. If NULL, no outcome-specific transformations
  will be performed.

- minDaysAtRisk:

  The minimum required number of days at risk.

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

- censorAtNewRiskWindow:

  If a subject is in multiple cohorts, should time-at-risk be censored
  when the new time-at-risk starts to prevent overlap?

- periodLength:

  The length in days of each period shown in the plot.

- numberOfPeriods:

  Number of periods to show in the plot. The periods are equally divided
  before and after the index date.

- highlightExposedEvents:

  (logical) Highlight event counts during exposure in a different color?

- includePostIndexTime:

  (logical) Show time after the index date?

- showFittedLines:

  (logical) Fit lines to the proportions and show them in the plot?

- targetLabel:

  A label to us for the target cohort.

- comparatorLabel:

  A label to us for the comparator cohort.

- title:

  Optional: the main title for the plot.

- fileName:

  Name of the file where the plot should be saved, for example
  'plot.png'. See
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  for supported file formats.

## Value

A ggplot object. Use the
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
function to save to file in a different format.

## Details

Creates a plot showing the number of events over time in the target and
comparator cohorts, both before and after index date. The plot also
distinguishes between events inside and outside the time-at-risk period.
This requires the user to (re)specify the time-at-risk using the same
arguments as the
[`createStudyPopulation()`](https://ohdsi.github.io/CohortMethod/reference/createStudyPopulation.md)
function. Note that it is not possible to specify that people with the
outcome prior should be removed, since the plot will show these prior
events.

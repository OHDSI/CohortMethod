# Create a parameter object for the function [`createStudyPopulation()`](https://ohdsi.github.io/CohortMethod/reference/createStudyPopulation.md)

Create a parameter object for the function
[`createStudyPopulation()`](https://ohdsi.github.io/CohortMethod/reference/createStudyPopulation.md)

## Usage

``` r
createCreateStudyPopulationArgs(
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 99999,
  minDaysAtRisk = 1,
  maxDaysAtRisk = 99999,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 0,
  endAnchor = "cohort end",
  censorAtNewRiskWindow = FALSE
)
```

## Arguments

- removeSubjectsWithPriorOutcome:

  Remove subjects that have the outcome prior to the risk window start?

- priorOutcomeLookback:

  How many days should we look back when identifying prior outcomes?

- minDaysAtRisk:

  The minimum required number of days at risk. Risk windows with fewer
  days than this number are removed from the analysis.

- maxDaysAtRisk:

  The maximum allowed number of days at risk. Risk windows that are
  longer will be truncated to this number of days.

- riskWindowStart:

  The start of the risk window (in days) relative to the startAnchor.

- startAnchor:

  The anchor point for the start of the risk window. Can be "cohort
  start" or "cohort end".

- riskWindowEnd:

  The end of the risk window (in days) relative to the endAnchor.

- endAnchor:

  The anchor point for the end of the risk window. Can be "cohort start"
  or "cohort end".

- censorAtNewRiskWindow:

  If a subject is in multiple cohorts, should time-at-risk be censored
  when the new time-at-risk starts to prevent overlap?

## Value

An object of type `CreateStudyPopulationArgs`.

## Details

Create an object defining the parameter values.

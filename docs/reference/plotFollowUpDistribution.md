# Plot the distribution of follow-up time

Plot the distribution of follow-up time

## Usage

``` r
plotFollowUpDistribution(
  population,
  targetLabel = "Target",
  comparatorLabel = "Comparator",
  yScale = "percent",
  logYScale = FALSE,
  dataCutoff = 0.95,
  title = NULL,
  fileName = NULL
)
```

## Arguments

- population:

  A data frame describing the study population as created using the
  [`createStudyPopulation`](https://ohdsi.github.io/CohortMethod/reference/createStudyPopulation.md)
  function. This should at least have these columns: treatment,
  timeAtRisk.

- targetLabel:

  A label to us for the target cohort.

- comparatorLabel:

  A label to us for the comparator cohort.

- yScale:

  Should be either 'percent' or 'count'.

- logYScale:

  Should the Y axis be on the log scale?

- dataCutoff:

  Fraction of the data (number censored) after which the graph will not
  be shown.

- title:

  The main title of the plot.

- fileName:

  Name of the file where the plot should be saved, for example
  'plot.png'. See the function `ggsave` in the ggplot2 package for
  supported file formats.

## Value

A ggplot object. Use the
[`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html) function
to save to file in a different format.

## Details

Plot the distribution of follow-up time, stratified by treatment
group.Follow-up time is defined as time-at-risk, so not censored at the
outcome.

# Get the distribution of follow-up time

Get the distribution of follow-up time

## Usage

``` r
getFollowUpDistribution(population, quantiles = c(0, 0.25, 0.5, 0.75, 1))
```

## Arguments

- population:

  A data frame describing the study population as created using the
  [`createStudyPopulation`](https://ohdsi.github.io/CohortMethod/reference/createStudyPopulation.md)
  function. This should at least have these columns: treatment,
  timeAtRisk.

- quantiles:

  The quantiles of the population to compute minimum follow-up time for.

## Value

A data frame with per treatment group at each quantile the amount of
follow-up time available.

## Details

Get the distribution of follow-up time as quantiles. Follow-up time is
defined as time-at-risk, so not censored at the outcome.

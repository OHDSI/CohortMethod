# Compute the minimum detectable relative risk

Compute the minimum detectable relative risk

## Usage

``` r
computeMdrr(
  population,
  alpha = 0.05,
  power = 0.8,
  twoSided = TRUE,
  modelType = "cox"
)
```

## Arguments

- population:

  A data frame describing the study population as created using the
  [`createStudyPopulation`](https://ohdsi.github.io/CohortMethod/reference/createStudyPopulation.md)
  function. This should at least have these columns: personSeqId,
  treatment, outcomeCount, timeAtRisk.

- alpha:

  Type I error.

- power:

  1 - beta, where beta is the type II error.

- twoSided:

  Consider a two-sided test?

- modelType:

  The type of outcome model that will be used. Possible values are
  "logistic", "poisson", or "cox". Currently only "cox" is supported.

## Value

A data frame with the MDRR and some counts.

## Details

Compute the minimum detectable relative risk (MDRR) and expected
standard error (SE) for a given study population, using the actual
observed sample size and number of outcomes. Currently, only
computations for Cox and logistic models are implemented. For Cox model,
the computations by Schoenfeld (1983) is used. For logistic models
Wald's z-test is used.

## References

Schoenfeld DA (1983) Sample-size formula for the proportional-hazards
regression model, Biometrics, 39(3), 499-503

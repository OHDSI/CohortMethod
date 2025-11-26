# Create CohortMethod multi-threading settings

Create CohortMethod multi-threading settings

## Usage

``` r
createMultiThreadingSettings(
  getDbCohortMethodDataThreads = 1,
  createPsThreads = 1,
  psCvThreads = 1,
  createStudyPopThreads = 1,
  trimMatchStratifyThreads = 1,
  computeSharedBalanceThreads = 1,
  computeBalanceThreads = 1,
  prefilterCovariatesThreads = 1,
  fitOutcomeModelThreads = 1,
  outcomeCvThreads = 1,
  calibrationThreads = 1
)
```

## Arguments

- getDbCohortMethodDataThreads:

  The number of parallel threads to use for building the cohortMethod
  data objects.

- createPsThreads:

  The number of parallel threads to use for fitting the propensity
  models.

- psCvThreads:

  The number of parallel threads to use for the cross- validation when
  estimating the hyperparameter for the propensity model. Note that the
  total number of CV threads at one time could be
  `createPsThreads * psCvThreads`.

- createStudyPopThreads:

  The number of parallel threads to use for creating the study
  population.

- trimMatchStratifyThreads:

  The number of parallel threads to use for trimming, matching and
  stratifying.

- computeSharedBalanceThreads:

  The number of parallel threads to use for computing shared covariate
  balance.

- computeBalanceThreads:

  The number of parallel threads to use for computing covariate balance.

- prefilterCovariatesThreads:

  The number of parallel threads to use for prefiltering covariates.

- fitOutcomeModelThreads:

  The number of parallel threads to use for fitting the outcome models.

- outcomeCvThreads:

  The number of parallel threads to use for the cross- validation when
  estimating the hyperparameter for the outcome model. Note that the
  total number of CV threads at one time could be
  `fitOutcomeModelThreads * outcomeCvThreads`.

- calibrationThreads:

  The number of parallel threads to use for empirical calibration.

## Value

An object of type `CmMultiThreadingSettings`.

## See also

[`createDefaultMultiThreadingSettings()`](https://ohdsi.github.io/CohortMethod/reference/createDefaultMultiThreadingSettings.md)

# Create a parameter object for the function [`createPs()`](https://ohdsi.github.io/CohortMethod/reference/createPs.md)

Create a parameter object for the function
[`createPs()`](https://ohdsi.github.io/CohortMethod/reference/createPs.md)

## Usage

``` r
createCreatePsArgs(
  excludeCovariateIds = c(),
  includeCovariateIds = c(),
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = TRUE,
  stopOnError = TRUE,
  prior = createPrior(priorType = "laplace", exclude = c(0), useCrossValidation = TRUE),
  control = createControl(noiseLevel = "silent", cvType = "auto", seed = 1,
    resetCoefficients = TRUE, tolerance = 2e-07, cvRepetitions = 10, startingVariance =
    0.01),
  estimator = "att"
)
```

## Arguments

- excludeCovariateIds:

  Exclude these covariates from the propensity model.

- includeCovariateIds:

  Include only these covariates in the propensity model.

- maxCohortSizeForFitting:

  If the target or comparator cohort are larger than this number, they
  will be downsampled before fitting the propensity model. The model
  will be used to compute propensity scores for all subjects. The
  purpose of the sampling is to gain speed. Setting this number to 0
  means no downsampling will be applied.

- errorOnHighCorrelation:

  If true, the function will test each covariate for correlation with
  the treatment assignment. If any covariate has an unusually high
  correlation (either positive or negative), this will throw and error.

- stopOnError:

  If an error occur, should the function stop? Else, the two cohorts
  will be assumed to be perfectly separable.

- prior:

  The prior used to fit the model. See Cyclops::createPrior() for
  details.

- control:

  The control object used to control the cross-validation used to
  determine the hyperparameters of the prior (if applicable). See
  Cyclops::createControl() for details.

- estimator:

  The type of estimator for the IPTW. Options are estimator = "ate" for
  the average treatment effect, estimator = "att" for the average
  treatment effect in the treated, and estimator = "ato" for the average
  treatment effect in the overlap population.

## Value

An object of type `CreatePsArgs`.

## Details

Create an object defining the parameter values.

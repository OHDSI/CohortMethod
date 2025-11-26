# Create a parameter object for the function [`fitOutcomeModel()`](https://ohdsi.github.io/CohortMethod/reference/fitOutcomeModel.md)

Create a parameter object for the function
[`fitOutcomeModel()`](https://ohdsi.github.io/CohortMethod/reference/fitOutcomeModel.md)

## Usage

``` r
createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = FALSE,
  useCovariates = FALSE,
  inversePtWeighting = FALSE,
  bootstrapCi = FALSE,
  bootstrapReplicates = 200,
  interactionCovariateIds = c(),
  excludeCovariateIds = c(),
  includeCovariateIds = c(),
  profileGrid = NULL,
  profileBounds = c(log(0.1), log(10)),
  prior = createPrior(priorType = "laplace", useCrossValidation = TRUE),
  control = createControl(cvType = "auto", seed = 1, resetCoefficients = TRUE,
    startingVariance = 0.01, tolerance = 2e-07, cvRepetitions = 10, noiseLevel = "quiet")
)
```

## Arguments

- modelType:

  The type of outcome model that will be used. Possible values are
  "logistic", "poisson", or "cox".

- stratified:

  Should the regression be conditioned on the strata defined in the
  population object (e.g. by matching or stratifying on propensity
  scores)?

- useCovariates:

  Whether to use the covariates in the `cohortMethodData` object in the
  outcome model.

- inversePtWeighting:

  Use inverse probability of treatment weighting (IPTW)

- bootstrapCi:

  Compute confidence interval using bootstrapping instead of likelihood
  profiling?

- bootstrapReplicates:

  When using bootstrapping to compute confidence intervals, how many
  replicates should be sampled?

- interactionCovariateIds:

  An optional vector of covariate IDs to use to estimate interactions
  with the main treatment effect.

- excludeCovariateIds:

  Exclude these covariates from the outcome model.

- includeCovariateIds:

  Include only these covariates in the outcome model.

- profileGrid:

  A one-dimensional grid of points on the log(relative risk) scale where
  the likelihood for coefficient of variables is sampled. See details.

- profileBounds:

  The bounds (on the log relative risk scale) for the adaptive sampling
  of the likelihood function. See details.

- prior:

  The prior used to fit the model. See
  [`Cyclops::createPrior()`](https://rdrr.io/pkg/Cyclops/man/createPrior.html)
  for details. The prior is only applied to non-treatment variables, so
  is not used when `useCovariates = FALSE`.

- control:

  The control object used to control the cross-validation used to
  determine the hyperparameters of the prior (if applicable). See
  [`Cyclops::createControl()`](https://rdrr.io/pkg/Cyclops/man/createControl.html)
  for details.

## Value

An object of type `ComputeCovariateBalanceArgs`.

## Details

Create an object defining the parameter values.

For likelihood profiling, either specify the `profileGrid` for a
completely user- defined grid, or `profileBounds` for an adaptive grid.
Both should be defined on the log effect size scale. When both
`profileGrid` and `profileGrid` are `NULL` likelihood profiling is
disabled.

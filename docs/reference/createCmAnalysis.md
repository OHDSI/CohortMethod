# Create a CohortMethod analysis specification

Create a CohortMethod analysis specification

## Usage

``` r
createCmAnalysis(
  analysisId = 1,
  description = "",
  getDbCohortMethodDataArgs,
  createStudyPopulationArgs,
  createPsArgs = NULL,
  trimByPsArgs = NULL,
  truncateIptwArgs = NULL,
  matchOnPsArgs = NULL,
  stratifyByPsArgs = NULL,
  computeSharedCovariateBalanceArgs = NULL,
  computeCovariateBalanceArgs = NULL,
  fitOutcomeModelArgs = NULL
)
```

## Arguments

- analysisId:

  An integer that will be used later to refer to this specific set of
  analysis choices.

- description:

  A short description of the analysis.

- getDbCohortMethodDataArgs:

  An object representing the arguments to be used when calling the
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md)
  function.

- createStudyPopulationArgs:

  An object representing the arguments to be used when calling the
  [`createStudyPopulation()`](https://ohdsi.github.io/CohortMethod/reference/createStudyPopulation.md)
  function.

- createPsArgs:

  An object representing the arguments to be used when calling the
  [`createPs()`](https://ohdsi.github.io/CohortMethod/reference/createPs.md)
  function.

- trimByPsArgs:

  An object representing the arguments to be used when calling the
  [`trimByPs()`](https://ohdsi.github.io/CohortMethod/reference/trimByPs.md)
  function.

- truncateIptwArgs:

  An object representing the arguments to be used when calling the
  [`truncateIptw()`](https://ohdsi.github.io/CohortMethod/reference/truncateIptw.md)
  function.

- matchOnPsArgs:

  An object representing the arguments to be used when calling the
  [`matchOnPs()`](https://ohdsi.github.io/CohortMethod/reference/matchOnPs.md)
  function.

- stratifyByPsArgs:

  An object representing the arguments to be used when calling the
  [`stratifyByPs()`](https://ohdsi.github.io/CohortMethod/reference/stratifyByPs.md)
  function.

- computeSharedCovariateBalanceArgs:

  An object representing the arguments to be used when calling the
  [`computeCovariateBalance()`](https://ohdsi.github.io/CohortMethod/reference/computeCovariateBalance.md)
  function per target-comparator-analysis.

- computeCovariateBalanceArgs:

  An object representing the arguments to be used when calling the
  [`computeCovariateBalance()`](https://ohdsi.github.io/CohortMethod/reference/computeCovariateBalance.md)
  function per target-comparator-outcome-analysis.

- fitOutcomeModelArgs:

  An object representing the arguments to be used when calling the
  [`fitOutcomeModel()`](https://ohdsi.github.io/CohortMethod/reference/fitOutcomeModel.md)
  function.

## Value

An object of type `CmAnalysis`, to be used with the
[runCmAnalyses](https://ohdsi.github.io/CohortMethod/reference/runCmAnalyses.md)
function.

## Details

Create a set of analysis choices, to be used with the
[`runCmAnalyses()`](https://ohdsi.github.io/CohortMethod/reference/runCmAnalyses.md)
function.

Providing a NULL value for any of the argument applies the corresponding
step will not be executed. For example, if `createPsArgs = NULL`, no
propensity scores will be computed.

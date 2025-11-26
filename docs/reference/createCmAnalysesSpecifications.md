# Create full CM analysis specifications

Create full CM analysis specifications

## Usage

``` r
createCmAnalysesSpecifications(
  cmAnalysisList,
  targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = TRUE,
  cmDiagnosticThresholds = createCmDiagnosticThresholds()
)
```

## Arguments

- cmAnalysisList:

  A list of objects of type `cmAnalysis` as created using the
  \`[createCmAnalysis](https://ohdsi.github.io/CohortMethod/reference/createCmAnalysis.md)
  function.

- targetComparatorOutcomesList:

  A list of objects of type `targetComparatorOutcomes` as created using
  the
  [createTargetComparatorOutcomes](https://ohdsi.github.io/CohortMethod/reference/createTargetComparatorOutcomes.md)
  function.

- analysesToExclude:

  Analyses to exclude. See the Analyses to Exclude section for details.

- refitPsForEveryOutcome:

  Should the propensity model be fitted for every outcome (i.e. after
  people who already had the outcome are removed)? If false, a single
  propensity model will be fitted, and people who had the outcome
  previously will be removed afterwards.

- refitPsForEveryStudyPopulation:

  Should the propensity model be fitted for every study population
  definition? If false, a single propensity model will be fitted, and
  the study population criteria will be applied afterwards.

- cmDiagnosticThresholds:

  An object of type `CmDiagnosticThresholds` as created using
  [`createCmDiagnosticThresholds()`](https://ohdsi.github.io/CohortMethod/reference/createCmDiagnosticThresholds.md).

## Value

An object of type `CmAnalysesSpecifications`.

## Details

### Analyses to Exclude

Normally, `runCmAnalyses` will run all combinations of
target-comparator-outcome-analyses settings. However, sometimes we may
not need all those combinations. Using the `analysesToExclude` argument,
we can remove certain items from the full matrix. This argument should
be a data frame with at least one of the following columns:

- targetId

- comparatorId

- nestingCohortId

- outcomeId

- analysisId

This data frame will be joined to the outcome model reference table
before executing, and matching rows will be removed. For example, if one
specifies only one target ID and analysis ID, then any analyses with
that target and that analysis ID will be skipped.

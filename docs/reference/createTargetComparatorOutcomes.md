# Create target-comparator-outcomes combinations.

Create target-comparator-outcomes combinations.

## Usage

``` r
createTargetComparatorOutcomes(
  targetId,
  comparatorId,
  outcomes,
  nestingCohortId = NULL,
  excludedCovariateConceptIds = c(),
  includedCovariateConceptIds = c()
)
```

## Arguments

- targetId:

  A cohort ID identifying the target exposure in the exposure table.

- comparatorId:

  A cohort ID identifying the comparator exposure in the exposure table.

- outcomes:

  A list of object of type `Outcome` as created by
  [`createOutcome()`](https://ohdsi.github.io/CohortMethod/reference/createOutcome.md).

- nestingCohortId:

  (Optional) the nesting cohort ID. If provided, this will override the
  nesting cohort ID used in
  [`createGetDbCohortMethodDataArgs()`](https://ohdsi.github.io/CohortMethod/reference/createGetDbCohortMethodDataArgs.md).

- excludedCovariateConceptIds:

  A list of concept IDs that cannot be used to construct covariates.
  This argument is to be used only for exclusion concepts that are
  specific to the target-comparator combination.

- includedCovariateConceptIds:

  A list of concept IDs that must be used to construct covariates. This
  argument is to be used only for inclusion concepts that are specific
  to the target-comparator combination.

## Value

An object of type `TargetComparatorOutcomes`.

## Details

Create a set of hypotheses of interest, to be used with the
[`runCmAnalyses()`](https://ohdsi.github.io/CohortMethod/reference/runCmAnalyses.md)
function.

# Create a study population

Create a study population

## Usage

``` r
createStudyPopulation(
  cohortMethodData,
  population = NULL,
  outcomeId = NULL,
  createStudyPopulationArgs = createCreateStudyPopulationArgs()
)
```

## Arguments

- cohortMethodData:

  An object of type
  [CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
  as generated using
  [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md).

- population:

  If specified, this population will be used as the starting point
  instead of the cohorts in the `cohortMethodData` object.

- outcomeId:

  The ID of the outcome. If NULL, no outcome-specific transformations
  will be performed.

- createStudyPopulationArgs:

  An object of type `CreateStudyPopulationArgs` as created by the
  [`createCreateStudyPopulationArgs()`](https://ohdsi.github.io/CohortMethod/reference/createCreateStudyPopulationArgs.md)
  function.

## Value

A `tibble` specifying the study population. This `tibble` will have the
following columns:

- `rowId`: A unique identifier for an exposure.

- `personSeqId`: The person sequence ID of the subject.

- `cohortStartdate`: The index date.

- `outcomeCount` The number of outcomes observed during the risk window.

- `timeAtRisk`: The number of days in the risk window.

- `survivalTime`: The number of days until either the outcome or the end
  of the risk window.

## Details

Create a study population by enforcing certain inclusion and exclusion
criteria, defining a risk window, and determining which outcomes fall
inside the risk window.

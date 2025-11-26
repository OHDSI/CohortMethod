# Run a list of analyses

Run a list of analyses

## Usage

``` r
runCmAnalyses(
  connectionDetails,
  cdmDatabaseSchema,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  exposureDatabaseSchema = cdmDatabaseSchema,
  exposureTable = "drug_era",
  outcomeDatabaseSchema = cdmDatabaseSchema,
  outcomeTable = "condition_occurrence",
  nestingCohortDatabaseSchema = cdmDatabaseSchema,
  nestingCohortTable = "cohort",
  outputFolder = "./CohortMethodOutput",
  multiThreadingSettings = createMultiThreadingSettings(),
  cmAnalysesSpecifications
)
```

## Arguments

- connectionDetails:

  An R object of type `connectionDetails` created using the
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.

- cdmDatabaseSchema:

  The name of the database schema that contains the OMOP CDM instance.
  Requires read permissions to this database. On SQL Server, this should
  specify both the database and the schema, so for example
  'cdm_instance.dbo'.

- tempEmulationSchema:

  Some database platforms like Oracle and Impala do not truly support
  temp tables. To emulate temp tables, provide a schema with write
  privileges where temp tables can be created.

- exposureDatabaseSchema:

  The name of the database schema that is the location where the
  exposure data used to define the exposure cohorts is available. If
  exposureTable = DRUG_ERA, exposureDatabaseSchema is not used by
  assumed to be cdmSchema. Requires read permissions to this database.

- exposureTable:

  The tablename that contains the exposure cohorts. If exposureTable
  \<\> DRUG_ERA, then expectation is exposureTable has format of COHORT
  table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
  COHORT_END_DATE.

- outcomeDatabaseSchema:

  The name of the database schema that is the location where the data
  used to define the outcome cohorts is available. If exposureTable =
  CONDITION_ERA, exposureDatabaseSchema is not used by assumed to be
  cdmSchema. Requires read permissions to this database.

- outcomeTable:

  The tablename that contains the outcome cohorts. If outcomeTable \<\>
  CONDITION_OCCURRENCE, then expectation is outcomeTable has format of
  COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
  COHORT_END_DATE.

- nestingCohortDatabaseSchema:

  The name of the database schema that is the location where the data
  used to define the nesting cohorts is available.

- nestingCohortTable:

  The tablename that contains the nesting cohorts. Must have the format
  of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
  COHORT_END_DATE.

- outputFolder:

  Name of the folder where all the outputs will written to.

- multiThreadingSettings:

  An object of type `CmMultiThreadingSettings` as created using the
  [`createMultiThreadingSettings()`](https://ohdsi.github.io/CohortMethod/reference/createMultiThreadingSettings.md)
  or
  [`createDefaultMultiThreadingSettings()`](https://ohdsi.github.io/CohortMethod/reference/createDefaultMultiThreadingSettings.md)
  functions.

- cmAnalysesSpecifications:

  An object of type `CmAnalysesSpecifications` as created using the
  [`createCmAnalysesSpecifications()`](https://ohdsi.github.io/CohortMethod/reference/createCmAnalysesSpecifications.md).

## Value

A tibble describing for each target-comparator-outcome-analysisId
combination where the intermediary and outcome model files can be found,
relative to the `outputFolder`.

## Details

Run a list of analyses for the target-comparator-outcomes of interest.
This function will run all specified analyses against all hypotheses of
interest, meaning that the total number of outcome models is
`length(cmAnalysisList) * length(targetComparatorOutcomesList)` (if all
analyses specify an outcome model should be fitted). When you provide
several analyses it will determine whether any of the analyses have
anything in common, and will take advantage of this fact. For example,
if we specify several analyses that only differ in the way the outcome
model is fitted, then this function will extract the data and fit the
propensity model only once, and re-use this in all the analysis.

After completion, a tibble containing references to all generated files
can be obtained using the
[`getFileReference()`](https://ohdsi.github.io/CohortMethod/reference/getFileReference.md)
function. A summary of the analysis results can be obtained using the
[`getResultsSummary()`](https://ohdsi.github.io/CohortMethod/reference/getResultsSummary.md)
function. Diagnostics can be loaded using the
[`getDiagnosticsSummary()`](https://ohdsi.github.io/CohortMethod/reference/getDiagnosticsSummary.md)
function.

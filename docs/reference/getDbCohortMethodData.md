# Get the cohort data from the server

This function executes a large set of SQL statements against the
database in OMOP CDM format to extract the data needed to perform the
analysis.

## Usage

``` r
getDbCohortMethodData(
  connectionDetails,
  cdmDatabaseSchema,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  targetId,
  comparatorId,
  outcomeIds,
  exposureDatabaseSchema = cdmDatabaseSchema,
  exposureTable = "drug_era",
  outcomeDatabaseSchema = cdmDatabaseSchema,
  outcomeTable = "condition_occurrence",
  nestingCohortDatabaseSchema = cdmDatabaseSchema,
  nestingCohortTable = "cohort",
  getDbCohortMethodDataArgs = createGetDbCohortMethodDataArgs()
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

- targetId:

  A unique identifier to define the target cohort. If exposureTable =
  DRUG_ERA, targetId is a concept ID and all descendant concepts within
  that concept ID will be used to define the cohort. If exposureTable
  \<\> DRUG_ERA, targetId is used to select the COHORT_DEFINITION_ID in
  the cohort-like table.

- comparatorId:

  A unique identifier to define the comparator cohort. If exposureTable
  = DRUG_ERA, comparatorId is a concept ID and all descendant concepts
  within that concept ID will be used to define the cohort. If
  exposureTable \<\> DRUG_ERA, comparatorId is used to select the
  COHORT_DEFINITION_ID in the cohort-like table.

- outcomeIds:

  A list of cohort IDs used to define outcomes.

- exposureDatabaseSchema:

  The name of the database schema that is the location where the
  exposure data used to define the exposure cohorts is available.

- exposureTable:

  The tablename that contains the exposure cohorts. If exposureTable
  \<\> DRUG_ERA, then expectation is `exposureTable` has format of
  COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
  COHORT_END_DATE.

- outcomeDatabaseSchema:

  The name of the database schema that is the location where the data
  used to define the outcome cohorts is available.

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

- getDbCohortMethodDataArgs:

  An object of type `GetDbCohortMethodDataArgs` as created by the
  [`createGetDbCohortMethodDataArgs()`](https://ohdsi.github.io/CohortMethod/reference/createGetDbCohortMethodDataArgs.md)
  function.

## Value

A
[CohortMethodData](https://ohdsi.github.io/CohortMethod/reference/CohortMethodData-class.md)
object.

## Details

Based on the arguments, the treatment and comparator cohorts are
retrieved, as well as outcomes occurring in exposed subjects. The
treatment and comparator cohorts can be identified using the DRUG_ERA
table, or through user-defined cohorts in a cohort table either inside
the CDM schema or in a separate schema. Similarly, outcomes are
identified using the CONDITION_ERA table or through user-defined cohorts
in a cohort table either inside the CDM schema or in a separate schema.
Optionally, the target and comparator cohorts can be restricted to be
within a nesting cohort, which can reside in a different database schema
and table.

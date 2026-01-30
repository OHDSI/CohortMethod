# Create a parameter object for the function [`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md)

Create a parameter object for the function
[`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md)

## Usage

``` r
createGetDbCohortMethodDataArgs(
  removeDuplicateSubjects = "keep first, truncate to second",
  firstExposureOnly = TRUE,
  washoutPeriod = 365,
  nestingCohortId = NULL,
  restrictToCommonPeriod = TRUE,
  studyStartDate = "",
  studyEndDate = "",
  maxCohortSize = 0,
  covariateSettings
)
```

## Arguments

- removeDuplicateSubjects:

  Remove subjects that are in both the target and comparator cohort? See
  details for allowed values.Note that this is typically done in the
  createStudyPopulation function, but can already be done here for
  efficiency reasons.

- firstExposureOnly:

  Should only the first exposure per subject be included? Note that this
  is typically done in the createStudyPopulation() function, but can
  already be done here for efficiency reasons.

- washoutPeriod:

  The minimum required continuous observation time prior to index date
  for a person to be included in the cohort. Note that this is typically
  done in the createStudyPopulation function, but can already be done
  here for efficiency reasons.

- nestingCohortId:

  A cohort definition ID identifying the records in the
  `nestingCohortTable` to use as nesting cohort.

- restrictToCommonPeriod:

  Restrict the analysis to the period when both treatments are observed?

- studyStartDate:

  A calendar date specifying the minimum date that a cohort index date
  can appear. Date format is 'yyyymmdd'.

- studyEndDate:

  A calendar date specifying the maximum date that a cohort index date
  can appear. Date format is 'yyyymmdd'. Important: the study end data
  is also used to truncate risk windows, meaning no outcomes beyond the
  study end date will be considered.

- maxCohortSize:

  If either the target or the comparator cohort is larger than this
  number it will be sampled to this size. maxCohortSize = 0 indicates no
  maximum size.

- covariateSettings:

  An object of type covariateSettings as created using the
  FeatureExtraction::createCovariateSettings() function, or a list of
  covariate settings objects.

## Value

An object of type `GetDbCohortMethodDataArgs`.

## Details

Create an object defining the parameter values.

The `removeduplicateSubjects` argument can have one of the following
values:

- `"keep first, truncate to second"`: When a subjects appear in both
  target and comparator cohort, only keep whichever cohort is first in
  time. If the other cohort starts before the first has ended, the first
  cohort will be truncated to stop the day before the second starts. If
  both cohorts start simultaneous, the person is removed from the
  analysis.

- `"keep first"`: When a subjects appear in both target and comparator
  cohort, only keep whichever cohort is first in time. If both cohorts
  start simultaneous, the person is removed from the analysis.

- `"remove all"`: Remove subjects that appear in both target and
  comparator cohort completely from the analysis."

- `"keep all"`: Do not remove subjects that appear in both target and
  comparator cohort

# Cohort Method Data

`CohortMethodData` is an S4 class that inherits from
[CoviarateData](https://rdrr.io/pkg/FeatureExtraction/man/CovariateData-class.html),
which in turn inherits from
[Andromeda](https://rdrr.io/pkg/Andromeda/man/Andromeda-class.html). It
contains information on the cohorts, their outcomes, and baseline
covariates. Information about multiple outcomes can be captured at once
for efficiency reasons.

A `CohortMethodData` is typically created using
[`getDbCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/getDbCohortMethodData.md),
can only be saved using
[`saveCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/saveCohortMethodData.md),
and loaded using
[`loadCohortMethodData()`](https://ohdsi.github.io/CohortMethod/reference/loadCohortMethodData.md).

## Usage

``` r
# S4 method for class 'CohortMethodData'
show(object)

# S4 method for class 'CohortMethodData'
summary(object)
```

## Arguments

- object:

  An object of type `CohortMethodData`.

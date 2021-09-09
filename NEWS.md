CohortMethod 4.2.1
==================

Changes:

1. Adding `highlightExposedEvents` and `includePostIndexTime` arguments to `plotTimeToEvent()`.

2. Adding `maxCohortSize` argument to the `computeCovariateBalance()` function. The target and comparator cohorts will be downsampled if they are larger, speeding up computation.

Bug fixes:

1. Fixed error thrown by `plotTimeToEvent()` when there are time periods in plot when nobody is observed.


CohortMethod 4.2.0
==================

Changes:

1. Adding the `trimByIptw()` function.

2. Adding the `estimator` argument to the `fitOutcomeModel()` function to select 'ate' (average treatment effect) or 'att' (average treatment effect in the treated) when performing IPTW.

3. Added the `maxWeight` argument to the `fitOutcomeModel()` function. Weights greater than this value will be set to this value.

4. Adding option to use adaptive likelihood profiling, and making this the default.

5. Adding `maxDaysAtRisk` argument to the `createStudyPopulation()` and `createCreateStudyPopulationArgs()` functions.


Bug fixes:

1. Fixing IPTW.

2. Fixing error when stratifying and base population is empty (but overall population is not).


CohortMethod 4.1.0
==================

Changes: 

1. Dropped `insertDbPopulation()` function. This didn't seem to be used by anyone, and would have required carrying the person ID throughout the pipeline.

2. Introducing new unique person identified called `personSeqId`, generated during data extraction. Person ID is now downloaded as string to avoid issues with 64-bit integers. Person ID is not used by CohortMethod, and is provided for reference only.

3. Adding log likelihood ratio to outcome model object.

4. Deprecating `oracleTempSchema` argument in favor of `tempEmulationSchema` in line with new `SqlRender` interface.

Bug fixes:

1. Still was not always including the likelihood profile in the outcome model objects.

2. Fixing issues when IDs are `integer64`.


CohortMethod 4.0.1
==================

Changes:

1. Always including the likelihood profile in the outcome model objects.

Bug fixes:

1. Fixing "argument 'excludeDrugsFromCovariates' is missing" error when calling `createGetDbCohortMethodDataArgs()` without deprecated argument `excludeDrugsFromCovariates`.

2. More testing and handling of empty exposure cohorts.

3. Fixing exclusion of covariate IDs when fitting propensity models.

4. Correct covariate balance computation when covariate values are integers.


CohortMethod 4.0.0
==================

Changes: 

1. Switching from ff to Andromeda for storing large data objects.

Bugfixes:

1. Fixed bug in IPTW.


CohortMethod 3.1.1
==================

Changes:

1. Updating documentation: adding literature reference for IPTW, and using new SqlRender interface in vignettes.

2. Changing default equipoise bounds from 0.25-0.75 to 0.3-0.7 to be consistent with Alec Walker's original paper.

Bugfixes:

1. Fixing some issues when sampling before fitting propensity models.


CohortMethod 3.1.0
==================

Changes:

1. Added plotTimeToEvent function

2. Deprecating addExposureDaysToStart and addExposureDaysToEnd arguments, adding new arguments called startAnchor and endAnchor. The hope is this is less confusing.

3. Fixing random seeds for reproducibility.

4. Changing default equipoise bounds from 0.25-0.75 to 0.3-0.7 to be consistent with Alec Walker's original paper.

Bugfixes:

1. No longer overriding ffmaxbytes and ffbatchbytes in .onLoad. Instead relying on FeatureExtraction to do that. Part of fixing chunk.default error caused by ff package on R v3.6.0 on machines with lots of memory.

2. Correct calculation in original population count when using study end date.


CohortMethod 3.0.2
==================

Changes:

1. (Much) faster variable ratio matching

CohortMethod 6.0.0
==================

This major release introduces many changes. The three most important ones are (1) changing the settings objects, (2) supporting nesting cohorts, and (3) covariate balance significance testing.


## Changes related to the settings objects

1. All settings objects have been changed to R6 classes, and are now used both when calling functions individually and when using `runCmAnalyses()`. The main rationale is that allows 3rd parties to more easily generate valid settings.

2. Dropped the `cdmVersion` argument in `getDbCohortMethodData()` and `runCmAnalyses()`. The version will be identified in the `cdm_source` table.

3. Dropped the `trimByIptw()` and `trimByPsToEquipoise()` functions. Added `equipoiseBounds` and `maxWeight` arguments to `createTrimByPsArgs()` so functionality remains.

4. Dropped the `matchOnPsAndCovariates()` function and added a `stratificationCovariateIds` argument to `createMatchOnPsArgs()` so functionality remains.

5. Dropped the `stratifyByPsAndCovariates()` function and added a `stratificationCovariateIds` argument to `createStratifyByPsPsArgs()` so functionality remains.

6. Renamed `createStudyPopArgs` argument of `createCmAnalysis()` to `createStudyPopulationArgs` for consistency.

7. Dropping the deprecated `attritionFractionThreshold` argument of `createCmDiagnosticThresholds()`. The amount of attrition is not a good measure of generalizability. Use the generalizability diagnostic instead, which measures the similarity between the target and analytic cohort characteristcs.

8. Changed the default outcome model type from 'logistic' to 'cox'.

9. Set the defaults of `createGetDbCohortMethodDataArgs()` to those most often used.

10. Dropped the `firstExposureOnly`, `restrictToCommonPeriod`, `washoutPeriod`, and `removeDuplicateSubjects` arguments from `CreateStudyPopulationArgs`. These were duplicated from `getDbCohortMethodData()`, and we'll keep them only there from now on.


## Changes related to nesting cohorts

11. Added ability to restrict to a nesting cohort (e.g. restricting drug exposures to a specific indication). See the `nestingCohortId` argument in the `createGetDbCohortMethodDataArgs()` and `createTargetComparatorOutcomes()` functions and the `nestingCohortDatabaseSchema` and `nestingCohortTable` arguments in the `getDbCohortMethodData()` function. 

12. The results schema now includes the `target_comparator` table that combines the `target_id`, `comparator_id`, and `nesting_cohort_id` into a single unique `target_comparator_id`. This new ID is a hash of its components, allowing results from multiple runs to be combined into a single database. 

13. In addition to restricting to a nesting cohort the population can now also be restricted by age and gender using the `minAge`, `maxAge`, and `genderConceptIds` arguments of `createGetDbCohortMethodDataArgs()`.


## Changes related to the new covariate balance diagnostic

14. Added optional significance testing to covariate balance. This avoids failing the balance diagnostic on smaller databases just because of random chance, and was found to be superior in our methods research. This introduces the following changes to the interface:

    - Added the `threshold` and `alpha` arguments to the `createComputeCovariateBalanceArgs()` function. These do not impact blinding when running `runCmAnalyses` but do add columns to the balance files, for when running single studies.
    - Added the `sdmAlpha` argument to the `createCmDiagnosticThresholds()` function. 

    This adds the `sdm_family_wise_min_p` and `shared_sdm_family_wise_min_p` fields to the `cm_diagnostics_summary` table when exporting to CSV. 
    For now, the default is not to use significance testing, but the family-wise min P can help understand if one would have passed when using it.


## Other important changes

15. Added a new option for the `removeDuplicateSubjects` argument:  "keep first, truncate to second". This is similar to "keep first", but also truncates the first exposure to stop the day before the second starts.

16. Now performing empirical calibration *after* removing estimates that fail diagnostics. In general this should lead to narrower calibrated confidence intervals.

17. If high correlation is detected when fitting a propensity model, but `stopOnError = FALSE`, the export will show the highly correlated covariates in the model with extreme coefficients (1e6 * correlation).

18. Added the ability to use bootstrap for computing confidence intervals. See the `bootstrapCi` and `bootstrapReplicates` arguments of `createFitOutcomeModelArgs()`.

19. All restrictions on the study populations performed by `getDbCohortMethodData()` are now step-by-step recorded in the attrition table.

20. Completely updated of all unit tests to increase coverage of functional tests, while also increasing speed.

21. Renamed the `showEquipoiseLabel` argument of `plotPs()` to `showEquipoiseLabel`.

22. Updated the `trimByPs()` function. This now supports three different trimming functions as described in the literature.


## Minor changes
 
23. Added support for grid-with-gradient likelihood profiles. Use the following arguments in `createFitOutcomeModelArgs()` to use:
    
    ```r
    profileGrid = seq(log(0.1), log(10), length.out = 8),
    profileBounds = NULL
    ```    
    
    This adds the `gradient` field to the `cm_likelihood_profile` table when exporting to CSV.

23. Removed mention of legacy function `grepCovariateNames()` from the vignette.

24. Added citation to the HADES paper to the package.

25. Dropped `insertExportedResultsInSqlite()`, `launchResultsViewerUsingSqlite`, and `launchResultsViewer()`. The `OhdsiShinyAppBuilder` package should be used directly instead.

26. Corrected the `minDaysAtRisk` argument. Days at risk is now computed as end - start + 1 (end day inclusive).

27. Added a vignette showing the results schema.

28. Changed the data type of the `interaction_covariate_id` field in the `cm_interaction_result` table from `INT` to `BIGINT`.

29. Fixed trimming by IPTW using `trimFraction` argument.


CohortMethod 5.5.2
==================

Bugfixes:

1. Reorganized `createPs()` to be much more efficient for large study populations (millions of patients).

2. Reorganized `fitOutcomeModel()` to be much more efficient for large study populations (millions of patients).


CohortMethod 5.5.1
==================

Bugfixes:

1. Fixed error thrown by Kaplan Meier curves functions when only one of the cohorts is empty.


CohortMethod 5.5.0
==================

Changes:

1. `createPs()` now checks if filtering of the covariate data is necessary (either because subject have been removed from the study population or because `excludeCovariateIds` or `includeCovariateIds` was specified). If no filtering is required, no extra copy of the covariate data data is created, saving IO time.

2. Added `minimumCaseCount` argument to `createCohortMethodDataSimulationProfile()`.

3. Preparing for `Andromeda 1.0.0`: no longer assuming Andromeda tables are sorted.

4. Changing dependency from `ShinyAppBuilder` to `OhdsiShinyAppBuilder`.


Bugfixes:

1. Fixed NA covariate prevalences when calling `createCohortMethodDataSimulationProfile()`.

2. Added some optimization to `createPs()` to prevent running out of memory for large data objects using Andromeda >= 1.0.0.


CohortMethod 5.4.0
==================

Changes:

1. Updating viewer code to work with newer versions of `OhdsiShinyModules` and `ShinyAppBuilder`.

2. Dropped `uploadExportedResults()` function.

3. The `cohorts` argument of `insertExportedResultsInSqlite()` has column `cohortId` renamed to `cohortDefinitionId`.

4. Added computation of MDRR for logistic models.



CohortMethod 5.3.0
==================

Changes:

1. Switching cohort IDs in results model to BIGINT.

Bugfixes:

1. Fix `enforceCellCount()` applied to covariate balance when all balance is NA.

2. Stopping fitting PS model early when either target or comparator is empty. Prevents error when target or comparator is empty, sampling is required, and Cyclops happens to fit a model instead of declaring ILL CONDITIONED. 

3. Message after matching on PS now shows correct number of subjects remaining after matching.


CohortMethod 5.2.1
==================

Changes:

1. Ask to delete files in output folder when calling `runCmAnalyses()` with different analyses settings than those used to create the files. Also cleaning the cache.

Bugfixes:

1. Fixed bug in parsing covariate filter settings for balance.

2. Updated vignettes to use latest `Capr` functions.


CohortMethod 5.2.0
==================

Changes:

1. The `computeCovariateBalance()` function now also computes standardized difference of mean comparing cohorts before and after PS adjustment, which can inform on generalizability.

2. Added the `getGeneralizabilityTable()` function.

3. Improved computation of overall standard deviation when computing covariate balance (actually computing the SD instead of taking the mean of the target and comparator). Should produce more accurate balance estimations.

4. Generated population objects now keep track of likely target estimator (e.g. 'ATT', or 'ATE'). This informs selection of base population when calling `getGeneralizabilityTable()`. 

5. Deprecated the `attritionFractionThreshold` argument of the `createCmDiagnosticThresholds` function, and instead added the `generalizabilitySdmThreshold` argument.

6. The results schema specifications of the `exportToCsv()` function has changed:
    - Removed the `attrition_fraction` and `attrition_diagnostic` fields from the `cm_diagnostics_summary ` table.
    - Added the `target_estimator` field to the `cm_result` add `cm_interaction_result` tables.
    - Added the `generalizability_max_sdm` and `generalizabiltiy_diagnostic` fields to the `cm_diagnostics_summary` table.
    - Added the `mean_before`, `mean_after`, `target_std_diff`, `comparator_std_diff`, and `target_comparator_std_diff` fields to both the `cm_covariate_balance` and `cm_shared_covariate_balance` tables.
    
7. Improve speed of covariate balance computation.

8. Adding one-sided (calibrated) p-values to results summary and results model.

9. Adding `unblind_for_evidence_synthesis` field to `cm_diagnostics_summary` table.

10. The `cm_diagnostics_summary` table now also contains negative controls. 
    

Bugfixes:

1. Fixing `runCmAnalyses()` when using `refitPsForEveryOutcome = TRUE`.

2. Handling edge case when exporting preference distribution and the target or comparator only has 1 subject.


CohortMethod 5.1.0
==================

Changes:

1. Now also not unblinding if shared balance diagnostic fails.

Bugfixes:

1. Fixing matching on PS and other covariates.

2. Now passing outcome-specific `riskWindowEnd` argument in `runCmAnalyses()` when specified.

3. Fixed error when calling `createStudyPopulation()` with "keep first" when there is only 1 person in the population.


CohortMethod 5.0.0
==================

Changes:

1. Setting the default Cyclops control object to use `resetCoefficients = TRUE` to ensure we always get the exact same model, irrespective of the number of threads used.

2. Adding checking of user input to all functions.

3. Removing deprecated `excludeDrugsFromCovariates` argument from `getDbCohortMethodData()` function.

4. Removing deprecated `oracleTempSchema` argument from `getDbCohortMethodData()` and `runCmAnalyses()` functions.

5. Removing deprecated `addExposureDaysToStart` and `addExposureDaysToStart` arguments from `createStudyPopulation()` and `plotTimeToEvent()` functions.

6. The `removeDuplicateSubjects` argument of `getDbCohortMethodData()` and `createStudyPopulation()` is no longer allowed to be a boolean.

7. Adding `computeEquipoise()` function.

8. Output likelihood profile as data frame instead of named vector for consistency with other HADES packages.

9. Added the `covariateFilter` argument to the `computeCovariateBalance` function, to allow balance to be computed only for a subset of covariates.

10. Rounding propensity scores to 10 digits to improve reproducibility across operating systems.

11. Setting `covariateCohortDatabaseSchema` and `covariateCohortTable` of cohort-based covariate builders to `exposureDatabaseSchema` and `exposureTable`, respectively if `covariateCohortTable` is `NULL`.

12. Now computing IPTW in `createPs()`, and truncating IPTW can be done in `truncateIptw()`. The `computeCovariateBalance()` function now computes balance using IPTW if no `stratumId` column is found in the `population` argument.

13. Removing PS of exactly 0 and exactly 1 when computing the standard deviation of the logit for the matching caliper to allow matching when some subjects have perfectly predictable treatment assignment.

14. Adding `maxRows` argument to `computePsAuc()` function to improve speed for very large study populations. 

15. Dropping support for CDM v4.

16. Major overhaul of the multiple-analyses framework:

    - Added the `createOutcome()` function, to be used with `createTargetComparatorOutcomes()`. This allow the `priorOutcomeLookback`,  `riskWindowStart`, `startAnchor`, `riskWindowEnd`, and `endAnchor` arguments to be specified per outcome. These settings (if provided) will override the settings created using the `createCreateStudyPopulationArgs()` function. In addition, the `createOutcome()` function has an `outcomeOfInterest` and `trueEffectSize` argument (see below).

    - Added the `createComputeCovariateBalanceArgs()` function, added the `computeSharedCovariateBalance`, `,computeSharedCovariateBalanceArgs`, `computeCovariateBalance`, and `computeCovariateBalanceArgs` arguments to the `createCmAnalysis()` function, and the `computeSharedBalanceThreads`, `computeBalanceThreads` arguments to the `runCmAnalyses()` function to allow computation of covariate balance across a target-comparator-analysis (shared) or for each target-comparator-analysis-outcome in the `runCmAnalyses()` function.

    - Dropping `targetType` and `comparatorType` options from the `createCmAnalysis()` function, since the notion of analysis-specific target and comparator selection strategies can also be implemented using the `analysesToExclude` argument of `runCmAnalyses()`.

    - Dropping `outcomeIdsOfInterest` argument of the `runCmAnalyses()` function. Instead, the `createOutcome()` function now has a `outcomeOfInterest` argument.

    - Settings related to multi-threading are combined in to a single settings object that be created using the new `createCmMultiThreadingSettings()` function.

    - Dropping `prefilterCovariates` from `runCmAnalyses()`. Prefiltering is now always done when specific covariates are used in the outcome model.

    - Removed the `summarizeAnalyses()` function. Instead, results are automatically summarized in `runCmAnalyses()`. The summary can be retrieved using the new `getResultsSummary()` and `getInteractionResultsSummary()` functions. Empirical calibration, MDRR, and attrition fraction are automatically computed.
    
    - Changing case in output of `getResultsSummary()` from `ci95lb` and `ci95ub` to `ci95Lb` and `ci95Ub`.

    - Added empirical calibration to the `getResultsSummary()` function. Controls can be identified by the `trueEffectSize` argument in the `createOutcome()` function.
    
    - Dropping arguments like `createPs` and `fitOutcomeModel` from the `createCmAnalysis()` function. Instead, not providing `createPsArgs` or `fitOutcomeModelArgs` is assumed to mean skipping propensity score creation or outcome model fitting, respectively.

17. Added the `exportToCsv()` function for exporting study results to CSV files that do not contain patient-level information and can therefore be shared between sites. The `getResultsDataModel()` function returns the data model for these CSV files.
	
18. Added the `uploadExportedResults()` and `insertExportedResultsInSqlite()` functions for uploading the results from the CSV files in a database. The `launchResultsViewer()` and `launchResultsViewerUsingSqlite()` functions were added for launching a Shiny app to view the results in the (SQLite) database.
  
Bug fixes:

1. Fixed error when using integer `maxWeight` when performing IPTW.


CohortMethod 4.2.3
==================

Changes;

1. Removed `RISCA` from the Suggests list. This package was used for a single unit test, but has a large amount of difficult-to-install dependencies.

Bug fixes:

1. Fixed error when failing model fitting caused by new `Cyclops` version.


CohortMethod 4.2.2
==================

Changes:

1. Added the `analysesToExclude` argument to `runCmAnalyses`, allowing the users to specify target-comparator-outcome-analysis combinations to exclude from execution.

2. Output of `computeCovariateBalance()` now also contains `domainId` and `isBinary` columns.

3. Added `plotCovariatePrevalence()` function.


Bug fixes:

1. Fixed erroneous sample size reported for comparator cohorts when computing covariate balance. (the actual sample size was fine)

2. Fixed error when all analyses have `fitOutcomeModel = FALSE`.

3. Fixed attrition counts when using `allowReverseMatch = TRUE`

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

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

1. Fixed error when using integer `maxWeight` when performng IPTW.


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

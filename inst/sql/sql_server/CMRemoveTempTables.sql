TRUNCATE TABLE #new_user_cohort;

DROP TABLE #new_user_cohort;

{@indication_concept_ids != ''} ? {
TRUNCATE TABLE #indicated_cohort;

DROP TABLE #indicated_cohort;
}

TRUNCATE TABLE #non_overlap_cohort;

DROP TABLE #non_overlap_cohort;

TRUNCATE TABLE #cohort_person;

DROP TABLE #cohort_person;

TRUNCATE TABLE #cohort_covariate;

DROP TABLE #cohort_covariate;

TRUNCATE TABLE #cohort_covariate_ref;

DROP TABLE #cohort_covariate_ref;

TRUNCATE TABLE #cohort_outcome;

DROP TABLE #cohort_outcome;

TRUNCATE TABLE #cohort_excluded_person;

DROP TABLE #cohort_excluded_person;
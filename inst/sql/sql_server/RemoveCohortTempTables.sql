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

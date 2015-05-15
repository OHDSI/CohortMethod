TRUNCATE TABLE #new_user_cohort;

DROP TABLE #new_user_cohort;

{@has_indication_concept_ids} ? {
TRUNCATE TABLE #indications;

DROP TABLE #indications;


TRUNCATE TABLE #indicated_cohort;

DROP TABLE #indicated_cohort;
}

{@has_exclusion_concept_ids} ? {
TRUNCATE TABLE #exclusions;

DROP TABLE #exclusions;
}

TRUNCATE TABLE #non_overlap_cohort;

DROP TABLE #non_overlap_cohort;

TRUNCATE TABLE #cohort_person;

DROP TABLE #cohort_person;

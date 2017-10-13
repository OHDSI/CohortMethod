{DEFAULT @cdm_version = '5'}
{DEFAULT @target_id = '' }

SELECT COUNT(*) AS row_count,
	COUNT(DISTINCT subject_id) AS person_count,
{@cdm_version == "4"} ? {	
	CASE WHEN cohort_concept_id = @target_id THEN 1 ELSE 0 END AS treatment
} : {
	CASE WHEN cohort_definition_id = @target_id THEN 1 ELSE 0 END AS treatment
}
FROM #cohort_person
{@cdm_version == "4"} ? {	
GROUP BY cohort_concept_id
} : {
GROUP BY cohort_definition_id
}
;

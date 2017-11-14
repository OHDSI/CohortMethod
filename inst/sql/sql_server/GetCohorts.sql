{DEFAULT @cdm_version = '5'}
{DEFAULT @target_id = ''}
{DEFAULT @sampled = FALSE}

SELECT row_id,
	subject_id,
{@cdm_version == "4"} ? {	
	CASE WHEN cohort_concept_id = @target_id THEN 1 ELSE 0 END AS treatment,
} : {
	CASE WHEN cohort_definition_id = @target_id THEN 1 ELSE 0 END AS treatment,
}
	cohort_start_date,
	days_from_obs_start,
	days_to_cohort_end,
	days_to_obs_end
{@sampled} ? {
FROM #cohort_sample
} : {
FROM #cohort_person
}
ORDER BY subject_id

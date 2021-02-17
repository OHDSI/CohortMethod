{DEFAULT @cdm_version = '5'}
{DEFAULT @target_id = ''}
{DEFAULT @sampled = FALSE}

SELECT row_id,
	person_seq_id,
	CAST(cohort.subject_id AS VARCHAR(30)) AS person_id,
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
FROM #cohort_sample cohort
} : {
FROM #cohort_person cohort
}
INNER JOIN (
	SELECT subject_id,
		ROW_NUMBER() OVER (ORDER BY subject_id) AS person_seq_id
	FROM (
		SELECT DISTINCT subject_id
{@sampled} ? {
		FROM #cohort_sample
} : {
		FROM #cohort_person
}
		) tmp
	) unique_ids
	ON cohort.subject_id = unique_ids.subject_id
ORDER BY cohort.subject_id

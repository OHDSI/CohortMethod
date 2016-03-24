{DEFAULT @cdm_version = '4'}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'}

SELECT subject_id AS row_id,
	@cohort_definition_id AS treatment,
	cohort_start_date,
	DATEDIFF(DAY, cohort_start_date, observation_period_end_date) + 1 AS time_to_obs_period_end,
	DATEDIFF(DAY, cohort_start_date, cohort_end_date) + 1 AS time_to_cohort_end
FROM #cohort_person cohort
ORDER BY subject_id

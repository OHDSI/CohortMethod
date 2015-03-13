SELECT subject_id AS row_id,
	cohort_definition_id AS treatment,
	cohort_start_date,
	DATEDIFF(dd, cohort_start_date, observation_period_end_date) AS time_to_obs_period_end,
	DATEDIFF(dd, cohort_start_date, cohort_end_date) AS time_to_cohort_end
FROM #cohort_person cohort
INNER JOIN observation_period
ON cohort.subject_id = observation_period.person_id
AND cohort_start_date >= observation_period_start_date
AND cohort_start_date <= observation_period_end_date
ORDER BY subject_id

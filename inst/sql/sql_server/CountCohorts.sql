{DEFAULT @target_id = '' }

SELECT COUNT(*) AS row_count,
	COUNT(DISTINCT subject_id) AS person_count,
	CASE WHEN cohort_definition_id = @target_id THEN 1 ELSE 0 END AS treatment
FROM #cohort_person
GROUP BY cohort_definition_id;

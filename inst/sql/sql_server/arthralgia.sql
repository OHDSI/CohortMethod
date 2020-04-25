/***********************************
File arthralgia.sql
***********************************/

INSERT INTO @resultsDatabaseSchema.coxibVsNonselVsGiBleed (
	cohort_definition_id,
	cohort_start_date,
	cohort_end_date,
	subject_id
	)
SELECT 4, -- Indication
	MIN(condition_start_date),
	MIN(condition_end_date),
	person_id
FROM @cdmDatabaseSchema.condition_occurrence
WHERE condition_concept_id IN (
		SELECT descendant_concept_id
		FROM @cdmDatabaseSchema.concept_ancestor
		WHERE ancestor_concept_id = 36516812 -- Arthralgia
		)
GROUP BY person_id;

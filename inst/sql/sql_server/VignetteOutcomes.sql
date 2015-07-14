/***********************************
File VignetteOutcomes.sql
***********************************/
IF OBJECT_ID('@resultsDatabaseSchema.outcomes', 'U') IS NOT NULL
  DROP TABLE @resultsDatabaseSchema.outcomes;

SELECT ancestor_concept_id AS cohort_concept_id,
	condition_start_date AS cohort_start_date,
	condition_end_date AS cohort_end_date,
	condition_occurrence.person_id AS subject_id
INTO @resultsDatabaseSchema.outcomes
FROM @cdmDatabaseSchema.condition_occurrence
INNER JOIN @cdmDatabaseSchema.visit_occurrence
	ON condition_occurrence.visit_occurrence_id = visit_occurrence.visit_occurrence_id
INNER JOIN @cdmDatabaseSchema.concept_ancestor
	ON condition_concept_id = descendant_concept_id
WHERE ancestor_concept_id IN (192671, 29735, 140673, 197494, 198185, 198199, 200528, 257315, 314658, 317376, 321319, 380731, 432661, 432867, 433516, 433701, 433753, 435140,
435459, 435524, 435783, 436665, 436676, 442619, 444252, 444429, 4131756, 4134120, 4134454, 4152280, 4165112, 4174262, 4182210, 4270490, 4286201, 4289933)
	AND visit_occurrence.place_of_service_concept_id IN (9201, 9203);

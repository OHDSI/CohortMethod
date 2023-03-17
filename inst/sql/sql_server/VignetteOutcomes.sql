/***********************************
File VignetteOutcomes.sql
***********************************/
DROP TABLE IF EXISTS @resultsDatabaseSchema.outcomes;

SELECT ancestor_concept_id AS cohort_definition_id,
	condition_start_date AS cohort_start_date,
	condition_end_date AS cohort_end_date,
	condition_occurrence.person_id AS subject_id
INTO @resultsDatabaseSchema.outcomes
FROM @cdmDatabaseSchema.condition_occurrence
INNER JOIN @cdmDatabaseSchema.visit_occurrence
	ON condition_occurrence.visit_occurrence_id = visit_occurrence.visit_occurrence_id
INNER JOIN @cdmDatabaseSchema.concept_ancestor
	ON condition_concept_id = descendant_concept_id
WHERE ancestor_concept_id IN (192671, 24609, 29735, 73754, 80004, 134718, 139099, 141932, 192367, 193739, 194997, 197236, 199074, 255573, 257007, 313459, 314658, 316084, 319843, 321596, 374366, 375292, 380094, 433753, 433811, 436665, 436676, 436940, 437784, 438134, 440358, 440374, 443617, 443800, 4084966, 4288310)
	AND visit_occurrence.visit_concept_id IN (9201, 9203);

/***********************************
File coxibsVsNonselVsGiBleed.sql 
***********************************/

USE @cdmSchema;

IF OBJECT_ID('@resultsSchema.dbo.coxibVsNonselVsGiBleed', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.dbo.coxibVsNonselVsGiBleed;

CREATE TABLE @resultsSchema.dbo.coxibVsNonselVsGiBleed (
  cohort_definition_id INT,
	cohort_start_date DATE,
	cohort_end_date DATE,
	subject_id BIGINT
	);

INSERT INTO @resultsSchema.dbo.coxibVsNonselVsGiBleed (
	cohort_definition_id,
	cohort_start_date,
	cohort_end_date,
	subject_id
	)
SELECT 1, -- Exposure
	drug_era_start_date,
	drug_era_end_date,
	person_id
FROM drug_era
WHERE drug_concept_id = 1118084;-- celecoxib	

INSERT INTO @resultsSchema.dbo.coxibVsNonselVsGiBleed (
	cohort_definition_id,
	cohort_start_date,
	cohort_end_date,
	subject_id
	)
SELECT 2, -- Comparator
	drug_era_start_date,
	drug_era_end_date,
	person_id
FROM drug_era
WHERE drug_concept_id = 1124300; --Diclofenac

INSERT INTO @resultsSchema.dbo.coxibVsNonselVsGiBleed (
	cohort_definition_id,
	cohort_start_date,
	cohort_end_date,
	subject_id
	)
SELECT 3, -- Outcome
	condition_start_date,
	condition_end_date,
	condition_occurrence.person_id
FROM condition_occurrence
INNER JOIN visit_occurrence
	ON condition_occurrence.visit_occurrence_id = visit_occurrence.visit_occurrence_id
WHERE condition_concept_id IN (
		SELECT descendant_concept_id
		FROM concept_ancestor
		WHERE ancestor_concept_id = 192671 -- GI - Gastrointestinal haemorrhage
		)
	AND visit_occurrence.place_of_service_concept_id IN (9201, 9203); 
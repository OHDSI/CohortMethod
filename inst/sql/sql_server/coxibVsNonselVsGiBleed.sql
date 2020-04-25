/***********************************
File coxibVsNonselVsGiBleed.sql
***********************************/

IF OBJECT_ID('@resultsDatabaseSchema.coxibVsNonselVsGiBleed', 'U') IS NOT NULL
  DROP TABLE @resultsDatabaseSchema.coxibVsNonselVsGiBleed;

CREATE TABLE @resultsDatabaseSchema.coxibVsNonselVsGiBleed (
  cohort_definition_id INT,
  cohort_start_date DATE,
	cohort_end_date DATE,
	subject_id BIGINT
	);

INSERT INTO @resultsDatabaseSchema.coxibVsNonselVsGiBleed (
	cohort_definition_id,
	cohort_start_date,
	cohort_end_date,
	subject_id
	)
SELECT DISTINCT 1, -- Target
	drug_era_start_date,
	drug_era_end_date,
	drug_era.person_id
FROM (
	SELECT person_id,
		MIN(drug_era_start_date) AS drug_era_start_date,
		MIN(drug_era_end_date) AS drug_era_end_date
	FROM  @cdmDatabaseSchema.drug_era
	WHERE drug_concept_id = 1118084 -- celecoxib
	GROUP BY person_id
	) drug_era
INNER JOIN @cdmDatabaseSchema.condition_occurrence
  ON drug_era.person_id = condition_occurrence.person_id
  AND condition_start_date <= drug_era_start_date
WHERE condition_concept_id IN (
	SELECT descendant_concept_id
	FROM @cdmDatabaseSchema.concept_ancestor
	WHERE ancestor_concept_id = 36516812 -- Arthralgia
	);

INSERT INTO @resultsDatabaseSchema.coxibVsNonselVsGiBleed (
	cohort_definition_id,
	cohort_start_date,
	cohort_end_date,
	subject_id
	)
SELECT DISTINCT 2, -- Comparator
	drug_era_start_date,
	drug_era_end_date,
	drug_era.person_id
FROM (
	SELECT person_id,
		MIN(drug_era_start_date) AS drug_era_start_date,
		MIN(drug_era_end_date) AS drug_era_end_date
	FROM  @cdmDatabaseSchema.drug_era
	WHERE drug_concept_id = 1124300 -- diclofenac
	GROUP BY person_id
	) drug_era
INNER JOIN @cdmDatabaseSchema.condition_occurrence
  ON drug_era.person_id = condition_occurrence.person_id
  AND condition_start_date <= drug_era_start_date
WHERE condition_concept_id IN (
	SELECT descendant_concept_id
	FROM @cdmDatabaseSchema.concept_ancestor
	WHERE ancestor_concept_id = 36516812 -- Arthralgia
	);

INSERT INTO @resultsDatabaseSchema.coxibVsNonselVsGiBleed (
	cohort_definition_id,
	cohort_start_date,
	cohort_end_date,
	subject_id
	)
SELECT 3, -- Outcome
	condition_start_date,
	condition_end_date,
	condition_occurrence.person_id
FROM @cdmDatabaseSchema.condition_occurrence
INNER JOIN @cdmDatabaseSchema.visit_occurrence
	ON condition_occurrence.visit_occurrence_id = visit_occurrence.visit_occurrence_id
WHERE condition_concept_id IN (
		SELECT descendant_concept_id
		FROM @cdmDatabaseSchema.concept_ancestor
		WHERE ancestor_concept_id = 192671 -- GI - Gastrointestinal haemorrhage
		)
	AND visit_occurrence.visit_concept_id IN (9201, 9203);

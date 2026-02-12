/************************************************************************
Copyright 2026 Observational Health Data Sciences and Informatics

This file is part of CohortMethod

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************************/

{DEFAULT @cdm_database_schema = 'CDM_SIM' }
{DEFAULT @exposure_database_schema = 'CDM_SIM' }
{DEFAULT @exposure_table = 'drug_era' }
{DEFAULT @target_id = '' }
{DEFAULT @comparator_id = '' }
{DEFAULT @study_start_date = '' }
{DEFAULT @study_end_date = '' }
{DEFAULT @first_only = FALSE}
{DEFAULT @washout_period = 0}
{DEFAULT @remove_duplicate_subjects = 'keep all'}
{DEFAULT @min_age = ''}
{DEFAULT @max_age = ''}
{DEFAULT @gender_concept_ids = ''}
{DEFAULT @restrict_to_common_period = FALSE}
{DEFAULT @use_nesting_cohort = FALSE}
{DEFAULT @nesting_cohort_database_schema = 'CDM_SIM'}
{DEFAULT @nesting_cohort_table = 'cohort'}
{DEFAULT @nesting_cohort_id = ''}
{DEFAULT @action = 'CREATE'}

{@action == 'CREATE'} ? {
  DROP TABLE IF EXISTS #cohort_person;
}

-- raw_cohorts
WITH raw_cohorts AS (
{@exposure_table == 'drug_era' } ? {
	SELECT exposure_table.person_id AS subject_id,
		drug_concept_id AS cohort_definition_id,
		drug_era_start_date AS cohort_start_date,
		drug_era_end_date AS cohort_end_date
	FROM  @exposure_database_schema.@exposure_table exposure_table
	WHERE exposure_table.drug_concept_id IN (@target_id, @comparator_id)
} : {
	SELECT exposure_table.subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM @exposure_database_schema.@exposure_table exposure_table
  WHERE cohort_definition_id IN (@target_id, @comparator_id)
}
)

{@remove_duplicate_subjects == 'keep first, truncate to second'} ? {
-- remove_duplicate_subjects: keep first, truncate to second
, remove_duplicate_subjects AS (
  SELECT first_overall.subject_id,
	  first_overall.cohort_definition_id,
	  first_overall.cohort_start_date,
	  CASE
	    WHEN first_per_cohort.cohort_start_date IS NOT NULL AND first_per_cohort.cohort_start_date < first_overall.cohort_end_date
	    THEN DATEADD(DAY, -1, first_per_cohort.cohort_start_date)
	    ELSE first_overall.cohort_end_date
	  END AS cohort_end_date
	FROM (
    SELECT subject_id,
  	  cohort_definition_id,
  		cohort_start_date,
  		cohort_end_date
  	FROM (
  		SELECT subject_id,
  		  cohort_definition_id,
  		  cohort_start_date,
  		  cohort_end_date,
  		  ROW_NUMBER() OVER (PARTITION BY subject_id ORDER BY cohort_start_date) AS cohort_number
  		FROM raw_cohorts
    ) tmp
    WHERE cohort_number = 1
  ) first_overall
  LEFT JOIN (
    SELECT subject_id,
  	  cohort_definition_id,
  		cohort_start_date,
  		cohort_end_date
  	FROM (
      SELECT subject_id,
    	  cohort_definition_id,
    		cohort_start_date,
    		cohort_end_date,
    		ROW_NUMBER() OVER (PARTITION BY subject_id, cohort_definition_id ORDER BY cohort_start_date) AS cohort_number
    	FROM raw_cohorts
    ) tmp
    WHERE cohort_number = 1
  ) first_per_cohort
    ON first_overall.subject_id = first_per_cohort.subject_id
      AND first_overall.cohort_definition_id != first_per_cohort.cohort_definition_id
  WHERE first_per_cohort.cohort_start_date IS NULL OR first_per_cohort.cohort_start_date != first_overall.cohort_start_date
)
} : {{@remove_duplicate_subjects == 'keep first'} ? {
-- remove_duplicate_subjects: keep first
, remove_duplicate_subjects AS (
  SELECT subject_id,
	  cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM (
		SELECT subject_id,
		  cohort_definition_id,
		  cohort_start_date,
		  cohort_end_date,
		  ROW_NUMBER() OVER (PARTITION BY subject_id ORDER BY cohort_start_date) AS cohort_number
		FROM raw_cohorts
  ) tmp
  WHERE cohort_number = 1
)
} : {{@remove_duplicate_subjects == 'remove all'} ? {
-- remove_duplicate_subjects: remove all
, remove_duplicate_subjects AS (
  SELECT raw_cohorts.subject_id,
	  cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM raw_cohorts
	INNER JOIN (
    SELECT subject_id,
      COUNT(DISTINCT(cohort_definition_id)) exposure_count
    FROM raw_cohorts
    GROUP BY subject_id
	) tmp
	ON
		raw_cohorts.subject_id = tmp.subject_id
	WHERE
		tmp.exposure_count = 1
)
}}}

{@first_only} ? {
-- first_only
, first_only AS (
  SELECT subject_id,
	  cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM (
		SELECT subject_id,
		  cohort_definition_id,
		  cohort_start_date,
		  cohort_end_date,
		  ROW_NUMBER() OVER (PARTITION BY subject_id, cohort_definition_id ORDER BY cohort_start_date) AS cohort_number
		FROM {@remove_duplicate_subjects == 'keep first, truncate to second' | @remove_duplicate_subjects == 'keep first' | @remove_duplicate_subjects == 'remove all'} ? {remove_duplicate_subjects} : {raw_cohorts}
	) tmp
  WHERE cohort_number = 1
)
}

-- joined_with_obs_period
, joined_with_obs_period AS (
SELECT subject_id,
	  cohort_definition_id,
		cohort_start_date,
		cohort_end_date,
		observation_period_start_date,
		observation_period_end_date
	FROM {@first_only} ? {first_only} : {{@remove_duplicate_subjects == 'keep first, truncate to second' | @remove_duplicate_subjects == 'keep first' | @remove_duplicate_subjects == 'remove all'} ? {remove_duplicate_subjects} : {raw_cohorts}}
  INNER JOIN @cdm_database_schema.observation_period
	  ON subject_id = person_id
	 WHERE cohort_start_date <= observation_period_end_date
	    AND cohort_start_date >= observation_period_start_date
)

{@washout_period != 0} ? {
-- washout_period
, washout_period AS (
SELECT subject_id,
	  cohort_definition_id,
		cohort_start_date,
		cohort_end_date,
		observation_period_start_date,
		observation_period_end_date
	FROM joined_with_obs_period
	WHERE DATEDIFF(DAY, observation_period_start_date, cohort_start_date) >= @washout_period
)
}

{@restrict_to_common_period} ? {
--restrict_to_common_period
, restrict_to_common_period AS (
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date,
		observation_period_start_date,
		observation_period_end_date
	FROM {@washout_period != 0} ? {washout_period} : {joined_with_obs_period} cohorts
  WHERE cohort_start_date >= (
		SELECT MAX(start_date)
		FROM (
			SELECT MIN(cohort_start_date) AS start_date
			FROM raw_cohorts
			GROUP BY cohort_definition_id
		) tmp
	) AND cohort_start_date <= (
		SELECT MIN(end_date)
		FROM (
			SELECT MAX(cohort_start_date) AS end_date
			FROM raw_cohorts
			GROUP BY cohort_definition_id
		) tmp
	)
)
}

{@min_age != '' | @max_age != '' | @gender_concept_ids != ''} ? {
--joined_with_person
, joined_with_person AS (
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date,
		observation_period_start_date,
		observation_period_end_date,
{@min_age != '' | @max_age != ''} ? {		DATEFROMPARTS(year_of_birth, COALESCE(month_of_birth, 1), COALESCE(day_of_birth, 1)) AS date_of_birth,}
		gender_concept_id
	FROM {@restrict_to_common_period} ? {restrict_to_common_period} : {{@washout_period != 0} ? {washout_period} : {joined_with_obs_period}} cohorts
	INNER JOIN @cdm_database_schema.person
		ON cohorts.subject_id = person.person_id
)
{@min_age != '' | @max_age != ''} ? {
--restrict_by_age
, restrict_by_age AS (
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date,
		observation_period_start_date,
		observation_period_end_date,
		gender_concept_id
	FROM joined_with_person
{@min_age != ''} ? {	WHERE DATEDIFF(DAY, date_of_birth, cohort_start_date) / 365.25 >= @min_age}
{@max_age != ''} ? {{@min_age != ''} ? {		AND} : {		WHERE} DATEDIFF(DAY, date_of_birth, cohort_start_date) / 365.25 < @max_age + 1}
)
}
{@gender_concept_ids != ''} ? {
--restrict_by_gender
, restrict_by_gender AS (
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date,
		observation_period_start_date,
		observation_period_end_date
	FROM {@min_age != '' | @max_age != ''} ? {restrict_by_age} : {joined_with_person}
	WHERE gender_concept_id IN (@gender_concept_ids)
)
}
}

{@use_nesting_cohort} ? {
-- use_nesting_cohort
, use_nesting_cohort AS (
	SELECT cohorts.subject_id,
		cohorts.cohort_definition_id,
	  cohorts.cohort_start_date,
		cohorts.cohort_end_date,
		observation_period_start_date,
		observation_period_end_date
	FROM {@gender_concept_ids != ''} ? {restrict_by_gender} : {{@min_age != '' | @max_age != ''} ? {restrict_by_age} : {{@restrict_to_common_period} ? {restrict_to_common_period} : {{@washout_period != 0} ? {washout_period} : {joined_with_obs_period}}}} cohorts
  INNER JOIN @nesting_cohort_database_schema.@nesting_cohort_table nesting
    ON cohorts.subject_id = nesting.subject_id
      AND cohorts.cohort_start_date >= nesting.cohort_start_date
      AND cohorts.cohort_start_date <= nesting.cohort_end_date
  WHERE nesting.cohort_definition_id = @nesting_cohort_id
)
}

{@study_start_date != '' | @study_end_date != ''} ? {
-- restrict_to_study_period
, restrict_to_study_period AS (
  SELECT subject_id,
		cohort_definition_id,
	  cohort_start_date,
		cohort_end_date,
		observation_period_start_date,
		observation_period_end_date
	FROM {@use_nesting_cohort} ? {use_nesting_cohort} : {{@gender_concept_ids != ''} ? {restrict_by_gender} : {{@min_age != '' | @max_age != ''} ? {restrict_by_age} : {{@restrict_to_common_period} ? {restrict_to_common_period} : {{@washout_period != 0} ? {washout_period} : {joined_with_obs_period}}}}}
{@study_start_date != '' } ? {  WHERE cohort_start_date >= CAST('@study_start_date' AS DATE)}
{@study_end_date != '' } ? {
{@study_start_date != '' } ? {    AND} : {  WHERE} cohort_start_date <= CAST('@study_end_date' AS DATE)
}
)
}

{@action == 'CREATE'} ? {
-- Generating a unique row_id that can be used instead of the compound key
-- (subjectId, cohort_definition_id, cohort_start_date) for easier joining etc.
SELECT ROW_NUMBER() OVER (ORDER BY subject_id, cohort_start_date) AS row_id,
  subject_id,
	cohort_definition_id,
	cohort_start_date,
	DATEDIFF(DAY, observation_period_start_date, cohort_start_date) AS days_from_obs_start,
	{@study_end_date != '' } ? {
	    CASE
			WHEN cohort_end_date <= CAST('@study_end_date' AS DATE)
				THEN DATEDIFF(DAY, cohort_start_date, cohort_end_date)
			ELSE
				DATEDIFF(DAY, cohort_start_date, CAST('@study_end_date' AS DATE))
		END
	} : {
		DATEDIFF(DAY, cohort_start_date, cohort_end_date)
	} AS days_to_cohort_end,
	{@study_end_date != '' } ? {
	    CASE
			WHEN observation_period_end_date <= CAST('@study_end_date' AS DATE)
				THEN DATEDIFF(DAY, cohort_start_date, observation_period_end_date)
			ELSE
				DATEDIFF(DAY, cohort_start_date, CAST('@study_end_date' AS DATE))
		END
	} : {
		DATEDIFF(DAY, cohort_start_date, observation_period_end_date)
	} AS days_to_obs_end,
	cohort_end_date
INTO #cohort_person
FROM {@study_start_date != '' | @study_end_date != ''} ? {restrict_to_study_period} : {{@use_nesting_cohort} ? {use_nesting_cohort} : {{@gender_concept_ids != ''} ? {restrict_by_gender} : {{@min_age != '' | @max_age != ''} ? {restrict_by_age} : {{@restrict_to_common_period} ? {restrict_to_common_period} : {{@washout_period != 0} ? {washout_period} : {joined_with_obs_period}}}}}}
} : {
-- Count the cohort sizes at the various stages
SELECT cohort_definition_id,
  COUNT(*) AS exposures,
  COUNT(DISTINCT subject_id) AS persons,
  'Original cohorts' AS description,
  CAST(1 AS FLOAT) seq_id
FROM raw_cohorts
GROUP BY cohort_definition_id

{@remove_duplicate_subjects == 'keep first, truncate to second' | @remove_duplicate_subjects == 'keep first' | @remove_duplicate_subjects == 'remove all'} ? {
UNION ALL

SELECT cohort_definition_id,
  COUNT(*) AS exposures,
  COUNT(DISTINCT subject_id) AS persons,
{@remove_duplicate_subjects == 'keep first, truncate to second'} ? {
  'Keep first, truncate when entering second cohort' AS description,
} : {{@remove_duplicate_subjects == 'keep first'} ? {
  'Keep first when in both cohorts' AS description,
} : {
  'Remove subjects in both cohorts' AS description,
}}
  CAST(2 AS FLOAT) seq_id
FROM remove_duplicate_subjects
GROUP BY cohort_definition_id
}

{@first_only} ? {
UNION ALL

SELECT cohort_definition_id,
  COUNT(*) AS exposures,
  COUNT(DISTINCT subject_id) AS persons,
  'First exposure only' AS description,
  CAST(3 AS FLOAT) seq_id
FROM first_only
GROUP BY cohort_definition_id
}

{@washout_period != 0} ? {
UNION ALL

SELECT cohort_definition_id,
  COUNT(*) AS exposures,
  COUNT(DISTINCT subject_id) AS persons,
  CONCAT(CAST(@washout_period AS VARCHAR), ' days of prior observation') AS description,
  CAST(4 AS FLOAT) seq_id
FROM washout_period
GROUP BY cohort_definition_id
}

{@restrict_to_common_period} ? {
UNION ALL

SELECT cohort_definition_id,
  COUNT(*) AS exposures,
  COUNT(DISTINCT subject_id) AS persons,
  'Restrict to common period' AS description,
  CAST(5 AS FLOAT) seq_id
FROM restrict_to_common_period
GROUP BY cohort_definition_id
}

{@min_age != '' | @max_age != ''} ? {
UNION ALL

SELECT cohort_definition_id,
  COUNT(*) AS exposures,
  COUNT(DISTINCT subject_id) AS persons,
  'Restrict by age' AS description,
  CAST(6 AS FLOAT) seq_id
FROM restrict_by_age
GROUP BY cohort_definition_id
}

{@gender_concept_ids != ''} ? {
UNION ALL

SELECT cohort_definition_id,
  COUNT(*) AS exposures,
  COUNT(DISTINCT subject_id) AS persons,
  'Restrict by gender' AS description,
  CAST(7 AS FLOAT) seq_id
FROM restrict_by_gender
GROUP BY cohort_definition_id
}

{@use_nesting_cohort} ? {
UNION ALL

SELECT cohort_definition_id,
  COUNT(*) AS exposures,
  COUNT(DISTINCT subject_id) AS persons,
  'Restrict to nesting cohort' AS description,
  CAST(8 AS FLOAT) seq_id
FROM use_nesting_cohort
GROUP BY cohort_definition_id
}

{@study_start_date != '' | @study_end_date != ''} ? {
UNION ALL

SELECT cohort_definition_id,
  COUNT(*) AS exposures,
  COUNT(DISTINCT subject_id) AS persons,
  'Restrict to study period' AS description,
  CAST(9 AS FLOAT) seq_id
FROM restrict_to_study_period
GROUP BY cohort_definition_id
}
}
;

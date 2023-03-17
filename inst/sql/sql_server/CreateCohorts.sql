/************************************************************************
@file CreateCohorts.sql

Copyright 2023 Observational Health Data Sciences and Informatics

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
{DEFAULT @restrict_to_common_period = FALSE}

DROP TABLE IF EXISTS #cohort_person;

-- Generating a unique row_id that can be used instead of the compound key
-- (subjectId, cohort_definition_id, cohort_start_date) for easier joining etc.
SELECT ROW_NUMBER() OVER (ORDER BY person_id, cohort_start_date) AS row_id,
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
FROM (
{@restrict_to_common_period} ? {
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM (
}
{@first_only} ? {
	SELECT subject_id,
		cohort_definition_id,
		MIN(cohort_start_date) AS cohort_start_date,
		MIN(cohort_end_date) AS cohort_end_date
	FROM (
}
{@remove_duplicate_subjects == 'keep first'} ? {

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
		FROM (
}
{@exposure_table == 'drug_era' } ? {
	SELECT exposure_table.person_id AS subject_id,
		drug_concept_id AS cohort_definition_id,
		drug_era_start_date AS cohort_start_date,
		drug_era_end_date AS cohort_end_date
	FROM  @exposure_database_schema.@exposure_table exposure_table
	{@remove_duplicate_subjects == 'remove all'} ? {
	INNER JOIN (	
		SELECT person_id,
			COUNT(DISTINCT(drug_concept_id)) drug_count
		FROM @exposure_database_schema.@exposure_table
		WHERE drug_concept_id IN (@target_id, @comparator_id)
		GROUP BY person_id
	) temp
	ON
		exposure_table.person_id = temp.person_id
	WHERE
		temp.drug_count = 1
		AND
	} : {
	WHERE
	}
		exposure_table.drug_concept_id IN (@target_id, @comparator_id)
} : {
	SELECT exposure_table.subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM @exposure_database_schema.@exposure_table exposure_table
		{@remove_duplicate_subjects == 'remove all'} ? {
			INNER JOIN
			(SELECT subject_id, COUNT(DISTINCT(cohort_definition_id)) cohort_count
			 FROM @exposure_database_schema.@exposure_table
			 WHERE cohort_definition_id IN (@target_id, @comparator_id)
		 	 GROUP BY subject_id) temp
			ON
				exposure_table.subject_id = temp.subject_id
			WHERE
				temp.cohort_count = 1
				AND
		} : {
			WHERE
		}
		cohort_definition_id IN (@target_id, @comparator_id)
}
{@remove_duplicate_subjects == 'keep first'} ? {
	) temp1
  ) temp2
  WHERE cohort_number = 1
}
	) raw_cohorts
{@first_only} ? {
  GROUP BY subject_id,
	cohort_definition_id
	) first_only
}
{@restrict_to_common_period} ? {
  WHERE cohort_start_date >= (
				SELECT MAX(start_date)
				FROM (
{@exposure_table == 'drug_era' } ? {
					SELECT MIN(drug_era_start_date) AS start_date
					FROM @exposure_database_schema.@exposure_table
					WHERE drug_concept_id IN (@target_id, @comparator_id)
					GROUP BY drug_concept_id
} : {
					SELECT MIN(cohort_start_date) AS start_date
					FROM @exposure_database_schema.@exposure_table
					WHERE cohort_definition_id IN (@target_id, @comparator_id)
					GROUP BY cohort_definition_id
}
				) tmp
				)
	AND cohort_start_date <= (
				SELECT MIN(end_date)
				FROM (
{@exposure_table == 'drug_era' } ? {
					SELECT MAX(drug_era_start_date) AS end_date
					FROM @exposure_database_schema.@exposure_table
					WHERE drug_concept_id IN (@target_id, @comparator_id)
					GROUP BY drug_concept_id
} : {
					SELECT MAX(cohort_start_date) AS end_date
					FROM @exposure_database_schema.@exposure_table
					WHERE cohort_definition_id IN (@target_id, @comparator_id)
					GROUP BY cohort_definition_id
}
				) tmp
				)
	) common_period
}

INNER JOIN @cdm_database_schema.observation_period
	ON subject_id = person_id
WHERE cohort_start_date <= observation_period_end_date
	AND cohort_start_date >= observation_period_start_date
{@study_start_date != '' } ? {AND cohort_start_date >= CAST('@study_start_date' AS DATE) }
{@study_end_date != '' } ? {AND cohort_start_date <= CAST('@study_end_date' AS DATE) }
{@washout_period != 0} ? {AND DATEDIFF(DAY, observation_period_start_date, cohort_start_date) >= @washout_period};

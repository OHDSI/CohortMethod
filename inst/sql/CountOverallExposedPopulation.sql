/************************************************************************
@file CountOverallExposedPopulation.sql

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

SELECT COUNT(DISTINCT subject_id) AS exposed_count,
	COUNT(*) AS exposure_count,
	cohort_definition_id AS treatment
FROM (
{@exposure_table == 'drug_era' } ? { 
	SELECT person_id AS subject_id,
		CASE
			WHEN ancestor_concept_id = @target_id
				THEN 1
			WHEN ancestor_concept_id = @comparator_id
				THEN 0
			ELSE - 1
			END AS cohort_definition_id,
		drug_era_start_date AS cohort_start_date,
		drug_era_end_date AS cohort_end_date
	FROM @cdm_database_schema.drug_era
	INNER JOIN @cdm_database_schema.concept_ancestor 
		ON drug_concept_id = descendant_concept_id
	WHERE ancestor_concept_id IN (@target_id, @comparator_id)
} : {
	SELECT subject_id,
		CASE
			WHEN cohort_definition_id = @target_id
				THEN 1
			WHEN cohort_definition_id = @comparator_id
				THEN 0
			ELSE - 1
		END AS cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM @exposure_database_schema.@exposure_table
	WHERE cohort_definition_id IN (@target_id, @comparator_id)
}
	) raw_cohorts
INNER JOIN @cdm_database_schema.observation_period
	ON subject_id = person_id
WHERE cohort_start_date <= observation_period_end_date
	AND cohort_start_date >= observation_period_start_date
{@study_start_date != '' } ? {AND cohort_start_date >= CAST('@study_start_date' AS DATE) } 
{@study_end_date != '' } ? {AND cohort_start_date <= CAST('@study_end_date' AS DATE) }
GROUP BY cohort_definition_id

/************************************************************************
@file GetCohorts.sql

Copyright 2015 Observational Health Data Sciences and Informatics

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

{DEFAULT @cdm_database = 'CDM4_SIM' }
{DEFAULT @target_id = '' }
{DEFAULT @comparator_id = '' }
{DEFAULT @has_indication_concept_ids = FALSE }
{DEFAULT @washout_window = 183 }
{DEFAULT @indication_lookback_window = 183 }
{DEFAULT @study_start_date = '' }
{DEFAULT @study_end_date = '' }
{DEFAULT @has_exclusion_concept_ids = FALSE }
{DEFAULT @exposure_database_schema = 'CDM4_SIM' }
{DEFAULT @exposure_table = 'drug_era' }
{DEFAULT @cdm_version = '4'}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'}

USE @cdm_database;

IF OBJECT_ID('tempdb..#indicated_cohort', 'U') IS NOT NULL
	DROP TABLE #indicated_cohort;

IF OBJECT_ID('tempdb..#new_user_cohort', 'U') IS NOT NULL
	DROP TABLE #new_user_cohort;

IF OBJECT_ID('tempdb..#non_overlap_cohort', 'U') IS NOT NULL
	DROP TABLE #non_overlap_cohort;

IF OBJECT_ID('tempdb..#cohort_person', 'U') IS NOT NULL
	DROP TABLE #cohort_person;

SELECT DISTINCT raw_cohorts.treatment,
	raw_cohorts.person_id,
	raw_cohorts.cohort_start_date,
	{@study_end_date != '' } ? { CASE
		WHEN raw_cohorts.cohort_end_date <= CAST('@study_end_date' AS DATE)
			THEN raw_cohorts.cohort_end_date
		ELSE CAST('@study_end_date' AS DATE)
		END } : {raw_cohorts.cohort_end_date} AS cohort_end_date,
	{@study_end_date != '' } ? { CASE
		WHEN op1.observation_period_end_date <= CAST('@study_end_date' AS DATE)
			THEN op1.observation_period_end_date
		ELSE CAST('@study_end_date' AS DATE)
		END } : {op1.observation_period_end_date} AS observation_period_end_date
INTO #new_user_cohort
FROM (
	{@exposure_table == 'drug_era' } ? { SELECT CASE
			WHEN ca1.ancestor_concept_id = @target_id
				THEN 1
			WHEN ca1.ancestor_concept_id = @comparator_id
				THEN 0
			ELSE - 1
			END AS treatment,
		de1.person_id,
		min(de1.drug_era_start_date) AS cohort_start_date,
		min(de1.drug_era_end_date) AS cohort_end_date
	FROM drug_era de1
	INNER JOIN concept_ancestor ca1
		ON de1.drug_concept_id = ca1.descendant_concept_id
	WHERE ca1.ancestor_concept_id IN (@target_id, @comparator_id)
	GROUP BY ca1.ancestor_concept_id,
		de1.person_id } : {
	SELECT CASE
			WHEN c1.@cohort_definition_id = @target_id
				THEN 1
			WHEN c1.@cohort_definition_id = @comparator_id
				THEN 0
			ELSE - 1
			END AS treatment,
		c1.subject_id AS person_id,
		min(c1.cohort_start_date) AS cohort_start_date,
		min(c1.cohort_end_date) AS cohort_end_date
	FROM @exposure_database_schema.@exposure_table c1
	WHERE c1.@cohort_definition_id IN (@target_id, @comparator_id)
	GROUP BY c1.@cohort_definition_id,
		c1.subject_id }
	) raw_cohorts
INNER JOIN observation_period op1
	ON raw_cohorts.person_id = op1.person_id
WHERE raw_cohorts.cohort_start_date < op1.observation_period_end_date
	AND raw_cohorts.cohort_start_date >= dateadd(dd, @washout_window, observation_period_start_date) {@study_start_date != '' } ? {AND raw_cohorts.cohort_start_date >= CAST('@study_start_date' AS DATE) } {@study_end_date != '' } ? {AND raw_cohorts.cohort_start_date <= CAST('@study_end_date' AS DATE) };

{@has_indication_concept_ids} ? {

/* select only users with the indication */
SELECT DISTINCT treatment,
	new_user_cohort.person_id,
	cohort_start_date,
	cohort_end_date,
	observation_period_end_date
INTO #indicated_cohort
FROM #new_user_cohort new_user_cohort
INNER JOIN (
		SELECT person_id,
			condition_start_date AS indication_date
		FROM condition_occurrence
		WHERE condition_concept_id IN (
				SELECT descendant_concept_id
				FROM concept_ancestor
				INNER JOIN #indications
				ON ancestor_concept_id  = concept_id
				)
		) indication
ON new_user_cohort.person_id = indication.person_id
	AND new_user_cohort.cohort_start_date <= dateadd(dd, @indication_lookback_window, indication_date)
	AND new_user_cohort.cohort_start_date >= indication_date
;
}

/* delete persons in both cohorts */
SELECT treatment,
	new_user_cohort.person_id,
	cohort_start_date,
	cohort_end_date
INTO #non_overlap_cohort
FROM {@has_indication_concept_ids} ? { #indicated_cohort new_user_cohort } : { #new_user_cohort new_user_cohort }
LEFT JOIN (
	SELECT person_id
	FROM (
		SELECT person_id,
			COUNT(treatment) AS num_cohorts
		FROM {@has_indication_concept_ids} ? { #indicated_cohort } : { #new_user_cohort }
		GROUP BY person_id
		) t1
	WHERE num_cohorts = 2
	) both_cohorts
	ON new_user_cohort.person_id = both_cohorts.person_id
WHERE both_cohorts.person_id IS NULL;

/* apply exclusion criteria  */
SELECT non_overlap_cohort.treatment AS @cohort_definition_id,
	non_overlap_cohort.person_id AS subject_id,
	non_overlap_cohort.cohort_start_date,
	non_overlap_cohort.cohort_end_date
INTO #cohort_person
FROM #non_overlap_cohort non_overlap_cohort {@has_exclusion_concept_ids} ? {
LEFT JOIN (
	SELECT *
	FROM condition_occurrence co1
	WHERE condition_concept_id IN (
			SELECT descendant_concept_id
			FROM concept_ancestor
			INNER JOIN #exclusions
			ON ancestor_concept_id = concept_id
			)
	) exclude_conditions
	ON non_overlap_cohort.person_id = exclude_conditions.person_id
		AND non_overlap_cohort.cohort_start_date > exclude_conditions.condition_start_date
LEFT JOIN (
	SELECT *
	FROM procedure_occurrence po1
	WHERE procedure_concept_id IN (
			SELECT descendant_concept_id
			FROM concept_ancestor
			INNER JOIN #exclusions
			ON ancestor_concept_id = concept_id
			)
	) exclude_procedures
	ON non_overlap_cohort.person_id = exclude_procedures.person_id
		AND non_overlap_cohort.cohort_start_date > exclude_procedures.procedure_date
LEFT JOIN (
	SELECT *
	FROM drug_exposure de1
	WHERE drug_concept_id IN (
			SELECT descendant_concept_id
			FROM concept_ancestor
			INNER JOIN #exclusions
			ON ancestor_concept_id = concept_id
			)
	) exclude_drugs
	ON non_overlap_cohort.person_id = exclude_drugs.person_id
		AND non_overlap_cohort.cohort_start_date > exclude_drugs.drug_exposure_start_date
WHERE exclude_conditions.person_id IS NULL
	AND exclude_procedures.person_id IS NULL
	AND exclude_drugs.person_id IS NULL };

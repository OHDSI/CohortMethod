/************************************************************************
@file GetOutcomes.sql

Copyright 2016 Observational Health Data Sciences and Informatics

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

{DEFAULT @cdm_database = 'CDM4_SIM' } /*cdm_database: @cdm_database*/
{DEFAULT @outcome_database_schema = 'CDM4_SIM' } /*outcome_database_schema: @outcome_database_schema*/
{DEFAULT @outcome_table = 'condition_occurrence' } /*outcome_table: @outcome_table*/ /*the table that contains the outcome information (condition_occurrence or COHORT)*/
{DEFAULT @outcome_concept_ids = '' } /*outcome_concept_ids: @outcome_concept_ids*/
{DEFAULT @outcome_condition_type_concept_ids = '' } /*outcome_condition_type_concept_ids: @outcome_condition_type_concept_ids*/ /*condition type only applies if @outcome_table = condition_occurrence*/
{DEFAULT @cdm_version = '4'}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'}

USE @cdm_database;

IF OBJECT_ID('tempdb..#cohort_outcome', 'U') IS NOT NULL
	DROP TABLE #cohort_outcome;

IF OBJECT_ID('tempdb..#cohort_excluded_person', 'U') IS NOT NULL
	DROP TABLE #cohort_excluded_person;

{@outcome_table == 'condition_occurrence' } ? {
SELECT cp1.@cohort_definition_id,
	cp1.subject_id as person_id,
	ca1.ancestor_concept_id AS outcome_id,
	DATEDIFF(DAY, cp1.cohort_start_date, co1.condition_start_date) + 1 AS time_to_event
  INTO #cohort_outcome
FROM #cohort_person cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
INNER JOIN (
	SELECT descendant_concept_id,
		ancestor_concept_id
	FROM concept_ancestor
	WHERE ancestor_concept_id IN (@outcome_concept_ids)
	) ca1
	ON co1.condition_concept_id = descendant_concept_id
WHERE {@outcome_condition_type_concept_ids != '' } ? { co1.condition_type_concept_id IN (@outcome_condition_type_concept_ids)
	AND } co1.condition_start_date => cp1.cohort_start_date
	AND co1.condition_start_date <= observation_period_end_date
GROUP BY cp1.@cohort_definition_id,
	cp1.subject_id,
	DATEDIFF(DAY, cp1.cohort_start_date, co1.condition_start_date) + 1,
	ca1.ancestor_concept_id } : { {@outcome_table == 'condition_era' } ? {

SELECT cp1.@cohort_definition_id,
	cp1.subject_id AS person_id,
	ca1.ancestor_concept_id AS outcome_id,
	DATEDIFF(DAY, cp1.cohort_start_date, co1.condition_era_start_date) + 1 AS time_to_event
  INTO #cohort_outcome
FROM #cohort_person cp1
INNER JOIN condition_era co1
	ON cp1.subject_id = co1.person_id
INNER JOIN (
	SELECT descendant_concept_id,
		ancestor_concept_id
	FROM concept_ancestor
	WHERE ancestor_concept_id IN (@outcome_concept_ids)
	) ca1
	ON co1.condition_concept_id = descendant_concept_id
WHERE {@outcome_condition_type_concept_ids != '' } ? { co1.condition_type_concept_id IN (@outcome_condition_type_concept_ids)
	AND } co1.condition_era_start_date >= cp1.cohort_start_date
	AND co1.condition_era_start_date <= observation_period_end_date
GROUP BY cp1.@cohort_definition_id,
	cp1.subject_id,
	DATEDIFF(DAY, cp1.cohort_start_date, co1.condition_era_start_date) + 1,
	ca1.ancestor_concept_id } : {

SELECT cp1.@cohort_definition_id,
	cp1.subject_id AS person_id,
	co1.@cohort_definition_id AS outcome_id,
	DATEDIFF(DAY, cp1.cohort_start_date, co1.cohort_start_date) + 1 AS time_to_event
  INTO #cohort_outcome
FROM #cohort_person cp1
INNER JOIN @outcome_database_schema.@outcome_table co1
	ON cp1.subject_id = co1.subject_id
WHERE co1.@cohort_definition_id IN (@outcome_concept_ids)
	AND co1.cohort_start_date >= cp1.cohort_start_date
	AND co1.cohort_start_Date <= observation_period_end_date
GROUP BY cp1.@cohort_definition_id,
	cp1.subject_id,
	DATEDIFF(DAY, cp1.cohort_start_date, co1.cohort_start_date) + 1,
	co1.@cohort_definition_id } };

---find people to exclude from each analysis (if outcome occurs prior to index)
{@outcome_table == 'condition_occurrence' } ? {
SELECT DISTINCT cp1.@cohort_definition_id,
	cp1.subject_id AS person_id,
	ca1.ancestor_concept_id AS outcome_id
    INTO #cohort_excluded_person
FROM #cohort_person cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
INNER JOIN (
	SELECT descendant_concept_id,
		ancestor_concept_id
	FROM concept_ancestor
	WHERE ancestor_concept_id IN (@outcome_concept_ids)
	) ca1
	ON co1.condition_concept_id = descendant_concept_id
WHERE {@outcome_condition_type_concept_ids != '' } ? { co1.condition_type_concept_id IN (@outcome_condition_type_concept_ids)
	AND } co1.condition_start_date < cp1.cohort_start_date } : { {@outcome_table == 'condition_era' } ? {

SELECT DISTINCT cp1.@cohort_definition_id,
	cp1.subject_id AS person_id,
	ca1.ancestor_concept_id AS outcome_id
   INTO #cohort_excluded_person
FROM #cohort_person cp1
INNER JOIN condition_era co1
	ON cp1.subject_id = co1.person_id
INNER JOIN (
	SELECT descendant_concept_id,
		ancestor_concept_id
	FROM concept_ancestor
	WHERE ancestor_concept_id IN (@outcome_concept_ids)
	) ca1
	ON co1.condition_concept_id = descendant_concept_id
WHERE {@outcome_condition_type_concept_ids != '' } ? { co1.condition_type_concept_id IN (@outcome_condition_type_concept_ids)
	AND } co1.condition_era_start_date < cp1.cohort_start_date } : {

SELECT DISTINCT cp1.@cohort_definition_id,
	cp1.subject_id AS person_id,
	co1.@cohort_definition_id AS outcome_id
     INTO #cohort_excluded_person
FROM #cohort_person cp1
INNER JOIN @outcome_database_schema.@outcome_table co1
	ON cp1.subject_id = co1.subject_id
WHERE co1.@cohort_definition_id IN (@outcome_concept_ids)
	AND co1.cohort_start_date < cp1.cohort_start_date } };

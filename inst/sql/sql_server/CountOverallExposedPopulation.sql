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

{DEFAULT @target_drug_concept_id = ''}  /*target_drug_concept_id: @target_drug_concept_id*/
{DEFAULT @comparator_drug_concept_id = ''} /*comparator_drug_concept_id: @comparator_drug_concept_id*/
{DEFAULT @study_start_date = ''} /*study_start_date: @study_start_date*/
{DEFAULT @study_end_date = ''} /*study_end_date: @study_end_date*/
{DEFAULT @exposure_database_schema = 'CDM4_SIM'} /*exposure_database_schema: @exposure_database_schema*/
{DEFAULT @exposure_table = 'drug_era'}  /*exposure_table: @exposure_table*/ /*the table that contains the exposure information (drug_era or COHORT)*/

SELECT COUNT(DISTINCT raw_cohorts.person_id) AS exposed_count,
raw_cohorts.treatment
FROM (
{@exposure_table == 'drug_era'} ? {
  SELECT CASE
  WHEN ca1.ancestor_concept_id = @target_drug_concept_id
  THEN 1
  WHEN ca1.ancestor_concept_id = @comparator_drug_concept_id
  THEN 0
  ELSE - 1
  END AS treatment,
  de1.person_id,
  de1.drug_era_start_date AS cohort_start_date,
  de1.drug_era_end_date AS cohort_end_date
  FROM drug_era de1
  INNER JOIN concept_ancestor ca1
  ON de1.drug_concept_id = ca1.descendant_concept_id
  WHERE ca1.ancestor_concept_id in (@target_drug_concept_id,@comparator_drug_concept_id)
} : {
  SELECT CASE
  WHEN c1.cohort_concept_id = @target_drug_concept_id
  THEN 1
  WHEN c1.cohort_concept_id = @comparator_drug_concept_id
  THEN 0
  ELSE - 1
  END AS treatment,
  c1.subject_id as person_id,
  c1.cohort_start_date AS cohort_start_date,
  c1.cohort_end_date AS cohort_end_date
  FROM @exposure_database_schema.@exposure_table c1
  WHERE c1.cohort_concept_id in (@target_drug_concept_id,@comparator_drug_concept_id)
}
) raw_cohorts
INNER JOIN observation_period op1
ON raw_cohorts.person_id = op1.person_id
WHERE raw_cohorts.cohort_start_date <= op1.observation_period_end_date
AND  raw_cohorts.cohort_end_date >= op1.observation_period_start_date
{@study_start_date != ''} ? {AND raw_cohorts.cohort_end_date >= CAST('@study_start_date' AS DATE)}
{@study_end_date != ''} ? {AND raw_cohorts.cohort_start_date <= CAST('@study_end_date' AS DATE)}
GROUP BY
raw_cohorts.treatment

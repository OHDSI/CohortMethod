{DEFAULT @target_drug_concept_id = ''}  /*target_drug_concept_id: @target_drug_concept_id*/
{DEFAULT @comparator_drug_concept_id = ''} /*comparator_drug_concept_id: @comparator_drug_concept_id*/
{DEFAULT @cdm_database_schema = 'CDM4_SIM.dbo'} /*cdm_database_schema: @cdm_database_schema*/
{DEFAULT @study_start_date = ''} /*study_start_date: @study_start_date*/
{DEFAULT @study_end_date = ''} /*study_end_date: @study_end_date*/
{DEFAULT @exposure_schema = 'CDM4_SIM'} /*exposure_schema: @exposure_schema*/
{DEFAULT @exposure_table = 'drug_era'}  /*exposure_table: @exposure_table*/ /*the table that contains the exposure information (drug_era or COHORT)*/

SELECT COUNT(DISTINCT raw_cohorts.person_id) AS exposed_count,
raw_cohorts.cohort_id
FROM (
{@exposure_table == 'drug_era'} ? {
  SELECT CASE 
  WHEN ca1.ancestor_concept_id = @target_drug_concept_id
  THEN 1
  WHEN ca1.ancestor_concept_id = @comparator_drug_concept_id
  THEN 0
  ELSE - 1
  END AS cohort_id,
  de1.person_id,
  de1.drug_era_start_date AS cohort_start_date,
  de1.drug_era_end_date AS cohort_end_date
  FROM @cdm_database_schema.drug_era de1
  INNER JOIN @cdm_database_schema.concept_ancestor ca1
  ON de1.drug_concept_id = ca1.descendant_concept_id
  WHERE ca1.ancestor_concept_id in (@target_drug_concept_id,@comparator_drug_concept_id)
} : {
  SELECT CASE 
  WHEN c1.cohort_definition_id = @target_drug_concept_id
  THEN 1
  WHEN c1.cohort_definition_id = @comparator_drug_concept_id
  THEN 0
  ELSE - 1
  END AS cohort_id,
  c1.subject_id as person_id,
  c1.cohort_start_date AS cohort_start_date,
  c1.cohort_end_date AS cohort_end_date
  FROM @exposure_schema.dbo.@exposure_table c1
  WHERE c1.cohort_definition_id in (@target_drug_concept_id,@comparator_drug_concept_id)
}
) raw_cohorts
INNER JOIN @cdm_database_schema.observation_period op1
ON raw_cohorts.person_id = op1.person_id
WHERE raw_cohorts.cohort_start_date <= op1.observation_period_end_date
AND  raw_cohorts.cohort_end_date >= op1.observation_period_start_date
{@study_start_date != ''} ? {AND raw_cohorts.cohort_end_date >= CAST('@study_start_date' AS DATE)}
{@study_end_date != ''} ? {AND raw_cohorts.cohort_start_date <= CAST('@study_end_date' AS DATE)}
GROUP BY
raw_cohorts.cohort_id
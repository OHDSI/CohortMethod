/* Authors:   Patrick Ryan, Martijn Schuemie
 * Last update date:  7 September 2014
 * 
 * Parameterized SQL to create cohorts to be used as input in fitting
 * large-scale analytics.
 */

{DEFAULT @cdm_schema = 'CDM4_SIM'} /*CDM4_SIM  CDM_TRUVEN_MDCD*/
{DEFAULT @results_schema = 'scratch'}
{DEFAULT @target_drug_concept_id = 755695}  /*fluoxetine*/
{DEFAULT @comparator_drug_concept_id = 739138} /*sertraline*/
{DEFAULT @indication_concept_ids = 439926} /*malaise and fatigue*/
{DEFAULT @washout_window = 183}
{DEFAULT @indication_lookback_window = 183}
{DEFAULT @study_start_date = ''}
{DEFAULT @study_end_date = ''}
{DEFAULT @exclusion_concept_ids = 4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019} 
{DEFAULT @exposure_table = 'DRUG_ERA'}  /*the table that contains the exposure information (DRUG_ERA or COHORT)*/

USE @results_schema;

IF OBJECT_ID('raw_cohort', 'U') IS NOT NULL --This should only do something in Oracle
  drop table raw_cohort;

IF OBJECT_ID('tempdb..#raw_cohort', 'U') IS NOT NULL
  drop table #raw_cohort;

create table #raw_cohort
(
	cohort_id int,
	person_id bigint,
	cohort_start_date date,
	cohort_end_date date,
	observation_period_end_date date
);


IF OBJECT_ID('cohort_person', 'U') IS NOT NULL --This should only do something in Oracle
  drop table cohort_person;

IF OBJECT_ID('tempdb..#cohort_person', 'U') IS NOT NULL
  drop table #cohort_person;

create table #cohort_person
(
	row_id bigint,
	cohort_id int,
	person_id bigint,
	cohort_start_date date,
	cohort_end_date date,
	observation_period_end_date date
);


/*made data table that contains cohorts and end of observation period*/
INSERT INTO #raw_cohort (cohort_id, person_id, cohort_start_date, cohort_end_date, observation_period_end_date)
SELECT DISTINCT raw_cohorts.cohort_id,
  raw_cohorts.person_id,
  raw_cohorts.cohort_start_date,
  {@study_end_date != ''} ? {
	CASE WHEN raw_cohorts.cohort_end_date <= '@study_end_date'
		THEN raw_cohorts.cohort_end_date
		ELSE '@study_end_date'
		END
  } : {raw_cohorts.cohort_end_date}
  AS cohort_end_date,
  {@study_end_date != ''} ? {
	CASE WHEN op1.observation_period_end_date <= '@study_end_date'
		THEN op1.observation_period_end_date
		ELSE '@study_end_date'
		END
  } : {op1.observation_period_end_date}
  AS observation_period_end_date
FROM (
	{@exposure_table == 'DRUG_ERA'} ? {
		SELECT CASE 
				WHEN ca1.ancestor_concept_id = @target_drug_concept_id
					THEN 1
				WHEN ca1.ancestor_concept_id = @comparator_drug_concept_id
					THEN 0
				ELSE - 1
				END AS cohort_id,
			de1.person_id,
			min(de1.drug_era_start_date) AS cohort_start_date,
			min(de1.drug_era_end_date) AS cohort_end_date
		FROM @cdm_schema.dbo.drug_era de1
		INNER JOIN @cdm_schema.dbo.concept_ancestor ca1
			ON de1.drug_concept_id = ca1.descendant_concept_id
		WHERE ca1.ancestor_concept_id in (@target_drug_concept_id,@comparator_drug_concept_id)
		GROUP BY ca1.ancestor_concept_id,
			de1.person_id
	}
	
	{@exposure_table == 'COHORT'} ? {
		SELECT CASE 
				WHEN c1.cohort_concept_id = @target_drug_concept_id
					THEN 1
				WHEN c1.cohort_concept_id = @comparator_drug_concept_id
					THEN 0
				ELSE - 1
				END AS cohort_id,
			c1.person_id,
			min(c1.cohort_start_date) AS cohort_start_date,
			min(@exposure_extension_window, c1.cohort_end_date) AS cohort_end_date
		FROM @cdm_schema.dbo.cohort c1
		WHERE c1.cohort_id in (@target_drug_concept_id,@comparator_drug_concept_id)
		GROUP BY c1.cohort_concept_id,
			c1.person_id
	}
	
	) raw_cohorts
INNER JOIN @cdm_schema.dbo.observation_period op1
	ON raw_cohorts.person_id = op1.person_id
{@indication_concept_ids != ''} ? {
INNER JOIN (
	SELECT person_id,
		condition_start_date AS indication_date
	FROM @cdm_schema.dbo.condition_occurrence
	WHERE condition_concept_id IN (
			SELECT descendant_concept_id
			FROM @cdm_schema.dbo.concept_ancestor
			WHERE ancestor_concept_id IN (@indication_concept_ids)
			)
	) indication
	ON raw_cohorts.person_id = indication.person_id
  AND raw_cohorts.cohort_start_date <= dateadd(dd, @indication_lookback_window, indication.indication_date)
  AND raw_cohorts.cohort_start_date >= indication.indication_date
}
WHERE raw_cohorts.cohort_start_date >= dateadd(dd, @washout_window, op1.observation_period_start_date)
	AND raw_cohorts.cohort_start_date <= op1.observation_period_end_date
  {@study_start_date != ''} ? {AND raw_cohorts.cohort_start_date >= '@study_start_date'}
	{@study_end_date != ''} ? {AND raw_cohorts.cohort_start_date <= '@study_end_date'};

	
	
	
/* delete persons in both cohorts and apply exclusion criteria  */
INSERT INTO #cohort_person (row_id, cohort_id, person_id, cohort_start_date, cohort_end_date, observation_period_end_date)
SELECT rc1.person_id*10+rc1.cohort_id as row_id,
	rc1.cohort_id,
	rc1.person_id,
	rc1.cohort_start_date,
	rc1.cohort_end_date,
	rc1.observation_period_end_date
FROM #raw_cohort rc1
LEFT JOIN (
	SELECT person_id
	FROM (
		SELECT person_id,
			count(cohort_id) AS num_cohorts
		FROM #raw_cohort
		GROUP BY person_id
		) t1
	WHERE num_cohorts = 2
	) both_cohorts
	ON rc1.person_id = both_cohorts.person_id
{@exclusion_concept_ids != ''} ? {
LEFT JOIN (
	SELECT *
	FROM @cdm_schema.dbo.condition_occurrence co1
	WHERE condition_concept_id IN (
			SELECT descendant_concept_id
			FROM @cdm_schema.dbo.concept_ancestor
			WHERE ancestor_concept_id IN (@exclusion_concept_ids)
			)
	) exclude_conditions
	ON rc1.person_id = exclude_conditions.person_id
		AND rc1.cohort_start_date > exclude_conditions.condition_start_date
LEFT JOIN (
	SELECT *
	FROM @cdm_schema.dbo.procedure_occurrence po1
	WHERE procedure_concept_id IN (
			SELECT descendant_concept_id
			FROM @cdm_schema.dbo.concept_ancestor
			WHERE ancestor_concept_id IN (@exclusion_concept_ids)
			)
	) exclude_procedures
	ON rc1.person_id = exclude_procedures.person_id
		AND rc1.cohort_start_date > exclude_procedures.procedure_date
LEFT JOIN (
	SELECT *
	FROM @cdm_schema.dbo.drug_exposure de1
	WHERE drug_concept_id IN (
			SELECT descendant_concept_id
			FROM @cdm_schema.dbo.concept_ancestor
			WHERE ancestor_concept_id IN (@exclusion_concept_ids)
			)
	) exclude_drugs
	ON rc1.person_id = exclude_drugs.person_id
		AND rc1.cohort_start_date > exclude_drugs.drug_exposure_start_date
}
WHERE both_cohorts.person_id IS NULL
  {@exclusion_concept_ids != ''} ? {
	AND exclude_conditions.person_id IS NULL
	AND exclude_procedures.person_id IS NULL
	AND exclude_drugs.person_id IS NULL
  }
;




TRUNCATE TABLE #raw_cohort;
DROP TABLE #raw_cohort;

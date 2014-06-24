{DEFAULT @CDM_schema = 'CDM4_SIM'}
{DEFAULT @target_drug_concept_id = 755695}  /*fluoxetine*/
{DEFAULT @comparator_drug_concept_id = 739138} /*sertraline*/
{DEFAULT @indication_concept_id = 439926} /*malaise and fatigue*/
{DEFAULT @washout_window = 183}
{DEFAULT @indication_lookback_window = 183}
{DEFAULT @exposure_extension_window = 7}
{DEFAULT @study_start_date = '11/1/2000'}
{DEFAULT @study_end_date = '1/1/2014'}
{DEFAULT @exclusion_concept_id = 4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019} 
{DEFAULT @outcome_concept_id = 194133}  /*low back pain*/
{DEFAULT @outcome_condition_type_concept_id = 38000215,38000216,38000217,38000218,38000183,38000232}  
{DEFAULT @max_outcome_count = 1}  /*number of conditions, 1 is first occurrence*/

USE @CDM_schema;

/*made data table that contains cohorts and end of observation period*/
SELECT DISTINCT raw_cohorts.cohort_id,
	raw_cohorts.person_id,
	raw_cohorts.cohort_start_date,
	CASE 
		WHEN raw_cohorts.cohort_end_date <= op1.observation_period_end_date
			AND raw_cohorts.cohort_end_date <= '@study_end_date'
			THEN raw_cohorts.cohort_end_date
		WHEN raw_cohorts.cohort_end_date > op1.observation_period_end_date
			AND op1.observation_period_end_date <= '@study_end_date'
			THEN op1.observation_period_end_date
		ELSE '@study_end_date'
		END AS cohort_censor_date
INTO #pre_cohorts
FROM (
	SELECT CASE 
			WHEN ca1.ancestor_concept_id = @target_drug_concept_id
				THEN 1
			WHEN ca1.ancestor_concept_id = @comparator_drug_concept_id
				THEN 0
			ELSE - 1
			END AS cohort_id,
		de1.person_id,
		min(de1.drug_era_start_date) AS cohort_start_date,
		min(dateadd(dd, @exposure_extension_window, de1.drug_era_end_date)) AS cohort_end_date
	FROM drug_era de1
	INNER JOIN concept_ancestor ca1
		ON de1.drug_concept_id = ca1.descendant_concept_id
	WHERE ca1.ancestor_concept_id = @target_drug_concept_id
		OR ca1.ancestor_concept_id = @comparator_drug_concept_id
	GROUP BY ca1.ancestor_concept_id,
		de1.person_id
	) raw_cohorts
INNER JOIN observation_period op1
	ON raw_cohorts.person_id = op1.person_id
INNER JOIN (
	SELECT person_id,
		condition_start_date AS indication_date
	FROM condition_occurrence
	WHERE condition_concept_id IN (
			SELECT descendant_concept_id
			FROM concept_ancestor
			WHERE ancestor_concept_id IN (@indication_concept_id)
			)
	) indication
	ON raw_cohorts.person_id = indication.person_id
WHERE raw_cohorts.cohort_start_date >= dateadd(dd, @washout_window, op1.observation_period_start_date)
	AND raw_cohorts.cohort_start_date <= op1.observation_period_end_date
	AND raw_cohorts.cohort_start_date <= dateadd(dd, @indication_lookback_window, indication.indication_date)
	AND raw_cohorts.cohort_start_date >= indication.indication_date
	AND raw_cohorts.cohort_start_date >= '@study_start_date'
	AND raw_cohorts.cohort_start_date <= '@study_end_date';

/* delete persons in both cohorts and apply exclusion criteria  */
SELECT *
INTO #cohorts
FROM #pre_cohorts pc1
WHERE person_id NOT IN (
		SELECT person_id
		FROM (
			SELECT person_id,
				count(cohort_id) AS num_cohorts
			FROM #pre_cohorts
			GROUP BY person_id
			) t1
		WHERE num_cohorts = 2
		)
	AND NOT EXISTS (
		SELECT *
		FROM condition_occurrence co1
		WHERE co1.person_id = pc1.person_id
			AND condition_concept_id IN (
				SELECT descendant_concept_id
				FROM concept_ancestor
				WHERE ancestor_concept_id IN (@exclusion_concept_id)
				)
			AND pc1.cohort_start_date > co1.condition_start_date
		)
	AND NOT EXISTS (
		SELECT *
		FROM procedure_occurrence po1
		WHERE po1.person_id = pc1.person_id
			AND procedure_concept_id IN (
				SELECT descendant_concept_id
				FROM concept_ancestor
				WHERE ancestor_concept_id IN (@exclusion_concept_id)
				)
			AND pc1.cohort_start_date > po1.procedure_date
		)
	AND NOT EXISTS (
		SELECT *
		FROM drug_exposure de1
		WHERE de1.person_id = pc1.person_id
			AND drug_concept_id IN (
				SELECT descendant_concept_id
				FROM concept_ancestor
				WHERE ancestor_concept_id IN (@exclusion_concept_id)
				)
			AND pc1.cohort_start_date > de1.drug_exposure_start_date
		);

/* find covariates  */
CREATE TABLE #covariates (
	cohort_id INT,
	person_id NUMERIC,
	covariate_id NUMERIC,
	covariate_value DECIMAL
	);

CREATE TABLE #concept_counts (
	concept_id INT,
	num_records INT
	);

SELECT DISTINCT c1.concept_id AS snomed_concept_id,
	c2.concept_id AS meddra_concept_id
INTO #snomed_to_all_meddra
FROM concept c1
INNER JOIN concept_ancestor ca1
	ON c1.concept_id = ca1.descendant_concept_id
		AND c1.vocabulary_id = 1
INNER JOIN concept c2
	ON ca1.ancestor_concept_id = c2.concept_id
		AND c2.vocabulary_id = 15;

SELECT DISTINCT c1.concept_id AS rxnorm_concept_id,
	c2.concept_id AS atc_concept_id
INTO #rxnorm_to_atc
FROM concept c1
INNER JOIN concept_ancestor ca1
	ON c1.concept_id = ca1.descendant_concept_id
		AND c1.vocabulary_id = 8
		AND c1.concept_class = 'Ingredient'
INNER JOIN concept c2
	ON ca1.ancestor_concept_id = c2.concept_id
		AND c2.vocabulary_id = 21
		AND len(c2.concept_code) IN (
			1,
			3,
			5
			);

--gender
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	gender_concept_id AS covariate_id,
	1 AS covariate_value
FROM #cohorts ca1
INNER JOIN person p1
	ON ca1.person_id = p1.person_id
WHERE gender_concept_id = 8507;

--age decile
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	floor((year(ca1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 10) + 1 AS covariate_id,
	1 AS covariate_value
FROM #cohorts ca1
INNER JOIN person p1
	ON ca1.person_id = p1.person_id;

--Number of distinct conditions in last 6mo
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	20 AS covariate_id,
	count(DISTINCT ce1.condition_concept_id) AS covariate_value
FROM #cohorts ca1
INNER JOIN condition_era ce1
	ON ca1.person_id = ce1.person_id
WHERE ce1.condition_era_start_date <= ca1.cohort_start_date
	AND ce1.condition_era_start_date >= dateadd(dd, - 365, ca1.cohort_start_date)
GROUP BY ca1.cohort_id,
	ca1.person_id;

--Number of distinct drugs in last 6mo
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	21 AS covariate_id,
	count(DISTINCT de1.drug_concept_id) AS covariate_value
FROM #cohorts ca1
INNER JOIN drug_era de1
	ON ca1.person_id = de1.person_id
WHERE de1.drug_era_start_date <= ca1.cohort_start_date
	AND de1.drug_era_start_date >= dateadd(dd, - 365, ca1.cohort_start_date)
GROUP BY ca1.cohort_id,
	ca1.person_id;

--Number of distinct procedures in last 6mo
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	22 AS covariate_id,
	count(DISTINCT po1.procedure_concept_id) AS covariate_value
FROM #cohorts ca1
INNER JOIN procedure_occurrence po1
	ON ca1.person_id = po1.person_id
WHERE po1.procedure_date <= ca1.cohort_start_date
	AND po1.procedure_date >= dateadd(dd, - 365, ca1.cohort_start_date)
GROUP BY ca1.cohort_id,
	ca1.person_id;

--Number of distinct outpatient visits in last 6mo
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	23 AS covariate_id,
	count(vo1.VISIT_OCCURRENCE_ID) AS covariate_value
FROM #cohorts ca1
INNER JOIN visit_occurrence vo1
	ON ca1.person_id = vo1.person_id
WHERE vo1.visit_start_date <= ca1.cohort_start_date
	AND vo1.visit_start_date >= dateadd(dd, - 365, ca1.cohort_start_date)
	AND vo1.place_of_service_concept_id = 9202
GROUP BY ca1.cohort_id,
	ca1.person_id;

--Number of distinct inpatient visits in last 6mo
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	24 AS covariate_id,
	count(vo1.VISIT_OCCURRENCE_ID) AS covariate_value
FROM #cohorts ca1
INNER JOIN visit_occurrence vo1
	ON ca1.person_id = vo1.person_id
WHERE vo1.visit_start_date <= ca1.cohort_start_date
	AND vo1.visit_start_date >= dateadd(dd, - 365, ca1.cohort_start_date)
	AND vo1.place_of_service_concept_id = 9201
GROUP BY ca1.cohort_id,
	ca1.person_id;

--Number of distinct ER visits in last 6mo
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	25 AS covariate_id,
	count(vo1.VISIT_OCCURRENCE_ID) AS covariate_value
FROM #cohorts ca1
INNER JOIN visit_occurrence vo1
	ON ca1.person_id = vo1.person_id
WHERE vo1.visit_start_date <= ca1.cohort_start_date
	AND vo1.visit_start_date >= dateadd(dd, - 365, ca1.cohort_start_date)
	AND vo1.place_of_service_concept_id = 9203
GROUP BY ca1.cohort_id,
	ca1.person_id;

--add covariate per SNOMED condition
DELETE
FROM #concept_counts;

INSERT INTO #concept_counts (
	concept_id,
	num_records
	)
SELECT ce1.condition_concept_id,
	count(ce1.person_id) AS num_records
FROM #cohorts ca1
INNER JOIN condition_era ce1
	ON ca1.person_id = ce1.person_id
WHERE ce1.condition_era_start_date <= ca1.cohort_start_date
	AND ce1.condition_era_start_date >= dateadd(dd, - 365, ca1.cohort_start_date)
	AND ce1.condition_concept_id > 0
GROUP BY ce1.condition_concept_id;

INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	ce1.condition_concept_id AS covariate_id,
	1 AS covariate_value
FROM #cohorts ca1
INNER JOIN condition_era ce1
	ON ca1.person_id = ce1.person_id
WHERE ce1.condition_era_start_date <= ca1.cohort_start_date
	AND ce1.condition_era_start_date >= dateadd(dd, - 365, ca1.cohort_start_date)
	AND ce1.condition_concept_id > 0
	AND ce1.condition_concept_id IN (
		SELECT concept_id
		FROM #concept_counts
		WHERE num_records > 100
		)
GROUP BY ca1.cohort_id,
	ca1.person_id,
	ce1.condition_concept_id;

--add covariate per MEDDRA concept SNOMED condition
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	stam1.meddra_concept_id AS covariate_id,
	1 AS covariate_value
FROM #covariates ca1
INNER JOIN #snomed_to_all_meddra stam1
	ON ca1.covariate_id = stam1.snomed_concept_id
GROUP BY ca1.cohort_id,
	ca1.person_id,
	stam1.meddra_concept_id;

--add covariate per RxNorm ingredient
DELETE
FROM #concept_counts;

INSERT INTO #concept_counts (
	concept_id,
	num_records
	)
SELECT de1.drug_concept_id,
	count(de1.person_id) AS num_records
FROM #cohorts ca1
INNER JOIN drug_era de1
	ON ca1.person_id = de1.person_id
WHERE de1.drug_era_start_date <= ca1.cohort_start_date
	AND de1.drug_era_start_date >= dateadd(dd, - 365, ca1.cohort_start_date)
	AND de1.drug_concept_id > 0
GROUP BY de1.drug_concept_id;

INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	de1.drug_concept_id AS covariate_id,
	1 AS covariate_value
FROM #cohorts ca1
INNER JOIN drug_era de1
	ON ca1.person_id = de1.person_id
WHERE de1.drug_era_start_date < ca1.cohort_start_date
	AND de1.drug_era_start_date >= dateadd(dd, - 365, ca1.cohort_start_date)
	AND de1.drug_concept_id > 0
	AND de1.drug_concept_id IN (
		SELECT concept_id
		FROM #concept_counts
		WHERE num_records > 100
		)
GROUP BY ca1.cohort_id,
	ca1.person_id,
	de1.drug_concept_id;

--add covariate per ATC class concept within RxNorm drug
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	rta1.atc_concept_id AS covariate_id,
	1 AS covariate_value
FROM #covariates ca1
INNER JOIN #rxnorm_to_atc rta1
	ON ca1.covariate_id = rta1.rxnorm_concept_id
GROUP BY ca1.cohort_id,
	ca1.person_id,
	rta1.atc_concept_id;

--number of drugs within each ATC3 groupings
INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	2000000000 + t1.atc_concept_id,
	count(DISTINCT ca1.covariate_id)
FROM #covariates ca1
INNER JOIN (
	SELECT c1.concept_id AS rxnorm_concept_id,
		c2.concept_id AS atc_concept_id
	FROM concept c1
	INNER JOIN concept_ancestor ca1
		ON c1.concept_id = ca1.descendant_concept_id
			AND c1.vocabulary_id = 8
			AND c1.concept_level = 2
	INNER JOIN concept c2
		ON c2.concept_id = ca1.ancestor_concept_id
			AND c2.vocabulary_id = 21
			AND len(c2.concept_code) IN (3)
	) t1
	ON ca1.covariate_id = t1.rxnorm_concept_id
GROUP BY ca1.cohort_id,
	ca1.person_id,
	2000000000 + t1.atc_concept_id;

--add covariate per procedure
DELETE
FROM #concept_counts;

INSERT INTO #concept_counts (
	concept_id,
	num_records
	)
SELECT po1.procedure_concept_id,
	count(po1.person_id) AS num_records
FROM #cohorts ca1
INNER JOIN procedure_occurrence po1
	ON ca1.person_id = po1.person_id
INNER JOIN concept c1
	ON po1.procedure_concept_id = c1.concept_id
		AND c1.vocabulary_id IN (
			3,
			4
			)
WHERE po1.procedure_date <= ca1.cohort_start_date
	AND po1.procedure_date >= dateadd(dd, - 365, ca1.cohort_start_date)
	AND po1.procedure_concept_id > 0
GROUP BY po1.procedure_concept_id;

INSERT INTO #covariates (
	cohort_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT ca1.cohort_id,
	ca1.person_id,
	po1.procedure_concept_id AS covariate_id,
	1 AS covariate_value
FROM #cohorts ca1
INNER JOIN procedure_occurrence po1
	ON ca1.person_id = po1.person_id
WHERE po1.procedure_date < ca1.cohort_start_date
	AND po1.procedure_date >= dateadd(dd, - 365, ca1.cohort_start_date)
	AND po1.procedure_concept_id > 0
	AND po1.procedure_concept_id IN (
		SELECT concept_id
		FROM #concept_counts
		WHERE num_records > 100
		)
GROUP BY ca1.cohort_id,
	ca1.person_id,
	po1.procedure_concept_id;

/*   find outcomes  */
SELECT cohort_id,
	person_id,
	max(outcomes.outcome_count) AS num_outcomes,
	min(datediff(dd, cohort_start_date, outcome_date)) AS time
INTO #outcomes
FROM (
	SELECT c1.cohort_id,
		c1.person_id,
		c1.cohort_start_date,
		co1.condition_start_date AS outcome_date,
		row_number() OVER (
			PARTITION BY c1.person_id ORDER BY co1.condition_start_date ASC
			) AS outcome_count
	FROM #cohorts c1
	INNER JOIN condition_occurrence co1
		ON c1.person_id = co1.person_id
	WHERE co1.condition_concept_id IN (
			SELECT descendant_concept_id
			FROM concept_ancestor
			WHERE ancestor_concept_id IN (@outcome_concept_id)
			)
		AND co1.condition_type_concept_id IN (@outcome_condition_type_concept_id)
		AND co1.condition_start_date > c1.cohort_start_date
		AND co1.condition_start_Date <= c1.cohort_censor_date
	) outcomes
WHERE outcomes.outcome_count <= @max_outcome_count
GROUP BY cohort_id,
	person_id;

--tables for propensity score fitting
SELECT 1 AS stratum_id,
	c1.person_id AS row_id,
	cohort_id AS y,
	0 AS time
INTO #ccd_outcome_input_for_ps
FROM #cohorts c1
ORDER BY c1.person_id;

SELECT 1 AS stratum_id,
	person_id AS row_id,
	covariate_id,
	covariate_value
INTO #ccd_covariate_input_for_ps
FROM #covariates
ORDER BY person_id;

--tables for final outcome model   *****need to get matching variables from strata
SELECT 1 AS stratum_id,
	c1.person_id AS row_id,
	CASE 
		WHEN o1.person_id IS NULL
			THEN 0
		ELSE o1.num_outcomes
		END AS y,
	CASE 
		WHEN o1.person_id IS NULL
			THEN datediff(dd, c1.cohort_start_date, c1.cohort_censor_date)
		ELSE o1.time
		END AS time
INTO #ccd_outcome_input_for_outcome
FROM #cohorts c1
LEFT JOIN #outcomes o1
	ON c1.cohort_id = o1.cohort_id
		AND c1.person_id = o1.person_id
ORDER BY c1.person_id;

SELECT stratum_id,
	row_id,
	covariate_id,
	covariate_value
INTO #ccd_covariate_input_for_outcome
FROM (
	SELECT 1 AS stratum_id,
		person_id AS row_id,
		covariate_id,
		covariate_value
	FROM #covariates
	
	UNION
	
	SELECT 1 AS stratum_id,
		person_id AS row_id,
		0 AS covariate_id,
		cohort_id AS covariate_value
	FROM #cohorts
	) t1;

/**summary of number of persona nd number of events*/
/*
select c1.cohort_id, count(c1.person_id) as num_persons, sum(case when o1.person_id is null then 0 else num_outcomes end) as num_outcomes
from #cohorts c1
left join #outcomes o1
on c1.cohort_id = o1.cohort_id
and c1.person_id = o1.person_id
group by c1.cohort_id
*/
TRUNCATE TABLE #pre_cohorts;

DROP TABLE #pre_cohorts;

TRUNCATE TABLE #cohorts;

DROP TABLE #cohorts;

TRUNCATE TABLE #covariates;

DROP TABLE #covariates;

TRUNCATE TABLE #concept_counts;

DROP TABLE #concept_counts;

TRUNCATE TABLE #snomed_to_all_meddra;

DROP TABLE #snomed_to_all_meddra;

TRUNCATE TABLE #rxnorm_to_atc;

DROP TABLE #rxnorm_to_atc;

TRUNCATE TABLE #outcomes;

DROP TABLE #outcomes;
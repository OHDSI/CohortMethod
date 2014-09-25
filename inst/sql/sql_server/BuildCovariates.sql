/* Authors:   Patrick Ryan, Martijn Schuemie
 * Last update date:  7 September 2014
 * 
 * Parameterized SQL to create covariates from cohorts to be used as input in
 * fitting large-scale analytics.
 */

{DEFAULT @target_drug_concept_id = 755695}  /*fluoxetine*/
{DEFAULT @exclusion_concept_ids = 4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019} 

{DEFAULT @use_covariate_demographics = TRUE}
{DEFAULT @use_covariate_condition_occurrence = TRUE}
{DEFAULT @use_covariate_condition_era = FALSE}
{DEFAULT @use_covariate_condition_group = FALSE}
{DEFAULT @use_covariate_drug_exposure = FALSE}
{DEFAULT @use_covariate_drug_era = FALSE}
{DEFAULT @use_covariate_drug_group = FALSE}
{DEFAULT @use_covariate_procedure_occurrence = FALSE}
{DEFAULT @use_covariate_procedure_group = FALSE}
{DEFAULT @use_covariate_observation = FALSE}
{DEFAULT @use_covariate_concept_counts = FALSE}
{DEFAULT @use_covariate_risk_scores = FALSE}
{DEFAULT @use_covariate_interaction_year = FALSE}  
{DEFAULT @use_covariate_interaction_month = FALSE}

{DEFAULT @excluded_covariate_concept_ids = 4027133,4032243,4146536,2002282,2213572,2005890,43534760,21601019} 
{DEFAULT @delete_covariates_small_count = 100}

IF OBJECT_ID('cohort_covariate', 'U') IS NOT NULL --This should only do something in Oracle
  drop table #cohort_covariate;

IF OBJECT_ID('tempdb..#cohort_covariate', 'U') IS NOT NULL
  drop table #cohort_covariate;

create table #cohort_covariate
(
	row_id bigint,
	cohort_id int,
	person_id bigint,
	covariate_id bigint,
	covariate_value float
);


IF OBJECT_ID('cohort_covariate_ref', 'U') IS NOT NULL --This should only do something in Oracle
  drop table #cohort_covariate_ref;

IF OBJECT_ID('tempdb..#cohort_covariate_ref', 'U') IS NOT NULL
  drop table #cohort_covariate_ref;

create table #cohort_covariate_ref
(
	covariate_id bigint,
	covariate_name varchar(max),
	analysis_id int,
	concept_id int
);

--covariate for exposure status, determining which patients are in which treatment group (only those in cohort 1 will get recorded)
INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	VALUES (1, 'Exposure status', 1, @target_drug_concept_id);


INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT row_id,
		cohort_id, 
		person_id, 
		1 as covariate_id, 
		cohort_id as covariate_value
	FROM #cohort_person
	WHERE cohort_id = 1
;


/**************************
***************************
DEMOGRAPHICS
***************************
**************************/	

{@use_covariate_demographics} ? {

	--gender
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name,analysis_id,concept_id)
	SELECT DISTINCT p1.gender_concept_id AS covariate_id,
		'Gender = ' + v1.concept_name as covariate_name,
		2 as analysis_id,
		p1.gender_concept_id as concept_id
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.person p1
		ON cp1.person_id = p1.person_id
	INNER JOIN 
		(SELECT concept_id, concept_name
		FROM @cdm_schema.dbo.concept
		WHERE LOWER(concept_class) = 'gender'
		) v1
	ON p1.gender_concept_id = v1.concept_id
	;
 
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		gender_concept_id AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.person p1
		ON cp1.person_id = p1.person_id
	WHERE p1.gender_concept_id in
		(SELECT concept_id
		FROM @cdm_schema.dbo.concept
		WHERE LOWER(concept_class) = 'gender'
		)
	;


	--race
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name,analysis_id,concept_id)
	SELECT DISTINCT p1.race_concept_id AS covariate_id,
		'Race = ' + v1.concept_name as covariate_name,
		3 as analysis_id,
		p1.race_concept_id as concept_id
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.person p1
		ON cp1.person_id = p1.person_id
	INNER JOIN 
		(SELECT concept_id, concept_name
		FROM @cdm_schema.dbo.concept
		WHERE LOWER(concept_class) = 'race'
		) v1
	ON p1.race_concept_id = v1.concept_id
	;
 
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		race_concept_id AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.person p1
		ON cp1.person_id = p1.person_id
	WHERE p1.race_concept_id in
		(SELECT concept_id
		FROM @cdm_schema.dbo.concept
		WHERE LOWER(concept_class) = 'race'
		)
	;	


	--ethnicity
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name,analysis_id,concept_id)
	SELECT DISTINCT p1.ethnicity_concept_id AS covariate_id,
		'Ethnicity = ' + v1.concept_name as covariate_name,
		4 as analysis_id,
		p1.ethnicity_concept_id as concept_id
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.person p1
		ON cp1.person_id = p1.person_id
	INNER JOIN 
		(SELECT concept_id, concept_name
		FROM @cdm_schema.dbo.concept
		WHERE LOWER(concept_class) = 'ethnicity'
		) v1
	ON p1.ethnicity_concept_id = v1.concept_id
	;
 
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		race_concept_id AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.person p1
		ON cp1.person_id = p1.person_id
	WHERE p1.ethnicity_concept_id in
		(SELECT concept_id
		FROM @cdm_schema.dbo.concept
		WHERE LOWER(concept_class) = 'ethnicity'
		)
	;	


	--age group
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT
		floor((year(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 10) + 10 AS covariate_id,
		'Age group: ' + CAST(floor((year(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 10)*10 as VARCHAR) + '-' + CAST((floor((year(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 10)+1)*10-1 as VARCHAR) AS covariate_name,
		4 as analysis_id,
		0 as concept_id
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.person p1
		ON cp1.person_id = p1.person_id
	WHERE (year(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) >= 0
	AND (year(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) < 100
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		FLOOR((YEAR(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 10) + 10 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.person p1
		ON cp1.person_id = p1.person_id
	WHERE (YEAR(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) >= 0
	AND (YEAR(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) < 100
	;
	



	--index year
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name,analysis_id, concept_id)
	SELECT DISTINCT YEAR(cohort_start_date) as covariate_id,
		'Index year: ' + CAST(YEAR(cohort_start_date) AS VARCHAR) as covariate_name,
		5 as analysis_id,
		0 as concept_id
	FROM #cohort_person cp1
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		YEAR(cohort_start_date) AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
	;
	
	
	--index month
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT MONTH(cohort_start_date)+20 as covariate_id,
		'Index month: ' + CAST(MONTH(cohort_start_date) AS VARCHAR) as covariate_name,
		6 as analysis_id,
		0 as concept_id
	FROM #cohort_person cp1
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		MONTH(cohort_start_date)+20 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
	;
	
	

} 



/**************************
***************************
CONDITION OCCURRENCE
***************************
**************************/	
{@use_covariate_condition_occurrence} ? {
	
	--conditions exist:  episode in last 365d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(co1.condition_concept_id AS BIGINT)*1000 + 101 AS covariate_id,
		'Condition occurrence record observed during 365d on or prior to cohort index:  ' +
		CAST(co1.condition_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		101 as analysis_id,
		co1.condition_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_occurrence co1
		ON cp1.person_id = co1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON co1.condition_concept_id = c1.concept_id
	WHERE co1.condition_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  co1.condition_start_date <= cp1.cohort_start_date
		AND co1.condition_start_date >= dateadd(dd, - 365, cp1.cohort_start_date)
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(co1.condition_concept_id AS BIGINT)*1000 + 101 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_occurrence co1
		ON cp1.person_id = co1.person_id
	WHERE co1.condition_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  co1.condition_start_date <= cp1.cohort_start_date
		AND co1.condition_start_date >= dateadd(dd, - 365, cp1.cohort_start_date)
	;
	
	
	
	
	--conditions:  episode in last 30d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(co1.condition_concept_id AS BIGINT)*1000 + 102 AS covariate_id,
		'Condition occurrence record observed during 30d on or prior to cohort index:  ' +
		CAST(co1.condition_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		102 as analysis_id,
		co1.condition_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_occurrence co1
		ON cp1.person_id = co1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON co1.condition_concept_id = c1.concept_id
	WHERE co1.condition_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  co1.condition_start_date <= cp1.cohort_start_date
		AND co1.condition_start_date >= dateadd(dd, - 30, cp1.cohort_start_date)
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(co1.condition_concept_id AS BIGINT)*1000 + 102 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_occurrence co1
		ON cp1.person_id = co1.person_id
	WHERE co1.condition_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  co1.condition_start_date <= cp1.cohort_start_date
		AND co1.condition_start_date >= dateadd(dd, - 30, cp1.cohort_start_date)
	;
	
	
	
	--conditions:  primary inpatient diagnosis in last 180d
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(co1.condition_concept_id AS BIGINT)*1000 + 103 AS covariate_id,
		'Condition occurrence record of primary inpatient diagnosis observed during 180d on or prior to cohort index:  ' +
		CAST(co1.condition_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		103 as analysis_id,
		co1.condition_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_occurrence co1
		ON cp1.person_id = co1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON co1.condition_concept_id = c1.concept_id
	WHERE co1.condition_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND co1.condition_type_concept_id in (38000183, 38000184, 38000199, 38000200)
	AND  co1.condition_start_date <= cp1.cohort_start_date
		AND co1.condition_start_date >= dateadd(dd, -180, cp1.cohort_start_date)
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(co1.condition_concept_id AS BIGINT)*1000 + 103 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_occurrence co1
		ON cp1.person_id = co1.person_id
	WHERE co1.condition_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND co1.condition_type_concept_id in (38000183, 38000184, 38000199, 38000200)
	AND  co1.condition_start_date <= cp1.cohort_start_date
		AND co1.condition_start_date >= dateadd(dd, -180, cp1.cohort_start_date)
	;
	
}	




/**************************
***************************
CONDITION ERA
***************************
**************************/	
{@use_covariate_condition_era} ? {
	
	
	--condition:  exist any time prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(ce1.condition_concept_id AS BIGINT)*1000 + 201 AS covariate_id,
		'Condition era record observed during anytime on or prior to cohort index:  ' +
		CAST(ce1.condition_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		201 as analysis_id,
		ce1.condition_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_era ce1
		ON cp1.person_id = ce1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON ce1.condition_concept_id = c1.concept_id
	WHERE ce1.condition_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
		AND  ce1.condition_era_start_date <= cp1.cohort_start_date
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(ce1.condition_concept_id AS BIGINT)*1000 + 201 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_era ce1
		ON cp1.person_id = ce1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON ce1.condition_concept_id = c1.concept_id
	WHERE ce1.condition_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
		AND  ce1.condition_era_start_date <= cp1.cohort_start_date
	;

	
	
	--concurrent on index date (era overlapping)
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(ce1.condition_concept_id AS BIGINT)*1000 + 202 AS covariate_id,
		'Condition era record observed concurrent (overlapping) with cohort index:  ' +
		CAST(ce1.condition_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		202 as analysis_id,
		ce1.condition_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_era ce1
		ON cp1.person_id = ce1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON ce1.condition_concept_id = c1.concept_id
	WHERE ce1.condition_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
		AND  ce1.condition_era_start_date <= cp1.cohort_start_date
		AND  ce1.condition_era_end_date >= cp1.cohort_start_date
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(ce1.condition_concept_id AS BIGINT)*1000 + 202 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_era ce1
		ON cp1.person_id = ce1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON ce1.condition_concept_id = c1.concept_id
	WHERE ce1.condition_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
		AND  ce1.condition_era_start_date <= cp1.cohort_start_date
		AND  ce1.condition_era_end_date >= cp1.cohort_start_date
	;
}	



/**************************
***************************
CONDITION GROUP
***************************
**************************/	
{@use_covariate_condition_group} ? {



	IF OBJECT_ID('condition_group', 'U') IS NOT NULL --This should only do something in Oracle
	  drop table condition_group;

	IF OBJECT_ID('tempdb..#condition_group', 'U') IS NOT NULL
	  drop table #condition_group;

	create table #condition_group
	(
		descendant_concept_id int,
		ancestor_concept_id int
	);


	--currently using MedDRA hierarchy
	--future extension:  expand to SNOMED classes as well
	
	INSERT INTO #condition_group (descendant_concept_id, ancestor_concept_id)
	select distinct ca1.descendant_concept_id, ca1.ancestor_concept_id 
	from
	(
	select covariate_id, covariate_name, analysis_id, concept_id
	from #cohort_covariate_ref
	where analysis_id > 100 and analysis_id < 300
	) ccr1
	inner join
	@cdm_schema.dbo.concept_ancestor ca1
	on ccr1.concept_id = ca1.descendant_concept_id
	inner join @cdm_schema.dbo.concept c1
	on ca1.ancestor_concept_id = c1.concept_id
	where c1.vocabulary_id = 15
	and c1.concept_class <> 'System Organ Class' 
	and c1.concept_id not in (36302170, 36303153, 36313966 /*Investigation concepts, too broad*/ {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } ) 
	;


	
	
	
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(cg1.ancestor_concept_id AS BIGINT)*1000 + ccr1.analysis_id AS covariate_id,
		CASE WHEN analysis_id = 101 THEN 'Condition occurrence record observed during 365d on or prior to cohort index within condition group:  '
			WHEN analysis_id = 102 THEN 'Condition occurrence record observed during 30d on or prior to cohort index within condition group:  ' 
			WHEN analysis_id = 103 THEN 'Condition occurrence record of primary inpatient diagnosis observed during 180d on or prior to cohort index within condition group:  '
			WHEN analysis_id = 201 THEN 'Condition era record observed during anytime on or prior to cohort index within condition group:  '
			WHEN analysis_id = 202 THEN 'Condition era record observed concurrent (overlapping) with cohort index within condition group:  '
			ELSE 'Other condition group analysis'
		END	
		+ CAST(cg1.ancestor_concept_id AS VARCHAR) + '-' + c1.concept_name
		AS covariate_name,
		ccr1.analysis_id,
		cg1.ancestor_concept_id as concept_id
	FROM #cohort_covariate cc1
		INNER JOIN 
		(
		select covariate_id, covariate_name, analysis_id, concept_id
		from #cohort_covariate_ref
		where analysis_id > 100 and analysis_id < 300
		) ccr1
		ON cc1.covariate_id = ccr1.covariate_id
		INNER JOIN
		#condition_group cg1
		ON ccr1.concept_id = cg1.descendant_concept_id
		INNER JOIN
		@cdm_schema.dbo.concept c1
		on cg1.ancestor_concept_id = c1.concept_id
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cc1.row_id,
		cc1.cohort_id,
		cc1.person_id,
		CAST(cg1.ancestor_concept_id AS BIGINT)*1000 + ccr1.analysis_id AS covariate_id,
		1 AS covariate_value
	FROM #cohort_covariate cc1
		INNER JOIN 
		(
		select covariate_id, covariate_name, analysis_id, concept_id
		from #cohort_covariate_ref
		where analysis_id > 100 and analysis_id < 300
		) ccr1
		ON cc1.covariate_id = ccr1.covariate_id
		INNER JOIN
		#condition_group cg1
		ON ccr1.concept_id = cg1.descendant_concept_id
	;
	
	
	


	
	
	TRUNCATE TABLE #condition_group;

	DROP TABLE #condition_group;


}



/**************************
***************************
DRUG EXPOSURE
***************************
**************************/	
{@use_covariate_drug_exposure} ? {
	
	--drug exist:  episode in last 365d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT)*1000 + 401 AS covariate_id,
		'Drug exposure record observed during 365d on or prior to cohort index:  ' +
		CAST(de1.drug_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		401 as analysis_id,
		de1.drug_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_exposure de1
		ON cp1.person_id = de1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON de1.drug_concept_id = c1.concept_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_exposure_start_date <= cp1.cohort_start_date
		AND de1.drug_exposure_start_date >= dateadd(dd, -365, cp1.cohort_start_date)
	;
	
		
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
    cp1.cohort_id,
		cp1.person_id,
		CAST(de1.drug_concept_id AS BIGINT)*1000 + 401 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_exposure de1
		ON cp1.person_id = de1.person_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_exposure_start_date <= cp1.cohort_start_date
		AND de1.drug_exposure_start_date >= dateadd(dd, -365, cp1.cohort_start_date)
	;
	
	
	
	--drug exist:  episode in last 30d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT)*1000 + 402 AS covariate_id,
		'Drug exposure record observed during 30d on or prior to cohort index:  ' +
		CAST(de1.drug_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		402 as analysis_id,
		de1.drug_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_exposure de1
		ON cp1.person_id = de1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON de1.drug_concept_id = c1.concept_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_exposure_start_date <= cp1.cohort_start_date
		AND de1.drug_exposure_start_date >= dateadd(dd, -30, cp1.cohort_start_date)
	;
	
		
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(de1.drug_concept_id AS BIGINT)*1000 + 402 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_exposure de1
		ON cp1.person_id = de1.person_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_exposure_start_date <= cp1.cohort_start_date
		AND de1.drug_exposure_start_date >= dateadd(dd, -30, cp1.cohort_start_date)
	;
	

	
}	


/**************************
***************************
DRUG ERA
***************************
**************************/	
{@use_covariate_drug_era} ? {
	
	--drug exist:  episode in last 365d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT)*1000 + 501 AS covariate_id,
		'Drug era record observed during 365d on or prior to cohort index:  ' +
		CAST(de1.drug_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		501 as analysis_id,
		de1.drug_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_era de1
		ON cp1.person_id = de1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON de1.drug_concept_id = c1.concept_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_era_start_date <= cp1.cohort_start_date
		AND de1.drug_era_end_date >= dateadd(dd, -365, cp1.cohort_start_date)
	;
	
		
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(de1.drug_concept_id AS BIGINT)*1000 + 501 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_era de1
		ON cp1.person_id = de1.person_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_era_start_date <= cp1.cohort_start_date
		AND de1.drug_era_end_date >= dateadd(dd, -365, cp1.cohort_start_date)
	;
	
	
	--drug exist:  episode in last 30d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT)*1000 + 502 AS covariate_id,
		'Drug era record observed during 30d on or prior to cohort index:  ' +
		CAST(de1.drug_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		502 as analysis_id,
		de1.drug_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_era de1
		ON cp1.person_id = de1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON de1.drug_concept_id = c1.concept_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_era_start_date <= cp1.cohort_start_date
		AND de1.drug_era_end_date >= dateadd(dd, -30, cp1.cohort_start_date)
	;
	
		
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(de1.drug_concept_id AS BIGINT)*1000 + 502 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_era de1
		ON cp1.person_id = de1.person_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_era_start_date <= cp1.cohort_start_date
		AND de1.drug_era_end_date >= dateadd(dd, -30, cp1.cohort_start_date)
	;	
	

	--concurrent on index date (era overlapping)
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT)*1000 + 503 AS covariate_id,
		'Drug era record observed concurrent (overlapping) with cohort index:  ' +
		CAST(de1.drug_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		503 as analysis_id,
		de1.drug_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_era de1
		ON cp1.person_id = de1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON de1.drug_concept_id = c1.concept_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_era_start_date <= cp1.cohort_start_date
		AND de1.drug_era_end_date >= cp1.cohort_start_date
	;
	
		
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(de1.drug_concept_id AS BIGINT)*1000 + 503 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_era de1
		ON cp1.person_id = de1.person_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_era_start_date <= cp1.cohort_start_date
		AND de1.drug_era_end_date >= cp1.cohort_start_date
	;		
	
	
	--drug exist:  episode in all time prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT)*1000 + 504 AS covariate_id,
		'Drug era record observed during anytime on or prior to cohort index:  ' +
		CAST(de1.drug_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		504 as analysis_id,
		de1.drug_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_era de1
		ON cp1.person_id = de1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON de1.drug_concept_id = c1.concept_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_era_start_date <= cp1.cohort_start_date
	;
	
		
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(de1.drug_concept_id AS BIGINT)*1000 + 504 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.drug_era de1
		ON cp1.person_id = de1.person_id
	WHERE de1.drug_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND de1.drug_concept_id not in (SELECT descendant_concept_id FROM @cdm_schema.dbo.concept_ancestor WHERE ancestor_concept_id IN (@target_drug_concept_id, @comparator_drug_concept_id))
	AND  de1.drug_era_start_date <= cp1.cohort_start_date
	;	
	
	
}	


/**************************
***************************
DRUG GROUP
***************************
**************************/	
{@use_covariate_drug_group} ? {

	IF OBJECT_ID('drug_group', 'U') IS NOT NULL --This should only do something in Oracle
	  drop table drug_group;

	IF OBJECT_ID('tempdb..#drug_group', 'U') IS NOT NULL
	  drop table #drug_group;

	create table #drug_group
	(
		descendant_concept_id int,
		ancestor_concept_id int
	);


	--ATC
	INSERT INTO #drug_group (descendant_concept_id, ancestor_concept_id)
	select distinct ca1.descendant_concept_id, ca1.ancestor_concept_id 
	from
	(
	select covariate_id, covariate_name, analysis_id, concept_id
	from #cohort_covariate_ref
	where analysis_id > 500 and analysis_id < 600
	) ccr1
	inner join
	@cdm_schema.dbo.concept_ancestor ca1
	on ccr1.concept_id = ca1.descendant_concept_id
	inner join @cdm_schema.dbo.concept c1
	on ca1.ancestor_concept_id = c1.concept_id
	where c1.vocabulary_id = 21
	and len(c1.concept_code) in (1,3,5)
	and c1.concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } ) 
	;


	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(cg1.ancestor_concept_id AS BIGINT)*1000 + ccr1.analysis_id AS covariate_id,
		CASE WHEN analysis_id = 501 THEN 'Drug era record observed during 365d on or prior to cohort index within drug group:  '
			WHEN analysis_id = 502 THEN 'Drug era record observed during 30d on or prior to cohort index within drug group:  '
			WHEN analysis_id = 503 THEN 'Drug era record observed concurrent (overlapping) with cohort index within drug group:  '
			ELSE 'Other drug group analysis'
		END	
		+ CAST(cg1.ancestor_concept_id AS VARCHAR) + '-' + c1.concept_name
		AS covariate_name,
		ccr1.analysis_id,
		cg1.ancestor_concept_id as concept_id
	FROM #cohort_covariate cc1
		INNER JOIN 
		(
		select covariate_id, covariate_name, analysis_id, concept_id
		from #cohort_covariate_ref
		where analysis_id > 500 and analysis_id < 600
		) ccr1
		ON cc1.covariate_id = ccr1.covariate_id
		INNER JOIN
		#drug_group cg1
		ON ccr1.concept_id = cg1.descendant_concept_id
		INNER JOIN
		@cdm_schema.dbo.concept c1
		on cg1.ancestor_concept_id = c1.concept_id
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cc1.row_id,
		cc1.cohort_id,
		cc1.person_id,
		CAST(cg1.ancestor_concept_id AS BIGINT)*1000 + ccr1.analysis_id AS covariate_id,
		1 AS covariate_value
	FROM #cohort_covariate cc1
		INNER JOIN 
		(
		select covariate_id, covariate_name, analysis_id, concept_id
		from #cohort_covariate_ref
		where analysis_id > 500 and analysis_id < 600
		) ccr1
		ON cc1.covariate_id = ccr1.covariate_id
		INNER JOIN
		#drug_group cg1
		ON ccr1.concept_id = cg1.descendant_concept_id
	;
	
	
	

	--number of drugs within each ATC3 groupings all time prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(cg1.ancestor_concept_id AS BIGINT)*1000 + 601 AS covariate_id,
		'Number of ingredients within the drug group observed all time on or prior to cohort index: '
		+ CAST(cg1.ancestor_concept_id AS VARCHAR) + '-' + c1.concept_name
		AS covariate_name,
		601 as analysis_id,
		cg1.ancestor_concept_id as concept_id
	FROM #cohort_covariate cc1
		INNER JOIN 
		(
		select covariate_id, covariate_name, analysis_id, concept_id
		from #cohort_covariate_ref
		where analysis_id  = 504
		) ccr1
		ON cc1.covariate_id = ccr1.covariate_id
		INNER JOIN
		#drug_group cg1
		ON ccr1.concept_id = cg1.descendant_concept_id
		INNER JOIN
		@cdm_schema.dbo.concept c1
		on cg1.ancestor_concept_id = c1.concept_id
		WHERE len(c1.concept_code) = 3
	;


	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT cc1.row_id,
		cc1.cohort_id,
		cc1.person_id,
		CAST(cg1.ancestor_concept_id AS BIGINT)*1000 + 601 AS covariate_id,
		COUNT(DISTINCT ccr1.concept_id) AS covariate_value
	FROM #cohort_covariate cc1
		INNER JOIN 
		(
		select covariate_id, covariate_name, analysis_id, concept_id
		from #cohort_covariate_ref
		where analysis_id  = 504
		) ccr1
		ON cc1.covariate_id = ccr1.covariate_id
		INNER JOIN
		#drug_group cg1
		ON ccr1.concept_id = cg1.descendant_concept_id
		INNER JOIN
		@cdm_schema.dbo.concept c1
		on cg1.ancestor_concept_id = c1.concept_id
	WHERE len(c1.concept_code) = 3
	GROUP BY cc1.row_id,
    cc1.cohort_id,
		cc1.person_id,
		CAST(cg1.ancestor_concept_id AS BIGINT)*1000 + 601
	;	
	
	
	TRUNCATE TABLE #drug_group;

	DROP TABLE #drug_group;

		
}	


/**************************
***************************
PROCEDURE OCCURRENCE
***************************
**************************/	
{@use_covariate_procedure_occurrence} ? {


	--procedures exist:  episode in last 365d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(po1.procedure_concept_id AS BIGINT)*1000 + 701 AS covariate_id,
		'Procedure occurrence record observed during 365d on or prior to cohort index:  ' +
		CAST(po1.procedure_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		701 as analysis_id,
		po1.procedure_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.procedure_occurrence po1
		ON cp1.person_id = po1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON po1.procedure_concept_id = c1.concept_id
	WHERE po1.procedure_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  po1.procedure_date <= cp1.cohort_start_date
		AND po1.procedure_date >= dateadd(dd, -365, cp1.cohort_start_date)
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(po1.procedure_concept_id AS BIGINT)*1000 + 701 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.procedure_occurrence po1
		ON cp1.person_id = po1.person_id
	WHERE po1.procedure_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  po1.procedure_date <= cp1.cohort_start_date
		AND po1.procedure_date >= dateadd(dd, -365, cp1.cohort_start_date)
	;
	

	--procedures exist:  episode in last 30d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(po1.procedure_concept_id AS BIGINT)*1000 + 702 AS covariate_id,
		'Procedure occurrence record observed during 30d on or prior to cohort index:  ' +
		CAST(po1.procedure_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		702 as analysis_id,
		po1.procedure_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.procedure_occurrence po1
		ON cp1.person_id = po1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON po1.procedure_concept_id = c1.concept_id
	WHERE po1.procedure_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  po1.procedure_date <= cp1.cohort_start_date
		AND po1.procedure_date >= dateadd(dd, -30, cp1.cohort_start_date)
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(po1.procedure_concept_id AS BIGINT)*1000 + 702 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.procedure_occurrence po1
		ON cp1.person_id = po1.person_id
	WHERE po1.procedure_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  po1.procedure_date <= cp1.cohort_start_date
		AND po1.procedure_date >= dateadd(dd, -30, cp1.cohort_start_date)
	;
	
	
}	


/**************************
***************************
PROCEDURE GROUP
***************************
**************************/	
{@use_covariate_procedure_group} ? {
	
	IF OBJECT_ID('procedure_group', 'U') IS NOT NULL --This should only do something in Oracle
	  drop table procedure_group;

	IF OBJECT_ID('tempdb..#procedure_group', 'U') IS NOT NULL
	  drop table #procedure_group;

	create table #procedure_group
	(
		descendant_concept_id int,
		ancestor_concept_id int
	);


	--SNOMED
	INSERT INTO #procedure_group (descendant_concept_id, ancestor_concept_id)
	select distinct ca1.descendant_concept_id, ca1.ancestor_concept_id 
	from
	(
	select covariate_id, covariate_name, analysis_id, concept_id
	from #cohort_covariate_ref
	where analysis_id > 700 and analysis_id < 800
	) ccr1
	inner join
	@cdm_schema.dbo.concept_ancestor ca1
	on ccr1.concept_id = ca1.descendant_concept_id
	inner join @cdm_schema.dbo.concept c1
	on ca1.ancestor_concept_id = c1.concept_id
	where c1.vocabulary_id = 1
	and c1.concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } ) 
	;


	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(cg1.ancestor_concept_id AS BIGINT)*1000 + ccr1.analysis_id AS covariate_id,
		CASE WHEN analysis_id = 701 THEN 'Procedure occurrence record observed during 365d on or prior to cohort index within procedure group:  '
			WHEN analysis_id = 702 THEN 'Procedure occurrence record observed during 30d on or prior to cohort index within procedure group:  '
			ELSE 'Other procedure group analysis'
		END	
		+ CAST(cg1.ancestor_concept_id AS VARCHAR) + '-' + c1.concept_name
		AS covariate_name,
		ccr1.analysis_id,
		cg1.ancestor_concept_id as concept_id
	FROM #cohort_covariate cc1
		INNER JOIN 
		(
		select covariate_id, covariate_name, analysis_id, concept_id
		from #cohort_covariate_ref
		where analysis_id > 700 and analysis_id < 800
		) ccr1
		ON cc1.covariate_id = ccr1.covariate_id
		INNER JOIN
		#procedure_group cg1
		ON ccr1.concept_id = cg1.descendant_concept_id
		INNER JOIN
		@cdm_schema.dbo.concept c1
		on cg1.ancestor_concept_id = c1.concept_id
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cc1.row_id,
		cc1.cohort_id,
		cc1.person_id,
		CAST(cg1.ancestor_concept_id AS BIGINT)*1000 + ccr1.analysis_id AS covariate_id,
		1 AS covariate_value
	FROM #cohort_covariate cc1
		INNER JOIN 
		(
		select covariate_id, covariate_name, analysis_id, concept_id
		from #cohort_covariate_ref
		where analysis_id > 700 and analysis_id < 800
		) ccr1
		ON cc1.covariate_id = ccr1.covariate_id
		INNER JOIN
		#procedure_group cg1
		ON ccr1.concept_id = cg1.descendant_concept_id
	;
	

	TRUNCATE TABLE #procedure_group;

	DROP TABLE #procedure_group;

	
}	


/**************************
***************************
OBSERVATION
***************************
**************************/	
{@use_covariate_observation} ? {
	
	
	--observation exist:  episode in last 365d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(o1.observation_concept_id AS BIGINT)*1000 + 901 AS covariate_id,
		'Observation record observed during 365d on or prior to cohort index:  ' +
		CAST(o1.observation_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		901 as analysis_id,
		o1.observation_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON o1.observation_concept_id = c1.concept_id
	WHERE o1.observation_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, -365, cp1.cohort_start_date)
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(o1.observation_concept_id AS BIGINT)*1000 + 901 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
	WHERE o1.observation_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, -365, cp1.cohort_start_date)
	;
	


	--observation exist:  episode in last 30d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(o1.observation_concept_id AS BIGINT)*1000 + 902 AS covariate_id,
		'Observation record observed during 30d on or prior to cohort index:  ' +
		CAST(o1.observation_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		902 as analysis_id,
		o1.observation_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON o1.observation_concept_id = c1.concept_id
	WHERE o1.observation_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, -30, cp1.cohort_start_date)
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(o1.observation_concept_id AS BIGINT)*1000 + 902 AS covariate_id,
		1 AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
	WHERE o1.observation_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, -30, cp1.cohort_start_date)
	;
		
	
	
	--for numeric values with valid range, latest value within 180 below low
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(t1.observation_concept_id AS BIGINT)*1000 + 903 AS covariate_id,
		'Observation numeric value below normal range for latest value within 180d of cohort index:  ' +
		CAST(t1.observation_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		903 as analysis_id,
		t1.observation_concept_id as concept_id
	FROM
	(
	SELECT cp1.cohort_id, 
		cp1.person_id, 
		o1.observation_concept_id, 
		o1.value_as_number, 
		o1.range_low, 
		o1.range_high, 
		ROW_NUMBER() OVER (PARTITION BY cp1.cohort_id, cp1.person_id, o1.observation_concept_id ORDER BY o1.observation_date desc) as rn1	
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
	WHERE o1.observation_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, -180, cp1.cohort_start_date)
	AND o1.value_as_number >= 0
	AND o1.range_low >= 0
	AND o1.range_high >= 0
	) t1
	LEFT JOIN @cdm_schema.dbo.concept c1
		ON t1.observation_concept_id = c1.concept_id
	WHERE RN1 = 1
	AND VALUE_AS_NUMBER < RANGE_LOW
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		row_id,
		cohort_id,
		person_id,
		CAST(observation_concept_id AS BIGINT)*1000 + 903 AS covariate_id,
		1 AS covariate_value
	FROM
	(
	SELECT cp1.row_id,
		cp1.cohort_id, 
		cp1.person_id, 
		o1.observation_concept_id, 
		o1.value_as_number, 
		o1.range_low, 
		o1.range_high, 
		ROW_NUMBER() OVER (PARTITION BY cp1.cohort_id, cp1.person_id, o1.observation_concept_id ORDER BY o1.observation_date desc) as rn1	
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
	WHERE o1.observation_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, -180, cp1.cohort_start_date)
	AND o1.value_as_number >= 0
	AND o1.range_low >= 0
	AND o1.range_high >= 0
	) t1
	WHERE RN1 = 1
	AND VALUE_AS_NUMBER < RANGE_LOW
	;
		
	
	
	--for numeric values with valid range, latest value above high
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(t1.observation_concept_id AS BIGINT)*1000 + 904 AS covariate_id,
		'Observation numeric value above normal range for latest value within 180d of cohort index:  ' +
		CAST(t1.observation_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		904 as analysis_id,
		t1.observation_concept_id as concept_id
	FROM
	(
	SELECT cp1.cohort_id, 
		cp1.person_id, 
		o1.observation_concept_id, 
		o1.value_as_number, 
		o1.range_low, 
		o1.range_high, 
		ROW_NUMBER() OVER (PARTITION BY cp1.cohort_id, cp1.person_id, o1.observation_concept_id ORDER BY o1.observation_date desc) as rn1	
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
	WHERE o1.observation_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, -180, cp1.cohort_start_date)
	AND o1.value_as_number >= 0
	AND o1.range_low >= 0
	AND o1.range_high >= 0
	) t1
	LEFT JOIN @cdm_schema.dbo.concept c1
		ON t1.observation_concept_id = c1.concept_id
	WHERE RN1 = 1
	AND VALUE_AS_NUMBER > RANGE_HIGH
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		row_id,
		cohort_id,
		person_id,
		CAST(observation_concept_id AS BIGINT)*1000 + 904 AS covariate_id,
		1 AS covariate_value
	FROM
	(
	SELECT cp1.row_id,
		cp1.cohort_id, 
		cp1.person_id, 
		o1.observation_concept_id, 
		o1.value_as_number, 
		o1.range_low, 
		o1.range_high, 
		ROW_NUMBER() OVER (PARTITION BY cp1.cohort_id, cp1.person_id, o1.observation_concept_id ORDER BY o1.observation_date desc) as rn1	
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
	WHERE o1.observation_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, -180, cp1.cohort_start_date)
	AND o1.value_as_number >= 0
	AND o1.range_low >= 0
	AND o1.range_high >= 0
	) t1
	WHERE RN1 = 1
	AND VALUE_AS_NUMBER > RANGE_HIGH
	;
	
	
	--observation exist:  episode in last 365d prior
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(o1.observation_concept_id AS BIGINT)*1000 + 905 AS covariate_id,
		'Number of observations observed during 365d on or prior to cohort index:  ' +
		CAST(o1.observation_concept_id AS VARCHAR) + '-' + 
		CASE WHEN c1.concept_name IS NOT NULL 
			THEN c1.concept_name
			ELSE 'Unknown invalid concept'
		END
		AS covariate_name,
		905 as analysis_id,
		o1.observation_concept_id as concept_id
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
		LEFT JOIN @cdm_schema.dbo.concept c1
		ON o1.observation_concept_id = c1.concept_id
	WHERE o1.observation_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, -365, cp1.cohort_start_date)
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		CAST(o1.observation_concept_id AS BIGINT)*1000 + 905 AS covariate_id,
		count(observation_id) AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
	WHERE o1.observation_concept_id not in (0 {@excluded_covariate_concept_ids != ''} ? {, @excluded_covariate_concept_ids } )
	AND  o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, -365, cp1.cohort_start_date)
	GROUP BY cp1.row_id,
  	cp1.cohort_id,
		cp1.person_id,
		CAST(o1.observation_concept_id AS BIGINT)*1000 + 905
	;	
	
	
}

		
/**************************
***************************
DATA DENSITY CONCEPT COUNTS
***************************
**************************/	
{@use_covariate_concept_counts} ? {


	--Number of distinct conditions observed in 365d on or prior to cohort index
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	VALUES (1000, 'Number of distinct conditions observed in 365d on or prior to cohort index', 1000, 0)
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		1000 AS covariate_id,
		COUNT(DISTINCT ce1.condition_concept_id) AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.condition_era ce1
		ON cp1.person_id = ce1.person_id
	WHERE ce1.condition_era_start_date <= cp1.cohort_start_date
		AND ce1.condition_era_end_date >= dateadd(dd, -365, cp1.cohort_start_date)
	GROUP BY cp1.row_id,
  	cp1.cohort_id,
		cp1.person_id
	;


	--Number of distinct drug ingredients observed in 365d on or prior to cohort index
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	VALUES (1001, 'Number of distinct drug ingredients observed in 365d on or prior to cohort index', 1001, 0)
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		1001 AS covariate_id,
		COUNT(DISTINCT de1.drug_concept_id) AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.drug_era de1
		ON cp1.person_id = de1.person_id
	WHERE de1.drug_era_start_date <= cp1.cohort_start_date
		AND de1.drug_era_start_date >= dateadd(dd, -365, cp1.cohort_start_date)
	GROUP BY cp1.row_id,
    cp1.cohort_id,
		cp1.person_id
	;

	
	--Number of distinct procedures observed in 365d on or prior to cohort index
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	VALUES (1002, 'Number of distinct procedures observed in 365d on or prior to cohort index', 1002, 0)
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		1002 AS covariate_id,
		COUNT(DISTINCT po1.procedure_concept_id) AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.procedure_occurrence po1
		ON cp1.person_id = po1.person_id
	WHERE po1.procedure_date <= cp1.cohort_start_date
		AND po1.procedure_date >= dateadd(dd, -365, cp1.cohort_start_date)
	GROUP BY cp1.row_id,
  	cp1.cohort_id,
		cp1.person_id
	;

	
	--Number of distinct observations observed in 365d on or prior to cohort index
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	VALUES (1003, 'Number of distinct observations observed in 365d on or prior to cohort index', 1003, 0)
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		1003 AS covariate_id,
		COUNT(DISTINCT o1.observation_concept_id) AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.observation o1
		ON cp1.person_id = o1.person_id
	WHERE o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, -365, cp1.cohort_start_date)
	GROUP BY cp1.row_id,
  	cp1.cohort_id,
		cp1.person_id
	;

	
	--Number of visits observed in 365d on or prior to cohort index
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	VALUES (1004, 'Number of visits observed in 365d on or prior to cohort index', 1004, 0)
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		1004 AS covariate_id,
		COUNT(vo1.visit_occurrence_id) AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.visit_occurrence vo1
		ON cp1.person_id = vo1.person_id
	WHERE vo1.visit_start_date <= cp1.cohort_start_date
		AND vo1.visit_start_date >= dateadd(dd, -365, cp1.cohort_start_date)
	GROUP BY cp1.row_id,
  	cp1.cohort_id,
		cp1.person_id
	;


	--Number of inpatient visits observed in 365d on or prior to cohort index
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	VALUES (1005, 'Number of inpatient visits observed in 365d on or prior to cohort index', 1005, 0)
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		1005 AS covariate_id,
		COUNT(vo1.visit_occurrence_id) AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.visit_occurrence vo1
		ON cp1.person_id = vo1.person_id
	WHERE vo1.visit_start_date <= cp1.cohort_start_date
		AND vo1.visit_start_date >= dateadd(dd, -365, cp1.cohort_start_date)
		AND vo1.place_of_service_concept_id = 9201
	GROUP BY cp1.row_id,
  	cp1.cohort_id,
		cp1.person_id
	;
	
	--Number of ER visits observed in 365d on or prior to cohort index
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	VALUES (1006, 'Number of ER visits observed in 365d on or prior to cohort index', 1006, 0)
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		cp1.row_id,
		cp1.cohort_id,
		cp1.person_id,
		1006 AS covariate_id,
		COUNT(vo1.visit_occurrence_id) AS covariate_value
	FROM #cohort_person cp1
	INNER JOIN @cdm_schema.dbo.visit_occurrence vo1
		ON cp1.person_id = vo1.person_id
	WHERE vo1.visit_start_date <= cp1.cohort_start_date
		AND vo1.visit_start_date >= dateadd(dd, -365, cp1.cohort_start_date)
		AND vo1.place_of_service_concept_id = 9203
	GROUP BY cp1.row_id,
  	cp1.cohort_id,
		cp1.person_id
	;	
	


	
}


/**************************
***************************
RISK SCORES
***************************
**************************/	
{@use_covariate_risk_scores} ? {


--CHARLSON

	IF OBJECT_ID('Charlson_codes', 'U') IS NOT NULL --This should only do something in Oracle
	  drop table Charlson_codes;

	IF OBJECT_ID('tempdb..#Charlson_codes', 'U') IS NOT NULL
	  drop table #Charlson_codes;

	create table #Charlson_codes
	(
		diag_category_id int,
		icd9 varchar(10)
	);
	
	
	IF OBJECT_ID('Charlson_scoring', 'U') IS NOT NULL --This should only do something in Oracle
	  drop table Charlson_scoring;

	IF OBJECT_ID('tempdb..#Charlson_scoring', 'U') IS NOT NULL
	  drop table #Charlson_scoring;

	create table #Charlson_scoring
	(
		diag_category_id int,
		diag_category_name varchar(255),
		weight int
	);
	
		
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.12')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.32')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.42')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.50')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.51')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.52')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.60')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.61')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.62')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.70')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.71')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.72')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.82')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'410.92')
	insert into #Charlson_codes (diag_category_id, icd9) values (1,'412')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'402.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'402.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'402.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'425')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'425.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'425.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'425.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'425.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'425.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'425.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'425.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'425.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'425.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.23')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.32')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.33')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.42')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.43')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'428.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (2,'429.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.23')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.24')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.29')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.32')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'440.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.03')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'441.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.82')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.83')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.84')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.89')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'442.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.23')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.24')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.29')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.82')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.89')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'443.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'447.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (3,'785.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'362.34')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'430')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'431')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'432')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'432.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'432.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'432.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'433.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'434')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'434.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'434.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'434.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'434.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'434.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'434.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'434.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'434.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'434.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'435')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'435.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'435.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'435.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'435.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'435.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'435.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'436')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'437')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'437.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'437.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'437.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.12')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.19')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.32')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.42')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.50')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.51')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.52')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.53')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.82')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.83')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.84')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.85')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.89')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'438.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'781.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'784.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'997.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'997.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'997.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'997.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (4,'997.09')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.12')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.13')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.42')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.43')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'290.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'331')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'331.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'331.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'331.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'331.19')
	insert into #Charlson_codes (diag_category_id, icd9) values (5,'331.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'415.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'416.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'416.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'491')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'491.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'491.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'491.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'491.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'491.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'491.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'491.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'491.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'492')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'492.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'492.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.12')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.82')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'493.92')
	insert into #Charlson_codes (diag_category_id, icd9) values (6,'494')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'710')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'710.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'710.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'710.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'710.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'710.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'710.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'710.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'710.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.32')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.33')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.89')
	insert into #Charlson_codes (diag_category_id, icd9) values (7,'714.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.50')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.51')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.60')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.61')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.70')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.71')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'531.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.50')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.51')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.60')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.61')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.70')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.71')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'532.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.50')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.51')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.60')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.61')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.70')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.71')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'533.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.50')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.51')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.60')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.61')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.70')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.71')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (8,'534.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (9,'571.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (9,'571.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (9,'571.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (9,'571.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (9,'571.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.03')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.12')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.13')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.23')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.32')
	insert into #Charlson_codes (diag_category_id, icd9) values (10,'250.33')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.42')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.43')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.50')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.51')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.52')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.53')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.60')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.61')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.62')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.63')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.70')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.71')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.72')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.73')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.82')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.83')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.92')
	insert into #Charlson_codes (diag_category_id, icd9) values (11,'250.93')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.12')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.82')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'342.92')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.03')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.04')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.09')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.32')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.42')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.60')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.61')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.89')
	insert into #Charlson_codes (diag_category_id, icd9) values (12,'344.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'585')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'585.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'585.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'585.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'585.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'585.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'585.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'585.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'586')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V42.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V45.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V45.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V45.12')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V56')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V56.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V56.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V56.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V56.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V56.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V56.32')
	insert into #Charlson_codes (diag_category_id, icd9) values (13,'V56.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'140')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'140.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'140.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'140.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'140.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'140.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'140.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'140.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'140.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'141')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'141.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'141.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'141.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'141.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'141.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'141.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'141.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'141.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'141.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'142')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'142.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'142.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'142.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'142.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'142.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'143')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'143.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'143.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'143.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'143.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'144')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'144.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'144.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'144.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'144.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'145')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'145.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'145.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'145.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'145.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'145.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'145.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'145.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'145.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'145.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'146.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'147')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'147.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'147.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'147.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'147.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'147.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'147.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'148')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'148.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'148.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'148.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'148.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'148.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'148.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'149')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'149.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'149.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'149.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'149.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'150')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'150.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'150.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'150.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'150.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'150.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'150.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'150.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'150.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'151')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'151.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'151.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'151.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'151.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'151.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'151.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'151.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'151.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'151.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'152')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'152.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'152.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'152.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'152.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'152.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'152.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'153.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'154')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'154.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'154.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'154.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'154.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'154.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'155')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'155.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'155.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'155.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'156')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'156.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'156.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'156.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'156.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'156.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'157')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'157.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'157.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'157.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'157.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'157.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'157.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'157.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'158')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'158.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'158.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'158.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'159')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'159.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'159.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'159.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'159.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'160')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'160.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'160.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'160.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'160.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'160.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'160.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'160.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'160.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'161')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'161.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'161.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'161.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'161.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'161.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'161.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'162')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'162.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'162.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'162.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'162.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'162.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'162.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'162.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'163')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'163.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'163.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'163.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'163.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'164')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'164.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'164.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'164.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'164.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'164.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'164.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'165')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'165.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'165.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'165.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'170.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'171')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'171.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'171.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'171.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'171.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'171.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'171.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'171.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'171.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'171.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'174')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'174.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'174.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'174.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'174.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'174.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'174.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'174.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'174.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'174.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'175')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'175.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'175.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'176')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'176.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'176.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'176.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'176.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'176.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'176.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'176.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'176.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'179')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'180')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'180.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'180.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'180.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'180.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'181')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'182')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'182.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'182.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'182.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'183')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'183.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'183.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'183.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'183.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'183.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'183.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'183.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'184')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'184.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'184.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'184.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'184.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'184.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'184.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'184.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'185')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'186')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'186.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'186.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'187')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'187.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'187.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'187.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'187.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'187.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'187.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'187.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'187.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'187.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'188.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'189')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'189.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'189.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'189.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'189.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'189.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'189.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'189.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'190.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'191.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'192')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'192.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'192.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'192.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'192.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'192.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'192.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'193')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'194')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'194.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'194.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'194.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'194.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'194.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'194.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'194.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'194.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'195')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'195.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'195.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'195.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'195.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'195.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'195.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'195.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.03')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.04')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.05')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.06')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.07')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.08')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.12')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.13')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.14')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.15')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.16')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.17')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.18')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.23')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.24')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.25')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.26')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.27')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.28')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.32')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.33')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.34')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.35')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.36')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.37')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.38')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.42')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.43')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.44')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.45')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.46')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.47')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.48')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.50')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.51')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.52')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.54')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.55')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.56')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.58')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.60')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.61')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.62')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.63')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.64')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.65')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.66')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.67')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.68')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.70')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.71')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.72')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.73')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.74')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.75')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.76')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.77')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.78')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.82')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.83')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.84')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.85')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.86')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.87')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'200.88')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.03')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.04')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.05')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.06')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.07')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.08')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.12')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.13')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.14')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.15')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.16')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.17')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.18')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.23')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.24')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.25')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.26')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.27')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.28')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.42')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.43')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.44')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.45')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.46')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.47')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.48')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.50')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.51')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.52')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.53')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.54')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.55')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.56')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.57')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.58')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.60')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.61')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.62')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.63')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.64')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.65')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.66')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.67')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.68')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.70')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.71')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.72')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.73')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.74')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.75')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.76')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.77')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.78')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.92')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.93')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.94')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.95')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.96')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.97')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'201.98')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.03')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.04')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.05')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.06')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.07')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.08')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.12')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.13')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.14')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.15')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.16')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.17')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.18')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.23')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.24')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.25')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.26')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.27')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.28')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.32')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.33')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.34')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.35')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.36')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.37')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.38')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.40')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.41')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.42')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.43')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.44')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.45')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.46')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.47')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.48')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.50')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.51')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.52')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.53')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.54')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.55')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.56')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.57')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.58')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.60')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.61')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.62')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.63')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.64')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.65')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.66')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.67')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.68')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.70')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.71')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.72')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.73')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.74')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.75')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.76')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.77')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.78')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.82')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.83')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.84')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.85')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.86')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.87')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.88')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.92')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.93')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.94')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.95')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.96')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.97')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'202.98')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'203.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'204.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.02')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.22')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.30')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.31')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'205.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'206.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'207.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.00')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.01')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.10')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.11')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.80')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.90')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.91')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'208.92')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'273.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'273.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (14,'V10.46')
	insert into #Charlson_codes (diag_category_id, icd9) values (15,'456.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (15,'456.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (15,'456.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (15,'456.20')
	insert into #Charlson_codes (diag_category_id, icd9) values (15,'456.21')
	insert into #Charlson_codes (diag_category_id, icd9) values (15,'572.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (15,'572.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (15,'572.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'196')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'196.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'196.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'196.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'196.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'196.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'196.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'196.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'196.9')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'197')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'197.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'197.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'197.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'197.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'197.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'197.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'197.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'197.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'197.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.2')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.3')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.4')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.5')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.6')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.7')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.8')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.81')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.82')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'198.89')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'199')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'199.0')
	insert into #Charlson_codes (diag_category_id, icd9) values (16,'199.1')
	insert into #Charlson_codes (diag_category_id, icd9) values (17,'042')

	
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (1,'Myocardial infarction', 1)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (2,'Congestive heart failure', 1)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (3,'Peripheral vascular disease', 1)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (4,'Cerebrovascular disease', 1)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (5,'Dementia', 1)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (6,'Chronic pulmonary disease', 1)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (7,'Rheumatologic disease', 1)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (8,'Peptic ulcer disease', 1)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (9,'Mild liver disease', 1)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (10,'Diabetes (mild to moderate)', 1)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (11,'Diabetes with chronic complications', 2)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (12,'Hemoplegia or paralegia', 2)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (13,'Renal disease', 2)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (14,'Any malignancy', 2)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (15,'Moderate to severe liver disease', 3)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (16,'Metastatic solid tumor', 6)
	insert into #Charlson_scoring (diag_category_id, diag_category_name, weight) values (17,'AIDS', 6)	
	
	
	
	
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	VALUES (1100, 'Charlson Index - Romano adaptation, using conditions all time on or prior to cohort index', 1100, 0)
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		row_id,
		cohort_id,
		person_id,
		1100 AS covariate_id,
		SUM(weight) AS covariate_value
	FROM
	(
		SELECT DISTINCT cp1.row_id, cp1.cohort_id, cp1.person_id, cs1.diag_category_id, cs1.weight
		FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_era ce1
			ON cp1.person_id = ce1.person_id
		INNER JOIN 
			(
				SELECT diag_category_id, stcm1.target_concept_id
				FROM #Charlson_codes cc1
				INNER JOIN 
					@cdm_schema.dbo.source_to_concept_map stcm1
					ON cc1.icd9 = stcm1.source_code
				WHERE stcm1.source_vocabulary_id = 2
					AND stcm1.target_vocabulary_id = 1
			) c1
			ON ce1.condition_concept_id = c1.target_concept_id
		INNER JOIN
			#Charlson_scoring cs1
			ON c1.diag_category_id = cs1.diag_category_id
		WHERE ce1.condition_era_start_date <= cp1.cohort_start_date
		) t1
	GROUP BY row_id,
    cohort_id,
		person_id
	;	
	
	
	TRUNCATE TABLE #Charlson_codes;

	DROP TABLE #Charlson_codes;
	
	TRUNCATE TABLE #Charlson_scoring;

	DROP TABLE #Charlson_scoring;



	
--DCSI

	IF OBJECT_ID('DCSI_scoring', 'U') IS NOT NULL --This should only do something in Oracle
	  drop table DCSI_scoring;

	IF OBJECT_ID('tempdb..#DCSI_scoring', 'U') IS NOT NULL
	  drop table #DCSI_scoring;

	create table #DCSI_scoring
	(
		DCSI_category varchar(255),
		DCSI_ICD9_code varchar(255),
		DCSI_concept_id int,
		DCSI_score int
	);

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Retinopathy' as DCSI_category,
		source_code,
		target_concept_id,
		1 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code like '250.5%'
		or source_code in ('362.01','362.1','362.83','362.53','362.81','362.82')
	)
	order by source_code
	;

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Retinopathy' as DCSI_category,
		source_code,
		target_concept_id,
		2 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code like '361%'
		or source_code like '369%'
		or source_code in ('362.02','379.23')
	)
	order by source_code
	;
	
	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Nephropathy' as DCSI_category,
		source_code,
		target_concept_id,
		1 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code in ('250.4','580','581','581.81','582','583')
	or source_code like '580%'
	or source_code like '581%'
	or source_code like '582%'
	or source_code like '583%'
	)
	order by source_code
	;

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Nephropathy' as DCSI_category,
		source_code,
		target_concept_id,
		2 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code in ('585','586','593.9')
	or source_code like '585%'
	or source_code like '586%'
	or source_code like '593.9%'
	)
	order by source_code
	;

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Neuropathy' as DCSI_category,
		source_code,
		target_concept_id,
		1 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code in ('356.9','250.6','358.1','951.0','951.1','951.3','713.5','357.2','596.54','337.0','337.1','564.5','536.3','458.0')
		or (source_code >= '354.0' and source_code <= '355.99') 
		or source_code like '356.9%'
		or source_code like '250.6%'
		or source_code like '358.1%'
		or source_code like '951.0%'
		or source_code like '951.1%'
		or source_code like '951.3%'
		or source_code like '713.5%'
		or source_code like '357.2%'
		or source_code like '337.0%'
		or source_code like '337.1%'
		or source_code like '564.5%'
		or source_code like '536.3%'
		or source_code like '458.0%'
	)
	order by source_code
	;

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Cerebrovascular' as DCSI_category,
		source_code,
		target_concept_id,
		1 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code like '435%'
	)
	order by source_code
	;

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Cerebrovascular' as DCSI_category,
		source_code,
		target_concept_id,
		2 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code in ('431','433','434','436')
	or source_code like '431%'
	or source_code like '433%'
	or source_code like '434%'
	or source_code like '436%'
	)
	order by source_code
	;

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Cardiovascular' as DCSI_category,
		source_code,
		target_concept_id,
		1 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code like '440%'
	or source_code like '411%'
	or source_code like '413%'
	or source_code like '414%'
	or source_code like '429.2%'
	)
	order by source_code
	;

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Cardiovascular' as DCSI_category,
		source_code,
		target_concept_id,
		2 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code like '410%'
	or source_code like '427.1%'
	or source_code like '427.3%'
	or source_code like '427.4%'
	or source_code like '427.5%'
	or source_code like '412%'
	or source_code like '428%'
	or source_code like '441%'
	or source_code in ('440.23','440.24')
	)
	order by source_code
	;

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Peripheral vascular disease' as DCSI_category,
		source_code,
		target_concept_id,
		1 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code like '250.7%'
	or source_code like '442.3%'
	or source_code like '892.1%'
	or source_code like '443.9%'
	or source_code in ('443.81')
	)
	order by source_code
	;

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Peripheral vascular disease' as DCSI_category,
		source_code,
		target_concept_id,
		2 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code like '785.4%'
	or source_code like '707.1%'
	or source_code like '040.0%'
	or source_code in ('444.22')
	)
	order by source_code
	;

	insert into #DCSI_scoring (DCSI_category, DCSI_ICD9_code, DCSI_concept_id, DCSI_score)
	select 'Metabolic' as DCSI_category,
		source_code,
		target_concept_id,
		2 as DCSI_score
	from @cdm_schema.dbo.SOURCE_TO_CONCEPT_MAP
	where SOURCE_VOCABULARY_ID = 2
	and target_vocabulary_id = 1
	and (source_code like '250.1%'
	or source_code like '250.2%'
	or source_code like '250.3%'
	)
	order by source_code
	;



	
	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	VALUES (1101, 'Diabetes Comorbidity Severity Index (DCSI), using conditions all time on or prior to cohort index', 1101, 0)
	;
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT 
		row_id,
		cohort_id,
		person_id,
		1101 AS covariate_id,
		SUM(max_score) AS covariate_value
	FROM
	(
		SELECT cp1.row_id, cp1.cohort_id, cp1.person_id, ds1.dcsi_category, max(ds1.DCSI_score) as max_score
		FROM #cohort_person cp1
		INNER JOIN @cdm_schema.dbo.condition_era ce1
			ON cp1.person_id = ce1.person_id
		INNER JOIN #DCSI_scoring ds1
			ON ce1.condition_concept_id = ds1.DCSI_concept_id
		WHERE ce1.condition_era_start_date <= cp1.cohort_start_date
		GROUP BY cp1.row_id,
      cp1.cohort_id,
			cp1.person_id,
			ds1.dcsi_category
		) t1
	GROUP BY row_id,
    cohort_id,
		person_id
	;	
	

	
	
	TRUNCATE TABLE #DCSI_scoring;

	DROP TABLE #DCSI_scoring;



/*************

other risk scores to consider adding:

CHADS2 for stroke
HAS_BLED

**************/

	
	
}


/**************************
***************************
INTERACTION YEAR
***************************
**************************/	
{@use_covariate_interaction_year} ? {

	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(ccr1.covariate_id AS BIGINT)*10000 + YEAR(cp1.cohort_start_date) AS covariate_id,
		ccr1.covariate_name + ' * interaction term with index year: ' + CAST(YEAR(cp1.cohort_start_date) AS VARCHAR) AS covariate_name,
		ccr1.analysis_id,
		ccr1.concept_id
	FROM #cohort_person cp1
		INNER JOIN
		#cohort_covariate cc1
			 ON cp1.person_id = cc1.person_id
			 AND cp1.cohort_id = cc1.cohort_id
		INNER JOIN
		#cohort_covariate_ref ccr1
			ON cc1.covariate_id = ccr1.covariate_id
		WHERE ccr1.analysis_id not in (5)
    and ccr1.covariate_id > 1
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cc1.row_id,
		cc1.cohort_id,
		cc1.person_id,
		CAST(cc1.covariate_id AS BIGINT)*10000 + CAST(YEAR(cp1.cohort_start_date) AS BIGINT) AS covariate_id,
		cc1.covariate_value AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN
		#cohort_covariate cc1
			 ON cp1.person_id = cc1.person_id
			 AND cp1.cohort_id = cc1.cohort_id
		INNER JOIN
		#cohort_covariate_ref ccr1
			ON cc1.covariate_id = ccr1.covariate_id
		WHERE ccr1.analysis_id not in (5)
    and ccr1.covariate_id > 1
	;
	
}


/**************************
***************************
INTERACTION MONTH
***************************
**************************/	
{@use_covariate_interaction_month} ? {

	INSERT INTO #cohort_covariate_ref (covariate_id, covariate_name, analysis_id, concept_id)
	SELECT DISTINCT CAST(ccr1.covariate_id AS BIGINT)*10000 + CAST(MONTH(cp1.cohort_start_date) AS BIGINT) AS covariate_id,
		ccr1.covariate_name + ' * interaction term with index month: ' + CAST(MONTH(cp1.cohort_start_date) AS VARCHAR) AS covariate_name,
		ccr1.analysis_id,
		ccr1.concept_id
	FROM #cohort_person cp1
		INNER JOIN
		#cohort_covariate cc1
			 ON cp1.person_id = cc1.person_id
			 AND cp1.cohort_id = cc1.cohort_id
		INNER JOIN
		#cohort_covariate_ref ccr1
			ON cc1.covariate_id = ccr1.covariate_id
		WHERE ccr1.analysis_id not in (6)
    and ccr1.covariate_id > 1
	;
	
	
	INSERT INTO #cohort_covariate (row_id, cohort_id, person_id, covariate_id, covariate_value)
	SELECT DISTINCT
		cc1.row_id,
		cc1.cohort_id,
		cc1.person_id,
		CAST(cc1.covariate_id AS BIGINT)*10000 + CAST(MONTH(cp1.cohort_start_date) AS BIGINT) AS covariate_id,
		cc1.covariate_value AS covariate_value
	FROM #cohort_person cp1
		INNER JOIN
		#cohort_covariate cc1
			 ON cp1.person_id = cc1.person_id
			 AND cp1.cohort_id = cc1.cohort_id
		INNER JOIN
		#cohort_covariate_ref ccr1
			ON cc1.covariate_id = ccr1.covariate_id
		WHERE ccr1.analysis_id not in (6)
    and ccr1.covariate_id > 1
	;
	
}


	

{@delete_covariates_small_count > 0} ? {	

DELETE FROM #cohort_covariate_ref 
WHERE covariate_id IN (
  SELECT covariate_id
	FROM #cohort_covariate
	GROUP BY covariate_id
  HAVING COUNT(person_id) < @delete_covariates_small_count
	);

DELETE FROM #cohort_covariate 
WHERE covariate_id IN (
  SELECT covariate_id
  FROM #cohort_covariate
	GROUP BY covariate_id
  HAVING COUNT(person_id) < @delete_covariates_small_count
	);

	

}	
	
	


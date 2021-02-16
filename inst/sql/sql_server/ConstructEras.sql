/************************************************************************
@file ConstructEras.sql

Copyright 2021 Observational Health Data Sciences and Informatics

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

{DEFAULT @grace_period = 30}
{DEFAULT @source_database_schema = 'cdm4_sim.dbo'}
{DEFAULT @source_table = 'drug_exposure'}
{DEFAULT @source_person_id = 'person_id'}
{DEFAULT @source_start_date = 'drug_exposure_start_date'}
{DEFAULT @source_end_date = 'drug_exposure_end_date'}
{DEFAULT @source_concept_id = 'drug_concept_id'}
{DEFAULT @source_type_concept_id = 'drug_type_concept_id'} /* If empty, type will be ignored */
{DEFAULT @target_database_schema = 'cdm4_sim.dbo'}
{DEFAULT @target_table = 'drug_era'}
{DEFAULT @target_id = 'drug_era_id'}
{DEFAULT @target_person_id = 'person_id'}
{DEFAULT @target_start_date = 'drug_era_start_date'}
{DEFAULT @target_end_date = 'drug_era_end_date'}
{DEFAULT @target_concept_id = 'drug_concept_id'}
{DEFAULT @target_type_concept_id = 'drug_type_concept_id'} /* If empty, type will be ignored */
{DEFAULT @target_count = 'drug_exposure_count'}
{DEFAULT @create_target_table = FALSE}
{DEFAULT @cdm_database_schema = 'cdm4_sim.dbo'} /* Holds the vocabulary for rolling up concepts */
{DEFAULT @roll_up_concept_class_id = 'Ingredient'} /* If empty, will not roll up */
{DEFAULT @roll_up_vocabulary_id = 'RxNorm'}
{DEFAULT @cdm_version = 5}

WITH cte_source_periods (
	row_num,
	person_id,
{@source_type_concept_id != '' & @target_type_concept_id != ''} ? {	type_concept_id,}
	start_date,
	end_date,
	concept_id
	)
AS (
	-- normalize drug_exposure_end_date to either the existing drug exposure end date, or add days supply, or add 1 day to the start date
	SELECT ROW_NUMBER() OVER (ORDER BY @source_person_id) AS row_num,
		@source_person_id AS person_id,
{@source_type_concept_id != '' & @target_type_concept_id != ''} ? {		@source_type_concept_id AS type_concept_id,}
		@source_start_date AS start_date,
{@source_table == 'drug_exposure'} ? {
		COALESCE(drug_exposure_end_date, DATEADD(DAY, days_supply, drug_exposure_start_date), drug_exposure_start_date) AS end_date,
} : {
		COALESCE(@source_end_date, @source_start_date) AS end_date,
}
{@roll_up_concept_class_id == ''} ? {		@source_concept_id AS concept_id} : {		ancestor.concept_id}
	FROM @source_database_schema.@source_table source_table
{@roll_up_concept_class_id != ''} ? {
	INNER JOIN @cdm_database_schema.concept_ancestor
		ON descendant_concept_id = source_table.@source_concept_id
	INNER JOIN @cdm_database_schema.concept ancestor
		ON ancestor_concept_id = ancestor.concept_id
{@cdm_version == 4} ? {
	WHERE ancestor.vocabulary_id = @roll_up_vocabulary_id
		AND ancestor.concept_class = '@roll_up_concept_class_id'
} : {
	WHERE ancestor.vocabulary_id = '@roll_up_vocabulary_id'
		AND ancestor.concept_class_id  = '@roll_up_concept_class_id'
}		
}
	),
cte_end_dates (
	person_id,
	concept_id,
	end_date
	)
AS -- the magic
	(
	SELECT person_id,
		concept_id,
		DATEADD(DAY, - @grace_period, event_date) AS end_date -- unpad the end date
	FROM (
		SELECT person_id,
			concept_id,
			event_date,
			event_type,
			MAX(start_ordinal) OVER (
				PARTITION BY person_id,
				concept_id ORDER BY event_date,
					event_type rows unbounded preceding
				) AS start_ordinal, -- this pulls the current start down from the prior rows so that the nulls from the end dates will contain a value we can compare with 
			ROW_NUMBER() OVER (
				PARTITION BY person_id,
				concept_id ORDER BY event_date,
					event_type
				) AS overall_ord -- this re-numbers the inner union so all rows are numbered ordered by the event date
		FROM (
			-- select the start dates, assigning a row number to each
			SELECT person_id,
				concept_id,
				start_date AS event_date,
				- 1 AS event_type,
				ROW_NUMBER() OVER (
					PARTITION BY person_id,
					concept_id ORDER BY start_date
					) AS start_ordinal
			FROM cte_source_periods
			
			UNION ALL
			
			-- pad the end dates by 30 to allow a grace period for overlapping ranges.
			SELECT person_id,
				concept_id,
				DATEADD(DAY, @grace_period, end_date),
				1 AS event_type,
				NULL
			FROM cte_source_periods
			) rawdata
		) e
	WHERE (2 * e.start_ordinal) - e.overall_ord = 0
	),
cte_ends (
	person_id,
	concept_id,
{@source_type_concept_id != '' & @target_type_concept_id != ''} ? {	type_concept_id,}
	start_date,
	end_date
	)
AS (
	SELECT d.person_id,
		d.concept_id,
{@source_type_concept_id != '' & @target_type_concept_id != ''} ? {		d.type_concept_id,}
		d.start_date,
		MIN(e.end_date) AS era_end_date
	FROM cte_source_periods d
	INNER JOIN cte_end_dates e
		ON d.person_id = e.person_id
			AND d.concept_id = e.concept_id
			AND e.end_date >= d.start_date
	GROUP BY d.row_num,
		d.person_id,
		d.concept_id,
{@source_type_concept_id != '' & @target_type_concept_id != ''} ? {		d.type_concept_id,}
		d.start_date
	)
{!@create_target_table} ? {
INSERT INTO @target_database_schema.@target_table (
{@target_id != ''} ? {@target_id,}
	@target_person_id, 
	@target_concept_id, 
{@source_type_concept_id != '' & @target_type_concept_id != ''} ? {@target_type_concept_id,}
	@target_start_date, 
{@target_count != ''} ? {@target_count,}	
	@target_end_date)
}
SELECT {@target_id != ''} ? {ROW_NUMBER() OVER (ORDER BY person_id) AS @target_id,}
    person_id AS @target_person_id,
	concept_id AS @target_concept_id,
{@source_type_concept_id != '' & @target_type_concept_id != ''} ? {	type_concept_id AS @target_type_concept_id,}
	MIN(start_date) AS @target_start_date,
{@target_count != ''} ? {	COUNT(*) AS @target_count,}	
	end_date AS @target_end_date
{@create_target_table} ? {INTO @target_database_schema.@target_table}	
FROM cte_ends
GROUP BY person_id,
	concept_id,
{@source_type_concept_id != '' & @target_type_concept_id != ''} ? {	type_concept_id,}
	end_date
ORDER BY person_id,
	concept_id;

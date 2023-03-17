/************************************************************************
@file SampleCohorts.sql

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

{DEFAULT @max_cohort_size = 2500000}

DROP TABLE IF EXISTS #cohort_sample;

SELECT row_id,
	subject_id,
	cohort_start_date,
	days_from_obs_start,
	days_to_cohort_end,
	days_to_obs_end,
	cohort_definition_id
INTO #cohort_sample
FROM (
	SELECT row_id,
		subject_id,
		cohort_start_date,
		days_from_obs_start,
		days_to_cohort_end,
		days_to_obs_end,
		cohort_definition_id,
		ROW_NUMBER() OVER ( PARTITION BY cohort_definition_id ORDER BY NEWID()) AS rn
	FROM #cohort_person
) random_order	
WHERE rn <= @max_cohort_size;

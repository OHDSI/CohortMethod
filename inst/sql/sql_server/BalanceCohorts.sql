/* This should only do something in Oracle. */
IF OBJECT_ID('new_cohorts', 'U') IS NOT NULL
  DROP TABLE new_cohorts;

IF OBJECT_ID('tempdb..#new_cohorts', 'U') IS NOT NULL
  DROP TABLE #new_cohorts;


CREATE TEMP TABLE #new_cohorts
AS
SELECT *
FROM #cohort_person
WHERE cohort_id = 0
ORDER BY RANDOM()
LIMIT @limit
;


INSERT INTO #new_cohorts (
    row_id,
    cohort_id,
    person_id,
    cohort_start_date,
    cohort_end_date,
    observation_period_end_date
    )
SELECT *
FROM #cohort_person
WHERE cohort_id = 1
ORDER BY RANDOM()
LIMIT @limit
;


TRUNCATE TABLE #cohort_person;
DROP TABLE #cohort_person;


ALTER TABLE #new_cohorts RENAME TO #cohort_person;

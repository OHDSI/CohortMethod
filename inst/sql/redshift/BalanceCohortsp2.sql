CREATE TEMP TABLE new_cohorts
AS
SELECT *
FROM cohorts
WHERE cohort_id = 0
ORDER BY RANDOM()
LIMIT %s
;


INSERT INTO new_cohorts (
    cohort_id,
    person_id,
    cohort_start_date,
    cohort_censor_date
    )
SELECT *
FROM cohorts
WHERE cohort_id = 1
ORDER BY RANDOM()
LIMIT %s
;


TRUNCATE TABLE cohorts;
DROP TABLE cohorts;


ALTER TABLE new_cohorts RENAME TO cohorts;

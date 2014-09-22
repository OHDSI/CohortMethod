CREATE TEMP TABLE raw_cohorts
AS
SELECT
    ( CASE
        WHEN ca.ancestor_concept_id = %s
            THEN 1
        WHEN ca.ancestor_concept_id = %s
            THEN 0
        ELSE -1
    END ) AS cohort_id,
    de.person_id,
    min(de.drug_era_start_date) AS cohort_start_date,
    min(de.drug_era_end_date +  7) AS cohort_end_date
FROM
    drug_era de INNER JOIN vocabulary.concept_ancestor ca
        ON de.drug_concept_id = ca.descendant_concept_id
WHERE
    ca.ancestor_concept_id = %s
    OR ca.ancestor_concept_id = %s
GROUP BY
    ca.ancestor_concept_id,
    de.person_id
;


/* Finds all persons which are in both cohorts.  */
CREATE TEMP TABLE both_cohorts
AS
SELECT
    person_id
FROM (
    SELECT
        person_id,
        count(cohort_id) AS num_cohorts
    FROM raw_cohorts
    GROUP BY person_id )
WHERE num_cohorts = 2
;


CREATE TEMP TABLE cohorts
AS
SELECT
    rc.cohort_id,
    rc.person_id,
    rc.cohort_start_date,
    rc.cohort_end_date as cohort_censor_date
FROM
    raw_cohorts rc LEFT JOIN both_cohorts
        ON rc.person_id = both_cohorts.person_id
WHERE both_cohorts.person_id IS NULL
;


TRUNCATE TABlE raw_cohorts;
DROP TABLE raw_cohorts;

TRUNCATE TABLE both_cohorts;
DROP TABLE both_cohorts;

CREATE TEMP TABLE outcomes
AS
SELECT
    c1.person_id,
    (co1.condition_start_date - cohort_start_date) AS time_to_outcome,
    ca1.ancestor_concept_id AS outcome_concept_id
FROM
    cohorts c1 INNER JOIN condition_occurrence co1
        ON c1.person_id = co1.person_id
    INNER JOIN (
        SELECT
            descendant_concept_id,
            ancestor_concept_id
        FROM vocabulary.concept_ancestor
        WHERE ancestor_concept_id IN (194133)
    ) ca1
        ON co1.condition_concept_id = descendant_concept_id
WHERE
    co1.condition_type_concept_id IN (
        38000215,
        38000216,
        38000217,
        38000218,
        38000183,
        38000232
    )
    AND co1.condition_start_date > c1.cohort_start_date
    AND co1.condition_start_Date <= c1.cohort_censor_date
GROUP BY
    c1.person_id,
    (co1.condition_start_date - cohort_start_date),
    ca1.ancestor_concept_id
;

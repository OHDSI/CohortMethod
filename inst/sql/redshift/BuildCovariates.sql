CREATE TEMP TABLE covariates (
    person_id NUMERIC,
    covariate_id NUMERIC,
    covariate_value DECIMAL
    )
;

CREATE TEMP TABLE concept_counts (
    concept_id INT,
    num_records INT
    )
;

/* Mapping between different vocabularies. */
CREATE TEMP TABLE snomed_to_all_meddra
AS
SELECT DISTINCT
    c1.concept_id AS snomed_concept_id,
    c2.concept_id AS meddra_concept_id
FROM
    vocabulary.concept c1 INNER JOIN vocabulary.concept_ancestor ca1
        ON c1.concept_id = ca1.descendant_concept_id
        AND c1.vocabulary_id = 1
    INNER JOIN vocabulary.concept c2
        ON ca1.ancestor_concept_id = c2.concept_id
        AND c2.vocabulary_id = 15
;

/* Mapping between different vocabularies. */
CREATE TEMP TABLE rxnorm_to_atc
AS
SELECT DISTINCT
    c1.concept_id AS rxnorm_concept_id,
    c2.concept_id AS atc_concept_id
FROM
    vocabulary.concept c1 INNER JOIN vocabulary.concept_ancestor ca1
        ON c1.concept_id = ca1.descendant_concept_id
        AND c1.vocabulary_id = 8
        AND c1.concept_class = 'Ingredient'
    INNER JOIN vocabulary.concept c2
        ON ca1.ancestor_concept_id = c2.concept_id
        AND c2.vocabulary_id = 21
        AND CHAR_LENGTH(c2.concept_code) IN (
            1,
            3,
            5
        )
;

/* Insert the gender into the covariates. */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT 
    ca1.person_id,
    gender_concept_id AS covariate_id,
    1 AS covariate_value
FROM cohorts ca1 INNER JOIN person p1
    ON ca1.person_id = p1.person_id
WHERE gender_concept_id = 8507 /* Male */
;


/* Insert the ages of the patients in 10 year increments.
 */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT 
    ca1.person_id,
    floor((EXTRACT(YEAR FROM ca1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 10) + 1 AS covariate_id,
    1 AS covariate_value
FROM
    cohorts ca1 INNER JOIN person p1
        ON ca1.person_id = p1.person_id
;

/* Insert number of distinct conditions in the last 6 months. There seems to be
 * a pretty long tail in the number of distinct conditions.
 */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT 
    ca1.person_id,
    20 AS covariate_id,
    count(DISTINCT ce1.condition_concept_id) AS covariate_value
FROM
    cohorts ca1 INNER JOIN condition_era ce1
        ON ca1.person_id = ce1.person_id
WHERE
    ce1.condition_era_start_date <= ca1.cohort_start_date
    AND ce1.condition_era_start_date >= ( ca1.cohort_start_date +  - 365)
GROUP BY ca1.person_id
;

/* Number of distinct drugs in last 6mo */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT 
    ca1.person_id,
    21 AS covariate_id,
    count(DISTINCT de1.drug_concept_id) AS covariate_value
FROM
    cohorts ca1 INNER JOIN drug_era de1
        ON ca1.person_id = de1.person_id
WHERE
    de1.drug_era_start_date <= ca1.cohort_start_date
    AND de1.drug_era_start_date >= ( ca1.cohort_start_date +  - 365)
GROUP BY ca1.person_id
;

/* Number of distinct procedures in last 6mo */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT 
    ca1.person_id,
    22 AS covariate_id,
    count(DISTINCT po1.procedure_concept_id) AS covariate_value
FROM
    cohorts ca1 INNER JOIN procedure_occurrence po1
        ON ca1.person_id = po1.person_id
WHERE
    po1.procedure_date <= ca1.cohort_start_date
    AND po1.procedure_date >= ( ca1.cohort_start_date +  - 365)
GROUP BY ca1.person_id
;


/* Number of distinct outpatient visits in last 6mo */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT
    ca1.person_id,
    23 AS covariate_id,
    count(vo1.VISIT_OCCURRENCE_ID) AS covariate_value
FROM
    cohorts ca1 INNER JOIN visit_occurrence vo1
        ON ca1.person_id = vo1.person_id
WHERE
    vo1.visit_start_date <= ca1.cohort_start_date
    AND vo1.visit_start_date >= ( ca1.cohort_start_date +  - 365)
    AND vo1.place_of_service_concept_id = 9202
GROUP BY ca1.person_id
;

/* Number of distinct inpatient visits in last 6mo */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT
    ca1.person_id,
    24 AS covariate_id,
    count(vo1.VISIT_OCCURRENCE_ID) AS covariate_value
FROM
    cohorts ca1 INNER JOIN visit_occurrence vo1
        ON ca1.person_id = vo1.person_id
WHERE
    vo1.visit_start_date <= ca1.cohort_start_date
    AND vo1.visit_start_date >= ( ca1.cohort_start_date +  - 365)
    AND vo1.place_of_service_concept_id = 9201
GROUP BY ca1.person_id
;

/* Number of distinct ER visits in last 6mo */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT
    ca1.person_id,
    25 AS covariate_id,
    count(vo1.VISIT_OCCURRENCE_ID) AS covariate_value
FROM
    cohorts ca1 INNER JOIN visit_occurrence vo1
        ON ca1.person_id = vo1.person_id
WHERE
    vo1.visit_start_date <= ca1.cohort_start_date
    AND vo1.visit_start_date >= ( ca1.cohort_start_date +  - 365)
    AND vo1.place_of_service_concept_id = 9203
GROUP BY ca1.person_id
;

/* Add covariate per SNOMED condition. */
DELETE
FROM concept_counts;

INSERT INTO concept_counts (
    concept_id,
    num_records
    )
SELECT
    ce1.condition_concept_id,
    count(ce1.person_id) AS num_records
FROM
    cohorts ca1 INNER JOIN condition_era ce1
        ON ca1.person_id = ce1.person_id
WHERE
    ce1.condition_era_start_date <= ca1.cohort_start_date
    AND ce1.condition_era_start_date >= ( ca1.cohort_start_date +  - 365)
    AND ce1.condition_concept_id > 0
GROUP BY ce1.condition_concept_id
;

/* This adds about 1000 new covariates. */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT
    ca1.person_id,
    ce1.condition_concept_id AS covariate_id,
    1 AS covariate_value
FROM
    cohorts ca1 INNER JOIN condition_era ce1
        ON ca1.person_id = ce1.person_id
WHERE
    ce1.condition_era_start_date <= ca1.cohort_start_date
    AND ce1.condition_era_start_date >= ( ca1.cohort_start_date +  - 365)
    AND ce1.condition_concept_id > 0
    AND ce1.condition_concept_id IN (
        SELECT concept_id
        FROM concept_counts
        WHERE num_records > 100
        )
GROUP BY 
    ca1.person_id,
    ce1.condition_concept_id
;

/* Add covariate per MEDDRA concept SNOMED condition. */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT 
    ca1.person_id,
    stam1.meddra_concept_id AS covariate_id,
    1 AS covariate_value
FROM
    covariates ca1 INNER JOIN snomed_to_all_meddra stam1
        ON ca1.covariate_id = stam1.snomed_concept_id
GROUP BY 
    ca1.person_id,
    stam1.meddra_concept_id
;

/* Add covariate per RxNorm ingredient. */
DELETE
FROM concept_counts;

INSERT INTO concept_counts (
    concept_id,
    num_records
    )
SELECT
    de1.drug_concept_id,
    count(de1.person_id) AS num_records
FROM
    cohorts ca1 INNER JOIN drug_era de1
        ON ca1.person_id = de1.person_id
WHERE
    de1.drug_era_start_date <= ca1.cohort_start_date
    AND de1.drug_era_start_date >= ( ca1.cohort_start_date +  - 365)
    AND de1.drug_concept_id > 0
    AND de1.drug_concept_id NOT IN (755695,739138)
GROUP BY de1.drug_concept_id
;


INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT
    ca1.person_id,
    de1.drug_concept_id AS covariate_id,
    1 AS covariate_value
FROM
    cohorts ca1 INNER JOIN drug_era de1
        ON ca1.person_id = de1.person_id
WHERE
    de1.drug_era_start_date < ca1.cohort_start_date
    AND de1.drug_era_start_date >= ( ca1.cohort_start_date +  - 365)
    AND de1.drug_concept_id > 0
    AND de1.drug_concept_id IN (
        SELECT concept_id
        FROM concept_counts
        WHERE num_records > 100
        )
GROUP BY 
    ca1.person_id,
    de1.drug_concept_id
;

/* Add covariate per ATC class concept within RxNorm drug. */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT 
    ca1.person_id,
    rta1.atc_concept_id AS covariate_id,
    1 AS covariate_value
FROM
    covariates ca1 INNER JOIN rxnorm_to_atc rta1
        ON ca1.covariate_id = rta1.rxnorm_concept_id
GROUP BY 
    ca1.person_id,
    rta1.atc_concept_id
;

/* Number of drugs within each ATC3 groupings */
INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT 
    ca1.person_id,
    2000000000 + t1.atc_concept_id,
    count(DISTINCT ca1.covariate_id)
FROM
    covariates ca1 INNER JOIN (
        SELECT
            c1.concept_id AS rxnorm_concept_id,
            c2.concept_id AS atc_concept_id
        FROM
            vocabulary.concept c1 INNER JOIN vocabulary.concept_ancestor ca1
                ON c1.concept_id = ca1.descendant_concept_id
                AND c1.vocabulary_id = 8
                AND c1.concept_level = 2
            INNER JOIN vocabulary.concept c2
                ON c2.concept_id = ca1.ancestor_concept_id
                AND c2.vocabulary_id = 21
                AND CHAR_LENGTH(c2.concept_code) IN (3)
    ) t1
        ON ca1.covariate_id = t1.rxnorm_concept_id
GROUP BY 
    ca1.person_id,
    2000000000 + t1.atc_concept_id
;

/* Add covariate per procedure. */
DELETE
FROM concept_counts;

INSERT INTO concept_counts (
    concept_id,
    num_records
    )
SELECT
    po1.procedure_concept_id,
    count(po1.person_id) AS num_records
FROM
    cohorts ca1 INNER JOIN procedure_occurrence po1
        ON ca1.person_id = po1.person_id
    INNER JOIN vocabulary.concept c1
        ON po1.procedure_concept_id = c1.concept_id
        AND c1.vocabulary_id IN (
            3,
            4
        )
WHERE
    po1.procedure_date <= ca1.cohort_start_date
    AND po1.procedure_date >= ( ca1.cohort_start_date +  - 365)
    AND po1.procedure_concept_id > 0
GROUP BY po1.procedure_concept_id
;

INSERT INTO covariates (
    person_id,
    covariate_id,
    covariate_value
    )
SELECT 
    ca1.person_id,
    po1.procedure_concept_id AS covariate_id,
    1 AS covariate_value
FROM
    cohorts ca1 INNER JOIN procedure_occurrence po1
        ON ca1.person_id = po1.person_id
WHERE
    po1.procedure_date < ca1.cohort_start_date
    AND po1.procedure_date >= ( ca1.cohort_start_date +  - 365)
    AND po1.procedure_concept_id > 0
    AND po1.procedure_concept_id IN (
        SELECT concept_id
        FROM concept_counts
        WHERE num_records > 100
    )
GROUP BY 
    ca1.person_id,
    po1.procedure_concept_id
;

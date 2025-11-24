-- Database migrations for version 6.0.0
-- This migration updates the schema:
-- 1. Adds the gradient field to the cm_likelihood_profile table.
-- 2. Adds the sdm_family_wise_min_p field to the cm_diagnostics_summary table.
-- 3. Adds the shared_sdm_family_wise_min_p field to the cm_diagnostics_summary table.
-- 4. Add the cm_target_comparator table.
-- 5. Adds the target_comparator_id field, changing the primary key to use this field instead of target_id and comparator_id, for these tables:
--   - cm_attrition
--   - cm_follow_up_dist
--   - cm_result
--   - cm_interaction_result
--   - cm_covariate_balance
--   - cm_diagnostics_summary
--   - cm_target_comparator_outcome
--   - cm_target_comparator_outcome
--   - cm_kaplan_meier_dist
--   - cm_likelihood_profile
--   - cm_preference_score_dist
--   - cm_propensity_model
--   - cm_shared_covariate_balance

ALTER TABLE @database_schema.@table_prefixcm_likelihood_profile ADD gradient FLOAT;
ALTER TABLE @database_schema.@table_prefixcm_diagnostics_summary ADD sdm_family_wise_min_p FLOAT;
ALTER TABLE @database_schema.@table_prefixcm_diagnostics_summary ADD shared_sdm_family_wise_min_p FLOAT;

CREATE TABLE @database_schema.@table_prefixcm_target_comparator (
  target_comparator_id BIGINT NOT NULL,
  target_id BIGINT NOT NULL,
  comparator_id BIGINT NOT NULL,
  nesting_cohort_id BIGINT,
  PRIMARY KEY(target_comparator_id)
);

INSERT INTO @database_schema.@table_prefixcm_target_comparator (
  target_comparator_id,
  target_id,
  comparator_id
)
SELECT target_id * 1000000 + comparator_id AS target_comparator_id,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_result
GROUP BY target_id,
  comparator_id;

-------------------------------------------------------------------
-- 1. cm_attrition
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_attrition_new (
  sequence_number INT NOT NULL,
  description VARCHAR,
  subjects INT,
  exposure_id BIGINT NOT NULL,
  target_comparator_id BIGINT NOT NULL,
  analysis_id INT NOT NULL,
  outcome_id BIGINT NOT NULL,
  database_id VARCHAR NOT NULL,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(sequence_number,exposure_id,target_comparator_id,analysis_id,outcome_id,database_id)
);


INSERT INTO @database_schema.@table_prefixcm_attrition_new (sequence_number,
  description,
  subjects,
  exposure_id,
  target_comparator_id,
  analysis_id,
  outcome_id,
  database_id ,
  target_id,
  comparator_id)
SELECT sequence_number,
  description,
  subjects,
  exposure_id,
  target_id * 1000000 + comparator_id AS target_comparator_id,
  analysis_id,
  outcome_id,
  database_id ,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_attrition;

DROP TABLE @database_schema.@table_prefixcm_attrition;

ALTER TABLE @database_schema.@table_prefixcm_attrition_new RENAME TO @table_prefixcm_attrition;


-------------------------------------------------------------------
-- 2. cm_follow_up_dist
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_follow_up_dist_new (
  target_comparator_id BIGINT NOT NULL,
  outcome_id BIGINT NOT NULL,
  analysis_id INT NOT NULL,
  target_min_days FLOAT,
  target_p_10_days FLOAT,
  target_p_25_days FLOAT,
  target_median_days FLOAT,
  target_p_75_days FLOAT,
  target_p_90_days FLOAT,
  target_max_days FLOAT,
  comparator_min_days FLOAT,
  comparator_p_10_days FLOAT,
  comparator_p_25_days FLOAT,
  comparator_median_days FLOAT,
  comparator_p_75_days FLOAT,
  comparator_p_90_days FLOAT,
  comparator_max_days FLOAT,
  target_min_date DATE,
  target_max_date DATE,
  comparator_min_date DATE,
  comparator_max_date DATE,
  database_id VARCHAR NOT NULL,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(target_comparator_id,outcome_id,analysis_id,database_id)
);

INSERT INTO @database_schema.@table_prefixcm_follow_up_dist_new (
  target_comparator_id,
  outcome_id,
  analysis_id,
  target_min_days,
  target_p_10_days,
  target_p_25_days,
  target_median_days,
  target_p_75_days,
  target_p_90_days,
  target_max_days,
  comparator_min_days,
  comparator_p_10_days,
  comparator_p_25_days,
  comparator_median_days,
  comparator_p_75_days,
  comparator_p_90_days,
  comparator_max_days,
  target_min_date,
  target_max_date,
  comparator_min_date,
  comparator_max_date,
  database_id,
  target_id,
  comparator_id)
SELECT target_id * 1000000 + comparator_id AS target_comparator_id,
  outcome_id,
  analysis_id,
  target_min_days,
  target_p_10_days,
  target_p_25_days,
  target_median_days,
  target_p_75_days,
  target_p_90_days,
  target_max_days,
  comparator_min_days,
  comparator_p_10_days,
  comparator_p_25_days,
  comparator_median_days,
  comparator_p_75_days,
  comparator_p_90_days,
  comparator_max_days,
  target_min_date,
  target_max_date,
  comparator_min_date,
  comparator_max_date,
  database_id,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_follow_up_dist;

DROP TABLE @database_schema.@table_prefixcm_follow_up_dist;

ALTER TABLE @database_schema.@table_prefixcm_follow_up_dist_new RENAME TO @table_prefixcm_follow_up_dist;


-------------------------------------------------------------------
-- 3. cm_result
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_result_new (
  analysis_id INT NOT NULL,
  target_comparator_id BIGINT NOT NULL,
  outcome_id BIGINT NOT NULL,
  rr FLOAT,
  ci_95_lb FLOAT,
  ci_95_ub FLOAT,
  p FLOAT,
  one_sided_p FLOAT,
  target_subjects INT,
  comparator_subjects INT,
  target_days INT,
  comparator_days INT,
  target_outcomes INT,
  comparator_outcomes INT,
  log_rr FLOAT,
  se_log_rr FLOAT,
  llr FLOAT,
  calibrated_rr FLOAT,
  calibrated_ci_95_lb FLOAT,
  calibrated_ci_95_ub FLOAT,
  calibrated_p FLOAT,
  calibrated_one_sided_p FLOAT,
  calibrated_log_rr FLOAT,
  calibrated_se_log_rr FLOAT,
  target_estimator VARCHAR,
  database_id VARCHAR NOT NULL,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(analysis_id,target_comparator_id,outcome_id,database_id)
);

INSERT INTO @database_schema.@table_prefixcm_result_new (
  analysis_id,
  target_comparator_id,
  outcome_id,
  rr,
  ci_95_lb,
  ci_95_ub,
  p,
  one_sided_p,
  target_subjects,
  comparator_subjects,
  target_days,
  comparator_days,
  target_outcomes,
  comparator_outcomes,
  log_rr,
  se_log_rr,
  llr,
  calibrated_rr,
  calibrated_ci_95_lb,
  calibrated_ci_95_ub,
  calibrated_p,
  calibrated_one_sided_p,
  calibrated_log_rr,
  calibrated_se_log_rr,
  target_estimator,
  database_id,
  target_id,
  comparator_id)
SELECT analysis_id,
  target_id * 1000000 + comparator_id AS target_comparator_id,
  outcome_id,
  rr,
  ci_95_lb,
  ci_95_ub,
  p,
  one_sided_p,
  target_subjects,
  comparator_subjects,
  target_days,
  comparator_days,
  target_outcomes,
  comparator_outcomes,
  log_rr,
  se_log_rr,
  llr,
  calibrated_rr,
  calibrated_ci_95_lb,
  calibrated_ci_95_ub,
  calibrated_p,
  calibrated_one_sided_p,
  calibrated_log_rr,
  calibrated_se_log_rr,
  target_estimator,
  database_id,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_result;

DROP TABLE @database_schema.@table_prefixcm_result;

ALTER TABLE @database_schema.@table_prefixcm_result_new RENAME TO @table_prefixcm_result;


-------------------------------------------------------------------
-- 4. cm_interaction_result
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_interaction_result_new (
  analysis_id INT NOT NULL,
  target_comparator_id BIGINT NOT NULL,
  outcome_id BIGINT NOT NULL,
  interaction_covariate_id INT NOT NULL,
  rr FLOAT,
  ci_95_lb FLOAT,
  ci_95_ub FLOAT,
  p FLOAT,
  target_subjects INT,
  comparator_subjects INT,
  target_days INT,
  comparator_days INT,
  target_outcomes INT,
  comparator_outcomes INT,
  log_rr FLOAT,
  se_log_rr FLOAT,
  calibrated_rr FLOAT,
  calibrated_ci_95_lb FLOAT,
  calibrated_ci_95_ub FLOAT,
  calibrated_p FLOAT,
  calibrated_log_rr FLOAT,
  calibrated_se_log_rr FLOAT,
  target_estimator VARCHAR,
  database_id VARCHAR NOT NULL,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(analysis_id,target_comparator_id,outcome_id,interaction_covariate_id,database_id)
);

INSERT INTO @database_schema.@table_prefixcm_interaction_result_new (
  analysis_id,
  target_comparator_id,
  outcome_id,
  interaction_covariate_id,
  rr,
  ci_95_lb,
  ci_95_ub,
  p,
  target_subjects,
  comparator_subjects,
  target_days,
  comparator_days,
  target_outcomes,
  comparator_outcomes,
  log_rr,
  se_log_rr,
  calibrated_rr,
  calibrated_ci_95_lb,
  calibrated_ci_95_ub,
  calibrated_p,
  calibrated_log_rr,
  calibrated_se_log_rr,
  target_estimator,
  database_id,
  target_id,
  comparator_id)
SELECT analysis_id,
  target_id * 1000000 + comparator_id AS target_comparator_id,
  outcome_id,
  interaction_covariate_id,
  rr,
  ci_95_lb,
  ci_95_ub,
  p,
  target_subjects,
  comparator_subjects,
  target_days,
  comparator_days,
  target_outcomes,
  comparator_outcomes,
  log_rr,
  se_log_rr,
  calibrated_rr,
  calibrated_ci_95_lb,
  calibrated_ci_95_ub,
  calibrated_p,
  calibrated_log_rr,
  calibrated_se_log_rr,
  target_estimator,
  database_id,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_interaction_result;

DROP TABLE @database_schema.@table_prefixcm_interaction_result;

ALTER TABLE @database_schema.@table_prefixcm_interaction_result_new RENAME TO @table_prefixcm_interaction_result;


-------------------------------------------------------------------
-- 5. cm_covariate_balance
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_covariate_balance_new (
  database_id VARCHAR NOT NULL,
  target_comparator_id BIGINT NOT NULL,
  outcome_id BIGINT NOT NULL,
  analysis_id INT NOT NULL,
  covariate_id BIGINT NOT NULL,
  target_mean_before FLOAT,
  comparator_mean_before FLOAT,
  mean_before FLOAT,
  std_diff_before FLOAT,
  mean_after FLOAT,
  target_mean_after FLOAT,
  comparator_mean_after FLOAT,
  std_diff_after FLOAT,
  target_std_diff FLOAT,
  comparator_std_diff FLOAT,
  target_comparator_std_diff FLOAT,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(database_id,target_comparator_id,outcome_id,analysis_id,covariate_id)
);

INSERT INTO @database_schema.@table_prefixcm_covariate_balance_new (
  database_id,
  target_comparator_id,
  outcome_id,
  analysis_id,
  covariate_id,
  target_mean_before,
  comparator_mean_before,
  mean_before,
  std_diff_before,
  mean_after,
  target_mean_after,
  comparator_mean_after,
  std_diff_after,
  target_std_diff,
  comparator_std_diff,
  target_comparator_std_diff,
  target_id,
  comparator_id)
SELECT database_id,
  target_id * 1000000 + comparator_id AS target_comparator_id,
  outcome_id,
  analysis_id,
  covariate_id,
  target_mean_before,
  comparator_mean_before,
  mean_before,
  std_diff_before,
  mean_after,
  target_mean_after,
  comparator_mean_after,
  std_diff_after,
  target_std_diff,
  comparator_std_diff,
  target_comparator_std_diff,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_covariate_balance;

DROP TABLE @database_schema.@table_prefixcm_covariate_balance;

ALTER TABLE @database_schema.@table_prefixcm_covariate_balance_new RENAME TO @table_prefixcm_covariate_balance;


-------------------------------------------------------------------
-- 6. cm_diagnostics_summary
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_diagnostics_summary_new (
  analysis_id INT NOT NULL,
  target_comparator_id BIGINT NOT NULL,
  outcome_id BIGINT NOT NULL,
  database_id VARCHAR NOT NULL,
  max_sdm FLOAT,
  sdm_family_wise_min_p FLOAT,
  shared_max_sdm FLOAT,
  shared_sdm_family_wise_min_p FLOAT,
  equipoise FLOAT,
  mdrr FLOAT,
  attrition_fraction FLOAT,
  generalizability_max_sdm FLOAT,
  ease FLOAT,
  balance_diagnostic VARCHAR(20),
  shared_balance_diagnostic VARCHAR(20),
  equipoise_diagnostic VARCHAR(20),
  mdrr_diagnostic VARCHAR(20),
  attrition_diagnostic VARCHAR(20),
  generalizability_diagnostic VARCHAR(20),
  ease_diagnostic VARCHAR(20),
  unblind INT,
  unblind_for_evidence_synthesis INT,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(analysis_id,target_comparator_id,outcome_id,database_id)
);

INSERT INTO @database_schema.@table_prefixcm_diagnostics_summary_new (
  analysis_id,
  target_comparator_id,
  outcome_id,
  database_id,
  max_sdm,
  sdm_family_wise_min_p,
  shared_max_sdm,
  shared_sdm_family_wise_min_p,
  equipoise,
  mdrr,
  attrition_fraction,
  generalizability_max_sdm,
  ease,
  balance_diagnostic,
  shared_balance_diagnostic,
  equipoise_diagnostic,
  mdrr_diagnostic,
  attrition_diagnostic,
  generalizability_diagnostic,
  ease_diagnostic,
  unblind,
  unblind_for_evidence_synthesis,
  target_id,
  comparator_id)
SELECT analysis_id,
  target_id * 1000000 + comparator_id AS target_comparator_id,
  outcome_id,
  database_id,
  max_sdm,
  sdm_family_wise_min_p,
  shared_max_sdm,
  shared_sdm_family_wise_min_p,
  equipoise,
  mdrr,
  attrition_fraction,
  generalizability_max_sdm,
  ease,
  balance_diagnostic,
  shared_balance_diagnostic,
  equipoise_diagnostic,
  mdrr_diagnostic,
  attrition_diagnostic,
  generalizability_diagnostic,
  ease_diagnostic,
  unblind,
  unblind_for_evidence_synthesis,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_diagnostics_summary;

DROP TABLE @database_schema.@table_prefixcm_diagnostics_summary;

ALTER TABLE @database_schema.@table_prefixcm_diagnostics_summary_new RENAME TO @table_prefixcm_diagnostics_summary;


-------------------------------------------------------------------
-- 7. cm_target_comparator_outcome
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_target_comparator_outcome_new (
  outcome_id BIGINT NOT NULL,
  outcome_of_interest INT,
  true_effect_size FLOAT,
  target_comparator_id BIGINT NOT NULL,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(outcome_id,target_comparator_id)
);

INSERT INTO @database_schema.@table_prefixcm_target_comparator_outcome_new (
  outcome_id,
  outcome_of_interest,
  true_effect_size,
  target_comparator_id,
  target_id,
  comparator_id)
SELECT outcome_id,
  outcome_of_interest,
  true_effect_size,
  target_id * 1000000 + comparator_id AS target_comparator_id,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_target_comparator_outcome;

DROP TABLE @database_schema.@table_prefixcm_target_comparator_outcome;

ALTER TABLE @database_schema.@table_prefixcm_target_comparator_outcome_new RENAME TO @table_prefixcm_target_comparator_outcome;


-------------------------------------------------------------------
-- 8. cm_kaplan_meier_dist
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_kaplan_meier_dist_new (
  time_day INT NOT NULL,
  target_survival FLOAT,
  target_survival_lb FLOAT,
  target_survival_ub FLOAT,
  comparator_survival FLOAT,
  comparator_survival_lb FLOAT,
  comparator_survival_ub FLOAT,
  target_at_risk INT,
  comparator_at_risk INT,
  target_comparator_id BIGINT NOT NULL,
  outcome_id BIGINT NOT NULL,
  analysis_id INT NOT NULL,
  database_id VARCHAR NOT NULL,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(time_day,target_comparator_id,outcome_id,analysis_id,database_id)
);

INSERT INTO @database_schema.@table_prefixcm_kaplan_meier_dist_new (
  time_day,
  target_survival,
  target_survival_lb,
  target_survival_ub,
  comparator_survival,
  comparator_survival_lb,
  comparator_survival_ub,
  target_at_risk,
  comparator_at_risk,
  target_comparator_id,
  outcome_id,
  analysis_id,
  database_id,
  target_id,
  comparator_id)
SELECT time_day,
  target_survival,
  target_survival_lb,
  target_survival_ub,
  comparator_survival,
  comparator_survival_lb,
  comparator_survival_ub,
  target_at_risk,
  comparator_at_risk,
  target_id * 1000000 + comparator_id AS target_comparator_id,
  outcome_id,
  analysis_id,
  database_id,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_kaplan_meier_dist;

DROP TABLE @database_schema.@table_prefixcm_kaplan_meier_dist;

ALTER TABLE @database_schema.@table_prefixcm_kaplan_meier_dist_new RENAME TO @table_prefixcm_kaplan_meier_dist;


-------------------------------------------------------------------
-- 9. cm_likelihood_profile
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_likelihood_profile_new (
  log_rr FLOAT NOT NULL,
  log_likelihood FLOAT,
  gradient FLOAT,
  target_comparator_id BIGINT NOT NULL,
  outcome_id BIGINT NOT NULL,
  analysis_id INT NOT NULL,
  database_id VARCHAR NOT NULL,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(log_rr,target_comparator_id,outcome_id,analysis_id,database_id)
);

INSERT INTO @database_schema.@table_prefixcm_likelihood_profile_new (
  log_rr,
  log_likelihood,
  gradient,
  target_comparator_id,
  outcome_id,
  analysis_id,
  database_id,
  target_id,
  comparator_id)
SELECT log_rr,
  log_likelihood,
  gradient,
  target_id * 1000000 + comparator_id AS target_comparator_id,
  outcome_id,
  analysis_id,
  database_id,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_likelihood_profile;

DROP TABLE @database_schema.@table_prefixcm_likelihood_profile;

ALTER TABLE @database_schema.@table_prefixcm_likelihood_profile_new RENAME TO @table_prefixcm_likelihood_profile;


-------------------------------------------------------------------
-- 10. cm_preference_score_dist
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_preference_score_dist_new (
  analysis_id INT NOT NULL,
  target_comparator_id BIGINT NOT NULL,
  database_id VARCHAR NOT NULL,
  preference_score FLOAT NOT NULL,
  target_density FLOAT,
  comparator_density FLOAT,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(analysis_id,target_comparator_id,database_id,preference_score)
);

INSERT INTO @database_schema.@table_prefixcm_preference_score_dist_new (
  analysis_id,
  target_comparator_id,
  database_id,
  preference_score,
  target_density,
  comparator_density,
  target_id,
  comparator_id)
SELECT analysis_id,
  target_id * 1000000 + comparator_id AS target_comparator_id,
  database_id,
  preference_score,
  target_density,
  comparator_density,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_preference_score_dist;

DROP TABLE @database_schema.@table_prefixcm_preference_score_dist;

ALTER TABLE @database_schema.@table_prefixcm_preference_score_dist_new RENAME TO @table_prefixcm_preference_score_dist;


-------------------------------------------------------------------
-- 11. cm_propensity_model
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_propensity_model_new (
  target_comparator_id BIGINT NOT NULL,
  analysis_id INT NOT NULL,
  database_id VARCHAR NOT NULL,
  covariate_id BIGINT NOT NULL,
  coefficient FLOAT,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(target_comparator_id,analysis_id,database_id,covariate_id)
);

INSERT INTO @database_schema.@table_prefixcm_propensity_model_new (
  target_comparator_id,
  analysis_id,
  database_id,
  covariate_id,
  coefficient,
  target_id,
  comparator_id)
SELECT target_id * 1000000 + comparator_id AS target_comparator_id,
  analysis_id,
  database_id,
  covariate_id,
  coefficient,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_propensity_model;

DROP TABLE @database_schema.@table_prefixcm_propensity_model;

ALTER TABLE @database_schema.@table_prefixcm_propensity_model_new RENAME TO @table_prefixcm_propensity_model;


-------------------------------------------------------------------
-- 12. cm_shared_covariate_balance
-------------------------------------------------------------------

CREATE TABLE @database_schema.@table_prefixcm_shared_covariate_balance_new (
  database_id VARCHAR NOT NULL,
  target_comparator_id BIGINT NOT NULL,
  analysis_id INT NOT NULL,
  covariate_id BIGINT NOT NULL,
  mean_before FLOAT,
  target_mean_before FLOAT,
  comparator_mean_before FLOAT,
  std_diff_before FLOAT,
  mean_after FLOAT,
  target_mean_after FLOAT,
  comparator_mean_after FLOAT,
  std_diff_after FLOAT,
  target_std_diff FLOAT,
  comparator_std_diff FLOAT,
  target_comparator_std_diff FLOAT,
  target_id BIGINT,
  comparator_id BIGINT,
  PRIMARY KEY(database_id,target_comparator_id,analysis_id,covariate_id)
);

INSERT INTO @database_schema.@table_prefixcm_shared_covariate_balance_new (
  database_id,
  target_comparator_id,
  analysis_id,
  covariate_id,
  mean_before,
  target_mean_before,
  comparator_mean_before,
  std_diff_before,
  mean_after,
  target_mean_after,
  comparator_mean_after,
  std_diff_after,
  target_std_diff,
  comparator_std_diff,
  target_comparator_std_diff,
  target_id,
  comparator_id)
SELECT database_id,
  target_id * 1000000 + comparator_id AS target_comparator_id,
  analysis_id,
  covariate_id,
  mean_before,
  target_mean_before,
  comparator_mean_before,
  std_diff_before,
  mean_after,
  target_mean_after,
  comparator_mean_after,
  std_diff_after,
  target_std_diff,
  comparator_std_diff,
  target_comparator_std_diff,
  target_id,
  comparator_id
FROM @database_schema.@table_prefixcm_shared_covariate_balance;

DROP TABLE @database_schema.@table_prefixcm_shared_covariate_balance;

ALTER TABLE @database_schema.@table_prefixcm_shared_covariate_balance_new RENAME TO @table_prefixcm_shared_covariate_balance;

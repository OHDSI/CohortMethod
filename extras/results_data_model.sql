{DEFAULT @table_prefix = ''}
{DEFAULT @cm_attrition = cm_attrition}
{DEFAULT @cm_follow_up_dist = cm_follow_up_dist}
{DEFAULT @cm_analysis = cm_analysis}
{DEFAULT @cm_result = cm_result}
{DEFAULT @cm_interaction_result = cm_interaction_result}
{DEFAULT @cm_covariate = cm_covariate}
{DEFAULT @cm_covariate_analysis = cm_covariate_analysis}
{DEFAULT @cm_covariate_balance = cm_covariate_balance}
{DEFAULT @cm_diagnostics_summary = cm_diagnostics_summary}
{DEFAULT @cm_target_comparator_outcome = cm_target_comparator_outcome}
{DEFAULT @cm_kaplan_meier_dist = cm_kaplan_meier_dist}
{DEFAULT @cm_likelihood_profile = cm_likelihood_profile}
{DEFAULT @cm_preference_score_dist = cm_preference_score_dist}
{DEFAULT @cm_propensity_model = cm_propensity_model}
{DEFAULT @cm_shared_covariate_balance = cm_shared_covariate_balance}
{DEFAULT @cm_target_comparator = cm_target_comparator}
  
CREATE TABLE @database_schema.@table_prefix@cm_attrition (
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
 
CREATE TABLE @database_schema.@table_prefix@cm_follow_up_dist (
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
 
CREATE TABLE @database_schema.@table_prefix@cm_analysis (
  	 analysis_id INT NOT NULL,
	 description VARCHAR,
	 definition VARCHAR,
	PRIMARY KEY(analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_result (
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
 
CREATE TABLE @database_schema.@table_prefix@cm_interaction_result (
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
 
CREATE TABLE @database_schema.@table_prefix@cm_covariate (
  	 covariate_id BIGINT NOT NULL,
	 covariate_name VARCHAR,
	 analysis_id INT NOT NULL,
	 covariate_analysis_id INT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(covariate_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_covariate_analysis (
  	 covariate_analysis_id INT NOT NULL,
	 covariate_analysis_name VARCHAR,
	 analysis_id INT NOT NULL,
	PRIMARY KEY(covariate_analysis_id,analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_covariate_balance (
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
 
CREATE TABLE @database_schema.@table_prefix@cm_diagnostics_summary (
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
 
CREATE TABLE @database_schema.@table_prefix@cm_target_comparator_outcome (
  	 outcome_id BIGINT NOT NULL,
	 outcome_of_interest INT,
	 true_effect_size FLOAT,
	 target_comparator_id BIGINT NOT NULL,
	 target_id BIGINT,
	 comparator_id BIGINT,
	PRIMARY KEY(outcome_id,target_comparator_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_kaplan_meier_dist (
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
 
CREATE TABLE @database_schema.@table_prefix@cm_likelihood_profile (
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
 
CREATE TABLE @database_schema.@table_prefix@cm_preference_score_dist (
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
 
CREATE TABLE @database_schema.@table_prefix@cm_propensity_model (
  	 target_comparator_id BIGINT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 coefficient FLOAT,
	 target_id BIGINT,
	 comparator_id BIGINT,
	PRIMARY KEY(target_comparator_id,analysis_id,database_id,covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefix@cm_shared_covariate_balance (
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
 
CREATE TABLE @database_schema.@table_prefix@cm_target_comparator (
  	 target_comparator_id BIGINT NOT NULL,
	 target_id BIGINT,
	 comparator_id BIGINT,
	 nesting_cohort_id BIGINT,
	PRIMARY KEY(target_comparator_id)
);


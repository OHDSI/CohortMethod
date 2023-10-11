CREATE TABLE @database_schema.@table_prefixcm_attrition (
  	 sequence_number INT NOT NULL,
	 description VARCHAR,
	 subjects INT,
	 exposure_id INT NOT NULL,
	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 outcome_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(sequence_number,exposure_id,target_id,comparator_id,analysis_id,outcome_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_follow_up_dist (
  	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
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
	PRIMARY KEY(target_id,comparator_id,outcome_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_analysis (
  	 analysis_id INT NOT NULL,
	 description VARCHAR,
	 definition VARCHAR,
	PRIMARY KEY(analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_result (
  	 analysis_id INT NOT NULL,
	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
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
	 llr FLOAT,
	 calibrated_rr FLOAT,
	 calibrated_ci_95_lb FLOAT,
	 calibrated_ci_95_ub FLOAT,
	 calibrated_p FLOAT,
	 calibrated_log_rr FLOAT,
	 calibrated_se_log_rr FLOAT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,target_id,comparator_id,outcome_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_interaction_result (
  	 analysis_id INT NOT NULL,
	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
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
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,target_id,comparator_id,outcome_id,interaction_covariate_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_covariate (
  	 covariate_id BIGINT NOT NULL,
	 covariate_name VARCHAR,
	 analysis_id INT NOT NULL,
	 covariate_analysis_id INT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(covariate_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_covariate_analysis (
  	 covariate_analysis_id INT NOT NULL,
	 covariate_analysis_name VARCHAR,
	 analysis_id INT NOT NULL,
	PRIMARY KEY(covariate_analysis_id,analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_covariate_balance (
  	 database_id VARCHAR NOT NULL,
	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 target_mean_before FLOAT,
	 comparator_mean_before FLOAT,
	 std_diff_before FLOAT,
	 target_mean_after FLOAT,
	 comparator_mean_after FLOAT,
	 std_diff_after FLOAT,
	PRIMARY KEY(database_id,target_id,comparator_id,outcome_id,analysis_id,covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_diagnostics_summary (
  	 analysis_id INT NOT NULL,
	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 max_sdm FLOAT,
	 shared_max_sdm FLOAT,
	 equipoise FLOAT,
	 mdrr FLOAT,
	 attrition_fraction FLOAT,
	 ease FLOAT,
	 balance_diagnostic VARCHAR(20),
	 shared_balance_diagnostic VARCHAR(20),
	 equipoise_diagnostic VARCHAR(20),
	 mdrr_diagnostic VARCHAR(20),
	 attrition_diagnostic VARCHAR(20),
	 ease_diagnostic VARCHAR(20),
	 unblind INT,
	PRIMARY KEY(analysis_id,target_id,comparator_id,outcome_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_target_comparator_outcome (
  	 outcome_id INT NOT NULL,
	 outcome_of_interest INT,
	 true_effect_size FLOAT,
	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	PRIMARY KEY(outcome_id,target_id,comparator_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_kaplan_meier_dist (
  	 time_day INT NOT NULL,
	 target_survival FLOAT,
	 target_survival_lb FLOAT,
	 target_survival_ub FLOAT,
	 comparator_survival FLOAT,
	 comparator_survival_lb FLOAT,
	 comparator_survival_ub FLOAT,
	 target_at_risk INT,
	 comparator_at_risk INT,
	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(time_day,target_id,comparator_id,outcome_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_likelihood_profile (
  	 log_rr FLOAT NOT NULL,
	 log_likelihood FLOAT,
	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 outcome_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(log_rr,target_id,comparator_id,outcome_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_preference_score_dist (
  	 analysis_id INT NOT NULL,
	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 preference_score FLOAT NOT NULL,
	 target_density FLOAT,
	 comparator_density FLOAT,
	PRIMARY KEY(analysis_id,target_id,comparator_id,database_id,preference_score)
);
 
CREATE TABLE @database_schema.@table_prefixcm_propensity_model (
  	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 coefficient FLOAT,
	PRIMARY KEY(target_id,comparator_id,analysis_id,database_id,covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefixcm_shared_covariate_balance (
  	 database_id VARCHAR NOT NULL,
	 target_id INT NOT NULL,
	 comparator_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 covariate_id BIGINT NOT NULL,
	 target_mean_before FLOAT,
	 comparator_mean_before FLOAT,
	 std_diff_before FLOAT,
	 target_mean_after FLOAT,
	 comparator_mean_after FLOAT,
	 std_diff_after FLOAT,
	PRIMARY KEY(database_id,target_id,comparator_id,analysis_id,covariate_id)
);


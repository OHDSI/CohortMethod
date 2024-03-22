-- Generated SQL statements to alter columns to BIGINT data type

-- Alter columns in cm_attrition table
ALTER TABLE @database_schema.@table_prefixcm_attrition ALTER COLUMN exposure_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_attrition ALTER COLUMN target_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_attrition ALTER COLUMN comparator_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_attrition ALTER COLUMN outcome_id TYPE BIGINT;

-- Alter columns in cm_follow_up_dist table
ALTER TABLE @database_schema.@table_prefixcm_follow_up_dist ALTER COLUMN target_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_follow_up_dist ALTER COLUMN comparator_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_follow_up_dist ALTER COLUMN outcome_id TYPE BIGINT;

-- Alter columns in cm_result table
ALTER TABLE @database_schema.@table_prefixcm_result ALTER COLUMN target_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_result ALTER COLUMN comparator_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_result ALTER COLUMN outcome_id TYPE BIGINT;

-- Alter columns in cm_interaction_result table
ALTER TABLE @database_schema.@table_prefixcm_interaction_result ALTER COLUMN target_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_interaction_result ALTER COLUMN comparator_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_interaction_result ALTER COLUMN outcome_id TYPE BIGINT;

-- Alter columns in cm_target_comparator_outcome table
ALTER TABLE @database_schema.@table_prefixcm_target_comparator_outcome ALTER COLUMN outcome_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_target_comparator_outcome ALTER COLUMN target_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_target_comparator_outcome ALTER COLUMN comparator_id TYPE BIGINT;

-- Alter columns in cm_kaplan_meier_dist table
ALTER TABLE @database_schema.@table_prefixcm_kaplan_meier_dist ALTER COLUMN target_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_kaplan_meier_dist ALTER COLUMN comparator_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_kaplan_meier_dist ALTER COLUMN outcome_id TYPE BIGINT;

-- Alter columns in cm_likelihood_profile table
ALTER TABLE @database_schema.@table_prefixcm_likelihood_profile ALTER COLUMN target_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_likelihood_profile ALTER COLUMN comparator_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_likelihood_profile ALTER COLUMN outcome_id TYPE BIGINT;

-- Alter columns in cm_shared_covariate_balance table
ALTER TABLE @database_schema.@table_prefixcm_shared_covariate_balance ALTER COLUMN target_id TYPE BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_shared_covariate_balance ALTER COLUMN comparator_id TYPE BIGINT;

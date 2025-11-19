-- Database migrations for version 6.0.0
-- This migration updates the schema:
-- 1. Adds the gradient field to the cm_likelihood_profile table.
-- 2. Adds the sdm_family_wise_min_p field to the cm_diagnostics_summary table.
-- 3. Adds the shared_sdm_family_wise_min_p field to the cm_diagnostics_summary table.
-- 4. Adds the nesting_cohort_id field to
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

ALTER TABLE @database_schema.@table_prefixcm_attrition ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_follow_up_dist ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_result ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_interaction_result ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_covariate_balance ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_diagnostics_summary ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_target_comparator_outcome ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_kaplan_meier_dist ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_likelihood_profile ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_preference_score_dist ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_propensity_model ADD nesting_cohort_id BIGINT;
ALTER TABLE @database_schema.@table_prefixcm_shared_covariate_balance ADD nesting_cohort_id BIGINT;

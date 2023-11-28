-- Database migrations for version 5.2.0
-- This migration updates the schema:
-- 1. Table cm_result add target_estimator, one_sided_p and calibrated_one_sided_p fields
-- 2. Tablec m_interaction_result add target_estimator field
-- 3. Table cm_covariate_balance and cm_shared_covariate_balance add:
--   - mean_before
--   - mean_after
--   - target_std_diff
--   - comparator_std_diff
--   - target_comparator_std_diff
-- 4. Table cm_diagnostics_summary deprecate:
--   - attrition_fraction
--   - attrition_diagnostic
--    Add:
--   - generalizability_max_sdm
--   - generalizability_diagnostic
--   - unblind_for_evidence_synthesis
ALTER TABLE @database_schema.@table_prefixcm_result ADD target_estimator VARCHAR(3), one_sided_p FLOAT, calibrated_one_sided_p FLOAT;
ALTER TABLE @database_schema.@table_prefixcm_interaction_result ADD target_estimator VARCHAR(3);
ALTER TABLE @database_schema.@table_prefixcm_covariate_balance ADD mean_before FLOAT, mean_after FLOAT, target_std_diff FLOAT, comparator_std_diff FLOAT, target_comparator_std_diff FLOAT;
ALTER TABLE @database_schema.@table_prefixcm_shared_covariate_balance ADD mean_before FLOAT, mean_after FLOAT, target_std_diff FLOAT, comparator_std_diff FLOAT, target_comparator_std_diff FLOAT;
ALTER TABLE @database_schema.@table_prefixcm_diagnostics_summary ADD generalizability_max_sdm FLOAT, generalizability_diagnostic VARCHAR(20), unblind_for_evidence_synthesis INT;

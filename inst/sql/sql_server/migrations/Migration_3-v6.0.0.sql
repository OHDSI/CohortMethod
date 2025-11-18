-- Database migrations for version 6.0.0
-- This migration updates the schema:
-- 1. Adds the gradient field to the cm_likelihood_profile table.
-- 2. Adds the sdm_family_wise_min_p field to the cm_diagnostics_summary table.
-- 3. Adds the shared_sdm_family_wise_min_p field to the cm_diagnostics_summary table.

ALTER TABLE @database_schema.@table_prefixcm_likelihood_profile ADD gradient FLOAT;
ALTER TABLE @database_schema.@table_prefixcm_diagnostics_summary ADD sdm_family_wise_min_p FLOAT;
ALTER TABLE @database_schema.@table_prefixcm_diagnostics_summary ADD shared_sdm_family_wise_min_p FLOAT;

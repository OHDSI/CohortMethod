-- Database migrations for version 6.0.0
-- This migration updates the schema:
-- 1. Adds the gradient field to the cm_likelihood_profile table.

ALTER TABLE @database_schema.@table_prefixsccs_likelihood_profile ADD gradient FLOAT;

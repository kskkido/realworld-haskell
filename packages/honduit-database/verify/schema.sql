-- Verify honduit:schema on pg

BEGIN;

SELECT pg_catalog.has_schema_privilege('honduit', 'usage');

ROLLBACK;

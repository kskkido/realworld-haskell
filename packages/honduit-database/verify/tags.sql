-- Verify honduit:tags on pg

BEGIN;

SELECT
  id,
  label
FROM 
  honduit.tags
WHERE
  FALSE;

ROLLBACK;

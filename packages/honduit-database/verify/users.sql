-- Verify honduit:users on pg

BEGIN;

SELECT
  id,
  uid
FROM 
  honduit.users
WHERE
  FALSE;

ROLLBACK;

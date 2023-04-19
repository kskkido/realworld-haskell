-- Verify honduit:user_auth_credentials on pg

BEGIN;

SELECT
  id,
  user_id,
  email,
  password
FROM 
  honduit.user_auth_credentials
WHERE
  FALSE;

ROLLBACK;

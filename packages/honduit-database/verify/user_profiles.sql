-- Verify honduit:user_profiles on pg

BEGIN;

SELECT
  id,
  user_id,
  username,
  bio,
  image
FROM 
  honduit.user_profiles
WHERE
  FALSE;

ROLLBACK;

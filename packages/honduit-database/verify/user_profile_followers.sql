-- Verify honduit:user_profile_followers on pg

BEGIN;

SELECT
  follower_id,
  following_id
FROM 
  honduit.user_profile_followers
WHERE
  FALSE;

ROLLBACK;

-- Verify honduit:article_favorites on pg

BEGIN;

SELECT
  article_id,
  user_profile_id
FROM 
  honduit.article_favorites
WHERE
  FALSE;

ROLLBACK;

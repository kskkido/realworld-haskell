-- Verify honduit:comments on pg

BEGIN;

SELECT
  id,
  created_at,
  updated_at,
  body,
  author_id
FROM 
  honduit.comments
WHERE
  FALSE;

ROLLBACK;

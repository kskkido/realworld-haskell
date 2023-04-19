-- Verify honduit:articles on pg

BEGIN;

SELECT
  slug,
  id,
  title,
  description,
  body,
  created_at,
  updated_at,
  author_id
FROM 
  honduit.articles
WHERE
  FALSE;

ROLLBACK;

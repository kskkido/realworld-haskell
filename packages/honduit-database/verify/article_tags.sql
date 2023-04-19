-- Verify honduit:article_tags on pg

BEGIN;

SELECT
  article_id,
  tag_id
FROM 
  honduit.article_tags
WHERE
  FALSE;

ROLLBACK;

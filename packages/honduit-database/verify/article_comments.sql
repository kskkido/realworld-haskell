-- Verify honduit:article_comments on pg

BEGIN;

SELECT
  article_id,
  comment_id
FROM 
  honduit.article_comments
WHERE
  FALSE;

ROLLBACK;

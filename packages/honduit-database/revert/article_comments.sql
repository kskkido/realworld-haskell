-- Revert honduit:article_comments from pg

BEGIN;

DROP TABLE honduit.article_comments;

COMMIT;

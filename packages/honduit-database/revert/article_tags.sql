-- Revert honduit:article_tags from pg

BEGIN;

DROP TABLE honduit.article_tags;

COMMIT;

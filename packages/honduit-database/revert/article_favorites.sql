-- Revert honduit:article_favorites from pg

BEGIN;

DROP TABLE honduit.article_favorites;

COMMIT;

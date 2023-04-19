-- Revert honduit:articles from pg

BEGIN;

DROP TABLE honduit.articles;

COMMIT;

-- Revert honduit:comments from pg

BEGIN;

DROP TABLE honduit.comments;

COMMIT;

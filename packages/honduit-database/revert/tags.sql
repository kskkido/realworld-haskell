-- Revert honduit:tags from pg

BEGIN;

DROP TABLE honduit.tags;

COMMIT;

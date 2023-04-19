-- Revert honduit:users from pg

BEGIN;

DROP TABLE honduit.users;

COMMIT;

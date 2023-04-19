-- Revert honduit:user_profiles from pg

BEGIN;

DROP TABLE honduit.user_profiles;

COMMIT;

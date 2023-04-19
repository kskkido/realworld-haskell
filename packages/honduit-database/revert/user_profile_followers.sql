-- Revert honduit:user_profile_followers from pg

BEGIN;

DROP TABLE honduit.user_profile_followers;

COMMIT;

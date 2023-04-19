-- Revert honduit:user_auth_credentials from pg

BEGIN;

DROP TABLE honduit.user_auth_credentials;

COMMIT;

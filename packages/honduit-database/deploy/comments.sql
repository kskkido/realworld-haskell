-- Deploy honduit:comments to pg
-- requires: schema
-- requires: user_profiles

BEGIN;

ALTER TABLE honduit.comments ALTER COLUMN created_at SET DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE honduit.comments ALTER COLUMN updated_at SET DEFAULT CURRENT_TIMESTAMP;

COMMIT;

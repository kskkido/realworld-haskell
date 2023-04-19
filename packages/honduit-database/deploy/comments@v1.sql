-- Deploy honduit:comments to pg
-- requires: schema
-- requires: user_profiles

BEGIN;

CREATE TABLE honduit.comments (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL,
  body TEXT NOT NULL,
  author_id INTEGER NOT NULL REFERENCES honduit.user_profiles
);

COMMIT;

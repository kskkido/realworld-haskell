-- Deploy honduit:articles to pg
-- requires: schema
-- requires: user_profiles

BEGIN;

CREATE TABLE honduit.articles (
  slug TEXT UNIQUE NOT NULL,
  id SERIAL PRIMARY KEY,
  title VARCHAR(255) NOT NULL,
  description TEXT NOT NULL,
  body TEXT NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  author_id INTEGER NOT NULL REFERENCES honduit.user_profiles
);

COMMIT;

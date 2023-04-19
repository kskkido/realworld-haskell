-- Deploy honduit:user_profiles to pg
-- requires: schema
-- requires: users

BEGIN;

CREATE TABLE honduit.user_profiles (
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES honduit.users,
  username VARCHAR (255) NOT NULL UNIQUE,
  bio TEXT,
  image TEXT
);

COMMIT;

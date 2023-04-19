-- Deploy honduit:user_auth_credentials to pg
-- requires: schema
-- requires: users

BEGIN;

CREATE TABLE honduit.user_auth_credentials (
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES honduit.users,
  email TEXT NOT NULL UNIQUE,
  password TEXT NOT NULL
);

COMMIT;

-- Deploy honduit:tags to pg
-- requires: schema

BEGIN;

CREATE TABLE honduit.tags (
  id serial PRIMARY KEY,
  label VARCHAR(255) UNIQUE
);

COMMIT;

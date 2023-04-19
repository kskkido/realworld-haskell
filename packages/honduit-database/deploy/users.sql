-- Deploy honduit:users to pg
-- requires: schema

BEGIN;

SET client_min_messages = 'warning';

CREATE TABLE honduit.users (
  id SERIAL PRIMARY KEY,
  uid VARCHAR (255) NOT NULL UNIQUE
);

COMMIT;

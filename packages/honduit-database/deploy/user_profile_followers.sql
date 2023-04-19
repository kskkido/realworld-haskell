-- Deploy honduit:user_profile_followers to pg
-- requires: schema
-- requires: user_profiles

BEGIN;

CREATE TABLE honduit.user_profile_followers(
  follower_id INTEGER NOT NULL REFERENCES honduit.user_profiles,
  following_id INTEGER NOT NULL REFERENCES honduit.user_profiles,
  PRIMARY KEY (follower_id, following_id)
);

COMMIT;

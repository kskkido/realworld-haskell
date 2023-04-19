-- Deploy honduit:article_favorites to pg
-- requires: schema
-- requires: articles
-- requires: user_profiles

BEGIN;

CREATE TABLE IF NOT EXISTS honduit.article_favorites (
  article_id INTEGER NOT NULL REFERENCES honduit.articles,
  user_profile_id INTEGER NOT NULL REFERENCES honduit.user_profiles,
  PRIMARY KEY (article_id, user_profile_id)
);

COMMIT;

-- Deploy honduit:article_tags to pg
-- requires: schema
-- requires: articles
-- requires: tags

BEGIN;

CREATE TABLE IF NOT EXISTS honduit.article_tags (
  article_id INTEGER NOT NULL REFERENCES honduit.articles,
  tag_id INTEGER NOT NULL REFERENCES honduit.tags,
  PRIMARY KEY (article_id, tag_id)
);

COMMIT;

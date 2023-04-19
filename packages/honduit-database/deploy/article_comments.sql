-- Deploy honduit:article_comments to pg
-- requires: schema
-- requires: articles
-- requires: comments

BEGIN;

CREATE TABLE IF NOT EXISTS honduit.article_comments (
  article_id INTEGER NOT NULL REFERENCES honduit.articles,
  comment_id INTEGER NOT NULL REFERENCES honduit.comments,
  PRIMARY KEY (article_id, comment_id)
);

COMMIT;

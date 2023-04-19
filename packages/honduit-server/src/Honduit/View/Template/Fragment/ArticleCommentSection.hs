module Honduit.View.Template.Fragment.ArticleCommentSection where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Htmx.Extension as Lucid.Htmx
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.CommentCreateForm as Capability.CommentCreateForm
import qualified Honduit.View.Template.Component.ArticleCommentSection as Component.ArticleCommentSection

data Props = Props
  { commentFormValues :: Type.CommentCreateForm
  , commentFormErrors :: Type.CommentCreateFormValidationRule
  , comments :: [Type.Comment]
  , article :: Type.Article
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m, Capability.CommentCreateForm.CommentCreateForm m) => Props -> Lucid.HtmlT m ()
render props = do
  Component.ArticleCommentSection.render $ Component.ArticleCommentSection.Props
    { onSubmit =
        [ Lucid.Htmx.hxPost_ $ Text.intercalate "/"
            [ "/fragments/article-comment-section"
            , props.article.slug
            , "comments"
            ]
        ]
    , onDelete = \comment ->
        [ Lucid.Htmx.hxDelete_ $ Text.intercalate "/"
            [ "/fragments/article-comment-section"
            , props.article.slug
            , "comments"
            , fromString $ show comment.id
            ]
        ]
    , commentFormValues = props.commentFormValues
    , commentFormErrors = props.commentFormErrors
    , comments = props.comments
    , article = props.article
    , attributes = props.attributes
    }

module Honduit.View.Template.Page.Article where

import RIO
import qualified Lucid
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.CommentCreateForm as Capability.CommentCreateForm
import qualified Honduit.View.Template.Layout.Body as Layout.Body
import qualified Honduit.View.Template.Layout.Head as Layout.Head
import qualified Honduit.View.Template.View.Article as View.Article

data Props = Props
  { article :: Type.Article
  , comments :: [Type.Comment]
  , commentFormValues :: Type.CommentCreateForm
  , commentFormErrors :: Type.CommentCreateFormValidationRule
  }
  deriving (Generic)

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m, Capability.CommentCreateForm.CommentCreateForm m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.html_
    [ 
    ] do
    Lucid.head_
      Layout.Head.render
    Lucid.body_
      [
      ] do
      Layout.Body.render
        ( Layout.Body.Props
          { attributes = []
          }
        ) do
        View.Article.render $ View.Article.Props
          { article = props.article
          , comments = props.comments
          , commentFormValues = props.commentFormValues
          , commentFormErrors = props.commentFormErrors
          }

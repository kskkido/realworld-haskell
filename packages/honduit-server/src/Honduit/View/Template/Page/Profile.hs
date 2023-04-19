module Honduit.View.Template.Page.Profile where

import RIO
import qualified Lucid
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Asset
import qualified Honduit.Core.Capability.Paginator as Paginator
import qualified Honduit.View.Template.Layout.Body as Layout.Body
import qualified Honduit.View.Template.Layout.Head as Layout.Head
import qualified Honduit.View.Template.View.Profile as View.Profile

data Props = Props
  { profile :: Type.Profile
  , articles :: Type.Paginated [Type.Article]
  , articleFeedFilter :: Type.ArticleFeedFilter
  }
  deriving (Generic)

render :: (Has.Has Type.Session r, MonadReader r m, Asset.Asset m, Paginator.Paginator m) => Props -> Lucid.HtmlT m ()
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
        View.Profile.render $ View.Profile.Props
          { profile = props.profile
          , articles = props.articles
          , articleFeedFilter = props.articleFeedFilter
          }

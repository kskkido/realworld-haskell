module Honduit.View.Template.Page.Home where

import RIO
import qualified Lucid
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.Paginator as Capability.Paginator
import qualified Honduit.View.Template.Layout.Body as Layout.Body
import qualified Honduit.View.Template.Layout.Head as Layout.Head
import qualified Honduit.View.Template.View.Home as View.Home

data Props = Props
  { feedFilter :: Type.ArticleFeedFilter
  , articles :: Type.Paginated [Type.Article]
  , tags :: [Type.Tag]
  }
  deriving (Generic)

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Props -> Lucid.HtmlT m ()
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
        View.Home.render $ View.Home.Props
          { feedFilter = props.feedFilter
          , articles = props.articles
          , tags = props.tags
          }

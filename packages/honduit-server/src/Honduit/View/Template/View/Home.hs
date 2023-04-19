module Honduit.View.Template.View.Home
  ( render
  , Props(..)
  ) where

import RIO
import qualified Lucid
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.Paginator as Capability.Paginator
import qualified Honduit.View.Template.Fragment.HomeArticleFeed as Fragment.HomeArticleFeed 

data Props = Props
  { feedFilter :: Type.ArticleFeedFilter
  , articles :: Type.Paginated [Type.Article]
  , tags :: [Type.Tag]
  }
  deriving (Generic)

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.div_
    [ Lucid.classes_
      [ "home-page"
      ]
    ] do
    Lucid.div_
      [ Lucid.classes_
        [ "banner"
        ]
      ] do
      Lucid.div_
        [ Lucid.classes_
          [ "container"
          ]
        ] do
        Lucid.h1_
          [ Lucid.classes_
            [ "logo-font"
            ]
          ] do
          fromString "conduit"
        Lucid.p_
          [ Lucid.classes_
            [ "logo-font"
            ]
          ] do
          fromString "A place to share your knowledge."
    Lucid.div_
      [ Lucid.classes_
        [ "container"
        , "page"
        ]
      ] do
        Fragment.HomeArticleFeed.render $ Fragment.HomeArticleFeed.Props
          { articles = props.articles
          , articleFeedFilter = props.feedFilter
          , tags = props.tags
          , tab = Fragment.HomeArticleFeed.Personal
          , attributes = []
          }

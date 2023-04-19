module Honduit.View.Template.Fragment.ProfileArticleFeed where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Htmx.Extension as Lucid.Htmx
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.ArticleFeedFilter as ArticleFeedFilter
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.Paginator as Capability.Paginator
import qualified Honduit.View.Template.Component.ArticleFeed as Component.ArticleFeed 

data Tab = 
    Published
  | Favorited
  deriving (Generic, Eq)

data Props = Props
  { profile :: Type.Profile
  , articles :: Type.Paginated [Type.Article]
  , articleFeedFilter :: Type.ArticleFeedFilter
  , tab :: Tab
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.div_
    [ Lucid.classes_
      [ "row"
      ]
    , Lucid.Htmx.hxTarget_ "this"
    , Lucid.Htmx.hxSwap_ "outerHTML"
    ] do
    Lucid.div_
      [ Lucid.classes_
        [ "col-xs-12 col-md-10 offset-md-1"
        ]
      ] do
      void $ Maybe.runMaybeT do
        asum
          [ do
              lift do
                Lucid.div_
                  [ Lucid.classes_
                    [ "feed-toggle"
                    ]
                  ] do
                  Lucid.ul_
                    [ Lucid.classes_
                      [ "nav"
                      , "nav-pills"
                      , "outline-active"
                      ]
                    ] do
                    Lucid.li_
                      [ Lucid.classes_
                        [ "nav-item"
                        ]
                      ] do
                      Lucid.button_
                        [ Lucid.classes_ $
                          [ "nav-link"
                          ] <>
                          ["active" | props.tab == Published]
                        , Lucid.Htmx.hxGet_ $ pathByTabByProfile Published props.profile
                        ] do
                        fromString "My Articles"
                    Lucid.li_
                      [ Lucid.classes_
                        [ "nav-item"
                        ]
                      ] do
                      Lucid.button_
                        [ Lucid.classes_ $
                          [ "nav-link"
                          ] <>
                          ["active" | props.tab == Favorited]
                        , Lucid.Htmx.hxGet_ $ pathByTabByProfile Favorited props.profile
                        ] do
                        fromString "Favorited Articles"
                Component.ArticleFeed.render $ Component.ArticleFeed.Props
                  { feedFilter = props.articleFeedFilter
                  , articles = props.articles
                  , toPath = \feedFilter -> pathByTabByProfile props.tab props.profile <> "?" <>
                      ( ArticleFeedFilter.toQueryString feedFilter &
                        Text.pack
                      )
                  , attributes = []
                  }
          ]

pathByTabByProfile :: Tab -> Type.Profile -> Text
pathByTabByProfile Published profile = Text.intercalate "/"
  [ "/fragments/profile-article-feed"
  , fromString $ show profile.id
  , "published"
  ]
pathByTabByProfile Favorited profile = Text.intercalate "/"
  [ "/fragments/profile-article-feed"
  , fromString $ show profile.id
  , "favorited"
  ]


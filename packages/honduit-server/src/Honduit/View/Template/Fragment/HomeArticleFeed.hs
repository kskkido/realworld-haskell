module Honduit.View.Template.Fragment.HomeArticleFeed where

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
    Personal
  | Global
  deriving (Generic, Eq)

data Props = Props
  { articles :: Type.Paginated [Type.Article]
  , articleFeedFilter :: Type.ArticleFeedFilter
  , tags :: [Type.Tag]
  , tab :: Tab
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Props -> Lucid.HtmlT m ()
render props = do
  session :: Type.Session <- asks Has.getter
  Lucid.div_
    [ Lucid.classes_
      [ "row"
      ]
    , Lucid.Htmx.hxTarget_ "this"
    , Lucid.Htmx.hxSwap_ "outerHTML"
    ] do
    Lucid.div_
      [ Lucid.classes_
        [ "col-md-9"
        ]
      ] do
      void $ Maybe.runMaybeT do
        asum
          [ do
              userProfile <- Maybe.MaybeT . pure $ case session of 
                Type.Authorized authorized -> pure authorized.userProfile
                _ -> Nothing
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
                          ["active" | props.tab == Personal]
                        , Lucid.Htmx.hxGet_ $ tabToPath Personal
                        ] do
                        fromString "Your Feed"
                    Lucid.li_
                      [ Lucid.classes_
                        [ "nav-item"
                        ]
                      ] do
                      Lucid.button_
                        [ Lucid.classes_ $
                          [ "nav-link"
                          ] <>
                          ["active" | props.tab == Global]
                        , Lucid.Htmx.hxGet_ $ tabToPath Global
                        ] do
                        fromString "Global Feed"
                Component.ArticleFeed.render $ Component.ArticleFeed.Props
                  { feedFilter = props.articleFeedFilter
                  , articles = props.articles
                  , toPath = \feedFilter -> tabToPath props.tab <> "?" <>
                      ( ArticleFeedFilter.toQueryString feedFilter &
                        Text.pack
                      )
                  , attributes = []
                  }
          , lift do
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
                        ["active"]
                      , Lucid.Htmx.hxGet_ $ tabToPath Global
                      ] do
                      fromString "Global Feed"
              Component.ArticleFeed.render $ Component.ArticleFeed.Props
                { feedFilter = props.articleFeedFilter
                , articles = props.articles
                , toPath = \feedFilter -> fold
                    [ tabToPath Global <> "?"
                    , fromString $ ArticleFeedFilter.toQueryString feedFilter
                    ]
                , attributes = []
                }
          ]
    Lucid.div_
      [ Lucid.classes_
        [ "col-md-3"
        ]
      ] do
      Lucid.div_
        [ Lucid.classes_
          [ "sidebar"
          ]
        ] do
        Lucid.p_
          [
          ] do
          fromString "Popular tags"
        Lucid.div_
          [ Lucid.classes_
            [ "tag-list"
            ]
          ] do
          flip foldMap props.tags $ \tag -> do
            Lucid.button_
              [ Lucid.classes_
                [ "tag-default"
                , "tag-pill"
                ]
              , Lucid.Htmx.hxGet_ $ fold
                [ tabToPath props.tab <> "?"
                , fromString $ ArticleFeedFilter.toQueryString
                    ( props.articleFeedFilter
                        { Type.tagId = pure tag.id
                        , Type.offset = pure 0
                        } :: Type.ArticleFeedFilter
                    )
                ]
              ]
              do
                Lucid.toHtml tag.title

tabToPath :: Tab -> Text
tabToPath Personal = "/fragments/home-article-feed/personal"
tabToPath Global = "/fragments/home-article-feed/global"

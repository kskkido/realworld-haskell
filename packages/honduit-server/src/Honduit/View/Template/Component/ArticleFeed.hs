module Honduit.View.Template.Component.ArticleFeed where

import RIO
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Data.Has as Has
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.PaginationMetadata as PaginationMetadata
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.Paginator as Capability.Paginator
import qualified Honduit.View.Template.Component.Pagination as Component.Pagination
import qualified Honduit.View.Template.Fragment.ArticlePreview as Fragment.ArticlePreview

data Props = Props
  { feedFilter :: Type.ArticleFeedFilter
  , articles :: Type.Paginated [Type.Article]
  , toPath :: Type.ArticleFeedFilter -> Text
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.div_
    ( [ Lucid.classes_
        [
        ]
      ] `Lucid.concatAttributes` props.attributes
    )
    do
      void $ Maybe.runMaybeT do
        asum
          [ do
              guard (not $ null props.articles.item) 
              lift do
                for_ props.articles.item $ \article -> do
                  Fragment.ArticlePreview.render $ Fragment.ArticlePreview.Props
                    { article = article
                    , attributes = []
                    } 
                Lucid.div_
                  [
                  ]
                  do
                    Component.Pagination.render $ Component.Pagination.Props
                      { metadata = PaginationMetadata.fromPaginated props.articles
                      , length = 8
                      , lookaround = 3
                      , toPath = \metadata -> props.toPath $
                          ( props.feedFilter
                            { Type.limit = pure metadata.pageSize
                            , Type.offset = pure $ metadata.page * metadata.pageSize
                            } :: Type.ArticleFeedFilter
                          )
                      , attributes = []
                      }
          , do
              guard (null props.articles.item)
              lift do
                Lucid.div_
                  [ Lucid.classes_
                    [ "article-preview"
                    ]
                  ] do
                  Lucid.span_
                    [
                    ] do
                    Lucid.toHtml "No articles here... yet."
          ]


module Honduit.View.Template.Fragment.ArticleMeta where
   
import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Lucid.Htmx.Extension as Lucid.Htmx
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.View.Template.Component.ArticleMeta as ArticleMeta

data Props = Props
  { article :: Type.Article
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m) => Props -> Lucid.HtmlT m ()
render props = do
  ArticleMeta.render $ ArticleMeta.Props
    { article = props.article
    , attributes =
      [ Lucid.Htmx.hxTarget_ "this"
      , Lucid.Htmx.hxSwap_ "outerHTML"
      ] `Lucid.concatAttributes` props.attributes
    , onFollowAuthor = 
      [ Lucid.Htmx.hxPost_ $ Text.intercalate "/"
          [ "/fragments/article-meta"
          , props.article.slug
          , "follow-author"
          ]
      ]
    , onUnfollowAuthor =
      [ Lucid.Htmx.hxDelete_ $ Text.intercalate "/"
          [ "/fragments/article-meta"
          , props.article.slug
          , "follow-author"
          ]
      ]
    , onFavoriteArticle =
      [ Lucid.Htmx.hxPost_ $ Text.intercalate "/"
          [ "/fragments/article-meta"
          , props.article.slug
          , "favorite-article"
          ]
      ]
    , onUnfavoriteArticle =
      [ Lucid.Htmx.hxDelete_ $ Text.intercalate "/"
          [ "/fragments/article-meta"
          , props.article.slug
          , "favorite-article"
          ]
      ]
    }

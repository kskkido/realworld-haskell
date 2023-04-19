module Honduit.View.Template.Fragment.ArticlePreviewFavoriteCounter where
   
import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Htmx.Extension as Lucid.Htmx
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.View.Template.Component.ArticlePreviewFavoriteCounter as ArticlePreviewFavoriteCounter

data Props = Props
  { article :: Type.Article
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m) => Props -> Lucid.HtmlT m ()
render props = do
  ArticlePreviewFavoriteCounter.render $ ArticlePreviewFavoriteCounter.Props
    { onFavorite = 
      [ Lucid.Htmx.hxPost_ $ Text.intercalate "/"
          [ "/fragments/article-preview-favorite-counter"
          , props.article.slug
          , "favorite"
          ]
      ]
    , onUnfavorite =
      [ Lucid.Htmx.hxDelete_ $ Text.intercalate "/"
          [ "/fragments/article-preview-favorite-counter"
          , props.article.slug
          , "favorite"
          ]
      ]
    , article = props.article
    , attributes = props.attributes
    }

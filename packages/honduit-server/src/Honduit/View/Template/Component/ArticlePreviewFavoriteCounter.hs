module Honduit.View.Template.Component.ArticlePreviewFavoriteCounter where
   
import RIO
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type

data Props = Props
  { article :: Type.Article
  , onFavorite :: [Lucid.Attribute]
  , onUnfavorite :: [Lucid.Attribute]
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m) => Props -> Lucid.HtmlT m ()
render props = do
  session :: Type.Session <- asks Has.getter
  case session of 
    Type.Authorized _ -> do
      Lucid.button_
        ( [ Lucid.classes_
            [ "btn"
            , "btn-sm"
            , fromMaybe "btn-outline-primary" do
                guard props.article.favorited
                pure "btn-primary"
            ]
          ] `Lucid.concatAttributes`
          ( if props.article.favorited then
            props.onUnfavorite else
            props.onFavorite
          ) `Lucid.concatAttributes`
          props.attributes
        ) do
        counter props
    _ -> do
      Lucid.button_
        ( [ Lucid.classes_
            [ "btn"
            , "btn-sm"
            , "pull-xs-right"
            , "btn-outline-primary"
            ]
          ] `Lucid.concatAttributes`
          props.attributes
        ) do
        counter props

counter :: Monad m => Props -> Lucid.HtmlT m ()
counter props = do
  Lucid.i_
    [ Lucid.classes_
      [ "ion-heart"
      , "mr-1"
      ]
    ] do
    mempty
  Lucid.span_
    [
    ] do
    Lucid.toHtml . fromString $ show props.article.favoritesCount

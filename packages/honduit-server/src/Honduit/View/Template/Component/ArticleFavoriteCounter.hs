module Honduit.View.Template.Component.ArticleFavoriteCounter where
   
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
      case props.article.favorited of
        True -> do
          Lucid.button_
            ( [ Lucid.classes_
                [ "btn"
                , "btn-sm"
                , "btn-outline-primary"
                ]
              ] `Lucid.concatAttributes`
              props.onUnfavorite `Lucid.concatAttributes`
              props.attributes
            ) do
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
              Lucid.toHtml "Unfavorite Article "
              Lucid.toHtml . fromString $ show props.article.favoritesCount
        _ -> do
          Lucid.button_
            ( [ Lucid.classes_
                [ "btn"
                , "btn-sm"
                , "btn-outline-primary"
                ]
              ] `Lucid.concatAttributes`
              props.onFavorite `Lucid.concatAttributes`
              props.attributes
            ) do
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
              Lucid.toHtml "Favorite Article "
              Lucid.toHtml . fromString $ show props.article.favoritesCount
    _ -> do
      Lucid.button_
        ( [ Lucid.classes_
            [ "btn"
            , "btn-sm"
            , "btn-outline-primary"
            ]
          ] `Lucid.concatAttributes`
          props.attributes
        ) do
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


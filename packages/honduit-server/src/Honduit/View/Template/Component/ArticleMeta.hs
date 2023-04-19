module Honduit.View.Template.Component.ArticleMeta where

import RIO
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Lucid.Htmx.Extension as Lucid.Htmx
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Has as Has
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock.UTCTime.Extension as Time.Clock.UTCTime
import qualified Data.Time.Calendar.Day.Extension as Time.Calendar.Day
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.View.Template.Component.ProfileFollowButton as ProfileFollowButton
import qualified Honduit.View.Template.Component.ArticleFavoriteCounter as ArticleFavoriteCounter

data Props = Props
  { article :: Type.Article
  , onFollowAuthor :: [Lucid.Attribute]
  , onUnfollowAuthor :: [Lucid.Attribute]
  , onFavoriteArticle :: [Lucid.Attribute]
  , onUnfavoriteArticle :: [Lucid.Attribute]
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m) => Props -> Lucid.HtmlT m ()
render props = do
  session :: Type.Session <- asks Has.getter
  defaultUserAvatarImgSource <- lift do
    Capability.Asset.defaultUserAvatarImgSource
  Lucid.div_
    ( [ Lucid.classes_
        [ "article-meta"
        ]
      ] `Lucid.concatAttributes`
      props.attributes
    ) do
    Lucid.a_
      [ Lucid.classes_
        [
        ]
      , Lucid.href_ $ fromString $ List.intercalate "/"
        [ "/profile"
        , show props.article.author.id
        ]
      ] do
      Lucid.img_
        [ Lucid.classes_
          [
          ]
        , Lucid.src_ $ Maybe.fromMaybe
            defaultUserAvatarImgSource
            props.article.author.image
        ]
    Lucid.div_
      [ Lucid.classes_
        [ "info"
        ]
      ] do
      Lucid.a_
        [ Lucid.classes_
          [ "author"
          ]
        , Lucid.href_ $ fromString $ List.intercalate "/"
          [ "/profile"
          , show props.article.author.id
          ]
        ] do
        Lucid.toHtml props.article.author.username
      Lucid.span_
        [ Lucid.classes_
          [ "date"
          ]
        ] do
        fromString $ Time.Calendar.Day.toString $ Time.Clock.UTCTime.toDay props.article.createdAt
    void $ Maybe.runMaybeT do 
      asum
        [ do
            userProfile <- Maybe.MaybeT . pure $
              case session of 
                Type.Authorized authorized -> pure authorized.userProfile
                _ -> Nothing
            guard (userProfile.id == props.article.author.id)
            lift do
              Lucid.span_
                [ Lucid.classes_
                  [ 
                  ]
                ] do
                Lucid.a_
                  [ Lucid.href_ $ fromString $ List.intercalate "/"
                    [ "/editor"
                    , Text.unpack props.article.slug
                    ]
                  , Lucid.classes_
                    [ "btn"
                    , "btn-outline-secondary"
                    , "btn-sm"
                    ]
                  ] do
                  Lucid.i_
                    [ Lucid.classes_
                      [ "ion-edit"
                      ]
                    ] do
                    Lucid.toHtml "Edit Article"
                Lucid.button_
                  [ Lucid.Htmx.hxDelete_ $ fromString $ List.intercalate "/"
                    [ "/article"
                    , Text.unpack props.article.slug
                    ]
                  , Lucid.classes_
                    [ "btn"
                    , "btn-outline-danger"
                    , "btn-sm"
                    ]
                  ] do
                  Lucid.i_
                    [ Lucid.classes_
                      [ "ion-trash-a"
                      ]
                    ] do
                    Lucid.toHtml "Delete Article"
        , lift do
            Lucid.span_
              [ Lucid.classes_
                [ 
                ]
              ] do
              ProfileFollowButton.render $ ProfileFollowButton.Props
                { profile = props.article.author
                , onFollow = props.onFollowAuthor
                , onUnfollow = props.onUnfollowAuthor
                , attributes = []
                }
              ArticleFavoriteCounter.render $ ArticleFavoriteCounter.Props
                { article = props.article
                , onFavorite = props.onFavoriteArticle
                , onUnfavorite = props.onUnfavoriteArticle
                , attributes = []
                }
        ]

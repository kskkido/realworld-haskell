module Honduit.View.Template.Component.ArticlePreview where

import RIO
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Data.Foldable as Foldable
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.View.Template.Component.ArticlePreviewFavoriteCounter as ArticlePreviewFavoriteCounter

data Props = Props
  { article :: Type.Article
  , onFavoriteArticle :: [Lucid.Attribute]
  , onUnfavoriteArticle :: [Lucid.Attribute]
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m) => Props -> Lucid.HtmlT m ()
render props = do
  defaultUserAvatarImgSource <- lift do
    Capability.Asset.defaultUserAvatarImgSource
  Lucid.div_
    ( [ Lucid.classes_
        [ "article-preview"
        ]
      ] `Lucid.concatAttributes`
      props.attributes
    ) do
    Lucid.div_
      [ Lucid.classes_
        [ "article-meta"
        ]
      ] do
      Lucid.a_
        [ Lucid.href_ $ fromString $ List.intercalate "/"
          [ "/profile"
          , show props.article.author.id
          ]
        ] do
        Lucid.img_
          [ Lucid.src_ $ fromMaybe
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
          fromString $ show props.article.createdAt
      ArticlePreviewFavoriteCounter.render $ ArticlePreviewFavoriteCounter.Props
        { article = props.article
        , onFavorite = props.onFavoriteArticle
        , onUnfavorite = props.onUnfavoriteArticle
        , attributes =
          [ Lucid.classes_
            [ "pull-xs-right"
            ]
          ]
        }
    Lucid.div_
      [ Lucid.classes_
        [ "preview-link"
        ]
      ] do
      Lucid.a_
        [ Lucid.href_ $ fromString $ List.intercalate "/"
          [ "/article"
          , Text.unpack props.article.slug
          ]
        ] do
        Lucid.h1_
          [
          ] do
          Lucid.toHtml props.article.title
        Lucid.p_
          [
          ] do
          Lucid.toHtml props.article.description
        Lucid.span_
          [
          ] do
          fromString "Read more..."
      Lucid.ul_
        [ Lucid.classes_
          [ "tag-list"
          ]
        ] do
        flip Foldable.foldMap props.article.tags \tag -> do
          Lucid.li_
            [ Lucid.classes_
              [ "tag-default"
              , "tag-pill"
              , "tag-outline"
              ]
            ] do
            Lucid.a_
              [ Lucid.href_ $ fromString $ fold
                [ "/?"
                , List.intercalate "&"
                  [ List.intercalate "="
                    [ "tagId"
                    , show tag.id
                    ]
                  ]
                ]
              ] do
              Lucid.toHtml tag.title


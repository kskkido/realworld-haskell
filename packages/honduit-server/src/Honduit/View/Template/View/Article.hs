module Honduit.View.Template.View.Article where

import RIO
import qualified RIO.List as List
import qualified Lucid
import qualified Data.Has as Has
import qualified Data.Foldable as Foldable
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.CommentCreateForm as Capability.CommentCreateForm
import qualified Honduit.View.Template.Fragment.ArticleMeta as ArticleMeta
import qualified Honduit.View.Template.Fragment.ArticleCommentSection as ArticleCommentSection

data Props = Props
  { article :: Type.Article
  , comments :: [Type.Comment]
  , commentFormValues :: Type.CommentCreateForm
  , commentFormErrors :: Type.CommentCreateFormValidationRule
  }

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m, Capability.CommentCreateForm.CommentCreateForm m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.div_
    [ Lucid.classes_
      [ "article-page"
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
          [
          ] do
          Lucid.toHtml props.article.title
        ArticleMeta.render $ ArticleMeta.Props
          { article = props.article
          , attributes = []
          }
    Lucid.div_
      [ Lucid.classes_
        [ "container"
        , "page"
        ]
      ] do
      Lucid.div_
        [ Lucid.classes_
          [ "row"
          , "article-content"
          ]
        ] do
        Lucid.div_
          [ Lucid.classes_
            [ "col-xs-12"
            ]
          ] do
          Lucid.div_
            [
            ] do
            Lucid.toHtml props.article.body
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
      Lucid.hr_ []
      ArticleCommentSection.render $ ArticleCommentSection.Props
        { commentFormValues = props.commentFormValues
        , commentFormErrors = props.commentFormErrors
        , comments = props.comments
        , article = props.article
        , attributes = []
        }

module Honduit.View.Template.Component.ArticleCommentSection where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Lucid.Htmx.Extension as Lucid.Htmx
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Foldable as Foldable
import qualified Data.Has as Has
import qualified Data.Time.Clock.UTCTime.Extension as Time.Clock.UTCTime
import qualified Data.Time.Calendar.Day.Extension as Time.Calendar.Day
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.FieldValidationRule as FieldValidationRule
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.CommentCreateForm as Capability.CommentCreateForm
import qualified Honduit.View.Template.Component.FormFieldValidation as FormFieldValidation

data Props = Props
  { commentFormValues :: Type.CommentCreateForm
  , commentFormErrors :: Type.CommentCreateFormValidationRule
  , comments :: [Type.Comment]
  , article :: Type.Article
  , onSubmit :: [Lucid.Attribute]
  , onDelete :: Type.Comment -> [Lucid.Attribute]
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m, Capability.CommentCreateForm.CommentCreateForm m) => Props -> Lucid.HtmlT m ()
render props = do
  session :: Type.Session <- asks Has.getter
  formConfig <- lift do
    Capability.CommentCreateForm.config
  defaultUserAvatarImgSource <- lift do
    Capability.Asset.defaultUserAvatarImgSource
  Lucid.div_
    ( [ Lucid.classes_
        [ "row"
        ]
      , Lucid.Htmx.hxTarget_ "this"
      , Lucid.Htmx.hxSwap_ "outerHTML"
      ] `Lucid.concatAttributes`
      props.attributes
    )
      do
    Lucid.div_
      [ Lucid.classes_
        [ "col-xs-12"
        , "col-md-8"
        , "offset-md-2"
        ]
      ] do 
      void $ Maybe.runMaybeT do
        _ <- Maybe.MaybeT . pure $ case session of 
          Type.Authorized authorized -> pure authorized.userProfile
          _ -> Nothing
        lift do
          Lucid.form_
            ( [ Lucid.classes_
                [ "card"
                , "comment-form"
                ]
              ] `Lucid.concatAttributes`
              props.onSubmit 
            ) do
            Lucid.div_
              [ Lucid.classes_
                [ "card-block"
                ]
              ] do
              Lucid.textarea_
                ( [ Lucid.classes_
                    [ "form-control"
                    , "form-control-lg"
                    ]
                  , Lucid.id_ "body"
                  , Lucid.name_ "body"
                  , Lucid.placeholder_ "Write a comment..."
                  , Lucid.value_ props.commentFormValues.body
                  ] `Lucid.concatAttributes`
                  Foldable.foldMap FieldValidationRule.toHtmlAttributes formConfig.body
                ) do
                Lucid.toHtml props.commentFormValues.body
              FormFieldValidation.render $ FormFieldValidation.Props
                { errors = props.commentFormErrors.body
                , attributes = []
                }
            Lucid.div_
              [ Lucid.classes_
                [ "card-footer"
                ]
              ] do
              let image = fromMaybe defaultUserAvatarImgSource do
                    userProfile <- case session of 
                      Type.Authorized authorized -> pure authorized.userProfile
                      _ -> Nothing
                    userProfile.image
              Lucid.img_
                [ Lucid.classes_
                  [ "comment-author-img"
                  ]
                , Lucid.src_ image
                ]
              Lucid.button_
                [ Lucid.classes_
                  [ "btn"
                  , "btn-sm"
                  , "btn-primary"
                  ]
                ] do
                Lucid.toHtml ("Post Comment" :: String)
      for_ props.comments \comment -> do
        Lucid.div_
          [ Lucid.classes_
            [ "card"
            ]
          ] do
          Lucid.div_
            [ Lucid.classes_
              [ "card-block"
              ]
            ] do
            Lucid.p_
              [ Lucid.classes_
                [ "card-text"
                ]
              ] do
              Lucid.toHtml comment.body
          Lucid.div_
            [ Lucid.classes_
              [ "card-footer"
              ]
            ] do
            Lucid.a_
              [ Lucid.href_ $ Text.intercalate "/"
                [ "/profile"
                , fromString $ show comment.author.id
                ]
              , Lucid.classes_
                [ "comment-author-image"
                ]
              ] do
              let image = fromMaybe defaultUserAvatarImgSource do
                    userProfile <- case session of 
                      Type.Authorized authorized -> pure authorized.userProfile
                      _ -> Nothing
                    userProfile.image
              Lucid.img_
                [ Lucid.classes_
                  [ "comment-author-img"
                  ]
                , Lucid.src_ image
                ]
            Lucid.a_
              [ Lucid.href_ $ Text.intercalate "/"
                [ "/profile"
                , fromString $ show comment.author.id
                ]
              , Lucid.classes_
                [ "comment-author"
                ]
              ] do
              Lucid.toHtml comment.author.username
            Lucid.span_
              [ Lucid.classes_
                [ "date-posted"
                ]
              ] do
              fromString $ Time.Calendar.Day.toString $ Time.Clock.UTCTime.toDay comment.createdAt
            void $ Maybe.runMaybeT do
              userProfile <- Maybe.MaybeT . pure $ case session of 
                Type.Authorized authorized -> pure authorized.userProfile
                _ -> Nothing
              guard (userProfile.id == comment.author.id)
              lift do
                Lucid.span_
                  [ Lucid.classes_
                    [ "mod-options"
                    ]
                  ] do
                  Lucid.i_
                    ( [ Lucid.classes_
                        [ "ion-trash-a"
                        ]
                      ] `Lucid.concatAttributes`
                      props.onDelete comment
                    )do
                    mempty

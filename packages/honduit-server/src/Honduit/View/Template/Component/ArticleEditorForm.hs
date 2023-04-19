module Honduit.View.Template.Component.ArticleEditorForm where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Data.Foldable as Foldable
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.FieldValidationRule as FieldValidationRule
import qualified Honduit.Core.Capability.ArticleEditorForm as Capability.ArticleEditorForm
import qualified Honduit.View.Template.Component.FormFieldValidation as FormFieldValidation
import qualified Honduit.View.Template.Component.TagsInput as TagsInput

data Props = Props
  { values :: Type.ArticleEditorForm
  , errors :: Type.ArticleEditorFormValidationRule
  , attributes :: [Lucid.Attribute]
  }

render :: (Monad m, Capability.ArticleEditorForm.ArticleEditorForm m) => Props -> Lucid.HtmlT m ()
render props = do
  config <- lift do
    Capability.ArticleEditorForm.config
  Lucid.form_
    ( [ Lucid.classes_
        [
        ]
      , Lucid.method_ "post"
      ] `Lucid.concatAttributes`
      props.attributes
    ) do
    Lucid.fieldset_
      [ Lucid.classes_
        [ "form-group"
        ]
      ] do
      Lucid.input_
        ( [ Lucid.classes_
            [ "form-control"
            , "form-control-lg"
            ]
          , Lucid.id_ "title"
          , Lucid.name_ "title"
          , Lucid.type_ "text"
          , Lucid.placeholder_ "Article title"
          , Lucid.value_ props.values.title
          ] `Lucid.concatAttributes`
          Foldable.foldMap FieldValidationRule.toHtmlAttributes config.title
        )
      FormFieldValidation.render $ FormFieldValidation.Props
        { errors = props.errors.title
        , attributes = []
        }
    Lucid.fieldset_
      [ Lucid.classes_
        [ "form-group"
        ]
      ] do
      Lucid.input_
        ( [ Lucid.classes_
            [ "form-control"
            ]
          , Lucid.id_ "description"
          , Lucid.name_ "description"
          , Lucid.type_ "text"
          , Lucid.placeholder_ "What's this article about?"
          , Lucid.value_ props.values.description
          ] `Lucid.concatAttributes`
          Foldable.foldMap FieldValidationRule.toHtmlAttributes config.description
        )
      FormFieldValidation.render $ FormFieldValidation.Props
        { errors = props.errors.description
        , attributes = []
        }
    Lucid.fieldset_
      [ Lucid.classes_
        [ "form-group"
        ]
      ] do
      Lucid.textarea_
        ( [ Lucid.classes_
            [ "form-control"
            ]
            , Lucid.id_ "body"
            , Lucid.name_ "body"
            , Lucid.rows_ "8"
            , Lucid.placeholder_ "Write your article (in markdown)"
            , Lucid.value_ props.values.body
          ] `Lucid.concatAttributes`
          Foldable.foldMap FieldValidationRule.toHtmlAttributes config.body
        ) do
        Lucid.toHtml props.values.body
      FormFieldValidation.render $ FormFieldValidation.Props
        { errors = props.errors.body
        , attributes = []
        }
    Lucid.fieldset_
      [ Lucid.classes_
        [ "form-group"
        ]
      ] do
      TagsInput.render $ TagsInput.Props
        { tags = props.values.tags
        , name = "tags"
        , attributes = Foldable.foldMap FieldValidationRule.toHtmlAttributes config.tags
        }
      FormFieldValidation.render $ FormFieldValidation.Props
        { errors = props.errors.tags
        , attributes = []
        }
    Lucid.button_
      [ Lucid.classes_
        [ "btn"
        , "btn-lg"
        , "pull-xs-right"
        , "btn-primary"
        ]
      ] do
      Lucid.toHtml ("Publish Article" :: String)


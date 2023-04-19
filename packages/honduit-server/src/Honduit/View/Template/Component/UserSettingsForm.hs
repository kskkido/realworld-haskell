module Honduit.View.Template.Component.UserSettingsForm where

import RIO
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Data.Foldable as Foldable
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.FieldValidationRule as FieldValidationRule
import qualified Honduit.Core.Capability.UserSettingsForm as Capability.UserSettingsForm
import qualified Honduit.View.Template.Component.FormFieldValidation as FormFieldValidation

data Props = Props
  { values :: Type.UserSettingsForm
  , errors :: Type.UserSettingsFormValidationRule
  , attributes :: [Lucid.Attribute]
  }

render :: (Capability.UserSettingsForm.UserSettingsForm m, Monad m) => Props  -> Lucid.HtmlT m ()
render props = do
  config <- lift do
    Capability.UserSettingsForm.config
  Lucid.form_
    ( [ Lucid.classes_
        [
        ]
      , Lucid.method_ "post"
      ] `Lucid.concatAttributes`
      props.attributes
    ) do
    Lucid.fieldset_
      [
      ] do
      Lucid.fieldset_
        [ Lucid.classes_
          [ "form-group"
          ]
        ] do
        Lucid.input_
          ( [ Lucid.classes_
              [ "form-control"
              ]
            , Lucid.id_ "image"
            , Lucid.name_ "image"
            , Lucid.type_ "text"
            , Lucid.placeholder_ "URL of profile picture"
            , Lucid.value_ props.values.image
            ] `Lucid.concatAttributes`
            Foldable.foldMap FieldValidationRule.toHtmlAttributes config.image
          )
        FormFieldValidation.render $ FormFieldValidation.Props
          { errors = props.errors.image
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
              , "form-control-lg"
              ]
            , Lucid.id_ "username"
            , Lucid.name_ "username"
            , Lucid.type_ "text"
            , Lucid.placeholder_ "Username"
            , Lucid.value_ props.values.username
            ] `Lucid.concatAttributes`
            Foldable.foldMap FieldValidationRule.toHtmlAttributes config.username
          )
        FormFieldValidation.render $ FormFieldValidation.Props
          { errors = props.errors.username
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
              , "form-control-lg"
              ]
              , Lucid.id_ "bio"
              , Lucid.name_ "bio"
              , Lucid.rows_ "8"
              , Lucid.placeholder_ "Short bio about yourself"
              , Lucid.value_ props.values.bio
            ] `Lucid.concatAttributes`
            Foldable.foldMap FieldValidationRule.toHtmlAttributes config.bio
          ) do
          Lucid.toHtml props.values.bio
        FormFieldValidation.render $ FormFieldValidation.Props
          { errors = props.errors.bio
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
              , "form-control-lg"
              ]
            , Lucid.id_ "email"
            , Lucid.name_ "email"
            , Lucid.type_ "email"
            , Lucid.placeholder_ "Email"
            , Lucid.value_ props.values.email
            ] `Lucid.concatAttributes`
            Foldable.foldMap FieldValidationRule.toHtmlAttributes config.email
          )
        FormFieldValidation.render $ FormFieldValidation.Props
          { errors = props.errors.email
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
              , "form-control-lg"
              ]
            , Lucid.id_ "password"
            , Lucid.name_ "password"
            , Lucid.type_ "password"
            , Lucid.placeholder_ "New Password"
            ] `Lucid.concatAttributes`
            Foldable.foldMap FieldValidationRule.toHtmlAttributes config.password
          )
        FormFieldValidation.render $ FormFieldValidation.Props
          { errors = props.errors.password
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
        Lucid.toHtml ("Update Settings" :: String)


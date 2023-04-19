module Honduit.View.Template.Component.RegisterForm where

import RIO
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Data.Foldable as Foldable
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.FieldValidationRule as FieldValidationRule
import qualified Honduit.Core.Capability.RegisterForm as Capability.RegisterForm
import qualified Honduit.View.Template.Component.FormFieldValidation as FormFieldValidation

data Props = Props
  { values :: Type.RegisterForm
  , errors :: Type.RegisterFormValidationRule
  , attributes :: [Lucid.Attribute]
  }

render :: (Capability.RegisterForm.RegisterForm m, Monad m) => Props -> Lucid.HtmlT m ()
render props = do
  config <- lift do
    Capability.RegisterForm.config
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
          , Lucid.id_ "username"
          , Lucid.name_ "username"
          , Lucid.type_ "username"
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
          , Lucid.placeholder_ "Password"
          , Lucid.value_ props.values.password
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
        , "btn-primary"
        , "pull-xs-right"
        ]
      ] do
      Lucid.toHtml ("Register" :: String)


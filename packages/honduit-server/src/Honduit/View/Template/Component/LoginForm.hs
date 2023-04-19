module Honduit.View.Template.Component.LoginForm where

import RIO
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Data.Foldable as Foldable
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.FieldValidationRule as FieldValidationRule
import qualified Honduit.Core.Capability.LoginForm as Capability.LoginForm
import qualified Honduit.View.Template.Component.FormFieldValidation as FormFieldValidation

data Props = Props
  { values :: Type.LoginForm
  , errors :: Type.LoginFormValidationRule
  , attributes :: [Lucid.Attribute]
  }

render :: (Capability.LoginForm.LoginForm m, Monad m) => Props -> Lucid.HtmlT m ()
render props = do
  config <- lift do
    Capability.LoginForm.config
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
      Lucid.toHtml ("Login" :: String)


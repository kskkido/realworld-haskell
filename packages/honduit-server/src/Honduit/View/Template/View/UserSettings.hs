module Honduit.View.Template.View.UserSettings where

import RIO
import qualified Lucid
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.UserSettingsForm as Capability.UserSettingsForm
import qualified Honduit.View.Template.Component.UserSettingsForm as UserSettingsForm

data Props = Props
  { values :: Type.UserSettingsForm
  , errors :: Type.UserSettingsFormValidationRule
  }

render :: (Capability.UserSettingsForm.UserSettingsForm m, Monad m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.div_
    [ Lucid.classes_
      [ "settings-page"
      ]
    ] do
    Lucid.div_
      [ Lucid.classes_
        [ "container"
        , "page"
        ]
      ] do
      Lucid.div_
        [ Lucid.classes_
          [ "row"
          ]
        ] do
        Lucid.div_
          [ Lucid.classes_
            [ "col-md-6"
            , "offset-md-3"
            , "col-xs-12"
            ]
          ] do
          Lucid.h1_
            [ Lucid.classes_
              [ "text-xs-center"
              ]
            ] do
            fromString "Your Settings"
          UserSettingsForm.render $ UserSettingsForm.Props
            { values = props.values
            , errors = props.errors
            , attributes =
              [ Lucid.id_ "user_profile_settings_form"
              ]
            }
          Lucid.hr_
            []
          Lucid.a_
            [ Lucid.href_ "logout"
            , Lucid.classes_
              [ "btn"
              , "btn-outline-danger"
              ]
            ] do
              Lucid.toHtml "Or click here to logout"



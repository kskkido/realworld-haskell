module Honduit.View.Template.View.Login where

import RIO
import qualified Lucid
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.LoginForm as Capability.LoginForm
import qualified Honduit.View.Template.Component.LoginForm as LoginForm

data Props = Props
  { values :: Type.LoginForm
  , errors :: Type.LoginFormValidationRule
  }

render :: (Capability.LoginForm.LoginForm m, Monad m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.div_
    [ Lucid.classes_
      [ "auth-page"
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
            Lucid.toHtml ("Login" :: String)
          LoginForm.render $ LoginForm.Props
            { values = props.values
            , errors = props.errors
            , attributes =
              [ Lucid.id_ "login_form"
              ]
            }


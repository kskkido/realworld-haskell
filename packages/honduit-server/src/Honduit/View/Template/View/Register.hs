module Honduit.View.Template.View.Register where

import RIO
import qualified Lucid
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.RegisterForm as Capability.RegisterForm
import qualified Honduit.View.Template.Component.RegisterForm as RegisterForm

data Props = Props
  { values :: Type.RegisterForm
  , errors :: Type.RegisterFormValidationRule
  }

render :: (Capability.RegisterForm.RegisterForm m, Monad m) => Props -> Lucid.HtmlT m ()
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
            Lucid.toHtml ("Register" :: String)
          RegisterForm.render $ RegisterForm.Props
            { values = props.values
            , errors = props.errors
            , attributes =
              [ Lucid.id_ "register_form"
              ]
            }


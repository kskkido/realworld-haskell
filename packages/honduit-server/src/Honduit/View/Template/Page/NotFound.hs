module Honduit.View.Template.Page.NotFound where

import RIO
import qualified Lucid
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.View.Template.Layout.Body as Layout.Body
import qualified Honduit.View.Template.Layout.Head as Layout.Head

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m) => Lucid.HtmlT m ()
render = do
  Lucid.html_
    [ 
    ] do
    Lucid.head_
      Layout.Head.render
    Lucid.body_
      [
      ] do
      Layout.Body.render
        ( Layout.Body.Props
          { attributes = []
          }
        ) do
        Lucid.div_
          [
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
              Lucid.h1_
                [ Lucid.classes_
                  [ "text-xs-center"
                  ]
                ] do
                Lucid.toHtml ("404" :: String)

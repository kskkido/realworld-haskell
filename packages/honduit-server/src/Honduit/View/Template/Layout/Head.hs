module Honduit.View.Template.Layout.Head where

import RIO
import qualified RIO.Text as Text
import qualified Lucid

render :: Monad m => Lucid.HtmlT m ()
render = do
  Lucid.meta_
    [ Lucid.charset_ "utf-8"
    ]
  Lucid.link_
    [ Lucid.rel_ "stylesheet"
    , Lucid.href_ $ Text.pack "/styles/index.css"
    ]
  Lucid.script_
    [ Lucid.src_ "/scripts/index.js"
    ]
    ( Text.pack ""
    )
  Lucid.link_
    [ Lucid.rel_ "stylesheet"
    , Lucid.href_ $ Text.pack "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
    ]
  Lucid.link_
    [ Lucid.rel_ "stylesheet"
    , Lucid.href_ $ Text.pack "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic"
    ]
  Lucid.link_
    [ Lucid.rel_ "stylesheet"
    , Lucid.href_ $ Text.pack "//demo.productionready.io/main.css"
    ]
  Lucid.script_
    [ Lucid.src_ "https://cdn.jsdelivr.net/npm/@alpinejs/intersect@3.x.x/dist/cdn.min.js"
    , Lucid.defer_ $ fromString ""
    ] do
      fromString ""
  Lucid.script_
    [ Lucid.src_ "https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
    , Lucid.defer_ $ fromString ""
    ] do
      fromString ""
  Lucid.script_
    [ Lucid.src_ "https://unpkg.com/htmx.org@1.9.3"
    , Lucid.integrity_ "sha384-lVb3Rd/Ca0AxaoZg5sACe8FJKF0tnUgR2Kd7ehUOG5GCcROv5uBIZsOqovBAcWua"
    , Lucid.crossorigin_ "anonymous"
    ]
    ( Text.pack ""
    )

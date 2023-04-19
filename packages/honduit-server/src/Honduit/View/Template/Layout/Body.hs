module Honduit.View.Template.Layout.Body where

import RIO
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Data.Has as Has

data Props = Props
  { attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m, Capability.Asset.Asset m) => Props -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
render props html = do
  session :: Type.Session <- asks Has.getter
  defaultUserAvatarImgSource <- lift do
    Capability.Asset.defaultUserAvatarImgSource
  Lucid.header_
    ( [ Lucid.classes_
        [ "w-full"
        , "flex"
        , "flex-col"
        , "justify-center"
        ]
      ] `Lucid.concatAttributes`
      props.attributes
    ) do
    Lucid.nav_
      [ Lucid.classes_
        [ "navbar"
        , "navbar-light"
        ]
      ] do
      Lucid.div_
        [ Lucid.classes_ ["container"]
        ] do
        Lucid.a_
          [ Lucid.classes_ ["navbar-brand"]
          , Lucid.href_ "/"
          ] do
          Lucid.toHtml "conduit"
        Lucid.ul_
          [ Lucid.classes_
            [ "nav"
            , "navbar-nav"
            , "pull-xs-right"
            ]
          ] do
          case session of
            Type.Authorized authorizedSession -> do
              Lucid.li_
                [ Lucid.classes_ ["nav-item"]
                ] do
                Lucid.a_
                  [ Lucid.classes_ ["nav-link"]
                  , Lucid.href_ "/"
                  ] do
                  Lucid.toHtml "Home"
              Lucid.li_
                [ Lucid.classes_ ["nav-item"]
                ] do
                Lucid.a_
                  [ Lucid.classes_ ["nav-link"]
                  , Lucid.href_ "/editor"
                  ] do
                  Lucid.i_
                    [ Lucid.classes_
                      [ "ion-compose"
                      ]
                    ] do
                    mempty
                  Lucid.toHtml "New Article"
              Lucid.li_
                [ Lucid.classes_ ["nav-item"]
                ] do
                Lucid.a_
                  [ Lucid.classes_ ["nav-link"]
                  , Lucid.href_ "/settings"
                  ] do
                  Lucid.i_
                    [ Lucid.classes_
                      [ "ion-gear-a"
                      ]
                    ] do
                    mempty
                  Lucid.toHtml "Settings"
              Lucid.li_
                [ Lucid.classes_ ["nav-item"]
                ] do
                Lucid.a_
                  [ Lucid.classes_ ["nav-link"]
                  , Lucid.href_ $ Text.pack $ List.intercalate "/"
                    [ "/profile"
                    , show authorizedSession.userProfile.id
                    ]
                  ] do
                  Lucid.img_
                    [ Lucid.classes_
                      [ "user-pic"
                      ]
                    , Lucid.src_ $ fromMaybe defaultUserAvatarImgSource authorizedSession.userProfile.image
                    ]
                  Lucid.toHtml $ authorizedSession.userProfile.username
            Type.Unauthorized _ -> do
              Lucid.li_
                [ Lucid.classes_ ["nav-item"]
                ] do
                Lucid.a_
                  [ Lucid.classes_ ["nav-link"]
                  , Lucid.href_ "/"
                  ] do
                  Lucid.toHtml "Home"
              Lucid.li_
                [ Lucid.classes_ ["nav-item"]
                ] do
                Lucid.a_
                  [ Lucid.classes_ ["nav-link"]
                  , Lucid.href_ "/login"
                  ] do
                  Lucid.toHtml "Sign in"
              Lucid.li_
                [ Lucid.classes_ ["nav-item"]
                ] do
                Lucid.a_
                  [ Lucid.classes_ ["nav-link"]
                  , Lucid.href_ "/register"
                  ] do
                  Lucid.toHtml "Sign up"
  Lucid.main_
    [ Lucid.id_ "main"
    , Lucid.classes_ []
    ]
    do
      html
  Lucid.footer_
    ( [ Lucid.classes_
        [ "w-full"
        , "flex"
        , "flex-col"
        , "justify-center"
        ]
      ] `Lucid.concatAttributes`
      props.attributes
    ) do
    Lucid.div_
      [ Lucid.classes_ ["container"]
      ] do
      Lucid.a_
        [ Lucid.classes_ ["logo-font"]
        , Lucid.href_ "/"
        ] do
        Lucid.toHtml "conduit"
      Lucid.span_
        [ Lucid.classes_ ["attribution"]
        ] do
        Lucid.toHtml "conduit"

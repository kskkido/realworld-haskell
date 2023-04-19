module Honduit.View.Template.View.Profile where

import RIO
import qualified Lucid
import qualified Data.Has as Has
import qualified Data.Maybe as Maybe
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Asset
import qualified Honduit.Core.Capability.Paginator as Paginator
import qualified Honduit.View.Template.Fragment.ProfileArticleFeed as ProfileArticleFeed
import qualified Honduit.View.Template.Fragment.ProfileFollowButton as ProfileFollowButton

data Props = Props
  { profile :: Type.Profile
  , articles :: Type.Paginated [Type.Article]
  , articleFeedFilter :: Type.ArticleFeedFilter
  }

render :: (Has.Has Type.Session r, MonadReader r m, Asset.Asset m, Paginator.Paginator m) => Props -> Lucid.HtmlT m ()
render props = do
  defaultUserAvatarImgSource <- lift do
    Asset.defaultUserAvatarImgSource
  Lucid.div_
    [ Lucid.classes_
      [ "profile-page"
      ]
    ] do
    Lucid.div_
      [ Lucid.classes_
        [ "user-info"
        ]
      ] do
      Lucid.div_
        [ Lucid.classes_
          [ "container"
          ]
        ] do
        Lucid.div_
          [ Lucid.classes_
            [ "row"
            ]
          ] do
          Lucid.div_
            [ Lucid.classes_
              [ "col-xs-12"
              , "col-md-10"
              , "offset-md-1"
              ]
            ] do
            Lucid.img_
              [ Lucid.classes_
                [ "user-img"
                , "mx-auto"
                ]
              , Lucid.src_ $ Maybe.fromMaybe
                  defaultUserAvatarImgSource
                  props.profile.image
              ]
            Lucid.h4_
              [ Lucid.classes_
                [
                ]
              ] do
              Lucid.toHtml props.profile.username
            Lucid.p_
              [ Lucid.classes_
                [
                ]
              ] do
              Lucid.toHtml $ Maybe.fromMaybe
                mempty
                props.profile.bio
            ProfileFollowButton.render $ ProfileFollowButton.Props
              { profile = props.profile
              , attributes = []
              }
    Lucid.div_
      [ Lucid.classes_
        [ "container"
        ]
      ] do
      ProfileArticleFeed.render $ ProfileArticleFeed.Props
        { profile = props.profile
        , articles = props.articles
        , articleFeedFilter = props.articleFeedFilter
        , tab = ProfileArticleFeed.Published
        , attributes = []
        }

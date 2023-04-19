module Honduit.View.Template.Component.ProfileFollowButton where
   
import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type

data Props = Props
  { profile :: Type.Profile
  , onFollow :: [Lucid.Attribute]
  , onUnfollow :: [Lucid.Attribute]
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m) => Props -> Lucid.HtmlT m ()
render props = do
  session :: Type.Session <- asks Has.getter
  void $ Maybe.runMaybeT do
    userProfile <- Maybe.MaybeT . pure $ case session of 
      Type.Authorized authorized -> pure authorized.userProfile
      _ -> Nothing
    asum
      [ do
          guard (userProfile.id == props.profile.id)
          lift do
            Lucid.a_
              [ Lucid.classes_
                [ "btn"
                , "btn-sm"
                , "action-btn"
                , "ng-binding"
                , "btn-outline-secondary"
                ]
              , Lucid.href_ $ Text.intercalate "/"
                  [ "/settings"
                  ]
              ] do
              Lucid.i_
                [ Lucid.classes_
                  [ "ion-gear-a"
                  ]
                ] do
                mempty
              Lucid.span_
                [
                ] do
                Lucid.toHtml $ "Edit " <> userProfile.username
      , do
          guard props.profile.following
          lift do
            Lucid.button_
              ( [ Lucid.classes_
                  [ "btn"
                  , "btn-sm"
                  , "action-btn"
                  , "ng-binding"
                  , "btn-outline-secondary"
                  ]
                ] `Lucid.concatAttributes`
                props.onUnfollow
              ) do
              Lucid.i_
                [ Lucid.classes_
                  [ "ion-minus-round"
                  ]
                ] do
                mempty
              Lucid.span_
                [
                ] do
                Lucid.toHtml $ "Unfollow " <> props.profile.username
      , do
          guard (not props.profile.following)
          lift do
            Lucid.button_
              ( [ Lucid.classes_
                  [ "btn"
                  , "btn-sm"
                  , "action-btn"
                  , "ng-binding"
                  , "btn-outline-secondary"
                  ]
                ] `Lucid.concatAttributes`
                props.onFollow
              ) do
              Lucid.i_
                [ Lucid.classes_
                  [ "ion-plus-round"
                  ]
                ] do
                mempty
              Lucid.span_
                [
                ] do
                Lucid.toHtml $ "Follow " <> props.profile.username
      ]

module Honduit.View.Template.Fragment.ProfileFollowButton where
   
import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Htmx.Extension as Lucid.Htmx
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.View.Template.Component.ProfileFollowButton as ProfileFollowButton

data Props = Props
  { profile :: Type.Profile
  , attributes :: [Lucid.Attribute]
  }

render :: (Has.Has Type.Session r, MonadReader r m) => Props -> Lucid.HtmlT m ()
render props = do
  ProfileFollowButton.render $ ProfileFollowButton.Props
    { onFollow = 
      [ Lucid.Htmx.hxPost_ $ Text.intercalate "/"
          [ "/fragments/profile-follow-button"
          , fromString $ show props.profile.id
          , "follow"
          ]
      ]
    , onUnfollow =
      [ Lucid.Htmx.hxDelete_ $ Text.intercalate "/"
          [ "/fragments/profile-follow-button"
          , fromString $ show props.profile.id
          , "follow"
          ]
      ]
    , profile = props.profile
    , attributes = props.attributes
    }

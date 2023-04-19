module Honduit.View.Template.Page.UserSettings where

import RIO
import qualified Lucid
import qualified Data.Has as Has
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.UserSettingsForm as Capability.UserSettingsForm
import qualified Honduit.View.Template.Layout.Body as Layout.Body
import qualified Honduit.View.Template.Layout.Head as Layout.Head
import qualified Honduit.View.Template.View.UserSettings as View.UserSettings

data Props = Props
  { values :: Type.UserSettingsForm
  , errors :: Type.UserSettingsFormValidationRule
  }
  deriving (Generic)

render :: (Has.Has Type.Session r, MonadReader r m, Capability.UserSettingsForm.UserSettingsForm m, Capability.Asset.Asset m) => Props -> Lucid.HtmlT m ()
render props = do
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
        View.UserSettings.render $ View.UserSettings.Props
          { values = props.values
          , errors = props.errors
          }


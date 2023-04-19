module Honduit.Core.Capability.UserSettingsForm where

import qualified Honduit.Core.Type as Type

class UserSettingsForm m where
  config :: m Type.UserSettingsFormValidationRule
  validate :: Type.UserSettingsForm -> m Type.UserSettingsForm


module Honduit.Core.Capability.LoginForm where

import qualified Honduit.Core.Type as Type

class LoginForm m where
  config :: m Type.LoginFormValidationRule
  validate :: Type.LoginForm -> m Type.LoginForm


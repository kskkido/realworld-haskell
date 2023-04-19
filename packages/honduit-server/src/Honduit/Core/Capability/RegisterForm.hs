module Honduit.Core.Capability.RegisterForm where

import qualified Honduit.Core.Type as Type

class RegisterForm m where
  config :: m Type.RegisterFormValidationRule
  validate :: Type.RegisterForm -> m Type.RegisterForm


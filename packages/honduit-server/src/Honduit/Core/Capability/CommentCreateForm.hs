module Honduit.Core.Capability.CommentCreateForm where

import qualified Honduit.Core.Type as Type

class CommentCreateForm m where
  config :: m Type.CommentCreateFormValidationRule
  validate :: Type.CommentCreateForm -> m Type.CommentCreateForm

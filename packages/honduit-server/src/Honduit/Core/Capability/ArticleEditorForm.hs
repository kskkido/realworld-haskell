module Honduit.Core.Capability.ArticleEditorForm where

import qualified Honduit.Core.Type as Type

class ArticleEditorForm m where
  config :: m Type.ArticleEditorFormValidationRule
  validate :: Type.ArticleEditorForm -> m Type.ArticleEditorForm


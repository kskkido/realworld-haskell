module Honduit.Core.FieldValidationRule where

import RIO
import qualified Lucid
import qualified Honduit.Core.Type as Type

toHtmlAttributes :: Type.FieldValidationRule -> [Lucid.Attribute]
toHtmlAttributes rule =
  case rule of
    Type.Required -> [Lucid.required_ ""]
    Type.MinLength n -> [Lucid.minlength_ $ fromString $ show n]
    Type.MaxLength n -> [Lucid.maxlength_ $ fromString $ show n]
    _ -> []


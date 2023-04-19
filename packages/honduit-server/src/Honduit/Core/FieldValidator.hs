module Honduit.Core.FieldValidator where

import RIO
import qualified RIO.Text as Text
import qualified Text.Email.Validate as Text.Email
import qualified Data.Validation as Validation
import qualified Honduit.Core.Type as Type

validateText :: Text -> Type.FieldValidationRule -> Validation.Validation [Type.FieldValidationRule] Text
validateText input rule =
  case rule of
    Type.Noop -> pure input
    Type.Required ->
      Validation.validate
        [rule]
        ( \value -> do
            guard (not $ Text.null value)
            pure value
        ) input
    Type.MinLength n ->
      Validation.validate
        [rule]
        ( \value -> do
            guard (Text.length value >= n)
            pure value
        ) input
    Type.MaxLength n ->
      Validation.validate
        [rule]
        ( \value -> do
            guard (Text.length value <= n)
            pure value
        ) input
    Type.Type t ->
      case t of
        Type.Email ->
          Text.Email.validate (Text.encodeUtf8 input) &
          bimap (const [rule]) (const input) &
          Validation.fromEither


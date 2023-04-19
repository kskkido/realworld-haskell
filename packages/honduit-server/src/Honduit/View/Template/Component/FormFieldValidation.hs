module Honduit.View.Template.Component.FormFieldValidation
  ( Props(..)
  , render
  ) where

import RIO
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Honduit.Core.Type as Type

data Props = Props
  { errors :: [Type.FieldValidationRule]
  , attributes :: [Lucid.Attribute]
  }

render :: (Monad m) => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.div_
    ( [
      ] `Lucid.concatAttributes`
      props.attributes
    ) do
    for_ props.errors $ \rule -> do
      Lucid.p_
        [
        ] do
        fromString $ case rule of
          Type.Noop -> mempty
          Type.Required -> do
            "Input is missing"
          Type.MinLength n -> do
            unwords
              [ "Length of input must be greater than or equal to"
              , show n
              ]
          Type.MaxLength n -> do
            unwords
              [ "Length of input must be less than or equal to"
              , show n
              ]
          Type.Type t ->
            case t of
              Type.Email -> do
                "Invalid email format"


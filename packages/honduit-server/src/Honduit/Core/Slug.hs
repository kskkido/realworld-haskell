module Honduit.Core.Slug where

import RIO hiding (truncate)
import qualified RIO.Text as Text
import qualified Data.Char as Char
import qualified Data.Text as Text

fromText :: Text-> Maybe Text
fromText text = do
  let ws = expand text
  guard (not $ null ws)
  pure $ truncate ws

expand :: Text -> [Text]
expand = Text.words . Text.toLower . Text.map f . Text.replace "'" ""
  where
    f x = if Char.isAlphaNum x then x else ' '

truncate :: [Text] -> Text
truncate = Text.intercalate "-"

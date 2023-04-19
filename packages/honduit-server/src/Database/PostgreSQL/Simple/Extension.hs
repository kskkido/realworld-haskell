module Database.PostgreSQL.Simple.Extension where

import RIO
import qualified Data.Text.Encoding as Text
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Database.PostgreSQL.Simple.FromField

textContent :: Parser Text
textContent = Text.decodeUtf8 <$> (quoted <|> plain)

{- the following methods were lifted and adapted from
     - internals of postgresql-simple
     - https://github.com/tomjaguarpaw/haskell-opaleye/issues/98
-}

-- | Recognizes a quoted string.
quoted :: Parser ByteString
quoted = char '"' *> option "" contents <* char '"'
  where
    esc = char '\\' *> (char '\\' <|> char '"')
    unQ = takeWhile1 (notInClass "\"\\")
    contents = mconcat <$> many (unQ <|> B.singleton <$> esc)

-- | Recognizes a plain string literal, not containing comma, quotes, or parens.
plain :: Parser ByteString
plain = takeWhile1 (notInClass ",\"()")

fromPGRow :: Typeable a => Parser a -> Field -> Maybe ByteString -> Conversion a
fromPGRow _ f Nothing = returnError UnexpectedNull f ""
fromPGRow parser f (Just bs) = do
  case parseOnly parser bs of
    Left err -> returnError ConversionFailed f err
    Right a  -> pure a

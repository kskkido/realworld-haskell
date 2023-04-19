module Honduit.Auth.Jwt.Type where

import RIO
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Time.Clock as Time.Clock

data JwtContext = JwtContext
  { jwtSecret :: ByteString.Lazy.ByteString
  , jwtClientId :: String
  , jwtExpiresInMs :: Time.Clock.NominalDiffTime
  , jwtPayloadKey :: String
  }
  deriving (Generic, Eq, Show)


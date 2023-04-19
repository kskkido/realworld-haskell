module Honduit.Interpreter.App.Type where

import RIO 
import qualified Data.Has as Has
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Time.Clock as Time.Clock
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Honduit.Auth.Jwt.Type as Jwt.Type
import qualified Honduit.Core.Type as Type

data AppContext = AppContext
  { database :: PostgreSQL.Connection
  , authCookieName :: String
  , authCookieMaxAge :: Time.Clock.DiffTime
  , authJwtSecret :: ByteString.Lazy.ByteString
  , authJwtExpiresInMs :: Time.Clock.NominalDiffTime
  , authJwtClientId :: String
  , authJwtPayloadKey :: String
  , defaultUserAvatarImgSource :: String
  , defaultArticleLimit :: Int
  , defaultArticleOffset :: Int
  , defaultPopularTagLimit :: Int
  , publicAssetsFilePath :: String
  , articleValidationRule :: Type.ArticleValidationRule
  , commentValidationRule :: Type.CommentValidationRule
  , profileValidationRule :: Type.ProfileValidationRule
  , tagValidationRule :: Type.TagValidationRule
  , userAuthCredentialValidationRule :: Type.UserAuthCredentialValidationRule
  }
  deriving (Generic)

instance Has.Has Jwt.Type.JwtContext AppContext where
  getter appContext =
    Jwt.Type.JwtContext
      appContext.authJwtSecret
      appContext.authJwtClientId
      appContext.authJwtExpiresInMs
      appContext.authJwtPayloadKey
  modifier fn appContext = appContext
    { authJwtSecret = jwtContext.jwtSecret
    , authJwtClientId = jwtContext.jwtClientId
    , authJwtExpiresInMs = jwtContext.jwtExpiresInMs
    , authJwtPayloadKey =  jwtContext.jwtPayloadKey
    }
    where jwtContext = fn $ Has.getter appContext


module Main
  ( main
  ) where

import RIO
import qualified RIO.Text as Text
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.Directory as Directory
import qualified Network.Wai.Handler.Warp as Wai
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.UTF8 as ByteString.UTF8
import qualified Data.Time.Clock as Time.Clock
import qualified Honduit.Core.Type
import qualified Honduit.Interpreter.App
import qualified Honduit.Interpreter.App.Type
import qualified Honduit.Server

data Config = Config
  { stage :: String
  , serverPort :: Int
  , databaseHost :: Text
  , databasePort :: Word16
  , databaseUser :: String
  , databaseName :: String
  , databasePassword :: String
  , tokenHeaderName :: String
  , authCookieMaxAge :: Time.Clock.DiffTime
  , authJwtSecret :: ByteString.Lazy.ByteString
  , authJwtClientId :: String
  , authJwtExpiresInMs :: Time.Clock.NominalDiffTime
  , authJwtPayloadKey :: String
  , publicAssetsFilePath :: String
  }

main :: IO.IO ()
main = do
  IO.putStrLn "Starting Server"
  result <- Except.runExceptT do
    config <- configFromSystem
    liftIO do
      context <- contextFromConfig config
      application <- either throwIO pure =<< Honduit.Interpreter.App.toIO context do
        Honduit.Server.application (Honduit.Server.handlerFromIO . Honduit.Interpreter.App.toIO context)
      IO.putStrLn $ "Running on: " <> show config.serverPort
      Wai.run config.serverPort application
  either IO.putStrLn (const $ pure ()) result

contextFromConfig :: Config -> IO.IO Honduit.Interpreter.App.Type.AppContext
contextFromConfig appConfig = do
  database <- do
    PostgreSQL.connect $ PostgreSQL.ConnectInfo
      (Text.unpack $ appConfig.databaseHost)
      appConfig.databasePort
      appConfig.databaseUser
      appConfig.databasePassword
      appConfig.databaseName
  articleValidationRule <- do
    pure $ Honduit.Core.Type.ArticleValidationRule
      { title = 
        [ Honduit.Core.Type.Required
        ]
      , description =
        [ Honduit.Core.Type.Required
        ]
      , body =
        [ Honduit.Core.Type.Required
        ]
      }
  commentValidationRule <- do
    pure $ Honduit.Core.Type.CommentValidationRule
      { body =
        [ Honduit.Core.Type.Required
        ]
      }
  profileValidationRule <- do
    pure $ Honduit.Core.Type.ProfileValidationRule
      { username =
        [ Honduit.Core.Type.Required
        ]
      , bio =
        [ Honduit.Core.Type.Noop
        ]
      , image =
        [ Honduit.Core.Type.Noop
        ]
      }
  tagValidationRule <- do
    pure $ Honduit.Core.Type.TagValidationRule
      { title =
        [ Honduit.Core.Type.Required
        ]
      }
  userAuthCredentialValidationRule <- do
    pure $ Honduit.Core.Type.UserAuthCredentialValidationRule
      { email =
        [ Honduit.Core.Type.Type Honduit.Core.Type.Email
        ]
      , password = 
        [ Honduit.Core.Type.Required
        ]
      }
  pure $
    Honduit.Interpreter.App.Type.AppContext
      { database = database
      , authCookieName = appConfig.tokenHeaderName
      , authCookieMaxAge = appConfig.authCookieMaxAge
      , authJwtSecret = appConfig.authJwtSecret
      , authJwtExpiresInMs = appConfig.authJwtExpiresInMs
      , authJwtClientId = appConfig.authJwtClientId
      , authJwtPayloadKey = appConfig.authJwtPayloadKey
      , defaultUserAvatarImgSource =  "https://api.realworld.io/images/smiley-cyrus.jpeg"
      , defaultArticleLimit = 10
      , defaultArticleOffset = 0
      , defaultPopularTagLimit = 10
      , publicAssetsFilePath = appConfig.publicAssetsFilePath
      , articleValidationRule = articleValidationRule
      , commentValidationRule = commentValidationRule
      , profileValidationRule = profileValidationRule
      , tagValidationRule = tagValidationRule
      , userAuthCredentialValidationRule = userAuthCredentialValidationRule
      }

configFromSystem :: Except.ExceptT String IO.IO Config
configFromSystem = do
  cwd <- liftIO Directory.getCurrentDirectory
  stage <- do
    value <- lift $ Maybe.runMaybeT do
      Maybe.MaybeT (Environment.lookupEnv "STAGE")
    maybe (Except.throwE "Invalid STAGE") pure value
  serverPort <- do
    value <- lift $ Maybe.runMaybeT do
      value <- Maybe.MaybeT (Environment.lookupEnv "SERVER_PORT")
      Maybe.MaybeT $ pure (readMaybe value)
    maybe (Except.throwE "Invalid SERVER_PORT") pure value
  databaseHost <- do
    value <- lift $ Maybe.runMaybeT do
      Text.pack <$> Maybe.MaybeT (Environment.lookupEnv "DATABASE_HOST")
    maybe (Except.throwE "Invalid DATABASE_HOST") pure value
  databasePort <- do
    value <- lift $ Maybe.runMaybeT do
      value <- Maybe.MaybeT (Environment.lookupEnv "DATABASE_PORT")
      Maybe.MaybeT $ pure (readMaybe value)
    maybe (Except.throwE "Invalid DATABASE_PORT") pure value
  databaseUser <- do
    value <- lift $ Maybe.runMaybeT do
      Maybe.MaybeT (Environment.lookupEnv "DATABASE_USER")
    maybe (Except.throwE "Invalid DATABASE_USER") pure value
  databaseName <- do
    value <- lift $ Maybe.runMaybeT do
      Maybe.MaybeT (Environment.lookupEnv "DATABASE_NAME")
    maybe (Except.throwE "Invalid DATABASE_NAME") pure value
  databasePassword <- do
    value <- lift $ Maybe.runMaybeT do
      Maybe.MaybeT (Environment.lookupEnv "DATABASE_PASSWORD")
    maybe (Except.throwE "Invalid DATABASE_PASSWORD") pure value
  tokenHeaderName <- do
    value <- lift $ Maybe.runMaybeT do
      Maybe.MaybeT (Environment.lookupEnv "TOKEN_HEADER_NAME")
    maybe (Except.throwE "Invalid TOKEN_HEADER_NAME") pure value
  authCookieMaxAge <- do
    value <- lift $ Maybe.runMaybeT do
      value <- Maybe.MaybeT (Environment.lookupEnv "AUTH_COOKIE_MAX_AGE")
      Maybe.MaybeT $ pure (fromInteger <$> readMaybe value)
    maybe (Except.throwE "Invalid AUTH_COOKIE_MAX_AGE") pure value
  authJwtSecret <- do
    value <- lift $ Maybe.runMaybeT do
      value <- Maybe.MaybeT (Environment.lookupEnv "AUTH_JWT_SECRET")
      pure $ ByteString.fromStrict $ ByteString.UTF8.fromString value
    maybe (Except.throwE "Invalid AUTH_JWT_SECRET") pure value
  authJwtExpiresInMs <- do
    value <- lift $ Maybe.runMaybeT do
      value <- Maybe.MaybeT (Environment.lookupEnv "AUTH_JWT_EXPIRES_IN_MS")
      Maybe.MaybeT $ pure (fromInteger <$> readMaybe value)
    maybe (Except.throwE "Invalid AUTH_JWT_EXPIRES_IN_MS") pure value
  authJwtClientId <- do
    value <- lift $ Maybe.runMaybeT do
      Maybe.MaybeT (Environment.lookupEnv "AUTH_JWT_CLIENT_ID")
    maybe (Except.throwE "Invalid AUTH_JWT_CLIENT_ID") pure value
  authJwtPayloadKey <- do
    value <- lift $ Maybe.runMaybeT do
      Maybe.MaybeT (Environment.lookupEnv "AUTH_JWT_PAYLOAD_KEY")
    maybe (Except.throwE "Invalid AUTH_JWT_PAYLOAD_KEY") pure value
  publicAssetsFilePath <- do 
    value <- lift $ Maybe.runMaybeT do
      Maybe.MaybeT (Environment.lookupEnv "PUBLIC_ASSETS_FILE_PATH")
    maybe (Except.throwE "Invalid PUBLIC_ASSETS_FILE_PATH") pure value
  pure $
    Config
      { stage = stage
      , serverPort = serverPort
      , databaseHost = databaseHost
      , databasePort = databasePort
      , databaseUser = databaseUser
      , databaseName = databaseName
      , databasePassword = databasePassword
      , tokenHeaderName = tokenHeaderName
      , authCookieMaxAge = authCookieMaxAge
      , authJwtSecret = authJwtSecret
      , authJwtClientId = authJwtClientId
      , authJwtExpiresInMs = authJwtExpiresInMs
      , authJwtPayloadKey = authJwtPayloadKey
      , publicAssetsFilePath = cwd FilePath.</> publicAssetsFilePath
      }

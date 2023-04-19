module Honduit.Core.Capability.Server where

import RIO
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cookie

class Server m where
  getAuthClearCookie :: m Cookie.SetCookie
  getAuthCookieFromPayload :: (Aeson.ToJSON a) => a -> m Cookie.SetCookie
  getPayloadFromAuthRequest :: (Aeson.FromJSON a) => Wai.Request -> Maybe.MaybeT m a
  publicAssetsFilePath :: m String
  defaultUserAvatarImgSource :: m String

module Honduit.Auth.Jwt where

import RIO hiding ((^.))
import Control.Lens
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Except as Except
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Has as Has
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Time.Clock as Time.Clock
import qualified Crypto.JWT as JWT
import qualified Crypto.JOSE.JWK as JOSE.JWK
import qualified Crypto.JOSE.JWS as JOSE.JWS
import qualified Crypto.JOSE.Compact as JOSE.Compact
import qualified Honduit.Auth.Jwt.Type as Type

getJwtValidationSettings :: (Has.Has Type.JwtContext r, MonadReader r m) => m JWT.JWTValidationSettings
getJwtValidationSettings = do
  context :: Type.JwtContext <- asks Has.getter
  pure $ JWT.defaultJWTValidationSettings (== fromString context.jwtClientId)

getJwtFromPayload :: (Has.Has Type.JwtContext r, MonadReader r m, MonadIO m, JOSE.JWS.MonadRandom m, Aeson.ToJSON a) => a -> Except.ExceptT JWT.JWTError m ByteString.Lazy.ByteString
getJwtFromPayload payload = do
  context :: Type.JwtContext <- asks Has.getter
  claimsSet <- getJwtClaimsSetFromPayload payload
  let jwk = JOSE.JWK.fromOctets context.jwtSecret
  alg <- JOSE.JWK.bestJWSAlg jwk
  jwt <- JWT.signClaims jwk (JOSE.JWS.newJWSHeader ((), alg)) claimsSet
  pure $ JOSE.Compact.encodeCompact jwt

getJwtClaimsSetFromPayload :: (Has.Has Type.JwtContext r, MonadReader r m, MonadIO m, Aeson.ToJSON a) => a -> m JWT.ClaimsSet
getJwtClaimsSetFromPayload payload = do
  context :: Type.JwtContext <- asks Has.getter
  liftIO do
    time <- Time.Clock.getCurrentTime
    pure $ JWT.emptyClaimsSet
      & JWT.claimAud ?~ JWT.Audience [fromString context.jwtClientId]
      & JWT.claimExp ?~ JWT.NumericDate (Time.Clock.addUTCTime context.jwtExpiresInMs time)
      & JWT.claimIat ?~ JWT.NumericDate time
      & JWT.addClaim (fromString context.jwtPayloadKey) (Aeson.toJSON payload)

getPayloadFromJwtClaimsSet :: (Has.Has Type.JwtContext r, MonadReader r m, Aeson.FromJSON a) => JWT.ClaimsSet -> Maybe.MaybeT m a
getPayloadFromJwtClaimsSet claimsSet = do
  context :: Type.JwtContext <- asks Has.getter
  Maybe.MaybeT $ pure $ do
    value <- Map.lookup (fromString context.jwtPayloadKey) (claimsSet ^. JWT.unregisteredClaims)
    Foldable.asum $ Aeson.fromJSON value

findJwtClaimsSetFromJwt :: (Has.Has Type.JwtContext r, MonadReader r m, MonadIO m) => ByteString -> Maybe.MaybeT m JWT.ClaimsSet
findJwtClaimsSetFromJwt token = do
  context :: Type.JwtContext <- asks Has.getter
  validationSettings <- getJwtValidationSettings
  claimsSet <- liftIO $ Except.runExceptT do
    jwt <- JOSE.Compact.decodeCompact $ ByteString.Lazy.fromStrict token
    JWT.verifyClaims
      validationSettings
      (JOSE.JWK.fromOctets context.jwtSecret)
      jwt
  Maybe.MaybeT $ pure $ case claimsSet of
    Left (_ :: JWT.JWTError) -> Nothing
    Right x -> Just x

module Honduit.Core.Session where

import RIO
import qualified Honduit.Core.Type as Type

toUserProfile :: Type.Session -> Maybe Type.Profile
toUserProfile (Type.Authorized authorized) = pure authorized.userProfile
toUserProfile _ = Nothing

fromUserProfile :: Type.Profile -> Type.Session
fromUserProfile profile = Type.Authorized $ Type.AuthorizedSession
  { userProfile = profile
  }

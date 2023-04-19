module Honduit.Core.Capability.Asset where

import RIO

class Asset m where
  defaultUserAvatarImgSource :: m Text

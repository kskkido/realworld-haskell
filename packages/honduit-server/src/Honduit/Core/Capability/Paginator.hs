module Honduit.Core.Capability.Paginator where

import RIO
import qualified Honduit.Core.Type as Type

class Paginator m where
  paginate :: Int -> Int -> Type.PaginationMetadata -> m [[Int]]

module Honduit.Core.PaginationMetadata where

import RIO
import qualified RIO.List as List
import qualified Data.Ord as Ord
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable
import qualified Honduit.Core.Type as Type

fromPaginated :: Type.Paginated a -> Type.PaginationMetadata
fromPaginated response =
  Type.PaginationMetadata
    { page = response.page
    , pageSize = response.pageSize
    , pageCount = response.pageCount
    , itemCount = response.itemCount
    }

toPagination :: Int -> Int -> Type.PaginationMetadata -> [[Int]]
toPagination len lookaround metadata =
  Maybe.fromMaybe [] do
    let edge = lookaround + 1
        head = 0
        last = max head (metadata.pageCount - 1)
        page = Ord.clamp (head, last) metadata.page
        step start _ acc = Maybe.fromMaybe [start] do
          prev <- List.lastMaybe acc
          pure $ acc ++ [prev + 1]
    Foldable.asum
      [ do
          guard (metadata.pageCount <= len)
          pure $
            [ foldr
                (step head)
                []
                (List.replicate (min len metadata.pageCount) Nothing)
            ]
      , do
          guard (page < edge || page > last - edge)
          pure $
            [ foldr
                (step head)
                []
                (List.replicate edge Nothing)
            ] ++
            [ foldr
                (step (last - edge + 1))
                []
                (List.replicate edge Nothing)
            ]
      , do
          pure $
            [ [head]
            ] ++
            [ foldr
                (step (page - lookaround))
                []
                (List.replicate (lookaround * 2 + 1) Nothing)
            ] ++
            [ [last]
            ]
      ]


module Honduit.Core.ArticleFeedFilter where

import RIO
import qualified RIO.List as List
import qualified Data.Maybe as Maybe
import qualified Honduit.Core.Type as Type

toQueryString :: Type.ArticleFeedFilter -> String
toQueryString feedFilter =
  List.intercalate
    "&"
    ( Maybe.catMaybes
        [ do
            value <- show <$> feedFilter.tagId
            pure $ "tagId=" <> value
        , do
            value <- show <$> feedFilter.authorId
            pure $ "authorId=" <> value
        , do
            value <- show <$> feedFilter.followerProfileId
            pure $ "follower=" <> value
        , do
            value <- show <$> feedFilter.favoritedProfileId
            pure $ "favorited=" <> value
        , do
            value <- show <$> feedFilter.limit
            pure $ "limit=" <> value
        , do
            value <- show <$> feedFilter.offset
            pure $ "offset=" <> value
        ]
    )

empty :: Type.ArticleFeedFilter
empty = Type.ArticleFeedFilter
  { tagId = Nothing
  , authorId = Nothing
  , followerProfileId = Nothing
  , favoritedProfileId = Nothing
  , limit = Nothing
  , offset = Nothing
  }

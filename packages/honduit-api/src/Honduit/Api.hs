module Honduit.Api where

import RIO
import qualified Servant
import qualified Servant.API
import qualified Honduit.Api.Type as Type

type Api = Servant.API.NamedRoutes RootRouter

data RootRouter mode = RootRouter
  { api ::
    mode Servant.API.:- 
    "api" Servant.API.:>
    Servant.API.NamedRoutes ApiRouter
  }
  deriving (Generic)

data ApiRouter mode = ApiRouter
  { users ::
    mode Servant.API.:-
    "users" Servant.API.:>
    Servant.API.NamedRoutes ApiUsersRouter
  , user ::
    mode Servant.API.:-
    "user" Servant.API.:>
    Servant.API.NamedRoutes ApiUserRouter
  , profiles ::
    mode Servant.API.:-
    "profiles" Servant.API.:>
    Servant.API.NamedRoutes ApiProfilesRouter
  , articles ::
    mode Servant.API.:-
    "articles" Servant.API.:>
    Servant.API.NamedRoutes ApiArticlesRouter
  , tags ::
    mode Servant.API.:-
    "tags" Servant.API.:>
    Servant.API.NamedRoutes ApiTagsRouter
  }
  deriving (Generic)

data ApiUsersRouter mode = ApiUsersRouter
  { register ::
    mode Servant.API.:-
    Type.UnauthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.JSON] Type.UserRegisterPostRequestBody Servant.API.:>
    Servant.API.Post '[Servant.API.JSON] Type.UserRegisterPostResponseBody
  , login ::
    mode Servant.API.:-
    "login" Servant.API.:>
    Type.UnauthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.JSON] Type.UserLoginPostRequestBody Servant.API.:>
    Servant.API.Post '[Servant.API.JSON] Type.UserLoginPostResponseBody
  }
  deriving (Generic)

data ApiUserRouter mode = ApiUserRouter
  { show ::
    mode Servant.API.:-
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.Get '[Servant.API.JSON] Type.UserGetResponseBody
  , edit ::
    mode Servant.API.:-
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.JSON] Type.UserPutRequestBody Servant.API.:>
    Servant.API.Put '[Servant.API.JSON] Type.UserPutResponseBody
  }
  deriving (Generic)

data ApiProfilesRouter mode = ApiProfilesRouter
  { show ::
    mode Servant.API.:-
    Servant.API.Capture "profileId" Int Servant.API.:>
    Type.AuthOptionalPath Servant.API.:>
    Servant.API.Get '[Servant.API.JSON] Type.ProfileGetResponseBody
  , follow ::
    mode Servant.API.:-
    Servant.API.Capture "profileId" Int Servant.API.:>
    "follow" Servant.API.:>
    Servant.API.NamedRoutes ApiProfileFollowerRouter
  }
  deriving (Generic)

data ApiProfileFollowerRouter mode = ApiProfileFollowerRouter
  { new ::
    mode Servant.API.:-
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.Post '[Servant.API.JSON] Type.ProfileFollowPostResponseBody
  , delete ::
    mode Servant.API.:-
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.JSON] Type.ProfileFollowDeleteResponseBody
  }
  deriving (Generic)

data ApiArticlesRouter mode = ApiArticlesRouter
  { index ::
    mode Servant.API.:-
    Type.AuthOptionalPath Servant.API.:>
    Servant.API.QueryParam "tagId" Int Servant.API.:>
    Servant.API.QueryParam "authorId" Int Servant.API.:>
    Servant.API.QueryParam "follower" Int Servant.API.:>
    Servant.API.QueryParam "favorited" Int Servant.API.:>
    Servant.API.QueryParam "limit" Int Servant.API.:>
    Servant.API.QueryParam "offset" Int Servant.API.:>
    Servant.API.Get '[Servant.API.JSON] Type.ArticlesGetResponseBody
  , new ::
    mode Servant.API.:-
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.JSON] Type.ArticlePostRequestBody Servant.API.:>
    Servant.API.Post '[Servant.API.JSON] Type.ArticlePostResponseBody
  , show ::
    mode Servant.API.:-
    Servant.API.Capture "slug" Text Servant.API.:>
    Type.AuthOptionalPath Servant.API.:>
    Servant.API.Get '[Servant.API.JSON] Type.ArticleGetResponseBody
  , edit ::
    mode Servant.API.:-
    Servant.API.Capture "slug" Text Servant.API.:>
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.JSON] Type.ArticlePutRequestBody Servant.API.:>
    Servant.API.Put '[Servant.API.JSON] Type.ArticlePutResponseBody
  , delete ::
    mode Servant.API.:-
    Servant.API.Capture "slug" Text Servant.API.:>
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.JSON] Servant.API.NoContent
    
  , feed ::
    mode Servant.API.:-
    "feed" Servant.API.:>
    Type.AuthOptionalPath Servant.API.:>
    Servant.API.QueryParam "limit" Int Servant.API.:>
    Servant.API.QueryParam "offset" Int Servant.API.:>
    Servant.API.Get '[Servant.API.JSON] Type.ArticlesFeedGetResponseBody
  , comments ::
    mode Servant.API.:-
    Servant.API.Capture "slug" Text Servant.API.:>
    "comments" Servant.API.:>
    Servant.API.NamedRoutes ApiArticleCommentsRouter
  , favorite ::
    mode Servant.API.:-
    Servant.API.Capture "slug" Text Servant.API.:>
    "favorite" Servant.API.:>
    Servant.API.NamedRoutes ApiArticleFavoriteRouter
  }
  deriving (Generic)

data ApiArticleCommentsRouter mode = ApiArticleCommentsRouter
  { index ::
    mode Servant.API.:-
    Type.AuthOptionalPath Servant.API.:>
    Servant.API.Get '[Servant.API.JSON] Type.ArticleCommentsGetResponseBody
  , new ::
    mode Servant.API.:-
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.JSON] Type.ArticleCommentPostRequestBody Servant.API.:>
    Servant.API.Post '[Servant.API.JSON] Type.ArticleCommentPostResponseBody
  , delete ::
    mode Servant.API.:-
    Servant.API.Capture "commentId" Int Servant.API.:>
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.JSON] Servant.API.NoContent
  }
  deriving (Generic)

data ApiArticleFavoriteRouter mode = ApiArticleFavoriteRouter
  { new ::
    mode Servant.API.:-
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.Post '[Servant.API.JSON] Type.ArticleFavoritePostResponseBody
  , delete ::
    mode Servant.API.:-
    Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.JSON] Type.ArticleFavoriteDeleteResponseBody
  }
  deriving (Generic)

data ApiTagsRouter mode = ApiTagsRouter
  { index ::
    mode Servant.API.:-
    Type.AuthOptionalPath Servant.API.:>
    Servant.API.Get '[Servant.API.JSON] Type.TagsGetResponseBody
  }
  deriving (Generic)

proxy :: Servant.Proxy Api
proxy = Servant.Proxy

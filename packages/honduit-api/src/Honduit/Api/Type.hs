module Honduit.Api.Type where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Network.HTTP.Media as Media
import qualified Servant.API
import qualified Servant.API.Experimental.Auth
import qualified Servant.Server.Experimental.Auth
import qualified Web.FormUrlEncoded as FormUrlEncoded

data UserLoginPostRequestBody = UserLoginPostRequestBody
  { user :: UserLoginPostRequestBodyUser
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserLoginPostRequestBodyUser = UserLoginPostRequestBodyUser
  { email :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserLoginPostResponseBody = UserLoginPostResponseBody
  { user :: User
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserRegisterPostRequestBody = UserRegisterPostRequestBody
  { user :: UserRegisterPostRequestBodyUser
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserRegisterPostRequestBodyUser = UserRegisterPostRequestBodyUser
  { email :: Text
  , username :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserRegisterPostResponseBody = UserRegisterPostResponseBody
  { user :: User
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserGetResponseBody = UserGetResponseBody
  { user :: User
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserPutRequestBody = UserPutRequestBody
  { user :: UserPutRequestBodyUser
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserPutRequestBodyUser = UserPutRequestBodyUser
  { email :: Text
  , username :: Text
  , password :: Maybe Text
  , image :: Text
  , bio :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserPutResponseBody = UserPutResponseBody
  { user :: User
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ProfileGetResponseBody = ProfileGetResponseBody
  { profile :: Profile
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ProfileFollowPostResponseBody = ProfileFollowPostResponseBody
  { profile :: Profile
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ProfileFollowDeleteResponseBody = ProfileFollowDeleteResponseBody
  { profile :: Profile
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticlesGetResponseBody = ArticlesGetResponseBody
  { articles :: [Article]
  , articlesCount :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticlePostRequestBody = ArticlePostRequestBody
  { title :: Text
  , description :: Text
  , body :: Text
  , tagList :: Maybe [Text]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticlePostResponseBody = ArticlePostResponseBody
  { article :: Article
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticlesFeedGetResponseBody = ArticlesFeedGetResponseBody
  { articles :: [Article]
  , articlesCount :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleGetResponseBody = ArticleGetResponseBody
  { article :: Article
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticlePutRequestBody = ArticlePutRequestBody
  { article :: ArticlePutRequestBodyArticle
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticlePutRequestBodyArticle = ArticlePutRequestBodyArticle
  { title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Text]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticlePutResponseBody = ArticlePutResponseBody
  { article :: Article
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleCommentPostRequestBody = ArticleCommentPostRequestBody
  { comment :: ArticleCommentPostRequestBodyComment
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleCommentPostRequestBodyComment = ArticleCommentPostRequestBodyComment
  { body :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleCommentPostResponseBody = ArticleCommentPostResponseBody
  { comment :: Comment
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleCommentsGetResponseBody = ArticleCommentsGetResponseBody
  { comments :: [Comment]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleFavoritePostResponseBody = ArticleFavoritePostResponseBody
  { article :: Article
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleFavoriteDeleteResponseBody = ArticleFavoriteDeleteResponseBody
  { article :: Article
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data TagsGetResponseBody = TagsGetResponseBody
  { tags :: [Text]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data User = User
  { email :: Text
  , token :: Text
  , username :: Text
  , bio :: Maybe Text
  , image :: Maybe Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data Profile = Profile
  { id :: Int
  , username :: Text
  , bio :: Text
  , image :: Text
  , following :: Bool
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data Article = Article
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Text]
  , createdAt :: Text
  , updatedAt :: Text
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: Profile
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data Comment = Comment
  { id :: Int
  , body :: Text
  , author :: Profile
  , createdAt :: Text
  , updatedAt :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data CommentCreateForm = CommentCreateForm
  { body :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FormUrlEncoded.FromForm CommentCreateForm

data ArticleEditorForm = ArticleEditorForm
  { title :: Text
  , description :: Text
  , body :: Text
  , tags :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FormUrlEncoded.FromForm ArticleEditorForm

data LoginForm = LoginForm
  { email :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FormUrlEncoded.FromForm LoginForm

data RegisterForm = RegisterForm
  { email :: Text
  , username :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FormUrlEncoded.FromForm RegisterForm

data UserProfileSettingsForm = UserProfileSettingsForm
  { image :: Text
  , username :: Text
  , bio :: Text
  , email :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FormUrlEncoded.FromForm UserProfileSettingsForm

data FieldValidationError =
    Noop
  | Required
  | MinLength Int
  | MaxLength Int
  | Type FieldValidationErrorType
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data FieldValidationErrorType =
    Email
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

newtype AccessToken = AccessToken
  { get :: Text
  }
  deriving (Generic, Eq, Show, Read, Aeson.FromJSON, Aeson.ToJSON)

data Client =
    Authorized AuthorizedClient
  | Unauthorized UnauthorizedClient
  deriving (Generic, Show, Eq, Read, Aeson.FromJSON, Aeson.ToJSON)

data AuthorizedClient = AuthorizedClient
  { userId :: Int
  , accessToken :: Text
  }
  deriving (Generic, Show, Eq, Read, Aeson.FromJSON, Aeson.ToJSON)

data UnauthorizedClient = UnauthorizedClient
  deriving (Generic, Show, Eq, Read, Aeson.FromJSON, Aeson.ToJSON)

type AuthOptionalPath = Servant.API.Experimental.Auth.AuthProtect "auth-optional-path"

type instance Servant.Server.Experimental.Auth.AuthServerData (Servant.API.Experimental.Auth.AuthProtect "auth-optional-path") = Client

type AuthRequiredPath = Servant.API.Experimental.Auth.AuthProtect "auth-required-path"

type instance Servant.Server.Experimental.Auth.AuthServerData (Servant.API.Experimental.Auth.AuthProtect "auth-required-path") = AuthorizedClient

type UnauthRequiredPath = Servant.API.Experimental.Auth.AuthProtect "unauth-required-path"

type instance Servant.Server.Experimental.Auth.AuthServerData (Servant.API.Experimental.Auth.AuthProtect "unauth-required-path") = UnauthorizedClient

data HtmlContentType = HtmlContentType

instance Servant.API.Accept HtmlContentType where
  contentType _ = "text" Media.// "html" Media./: ("charset", "utf-8")
instance Servant.API.MimeRender HtmlContentType ByteString.Lazy.ByteString where
  mimeRender _ bs = bs

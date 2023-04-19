{-# LANGUAGE DeriveAnyClass #-}

module Honduit.Core.Type where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec.ByteString
import qualified Data.Attoparsec.Text as Attoparsec.Text
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Database.PostgreSQL.Simple.Extension as PostgreSQL
import qualified Database.PostgreSQL.Simple.Types as PostgreSQL.Types
import qualified Database.PostgreSQL.Simple.FromField as PostgreSQL.FromField
import qualified Database.PostgreSQL.Simple.FromRow as PostgreSQL.FromRow
import qualified Web.FormUrlEncoded as FormUrlEncoded

data Exception =
    AuthTokenException
  | AuthorizationException
  | ResourceNotFoundException
  | ArticleEditorFormValidationException ArticleEditorFormValidationRule
  | CommentCreateFormValidationException CommentCreateFormValidationRule
  | LoginFormValidationException LoginFormValidationRule
  | RegisterFormValidationException RegisterFormValidationRule
  | UserSettingsFormValidationException UserSettingsFormValidationRule
  deriving (Generic, Eq, Show, RIO.Exception, Aeson.FromJSON, Aeson.ToJSON)

data AuthToken = AuthToken
  { userId :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleValidationRule = ArticleValidationRule
  { title :: [FieldValidationRule]
  , description :: [FieldValidationRule]
  , body :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data CommentValidationRule = CommentValidationRule
  { body :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ProfileValidationRule = ProfileValidationRule
  { username :: [FieldValidationRule]
  , bio :: [FieldValidationRule]
  , image :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup ProfileValidationRule where
  x <> y = ProfileValidationRule
    (x.username <> y.username)
    (x.bio <> y.bio)
    (x.image <> y.image)

data TagValidationRule = TagValidationRule
  { title :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserAuthCredentialValidationRule = UserAuthCredentialValidationRule
  { email :: [FieldValidationRule]
  , password :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup UserAuthCredentialValidationRule where
  x <> y = UserAuthCredentialValidationRule
    (x.email <> y.email)
    (x.password <> y.password)

data UserUpdatePayloadValidationRule = UserUpdatePayloadValidationRule
  { email :: [FieldValidationRule]
  , username :: [FieldValidationRule]
  , password :: [FieldValidationRule]
  , image :: [FieldValidationRule]
  , bio :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup UserUpdatePayloadValidationRule where
  x <> y = UserUpdatePayloadValidationRule
    (x.email <> y.email)
    (x.username <> y.username)
    (x.password <> y.password)
    (x.image <> y.image)
    (x.bio <> y.bio)
instance Monoid UserUpdatePayloadValidationRule where
  mempty = UserUpdatePayloadValidationRule
    []
    []
    []
    []
    []

data UserLoginPayloadValidationRule = UserLoginPayloadValidationRule
  { email :: [FieldValidationRule]
  , password :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup UserLoginPayloadValidationRule where
  x <> y = UserLoginPayloadValidationRule
    (x.email <> y.email)
    (x.password <> y.password)
instance Monoid UserLoginPayloadValidationRule where
  mempty = UserLoginPayloadValidationRule
    []
    []

data UserRegisterPayloadValidationRule = UserRegisterPayloadValidationRule
  { email :: [FieldValidationRule]
  , username :: [FieldValidationRule]
  , password :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup UserRegisterPayloadValidationRule where
  x <> y = UserRegisterPayloadValidationRule
    (x.email <> y.email)
    (x.username <> y.username)
    (x.password <> y.password)
instance Monoid UserRegisterPayloadValidationRule where
  mempty = UserRegisterPayloadValidationRule
    []
    []
    []

data ArticleFeedFilter = ArticleFeedFilter
  { tagId :: Maybe Int
  , authorId :: Maybe Int
  , followerProfileId :: Maybe Int
  , favoritedProfileId :: Maybe Int
  , limit :: Maybe Int
  , offset :: Maybe Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleCreatePayload = ArticleCreatePayload
  { title :: Text
  , description :: Text
  , body :: Text
  , tags :: [Text]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleCreatePayloadValidationRule = ArticleCreatePayloadValidationRule
  { title :: [FieldValidationRule]
  , description :: [FieldValidationRule]
  , body :: [FieldValidationRule]
  , tags :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup ArticleCreatePayloadValidationRule where
  x <> y = ArticleCreatePayloadValidationRule
    (x.title <> y.title)
    (x.description <> y.description)
    (x.body <> y.body)
    (x.tags <> y.tags)
instance Monoid ArticleCreatePayloadValidationRule where
  mempty = ArticleCreatePayloadValidationRule
    []
    []
    []
    []

data ArticleUpdatePayload = ArticleUpdatePayload
  { title :: Text
  , description :: Text
  , body :: Text
  , tags :: [Text]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data ArticleUpdatePayloadValidationRule = ArticleUpdatePayloadValidationRule
  { title :: [FieldValidationRule]
  , description :: [FieldValidationRule]
  , body :: [FieldValidationRule]
  , tags :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup ArticleUpdatePayloadValidationRule where
  x <> y = ArticleUpdatePayloadValidationRule
    (x.title <> y.title)
    (x.description <> y.description)
    (x.body <> y.body)
    (x.tags <> y.tags)
instance Monoid ArticleUpdatePayloadValidationRule where
  mempty = ArticleUpdatePayloadValidationRule
    []
    []
    []
    []

data AuthUser = AuthUser
  { id :: Int
  , email :: Text
  , token :: Text
  , username :: Text
  , bio :: Maybe Text
  , image :: Maybe Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data User = User
  { id :: Int
  , email :: Text
  , username :: Text
  , bio :: Maybe Text
  , image :: Maybe Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

data UserSettings = UserSettings
  { id :: Int
  , email :: Text
  , username :: Text
  , password :: Text
  , bio :: Maybe Text
  , image :: Maybe Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

data UserEntity = UserEntity
  { id :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

data UserAuthCredentialEntity = UserAuthCredentialEntity
  { id :: Int
  , userId :: Int
  , email :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

data UserLoginPayload = UserLoginPayload
  { email :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserRegisterPayload = UserRegisterPayload
  { email :: Text
  , username :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserUpdatePayload = UserUpdatePayload
  { image :: Text
  , username :: Text
  , bio :: Text
  , email :: Text
  , password :: Maybe Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UserAuthCredentialUpdatePayload = UserAuthCredentialUpdatePayload
  { email :: Text
  , password :: Maybe Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data Profile = Profile
  { id :: Int
  , userId :: Int
  , username :: Text
  , bio :: Maybe Text
  , image :: Maybe Text
  , following :: Bool
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow, PostgreSQL.FromRow)

data ProfileEntity = ProfileEntity
  { id :: Int
  , userId :: Int
  , username :: Text
  , bio :: Maybe Text
  , image :: Maybe Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow, PostgreSQL.FromRow)

data ProfileFollowerEntity = ProfileFollowerEntity
  { followerProfileId :: Int
  , followingProfileId :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow, PostgreSQL.FromRow)

data ProfileFavoriteArticleEntity = ProfileFavoriteArticleEntity
  { profileId :: Int
  , articleId :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow, PostgreSQL.FromRow)

data ProfileUpdatePayload = ProfileUpdatePayload
  { username :: Text
  , bio :: Text
  , image :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data Article = Article
  { slug :: Text
  , id :: Int
  , title :: Text
  , description :: Text
  , body :: Text
  , tags :: [Tag]
  , createdAt :: Time.Clock.UTCTime
  , updatedAt :: Time.Clock.UTCTime
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: Profile
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance PostgreSQL.FromRow Article where
  fromRow = Article
    <$> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.field
    <*> (PostgreSQL.Types.fromPGArray <$> PostgreSQL.FromRow.field)
    <*> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.fromRow

data ArticleEntity = ArticleEntity
  { slug :: Text
  , id :: Int
  , title :: Text
  , description :: Text
  , body :: Text
  , authorId :: Int
  , createdAt :: Time.Clock.UTCTime
  , updatedAt :: Time.Clock.UTCTime
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

data ArticleCommentEntity = ArticleCommentEntity
  { articleId :: Int
  , commentId :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow, PostgreSQL.FromRow)

data ArticleTagEntity = ArticleTagaEntity
  { articleId :: Int
  , tagId :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

data ArticleFavoriteEntity = ArticleFavoriteEntity
  { articleId :: Int
  , profileId :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

data Comment = Comment
  { id :: Int
  , body :: Text
  , author :: Profile
  , createdAt :: Time.Clock.UTCTime
  , updatedAt :: Time.Clock.UTCTime
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance PostgreSQL.FromRow Comment where
  fromRow = Comment
    <$> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.fromRow
    <*> PostgreSQL.FromRow.field
    <*> PostgreSQL.FromRow.field

data CommentEntity = CommentEntity
  { id :: Int
  , body :: Text
  , authorId :: Int
  , createdAt :: Time.Clock.UTCTime
  , updatedAt :: Time.Clock.UTCTime
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.ToRow, PostgreSQL.FromRow)

data CommentCreatePayload = CommentCreatePayload
  { body :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data CommentCreatePayloadValidationRule = CommentCreatePayloadValidationRule
  { body :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup CommentCreatePayloadValidationRule where
  x <> y = CommentCreatePayloadValidationRule
    (x.body <> y.body)
instance Monoid CommentCreatePayloadValidationRule where
  mempty = CommentCreatePayloadValidationRule
    []

data Tag = Tag
  { id :: Int
  , title :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

instance PostgreSQL.FromField.FromField Tag where
  fromField = PostgreSQL.fromPGRow do
    _ <- Attoparsec.ByteString.char '('
    id <- do
      text <- PostgreSQL.textContent
      either fail pure do
        Attoparsec.Text.parseOnly (Attoparsec.Text.signed Attoparsec.Text.decimal) text
    _ <- Attoparsec.ByteString.char ','
    title <- PostgreSQL.textContent
    _ <- Attoparsec.ByteString.char ')'
    pure $ Tag
      { id = id
      , title = title
      }

data TagEntity = TagEntity
  { id :: Int
  , title :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON, PostgreSQL.FromRow, PostgreSQL.ToRow)

data FieldValidationRule =
    Noop
  | Required
  | MinLength Int
  | MaxLength Int
  | Type FieldValidationTypeRule
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data FieldValidationTypeRule =
    Email
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data Paginated a = Paginated
  { item :: a
  , itemCount :: Int
  , itemOffset :: Int
  , page :: Int
  , pageSize :: Int
  , pageCount :: Int
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data PaginationMetadata = PaginationMetadata
  { page :: Int
  , pageSize :: Int
  , pageCount :: Int
  , itemCount :: Int
  }
  deriving (Generic, Show, Eq)

data Session =
    Authorized AuthorizedSession
  | Unauthorized UnauthorizedSession
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data AuthorizedSession = AuthorizedSession
  { userProfile :: Profile
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

data UnauthorizedSession = UnauthorizedSession
  deriving (Generic, Show, Eq, Read, Aeson.FromJSON, Aeson.ToJSON)

data HomeArticleFeedTab = 
    PersonalFeedTab
  | GlobalFeedTab
  deriving (Generic, Eq, Read, Aeson.FromJSON, Aeson.ToJSON)

instance Show HomeArticleFeedTab where
  show PersonalFeedTab = "personal"
  show GlobalFeedTab = "global"

data UserSearchInput = UserSearchInput
  { userId :: Maybe Int
  , email :: Maybe Text
  , password :: Maybe Text
  }
  deriving (Generic, Show, Eq)

data UserCreateInput = UserCreateInput
  { email :: Text
  , password :: Text
  , username :: Text
  }
  deriving (Generic, Show, Eq)

data UserUpdateInput = UserUpdateInput
  { userId :: Int
  , email :: Text
  , password :: Maybe Text
  , username :: Text
  , bio :: Text
  , image :: Text
  }
  deriving (Generic, Show, Eq)

data UserTokenCreateInput = UserTokenCreateInput
  { user :: User
  }
  deriving (Generic, Show, Eq)

data UserSettingsSearchInput = UserSettingsSearchInput
  { userId :: Int
  }

data ArticleDeleteInput = ArticleDeleteInput
  { slug :: Text
  , author :: Profile
  }
  deriving (Generic, Show, Eq)

data ArticleSearchInput = ArticleSearchInput
  { userProfile :: Maybe Profile
  , slug :: Text
  }
  deriving (Generic, Show, Eq)

data ArticlesSearchInput = ArticlesSearchInput
  { userProfile :: Maybe Profile
  , tagId :: Maybe Int
  , authorId :: Maybe Int
  , followerProfileId :: Maybe Int
  , favoritedProfileId :: Maybe Int
  , limit :: Maybe Int
  , offset :: Maybe Int
  }
  deriving (Generic, Show, Eq)

data ArticleCreateInput = ArticleCreateInput
  { title :: Text
  , description :: Text
  , body :: Text
  , author :: Profile
  , tags :: [Text]
  }
  deriving (Generic, Show, Eq)

data ArticleUpdateInput = ArticleUpdateInput
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , tags :: [Text]
  , author :: Profile
  }

data ProfileSearchInput = ProfileSearchInput
  { userProfile :: Maybe Profile
  , profileId :: Maybe Int
  , userId :: Maybe Int
  }
  deriving (Generic, Show, Eq)

data CommentDeleteInput = CommentDeleteInput
  { commentId :: Int
  , author :: Profile
  }
  deriving (Generic, Show, Eq)

data CommentSearchInput = CommentSearchInput
  { userProfile :: Maybe Profile
  , commentId :: Int
  }
  deriving (Generic, Show, Eq)

data ArticleCommentCreateInput = ArticleCommentCreateInput
  { author :: Profile
  , body :: Text
  , slug :: Text
  }
  deriving (Generic, Show, Eq)

data ArticleCommentsSearchInput = ArticleCommentsSearchInput
  { userProfile :: Maybe Profile
  , slug :: Text
  }
  deriving (Generic, Show, Eq)

data ArticleFavoriteCreateInput = ArticleFavoriteCreateInput
  { profile :: Profile
  , slug :: Text
  }
  deriving (Generic, Show, Eq)

data ArticleFavoriteDeleteInput = ArticleFavoriteDeleteInput
  { profile :: Profile
  , slug :: Text
  }
  deriving (Generic, Show, Eq)

data ArticleTagsSearchInput = ArticleTagsSearchInput
  { slug :: Maybe Text
  , articleId :: Maybe Int
  }

data ArticleTagsCreateInput = ArticleTagsCreateInput
  { slug :: Text
  , tags :: [Tag]
  }

data ArticleTagsDeleteInput = ArticleTagsDeleteInput
  { slug :: Text
  }

data ArticleTagsUpdateInput = ArticleTagsUpdateInput
  { slug :: Text
  , tags :: [Tag]
  }

data TagSearchInput = TagSearchInput
  { tagId :: Maybe Int
  , title :: Maybe Text
  }
  deriving (Generic, Show, Eq)

data TagsSearchInput = TagsSearchInput
  { limit :: Maybe Int
  }
  deriving (Generic, Show, Eq)

data TagCreateInput = TagCreateInput
  { title :: Text
  }
  deriving (Generic, Show, Eq)

data ProfileFollowerDeleteInput = ProfileFollowerDeleteInput
  { followingId :: Int
  , follower :: Profile
  }

data ProfileFollowerCreateInput = ProfileFollowerCreateInput
  { followingId :: Int
  , follower :: Profile
  }

data ArticleEditorForm = ArticleEditorForm
  { title :: Text
  , description :: Text
  , body :: Text
  , tags :: [Text]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FormUrlEncoded.FromForm ArticleEditorForm

data ArticleEditorFormValidationRule = ArticleEditorFormValidationRule
  { title :: [FieldValidationRule]
  , description :: [FieldValidationRule]
  , body :: [FieldValidationRule]
  , tags :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup ArticleEditorFormValidationRule where
  x <> y = ArticleEditorFormValidationRule
    { title = x.title <> y.title
    , description = x.description <> y.description
    , body = x.body <> y.body
    , tags = x.tags <> y.tags
    }
instance Monoid ArticleEditorFormValidationRule where
  mempty = ArticleEditorFormValidationRule
    { title = mempty
    , description = mempty
    , body = mempty
    , tags = mempty
    }

data CommentCreateForm = CommentCreateForm
  { body :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FormUrlEncoded.FromForm CommentCreateForm

data CommentCreateFormValidationRule = CommentCreateFormValidationRule
  { body :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup CommentCreateFormValidationRule where
  x <> y = CommentCreateFormValidationRule
    { body = x.body <> y.body
    }
instance Monoid CommentCreateFormValidationRule where
  mempty = CommentCreateFormValidationRule
    { body = mempty
    }

data LoginForm = LoginForm
  { email :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FormUrlEncoded.FromForm LoginForm

data LoginFormValidationRule = LoginFormValidationRule
  { email :: [FieldValidationRule]
  , password :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup LoginFormValidationRule where
  x <> y = LoginFormValidationRule
    { email = x.email <> y.email
    , password = x.password <> y.password
    }
instance Monoid LoginFormValidationRule where
  mempty = LoginFormValidationRule
    { email = mempty
    , password = mempty
    }

data RegisterForm = RegisterForm
  { email :: Text
  , username :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FormUrlEncoded.FromForm RegisterForm

data RegisterFormValidationRule = RegisterFormValidationRule
  { email :: [FieldValidationRule]
  , username :: [FieldValidationRule]
  , password :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup RegisterFormValidationRule where
  x <> y = RegisterFormValidationRule
    { email = x.email <> y.email
    , username = x.username <> y.username
    , password = x.password <> y.password
    }
instance Monoid RegisterFormValidationRule where
  mempty = RegisterFormValidationRule
    { email = mempty
    , password = mempty
    , username = mempty
    }

data UserSettingsForm = UserSettingsForm
  { image :: Text
  , username :: Text
  , bio :: Text
  , email :: Text
  , password :: Text
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance FormUrlEncoded.FromForm UserSettingsForm

data UserSettingsFormValidationRule = UserSettingsFormValidationRule
  { image :: [FieldValidationRule]
  , username :: [FieldValidationRule]
  , bio :: [FieldValidationRule]
  , email :: [FieldValidationRule]
  , password :: [FieldValidationRule]
  }
  deriving (Generic, Show, Eq, Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup UserSettingsFormValidationRule where
  x <> y = UserSettingsFormValidationRule
    { image = x.image <> y.image
    , username = x.username <> y.username
    , bio = x.bio <> y.bio
    , email = x.email <> y.email
    , password = x.password <> y.password
    }
instance Monoid UserSettingsFormValidationRule where
  mempty = UserSettingsFormValidationRule
    { image = mempty
    , username = mempty
    , bio = mempty
    , email = mempty
    , password = mempty
    }

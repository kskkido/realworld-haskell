module Honduit.Interpreter.App where

import RIO 
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Except.Extension as Except
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Has as Has
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Validation as Validation
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.UTF8 as ByteString.UTF8
import qualified System.IO.Error as IO.Error
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Database.PostgreSQL.Simple.SqlQQ as PostgreSQL.SqlQQ
import qualified Network.Wai
import qualified Network.HTTP.Types.Header
import qualified Web.Cookie as Cookie
import qualified Crypto.JOSE.JWS as JOSE.JWS
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Slug as Slug
import qualified Honduit.Core.FieldValidator as FieldValidator
import qualified Honduit.Core.PaginationMetadata as PaginationMetadata
import qualified Honduit.Core.Capability.Repository as Capability.Repository
import qualified Honduit.Core.Capability.ArticleEditorForm as Capability.ArticleEditorForm
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.CommentCreateForm as Capability.CommentCreateForm
import qualified Honduit.Core.Capability.LoginForm as Capability.LoginForm
import qualified Honduit.Core.Capability.Paginator as Capability.Paginator
import qualified Honduit.Core.Capability.RegisterForm as Capability.RegisterForm
import qualified Honduit.Core.Capability.UserSettingsForm as Capability.UserSettingsForm
import qualified Honduit.Core.Capability.Server as Capability.Server
import qualified Honduit.Auth.Jwt as Jwt
import qualified Honduit.Auth.Jwt.Type as Jwt.Type
import qualified Honduit.Interpreter.App.Type as Type

newtype App a = App
  { unwrap :: Reader.ReaderT Type.AppContext IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Type.AppContext
    , JOSE.JWS.MonadRandom
    )

instance Except.MonadError Type.Exception App where
    throwError :: Type.Exception -> App a
    throwError = liftIO . throwIO
    catchError :: App a -> (Type.Exception -> App a) -> App a
    catchError app handler = App $ ReaderT $ \context -> do
      let ioAction = runReaderT (app.unwrap) context
      ioAction `catch` \exception -> runReaderT ((handler exception).unwrap) context

instance Capability.Repository.Repository App where
  createUserToken = createUserToken
  createUser = createUser
  getUserByLogin = getUserByLogin
  getUser = getUser
  updateUser = updateUser
  getUserSettings = getUserSettings
  getProfile = getProfile
  createProfileFollower = createProfileFollower
  deleteProfileFollower = deleteProfileFollower
  getArticles = getArticles
  getArticlesCount = getArticlesCount
  getPaginatedArticles = getPaginatedArticles
  createArticle = createArticle
  updateArticle = updateArticle
  deleteArticle = deleteArticle
  getArticle = getArticle
  getArticleComments = getArticleComments
  createArticleComment = createArticleComment
  deleteComment = deleteComment
  createArticleFavorite = createArticleFavorite
  deleteArticleFavorite = deleteArticleFavorite
  getTags = getTags
  getPopularTags = getPopularTags

instance Capability.Server.Server App where
  getAuthClearCookie = getAuthClearCookie
  getAuthCookieFromPayload = getAuthCookieFromPayload
  getPayloadFromAuthRequest = getPayloadFromAuthRequest
  defaultUserAvatarImgSource = do
    context <- ask
    pure $ context.defaultUserAvatarImgSource
  publicAssetsFilePath = do
    context <- ask
    pure $ context.publicAssetsFilePath

instance Capability.Paginator.Paginator App where
  paginate len lookaround metadata = do
    pure $ PaginationMetadata.toPagination len lookaround metadata

instance Capability.ArticleEditorForm.ArticleEditorForm App where
  config = getArticleEditorFormConfig
  validate = validateArticleEditorForm

instance Capability.CommentCreateForm.CommentCreateForm App where
  config = getCommentCreateFormConfig
  validate = validateCommentCreateForm

instance Capability.LoginForm.LoginForm App where
  config = getLoginFormConfig
  validate = validateLoginForm

instance Capability.RegisterForm.RegisterForm App where
  config = getRegisterFormConfig
  validate = validateRegisterForm

instance Capability.UserSettingsForm.UserSettingsForm App where
  config = getUserSettingsFormConfig
  validate = validateUserSettingsForm

instance Capability.Asset.Asset App where
  defaultUserAvatarImgSource = do
    context :: Type.AppContext <- asks Has.getter
    pure $ Text.pack context.defaultUserAvatarImgSource

toIO :: Type.AppContext -> App a -> IO (Either Type.Exception a)
toIO context (App action) = do
  try $ Reader.runReaderT action context

getAuthClearCookie :: (Has.Has Type.AppContext r, MonadReader r m) => m Cookie.SetCookie
getAuthClearCookie = do
  context :: Type.AppContext <- asks Has.getter
  pure $ Cookie.defaultSetCookie
    { Cookie.setCookieName = ByteString.UTF8.fromString context.authCookieName
    , Cookie.setCookieMaxAge = pure $ -1
    , Cookie.setCookieSecure = True
    , Cookie.setCookieHttpOnly = True
    , Cookie.setCookieSameSite = pure Cookie.sameSiteStrict
    , Cookie.setCookieValue = ""
    }

getAuthCookieFromPayload :: (Has.Has Type.AppContext r, Has.Has Jwt.Type.JwtContext r, MonadReader r m, Except.MonadError Type.Exception m, MonadIO m, JOSE.JWS.MonadRandom m, Aeson.ToJSON a) => a -> m Cookie.SetCookie
getAuthCookieFromPayload payload = do
  context :: Type.AppContext <- asks Has.getter
  jwt <- Except.modifyError (const Type.AuthTokenException) do
    Jwt.getJwtFromPayload payload
  pure $ Cookie.defaultSetCookie
    { Cookie.setCookieName = ByteString.UTF8.fromString context.authCookieName
    , Cookie.setCookieMaxAge = pure context.authCookieMaxAge
    , Cookie.setCookieSecure = True
    , Cookie.setCookieHttpOnly = True
    , Cookie.setCookieSameSite = pure Cookie.sameSiteStrict
    , Cookie.setCookieValue = ByteString.Lazy.toStrict jwt
    }

getPayloadFromAuthRequest :: (Has.Has Type.AppContext r, Has.Has Jwt.Type.JwtContext r, MonadReader r m, MonadIO m, Aeson.FromJSON a) => Network.Wai.Request -> Maybe.MaybeT m a
getPayloadFromAuthRequest request = do
  context :: Type.AppContext <- asks Has.getter
  let headers = Network.Wai.requestHeaders request
  token <- Maybe.MaybeT $ pure do
    cookies <- Cookie.parseCookies <$> lookup Network.HTTP.Types.Header.hCookie headers
    lookup (ByteString.UTF8.fromString $ context.authCookieName) cookies
  claims <- Jwt.findJwtClaimsSetFromJwt token
  Jwt.getPayloadFromJwtClaimsSet claims

createUserToken :: (Has.Has Type.AppContext r, Has.Has Jwt.Type.JwtContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m, JOSE.JWS.MonadRandom m) => Type.UserTokenCreateInput -> m Text
createUserToken input = do
  let authToken = Type.AuthToken input.user.id
  Except.modifyError (const Type.AuthTokenException) do
    token <- ByteString.Lazy.toStrict <$> Jwt.getJwtFromPayload authToken
    pure $ Text.pack $ ByteString.UTF8.toString token

createUser :: (Has.Has Type.AppContext r, MonadReader r m, Except.MonadError Type.Exception m, MonadIO m) => Type.UserCreateInput -> m Type.User
createUser input = do
  context :: Type.AppContext <- asks Has.getter
  payload <- validateRegisterForm $ Type.RegisterForm
    { email = input.email
    , username = input.username
    , password = input.password
    }
  liftIO do
    PostgreSQL.withTransaction context.database do
      [PostgreSQL.Only userId :: PostgreSQL.Only Int] <- do
        PostgreSQL.query
          context.database 
          [PostgreSQL.SqlQQ.sql|
            INSERT INTO
              honduit.users(uid)
            VALUES
              (?)
            RETURNING
              id
          |]
          ( PostgreSQL.Only payload.email )
      [user :: Type.User] <- do
        PostgreSQL.query
          context.database
          [PostgreSQL.SqlQQ.sql|
            INSERT INTO
              honduit.user_auth_credentials(user_id, email, password)
            VALUES
              (?,?,?)
            ;
            INSERT INTO
              honduit.user_profiles(user_id, username)
            VALUES
              (?,?)
            ;
            SELECT
              users.id,
              user_auth_credentials.email,
              user_profiles.username,
              user_profiles.bio,
              user_profiles.image
            FROM
              honduit.users as users
            JOIN 
              honduit.user_auth_credentials as user_auth_credentials ON user_auth_credentials.user_id = users.id
            JOIN 
              honduit.user_profiles as user_profiles ON user_profiles.user_id = users.id
            WHERE
              users.id = ?
          |]
          ( userId
          , payload.email
          , payload.password
          , userId
          , payload.username
          , userId
          )
      pure user

getRegisterFormConfig :: (Has.Has Type.AppContext r, MonadReader r m, Except.MonadError Type.Exception m) => m Type.RegisterFormValidationRule
getRegisterFormConfig = do
  context :: Type.AppContext <- asks Has.getter
  pure $ Type.RegisterFormValidationRule
    { email = context.userAuthCredentialValidationRule.email
    , username = context.profileValidationRule.username
    , password = context.userAuthCredentialValidationRule.password
    }

validateRegisterForm :: (Has.Has Type.AppContext r, MonadReader r m, Except.MonadError Type.Exception m) => Type.RegisterForm -> m Type.RegisterForm
validateRegisterForm payload = do
  context :: Type.AppContext <- asks Has.getter
  config <- getRegisterFormConfig
  Except.modifyError Type.RegisterFormValidationException do
    Except.except $ Validation.toEither $ Type.RegisterForm
      <$>
        flip Foldable.foldMap config.email
          ( first ( \e -> mempty { Type.email = e } :: Type.RegisterFormValidationRule )
          . FieldValidator.validateText payload.email
          )
      <*>
        flip Foldable.foldMap config.username
          ( first ( \e -> mempty { Type.username = e } :: Type.RegisterFormValidationRule )
          . FieldValidator.validateText payload.username
          )
      <*>
        flip Foldable.foldMap config.password
          ( first ( \e -> mempty { Type.password = e } :: Type.RegisterFormValidationRule )
          . FieldValidator.validateText payload.password
          )

getUser :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.UserSearchInput -> m Type.User
getUser filter = do
  context :: Type.AppContext <- asks Has.getter
  result <- liftIO $ tryIO do
    [user :: Type.User] <- do
      PostgreSQL.query
        context.database
        [PostgreSQL.SqlQQ.sql|
          SELECT
            users.id,
            user_auth_credentials.email,
            user_profiles.username,
            user_profiles.bio,
            user_profiles.image
          FROM
            honduit.users as users
          JOIN 
            honduit.user_auth_credentials as user_auth_credentials ON user_auth_credentials.user_id = users.id
          JOIN 
            honduit.user_profiles as user_profiles ON user_profiles.user_id = users.id
          WHERE
            (? IS NULL OR users.id = ?) AND
            (? IS NULL OR user_auth_credentials.email = ?) AND
            (? IS NULL OR user_auth_credentials.password = ?)
        |]
        ( filter.userId
        , filter.userId
        , filter.email
        , filter.email
        , filter.password
        , filter.password
        )
    pure user
  case result of
    Left _ -> Except.throwError Type.ResourceNotFoundException
    Right user -> pure user

getUserByLogin :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.UserLoginPayload -> m Type.User
getUserByLogin input = do
  context :: Type.AppContext <- asks Has.getter
  payload <- validateLoginForm $ Type.LoginForm
    { email = input.email
    , password = input.password
    }
  getUser $ Type.UserSearchInput
    { userId = Nothing
    , email = pure payload.email
    , password = pure payload.password
    }

getLoginFormConfig :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m) => m Type.LoginFormValidationRule
getLoginFormConfig = do
    context :: Type.AppContext <- asks Has.getter
    pure $ Type.LoginFormValidationRule
      { email = context.userAuthCredentialValidationRule.email
      , password = context.userAuthCredentialValidationRule.password
      }

validateLoginForm :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m) => Type.LoginForm -> m Type.LoginForm
validateLoginForm payload = do
  config <- getLoginFormConfig
  Except.modifyError Type.LoginFormValidationException do
    Except.except $ Validation.toEither $ Type.LoginForm
      <$>
        flip Foldable.foldMap config.email
          ( first ( \e -> mempty { Type.email = e } ::  Type.LoginFormValidationRule )
          . FieldValidator.validateText payload.email
          )
      <*>
        flip Foldable.foldMap config.password
          ( first ( \e -> mempty { Type.password = e } ::  Type.LoginFormValidationRule )
          . FieldValidator.validateText payload.password
          )

updateUser :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.UserUpdateInput -> m Type.User
updateUser input = do
  context :: Type.AppContext <- asks Has.getter
  payload <- validateUserSettingsForm $ Type.UserSettingsForm
    { image = input.image
    , username = input.username
    , bio = input.bio
    , email = input.email
    , password = fromMaybe "" input.password
    }
  liftIO do
    PostgreSQL.withTransaction context.database do
      [user :: Type.User] <- do
        PostgreSQL.query
          context.database
          [PostgreSQL.SqlQQ.sql|
            UPDATE 
              honduit.user_auth_credentials
            SET
              email = COALESCE(NULLIF(?, ''), email),
              password = COALESCE(NULLIF(?, ''), password)
            WHERE
              user_id = ?
            ;
            UPDATE 
              honduit.user_profiles
            SET
              username = COALESCE(NULLIF(?, ''), username),
              bio = NULLIF(?, ''),
              image = NULLIF(?, '')
            WHERE
              user_id = ?
            ;
            SELECT
              users.id,
              user_auth_credentials.email,
              user_profiles.username,
              user_profiles.bio,
              user_profiles.image
            FROM
              honduit.users as users
            JOIN 
              honduit.user_auth_credentials as user_auth_credentials ON user_auth_credentials.user_id = users.id
            JOIN 
              honduit.user_profiles as user_profiles ON user_profiles.user_id = users.id
            WHERE
              users.id = ?
          |]
          ( payload.email
          , payload.password
          , input.userId
          , payload.username
          , payload.bio
          , payload.image
          , input.userId
          , input.userId
          )
      pure user

getUserSettingsFormConfig :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m) => m Type.UserSettingsFormValidationRule
getUserSettingsFormConfig = do
  context :: Type.AppContext <- asks Has.getter
  pure $ Type.UserSettingsFormValidationRule
    { image = context.profileValidationRule.image
    , username = context.profileValidationRule.username
    , bio = context.profileValidationRule.bio
    , email = context.userAuthCredentialValidationRule.email
    , password = [Type.Noop] ++ filter
        ( \rule -> rule /= Type.Required )
        context.userAuthCredentialValidationRule.password
    }

validateUserSettingsForm :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m) => Type.UserSettingsForm -> m Type.UserSettingsForm
validateUserSettingsForm payload = do
  config <- getUserSettingsFormConfig
  Except.modifyError Type.UserSettingsFormValidationException do
    Except.except $ Validation.toEither $ Type.UserSettingsForm
      <$>
        flip Foldable.foldMap config.image
          ( first ( \e -> mempty { Type.image = e } ::  Type.UserSettingsFormValidationRule )
          . FieldValidator.validateText payload.image
          )
      <*>
        flip Foldable.foldMap config.username
          ( first ( \e -> mempty { Type.username = e } ::  Type.UserSettingsFormValidationRule )
          . FieldValidator.validateText payload.username
          )
      <*>
        flip Foldable.foldMap config.bio
          ( first ( \e -> mempty { Type.bio = e } ::  Type.UserSettingsFormValidationRule )
          . FieldValidator.validateText payload.bio
          )
      <*>
        flip Foldable.foldMap config.email
          ( first ( \e -> mempty { Type.email = e } ::  Type.UserSettingsFormValidationRule )
          . FieldValidator.validateText payload.email
          )
      <*>
        flip Foldable.foldMap config.password
          ( first ( \e -> mempty { Type.password = e } ::  Type.UserSettingsFormValidationRule )
          . FieldValidator.validateText payload.password
          )

getUserSettings :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.UserSettingsSearchInput -> m Type.UserSettings
getUserSettings filter = do
  context :: Type.AppContext <- asks Has.getter
  result <- liftIO $ tryIO do
    [settings] <- PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          users.id,
          user_auth_credentials.email,
          user_profiles.username,
          user_auth_credentials.password,
          user_profiles.bio,
          user_profiles.image
        FROM
          honduit.users as users
        JOIN 
          honduit.user_auth_credentials as user_auth_credentials ON user_auth_credentials.user_id = users.id
        JOIN 
          honduit.user_profiles as user_profiles ON user_profiles.user_id = users.id
        WHERE
          users.id = ?
      |]
      ( PostgreSQL.Only filter.userId
      )
    pure settings
  case result of 
    Left _ -> Except.throwError Type.ResourceNotFoundException
    Right settings -> pure settings

getProfile :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ProfileSearchInput -> m Type.Profile
getProfile filter = do
  context :: Type.AppContext <- asks Has.getter
  result <- liftIO $ tryIO do
    [profile :: Type.Profile] <- PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          user_profiles.id,
          user_profiles.user_id,
          user_profiles.username,
          user_profiles.bio,
          user_profiles.image,
          EXISTS(
            SELECT
              1
            FROM
              honduit.user_profile_followers as user_profile_followers
            WHERE
              user_profile_followers.follower_id = COALESCE(?, -1) AND
              user_profile_followers.following_id = user_profiles.id
          ) as following
        FROM
          honduit.user_profiles as user_profiles
        WHERE
          (? IS NULL OR user_profiles.id = ?) AND
          (? IS NULL OR user_profiles.user_id = ?)
      |]
      ( do
          userProfile <- filter.userProfile
          pure $ userProfile.id
      , filter.profileId
      , filter.profileId
      , filter.userId
      , filter.userId
      )
    pure profile
  case result of 
    Left _ -> Except.throwError Type.ResourceNotFoundException
    Right profile -> pure profile

createProfileFollower :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ProfileFollowerCreateInput -> m Type.Profile
createProfileFollower input = do
  context :: Type.AppContext <- asks Has.getter
  void $ liftIO do
    PostgreSQL.execute
      context.database
      [PostgreSQL.SqlQQ.sql|
        INSERT INTO
          honduit.user_profile_followers(following_id, follower_id)
        VALUES
          (?,?)
      |]
      ( input.followingId
      , input.follower.id
      )
  getProfile $ Type.ProfileSearchInput
    { userProfile = pure input.follower
    , profileId = pure input.followingId
    , userId = Nothing
    }

deleteProfileFollower :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ProfileFollowerDeleteInput -> m Type.Profile
deleteProfileFollower input = do
  context :: Type.AppContext <- asks Has.getter
  void $ liftIO do
    PostgreSQL.execute
      context.database
      [PostgreSQL.SqlQQ.sql|
        DELETE
        FROM
          honduit.user_profile_followers
        WHERE
          following_id = ? AND
          follower_id = ?
      |]
      ( input.followingId
      , input.follower.id
      )
  getProfile $ Type.ProfileSearchInput
    { userProfile = pure input.follower
    , profileId = pure input.followingId
    , userId = Nothing
    }

getArticle :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleSearchInput -> m Type.Article
getArticle filter = do
  context :: Type.AppContext <- asks Has.getter
  result <- liftIO $ tryIO do
    [article :: Type.Article] <- PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        WITH
          authors as (
            SELECT
              user_profiles.id,
              user_profiles.user_id,
              user_profiles.username,
              user_profiles.bio,
              user_profiles.image,
              EXISTS(
                SELECT
                  1
                FROM
                  honduit.user_profile_followers as user_profile_followers
                WHERE
                  user_profile_followers.follower_id = COALESCE(?, -1) AND
                  user_profile_followers.following_id = user_profiles.id
              ) as following
            FROM
              honduit.user_profiles as user_profiles
          ),
          formatted_articles as (
            SELECT
              articles.slug,
              articles.id,
              articles.title,
              articles.description,
              articles.body,
              ARRAY(
                SELECT
                  ROW(
                    _tags.id,
                    _tags.label
                  )
                FROM
                  honduit.article_tags as article_tags
                JOIN 
                  honduit.tags as _tags ON _tags.id = article_tags.tag_id
                WHERE
                  article_tags.article_id = articles.id
              ) as tags,
              articles.created_at,
              articles.updated_at,
              EXISTS(
                SELECT
                  1
                FROM
                  honduit.article_favorites as article_favorites
                WHERE
                  article_favorites.article_id = articles.id AND
                  article_favorites.user_profile_id = COALESCE(?, -1)
              ) as following_article,
              ( SELECT
                  COUNT(*)
                FROM
                  honduit.article_favorites as article_favorites
                WHERE
                  article_favorites.article_id = articles.id
              ) as favorites_count,
              authors.id as author_id,
              authors.user_id as author_user_id,
              authors.username as author_username,
              authors.bio as author_bio,
              authors.image as author_image,
              authors.following as author_following
            FROM
              honduit.articles as articles
            JOIN
              authors as authors ON authors.id = articles.author_id
          )
        SELECT
          *
        FROM
          formatted_articles as formatted_articles
        WHERE
          formatted_articles.slug = ?
        LIMIT
          1
      |]
      ( do
          userProfile <- filter.userProfile
          pure $ userProfile.id
      , do
          userProfile <- filter.userProfile
          pure $ userProfile.id
      , filter.slug
      )
    pure article
  case result of
    Left _ -> Except.throwError Type.ResourceNotFoundException
    Right article -> pure article

getArticles :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticlesSearchInput -> m [Type.Article]
getArticles filter = do
  context :: Type.AppContext <- asks Has.getter
  liftIO do
    PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        WITH
          authors as (
            SELECT
              user_profiles.id,
              user_profiles.user_id,
              user_profiles.username,
              user_profiles.bio,
              user_profiles.image,
              EXISTS(
                SELECT
                  1
                FROM
                  honduit.user_profile_followers as user_profile_followers
                WHERE
                  user_profile_followers.follower_id = COALESCE(?, -1) AND
                  user_profile_followers.following_id = user_profiles.id
              ) as following
            FROM
              honduit.user_profiles as user_profiles
          ),
          formatted_articles as (
            SELECT
              articles.slug,
              articles.id,
              articles.title,
              articles.description,
              articles.body,
              ARRAY(
                SELECT
                  ROW(
                    _tags.id,
                    _tags.label
                  )
                FROM
                  honduit.article_tags as article_tags
                JOIN 
                  honduit.tags as _tags ON _tags.id = article_tags.tag_id
                WHERE
                  article_tags.article_id = articles.id
              ) as tags,
              articles.created_at,
              articles.updated_at,
              EXISTS(
                SELECT
                  1
                FROM
                  honduit.article_favorites as article_favorites
                WHERE
                  article_favorites.article_id = articles.id AND
                  article_favorites.user_profile_id = COALESCE(?, -1)
              ) as following_article,
              ( SELECT
                  COUNT(*)
                FROM
                  honduit.article_favorites as article_favorites
                WHERE
                  article_favorites.article_id = articles.id
              ) as favorites_count,
              authors.id as author_id,
              authors.user_id as author_user_id,
              authors.username as author_username,
              authors.bio as author_bio,
              authors.image as author_image,
              authors.following as author_following
            FROM
              honduit.articles as articles
            JOIN
              authors as authors ON authors.id = articles.author_id
          )
        SELECT
          *
        FROM
          formatted_articles as formatted_articles
        WHERE
          ( ? is NULL OR formatted_articles.author_id = ? ) AND
          ( ? is NULL OR EXISTS(
            SELECT
              1
            FROM
              honduit.user_profile_followers as user_profile_followers
            WHERE
              user_profile_followers.following_id = formatted_articles.author_id AND
              user_profile_followers.follower_id = ?
          ) ) AND
          ( ? is NULL OR EXISTS(
            SELECT
              tags.*
            FROM
              UNNEST(formatted_articles.tags) AS tags(id INT, label VARCHAR)
            WHERE
              tags.id = ?
          ) ) AND
          ( ? is NULL OR EXISTS(
            SELECT
              1
            FROM 
              honduit.article_favorites as article_favorites
            WHERE
              article_favorites.article_id = formatted_articles.id AND
              article_favorites.user_profile_id = ?
          ) )
        ORDER BY
          created_at DESC
        LIMIT
          GREATEST(0, ?)
        OFFSET
          GREATEST(0, ?)
      |]
      ( do
          userProfile <- filter.userProfile
          pure $ userProfile.id
      , do
          userProfile <- filter.userProfile
          pure $ userProfile.id
      , filter.authorId
      , filter.authorId
      , filter.followerProfileId
      , filter.followerProfileId
      , filter.tagId
      , filter.tagId
      , filter.favoritedProfileId
      , filter.favoritedProfileId
      , filter.limit
      , filter.offset
      )

getArticleCount :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleSearchInput -> m Int
getArticleCount filter = do
  context :: Type.AppContext <- asks Has.getter
  liftIO do
    [PostgreSQL.Only count] <- PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          COUNT(*)
        FROM
          honduit.articles as articles
        WHERE
          articles.slug = ? OR
          articles.slug LIKE ?
      |]
      ( filter.slug
      , filter.slug <> "-%"
      )
    pure count

getArticlesCount :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticlesSearchInput -> m Int
getArticlesCount filter = do
  context :: Type.AppContext <- asks Has.getter
  liftIO do
    [PostgreSQL.Only count] <- PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        WITH
          authors as (
            SELECT
              user_profiles.id,
              user_profiles.user_id,
              user_profiles.username,
              user_profiles.bio,
              user_profiles.image,
              FALSE as following
            FROM
              honduit.user_profiles as user_profiles
          ),
          formatted_articles as (
            SELECT
              articles.slug,
              articles.id,
              articles.title,
              articles.description,
              articles.body,
              ARRAY(
                SELECT
                  ROW(
                    _tags.id,
                    _tags.label
                  )
                FROM
                  honduit.article_tags as article_tags
                JOIN 
                  honduit.tags as _tags ON _tags.id = article_tags.tag_id
                WHERE
                  article_tags.article_id = articles.id
              ) as tags,
              articles.created_at,
              articles.updated_at,
              FALSE as following_article,
              ( SELECT
                  COUNT(*)
                FROM
                  honduit.article_favorites as article_favorites
                WHERE
                  article_favorites.article_id = articles.id
              ) as favorites_count,
              authors.id as author_id,
              authors.user_id as author_user_id,
              authors.username as author_username,
              authors.bio as author_bio,
              authors.image as author_image,
              authors.following as author_following
            FROM
              honduit.articles as articles
            JOIN
              authors as authors ON authors.id = articles.author_id
          )
        SELECT
          COUNT(*)
        FROM
          formatted_articles as formatted_articles
        WHERE
          ( ? is NULL OR EXISTS(
            SELECT
              1
            FROM
              honduit.user_profile_followers as user_profile_followers
            WHERE
              user_profile_followers.following_id = formatted_articles.author_id AND
              user_profile_followers.follower_id = ?
          ) ) AND
          ( ? is NULL OR EXISTS(
            SELECT
              tags.*
            FROM
              UNNEST(formatted_articles.tags) AS tags(id INT, label VARCHAR)
            WHERE
              tags.id = ?
          ) ) AND
          ( ? is NULL OR EXISTS(
            SELECT
              1
            FROM 
              honduit.article_favorites as article_favorites
            WHERE
              article_favorites.article_id = formatted_articles.id AND
              article_favorites.user_profile_id = ?
          ) )
      |]
      ( filter.followerProfileId
      , filter.followerProfileId
      , filter.tagId
      , filter.tagId
      , filter.favoritedProfileId
      , filter.favoritedProfileId
      )
    pure count

getPaginatedArticles :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticlesSearchInput -> m (Type.Paginated [Type.Article])
getPaginatedArticles filter = do
  context :: Type.AppContext <- asks Has.getter
  let offset = fromMaybe 0 filter.offset
      limit = Foldable.minimum
        [ fromMaybe context.defaultArticleLimit filter.limit
        , context.defaultArticleLimit
        ]
      filterWithDefault = filter 
        { Type.offset = pure offset
        , Type.limit = pure limit
        } :: Type.ArticlesSearchInput
  articles <- getArticles filterWithDefault
  articlesCount <- getArticlesCount filterWithDefault
  pure $ Type.Paginated
    { item = articles
    , itemCount = articlesCount
    , itemOffset = offset
    , page = floor (fromIntegral offset / fromIntegral limit :: Double)
    , pageSize = limit
    , pageCount = ceiling (fromIntegral articlesCount / fromIntegral limit :: Double)
    }

createArticleSlug :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Text -> m Text
createArticleSlug text = do
  slug <- maybe (Except.throwError Type.ResourceNotFoundException) pure do
    Slug.fromText text
  count <- getArticleCount $ Type.ArticleSearchInput
    { userProfile = Nothing
    , slug = slug
    }
  pure $ fromMaybe slug do
    guard (count > 0)
    let suffix = fromString $ show $ count + 1
    pure $ Slug.truncate [slug, suffix]

createArticle :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleCreateInput -> m Type.Article
createArticle input = do
  context :: Type.AppContext <- asks Has.getter
  payload <- validateArticleEditorForm $ Type.ArticleEditorForm
    { title = input.title
    , description = input.description
    , body = input.body
    , tags = input.tags
    }
  slug <- createArticleSlug payload.title
  liftIO do
    PostgreSQL.withTransaction context.database do
      PostgreSQL.execute
        context.database
          [PostgreSQL.SqlQQ.sql|
            INSERT INTO
              honduit.articles(slug,title,description,body,author_id)
            VALUES
              (?,?,?,?,?)
          |]
          ( slug
          , payload.title
          , payload.description
          , payload.body
          , input.author.id
          )
      Except.modifyError (IO.Error.userError . show) do
        flip Reader.runReaderT context do
          article <- getArticle $ Type.ArticleSearchInput
            { userProfile = pure input.author
            , slug = slug
            }
          tags <- for (List.nub payload.tags) \title -> do
            createOrGetTag $ Type.TagCreateInput
              { title = title
              }
          void do
            createArticleTags $ Type.ArticleTagsCreateInput
              { slug = slug
              , tags = tags
              }
          pure article

updateArticle :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleUpdateInput -> m Type.Article
updateArticle input = do
  context :: Type.AppContext <- asks Has.getter
  payload <- validateArticleEditorForm $ Type.ArticleEditorForm
    { title = input.title
    , description = input.description
    , body = input.body
    , tags = input.tags
    }
  liftIO do
    PostgreSQL.withTransaction context.database do
      PostgreSQL.execute
        context.database
          [PostgreSQL.SqlQQ.sql|
            UPDATE
              honduit.articles
            SET
              title = COALESCE(NULLIF(?, ''), title),
              description = ?,
              body = ?
            WHERE
              slug = ? AND
              author_id = ?
          |]
          ( payload.title
          , payload.description
          , payload.body
          , input.slug
          , input.author.id
          )
      Except.modifyError (IO.Error.userError . show) do
        flip Reader.runReaderT context do
          article <- getArticle $ Type.ArticleSearchInput
            { userProfile = pure input.author
            , slug = input.slug
            }
          tags <- for (List.nub payload.tags) \title -> do
            createOrGetTag $ Type.TagCreateInput
              { title = title
              }
          void do
            updateArticleTags $ Type.ArticleTagsUpdateInput
              { slug = input.slug
              , tags = tags
              }
          pure article

deleteArticle :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleDeleteInput -> m ()
deleteArticle input = do
  context :: Type.AppContext <- asks Has.getter
  article <- getArticle $ Type.ArticleSearchInput
    { userProfile = pure input.author
    , slug = input.slug
    }
  when (article.author.id /= input.author.id) do
    Except.throwError Type.AuthorizationException
  liftIO do
    PostgreSQL.withTransaction context.database do
      void do
        PostgreSQL.execute
          context.database
          [PostgreSQL.SqlQQ.sql|
            DELETE FROM
              honduit.article_favorites
            WHERE
              article_id = ?
          |]
          ( PostgreSQL.Only article.id
          )
      void do
        PostgreSQL.execute
          context.database
          [PostgreSQL.SqlQQ.sql|
            DELETE FROM
              honduit.article_tags
            WHERE
              article_id = ?
          |]
          ( PostgreSQL.Only article.id
          )
      void do
        PostgreSQL.execute
          context.database
          [PostgreSQL.SqlQQ.sql|
            DELETE FROM
              honduit.article_comments
            WHERE
              article_id = ?
          |]
          ( PostgreSQL.Only article.id
          )
      commentIds <- do
        rows :: [PostgreSQL.Only Int] <- PostgreSQL.query
          context.database
          [PostgreSQL.SqlQQ.sql|
            DELETE FROM
              honduit.article_comments
            WHERE
              article_id = ?
            RETURNING
              comment_id
          |]
          ( PostgreSQL.Only article.id
          )
        pure $ rows <&> \(PostgreSQL.Only commentId) -> commentId
      void do 
        PostgreSQL.execute
          context.database
          [PostgreSQL.SqlQQ.sql|
            DELETE FROM
              honduit.comments
            WHERE
              id IN ?
          |]
          ( PostgreSQL.Only $ PostgreSQL.In commentIds
          )
      void do
        PostgreSQL.execute
          context.database
          [PostgreSQL.SqlQQ.sql|
            DELETE FROM
              honduit.articles
            WHERE
              id = ?
          |]
          ( PostgreSQL.Only article.id
          )

getArticleEditorFormConfig :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m) => m Type.ArticleEditorFormValidationRule
getArticleEditorFormConfig = do
  context :: Type.AppContext <- asks Has.getter
  pure $ Type.ArticleEditorFormValidationRule
    { title = context.articleValidationRule.title
    , description = context.articleValidationRule.description
    , body = context.articleValidationRule.body
    , tags = context.tagValidationRule.title
    }

validateArticleEditorForm :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m) => Type.ArticleEditorForm -> m Type.ArticleEditorForm
validateArticleEditorForm payload = do
  config <- getArticleEditorFormConfig
  Except.modifyError Type.ArticleEditorFormValidationException do
    Except.except $ Validation.toEither $ Type.ArticleEditorForm
      <$>
        flip Foldable.foldMap config.title
          ( first ( \e -> mempty { Type.title = e } :: Type.ArticleEditorFormValidationRule )
          . FieldValidator.validateText payload.title
          )
      <*>
        flip Foldable.foldMap config.description
          ( first ( \e -> mempty { Type.description = e } :: Type.ArticleEditorFormValidationRule )
          . FieldValidator.validateText payload.description
          )
      <*>
        flip Foldable.foldMap config.body
          ( first ( \e -> mempty { Type.body = e } :: Type.ArticleEditorFormValidationRule )
          . FieldValidator.validateText payload.body
          )
      <*>
        Foldable.foldr (liftA2 (:)) (pure [])
          ( payload.tags <&> \tag ->
              flip Foldable.foldMap config.tags
                ( first ( \e -> mempty { Type.tags = e } :: Type.ArticleEditorFormValidationRule )
                . FieldValidator.validateText tag
                )
          )

getComment :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.CommentSearchInput -> m Type.Comment
getComment filter = do
  context :: Type.AppContext <- asks Has.getter
  result <- liftIO $ tryIO do
    [comment :: Type.Comment] <- PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        WITH
          authors as (
            SELECT
              user_profiles.id,
              user_profiles.user_id,
              user_profiles.username,
              user_profiles.bio,
              user_profiles.image
              EXISTS(
                SELECT
                  1
                FROM
                  honduit.user_profile_followers as user_profile_followers
                WHERE
                  user_profile_followers.follower_id = COALESCE(?, -1) AND
                  user_profile_followers.following_id = user_profiles.id
              ) as following
            FROM
              honduit.user_profiles as user_profiles
          )
        SELECT
          comments.id,
          comments.body,
          authors.id as author_id,
          authors.user_id as author_user_id,
          authors.username as author_username,
          authors.bio as author_bio,
          authors.image as author_image,
          authors.following as author_following,
          comments.created_at,
          comments.updated_at
        FROM
          honduit.comments as comments
        JOIN
          authors as authors ON authors.id = comments.author_id
        WHERE
          comments.id = ?
      |]
      ( do
          userProfile <- filter.userProfile
          pure $ userProfile.id
      , filter.commentId
      )
    pure comment
  case result of 
    Left _ -> Except.throwError Type.ResourceNotFoundException
    Right comment -> pure comment

getArticleComments :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleCommentsSearchInput -> m [Type.Comment]
getArticleComments filter = do
  context :: Type.AppContext <- asks Has.getter
  liftIO do
    PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        WITH
          authors as (
            SELECT
              user_profiles.id,
              user_profiles.user_id,
              user_profiles.username,
              user_profiles.bio,
              user_profiles.image,
              EXISTS(
                SELECT
                  1
                FROM
                  honduit.user_profile_followers as user_profile_followers
                WHERE
                  user_profile_followers.follower_id = COALESCE(?, -1) AND
                  user_profile_followers.following_id = user_profiles.id
              ) as following
            FROM
              honduit.user_profiles as user_profiles
          )
        SELECT
          comments.id,
          comments.body,
          authors.id as author_id,
          authors.user_id as author_user_id,
          authors.username as author_username,
          authors.bio as author_bio,
          authors.image as author_image,
          authors.following as author_following,
          comments.created_at,
          comments.updated_at
        FROM
          honduit.comments as comments
        JOIN
          authors as authors ON authors.id = comments.author_id
        JOIN
          honduit.article_comments as article_comments ON article_comments.comment_id = comments.id
        WHERE
          EXISTS(
            SELECT
              1
            FROM
              honduit.articles as articles
            WHERE
              ? = articles.slug AND
              article_comments.article_id = articles.id
          )
      |]
      ( do
          userProfile <- filter.userProfile
          pure $ userProfile.id
      , filter.slug
      )

createArticleComment :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleCommentCreateInput -> m Type.Comment
createArticleComment input = do
  context :: Type.AppContext <- asks Has.getter
  payload <- validateCommentCreateForm $ Type.CommentCreateForm
    { body = input.body
    }
  article <- getArticle $ Type.ArticleSearchInput
    { userProfile = pure input.author
    , slug = input.slug
    }
  commentId <- liftIO do
    PostgreSQL.withTransaction context.database do
      [PostgreSQL.Only commentId :: PostgreSQL.Only Int] <- do
        PostgreSQL.query
          context.database
          [PostgreSQL.SqlQQ.sql|
            INSERT INTO
              honduit.comments(body, author_id)
            VALUES
              (?,?)
            RETURNING
              id
          |]
          ( payload.body
          , input.author.id
          )
      void do 
        PostgreSQL.execute
          context.database
          [PostgreSQL.SqlQQ.sql|
            INSERT INTO
              honduit.article_comments(article_id, comment_id)
            VALUES
              (?,?)
          |]
          ( article.id
          , commentId
          )
      pure commentId
  getComment $ Type.CommentSearchInput
    { userProfile = pure input.author
    , commentId = commentId
    }

getCommentCreateFormConfig :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m) => m Type.CommentCreateFormValidationRule
getCommentCreateFormConfig = do
  context :: Type.AppContext <- asks Has.getter
  pure $ Type.CommentCreateFormValidationRule
    { body = context.commentValidationRule.body
    }

validateCommentCreateForm :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m) => Type.CommentCreateForm -> m Type.CommentCreateForm
validateCommentCreateForm payload = do
  context :: Type.AppContext <- asks Has.getter
  Except.modifyError Type.CommentCreateFormValidationException do
    Except.except $ Validation.toEither $ Type.CommentCreateForm
      <$>
        flip Foldable.foldMap context.commentValidationRule.body
          ( first ( \e -> mempty { Type.body = e } :: Type.CommentCreateFormValidationRule )
          . FieldValidator.validateText payload.body
          )

deleteComment :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.CommentDeleteInput -> m ()
deleteComment input = do
  context :: Type.AppContext <- asks Has.getter
  void $ liftIO $ PostgreSQL.withTransaction context.database do
    void do
      PostgreSQL.execute
        context.database
        [PostgreSQL.SqlQQ.sql|
          DELETE FROM
            honduit.article_comments
          WHERE
            id = ? AND
        |]
        ( PostgreSQL.Only input.commentId
        )
    void do
      PostgreSQL.execute
        context.database
        [PostgreSQL.SqlQQ.sql|
          DELETE FROM
            honduit.comments
          WHERE
            id = ? AND
            author_id = ?
        |]
        ( input.commentId
        , input.author.id
        )

createArticleFavorite :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleFavoriteCreateInput -> m Type.Article
createArticleFavorite input = do
  context :: Type.AppContext <- asks Has.getter
  article <- getArticle $ Type.ArticleSearchInput
    { userProfile = pure input.profile
    , slug = input.slug
    }
  void $ liftIO do
    PostgreSQL.execute
      context.database
      [PostgreSQL.SqlQQ.sql|
        INSERT INTO
          honduit.article_favorites(article_id, user_profile_id)
        VALUES
          (?,?)
      |]
      ( article.id
      , input.profile.id
      )
  getArticle $ Type.ArticleSearchInput
    { userProfile = pure input.profile
    , slug = input.slug
    }

deleteArticleFavorite :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleFavoriteDeleteInput -> m Type.Article
deleteArticleFavorite input = do
  context :: Type.AppContext <- asks Has.getter
  article <- getArticle $ Type.ArticleSearchInput
    { userProfile = pure input.profile
    , slug = input.slug
    }
  void $ liftIO do
    PostgreSQL.execute
      context.database
      [PostgreSQL.SqlQQ.sql|
        DELETE FROM
          honduit.article_favorites
        WHERE
          article_id = ? AND
          user_profile_id = ?
      |]
      ( article.id
      , input.profile.id
      )
  getArticle $ Type.ArticleSearchInput
    { userProfile = pure input.profile
    , slug = input.slug
    }

getTags :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.TagsSearchInput -> m [Type.Tag]
getTags filter = do
  context :: Type.AppContext <- asks Has.getter
  liftIO do
    PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          tags.id,
          tags.label
        FROM
          honduit.tags as tags
        INNER JOIN
          honduit.article_tags as article_tags
          ON article_tags.tag_id = tags.id
        GROUP BY
          tags.id,
          tags.label
        ORDER BY
          COUNT(article_tags.article_id) DESC
        LIMIT
          ?
      |]
      ( PostgreSQL.Only filter.limit
      )

getPopularTags :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => m [Type.Tag]
getPopularTags = do
  context :: Type.AppContext <- asks Has.getter
  getTags $ Type.TagsSearchInput
    { limit = pure context.defaultPopularTagLimit
    }

getTag :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.TagSearchInput -> m Type.Tag
getTag filter = do
  context :: Type.AppContext <- asks Has.getter
  result <- liftIO $ tryIO do
    [tag :: Type.Tag] <- PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          tags.id as id,
          tags.label as title
        FROM
          honduit.tags as tags
        WHERE
          (? IS NULL OR tags.id = ?) AND
          (? IS NULL OR tags.label = ?)
      |]
      ( filter.tagId
      , filter.tagId
      , filter.title
      , filter.title
      )
    pure tag
  case result of 
    Left _ -> Except.throwError Type.ResourceNotFoundException
    Right tag -> pure tag

createOrGetTag :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.TagCreateInput -> m Type.Tag
createOrGetTag payload = do
  context :: Type.AppContext <- asks Has.getter
  result <- Except.tryError $ getTag $ Type.TagSearchInput
    { title = pure payload.title
    , tagId = Nothing
    }
  case result of 
    Left exception -> case exception of 
      Type.ResourceNotFoundException -> createTag payload
      _ -> Except.throwError exception
    Right tag -> pure tag

createTag :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.TagCreateInput -> m Type.Tag
createTag payload = do
  context :: Type.AppContext <- asks Has.getter
  tagId <- liftIO do
    [PostgreSQL.Only tagId :: PostgreSQL.Only Int] <- PostgreSQL.query
      context.database
      [PostgreSQL.SqlQQ.sql|
        INSERT INTO
          honduit.tags(label)
        VALUES
          (?)
        RETURNING
          id
      |]
      ( PostgreSQL.Only payload.title
      )
    pure tagId
  getTag $ Type.TagSearchInput
    { tagId = pure tagId
    , title = Nothing
    }

getArticleTags :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleTagsSearchInput -> m [Type.Tag]
getArticleTags filter = do
  context :: Type.AppContext <- asks Has.getter
  liftIO do
    PostgreSQL.query
      context.database 
      [PostgreSQL.SqlQQ.sql|
        SELECT
          tags.id as id,
          tags.label as title
        FROM
          honduit.tags as tags
        INNER JOIN
          honduit.article_tags as article_tags
        ON
          article_tags.tag_id = tags.id
        JOIN
          honduit.articles as articles
        ON
          article_tags.article_id = articles.id
        WHERE
          (? IS NULL OR articles.slug = ?) AND
          (? IS NULL OR articles.id = ?)
      |]
      ( filter.slug
      , filter.slug
      , filter.articleId
      , filter.articleId
      )

createArticleTags :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleTagsCreateInput -> m [Type.Tag]
createArticleTags input = do
  context :: Type.AppContext <- asks Has.getter
  article <- getArticle $ Type.ArticleSearchInput
    { userProfile = Nothing
    , slug = input.slug
    }
  liftIO do
    PostgreSQL.withTransaction context.database do
      for input.tags \tag -> do
        result :: Maybe Type.Tag <- List.headMaybe <$> do
          PostgreSQL.query
            context.database
            [PostgreSQL.SqlQQ.sql|
              SELECT
                tags.id,
                tags.label
              FROM
                honduit.tags as tags
              INNER JOIN
                honduit.article_tags as article_tags
              ON
                article_tags.tag_id = tags.id
              WHERE
                article_tags.article_id = ? AND
                article_tags.tag_id = ? 
            |]
            ( article.id
            , tag.id
            )
        case result of
          Just _ -> do
            pure tag
          Nothing -> do
            do
              PostgreSQL.execute
                context.database
                  [PostgreSQL.SqlQQ.sql|
                    INSERT INTO
                      honduit.article_tags(article_id, tag_id)
                    VALUES
                      (?,?)
                  |]
                  ( article.id
                  , tag.id
                  )
            pure tag

deleteArticleTags :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleTagsDeleteInput -> m ()
deleteArticleTags input = do
  context :: Type.AppContext <- asks Has.getter
  article <- getArticle $ Type.ArticleSearchInput
    { userProfile = Nothing
    , slug = input.slug
    }
  void $ liftIO do
    PostgreSQL.execute
      context.database 
      [PostgreSQL.SqlQQ.sql|
        DELETE FROM
          honduit.article_tags
        WHERE
          article_id = ?
      |]
      ( PostgreSQL.Only article.id
      )


updateArticleTags :: (Has.Has Type.AppContext r, Except.MonadError Type.Exception m, MonadReader r m, MonadIO m) => Type.ArticleTagsUpdateInput -> m [Type.Tag]
updateArticleTags input = do
  context :: Type.AppContext <- asks Has.getter
  liftIO do
    PostgreSQL.withTransaction context.database do
      Except.modifyError (IO.Error.userError . show) do
        flip Reader.runReaderT context do
          deleteArticleTags $ Type.ArticleTagsDeleteInput
            { slug = input.slug
            }
          createArticleTags $ Type.ArticleTagsCreateInput
            { slug = input.slug
            , tags = input.tags
            }

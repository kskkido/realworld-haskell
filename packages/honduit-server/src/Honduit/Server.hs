module Honduit.Server where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Except.Extension as Except
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Lucid
import qualified System.IO as IO
import qualified Network.Wai
import qualified Web.Cookie as Cookie
import qualified Servant
import qualified Servant.API
import qualified Servant.Server.StaticFiles
import qualified Servant.Server.Experimental.Auth
import qualified Honduit.Core.Type as Type
import qualified Honduit.Core.Session as Session
import qualified Honduit.Core.ArticleFeedFilter as ArticleFeedFilter
import qualified Honduit.Core.Capability.Repository as Capability.Repository
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.Server as Capability.Server
import qualified Honduit.Core.Capability.Paginator as Capability.Paginator
import qualified Honduit.Core.Capability.ArticleEditorForm as Capability.ArticleEditorForm
import qualified Honduit.Core.Capability.CommentCreateForm as Capability.CommentCreateForm
import qualified Honduit.Core.Capability.LoginForm as Capability.LoginForm
import qualified Honduit.Core.Capability.RegisterForm as Capability.RegisterForm
import qualified Honduit.Core.Capability.UserSettingsForm as Capability.UserSettingsForm
import qualified Honduit.Api
import qualified Honduit.Api.Type
import qualified Honduit.View.Template.Page.Article as Template.Page.Article
import qualified Honduit.View.Template.Page.ArticleCreate as Template.Page.ArticleCreate
import qualified Honduit.View.Template.Page.ArticleUpdate as Template.Page.ArticleUpdate
import qualified Honduit.View.Template.Page.Home as Template.Page.Home
import qualified Honduit.View.Template.Page.Login as Template.Page.Login
import qualified Honduit.View.Template.Page.NotFound as Template.Page.NotFound
import qualified Honduit.View.Template.Page.Profile as Template.Page.Profile
import qualified Honduit.View.Template.Page.Register as Template.Page.Register
import qualified Honduit.View.Template.Page.UserSettings as Template.Page.UserSettings
import qualified Honduit.View.Template.Fragment.ArticleCommentSection as Template.Fragment.ArticleCommentSection
import qualified Honduit.View.Template.Fragment.ArticleFavoriteCounter as Template.Fragment.ArticleFavoriteCounter
import qualified Honduit.View.Template.Fragment.ArticlePreviewFavoriteCounter as Template.Fragment.ArticlePreviewFavoriteCounter
import qualified Honduit.View.Template.Fragment.ProfileFollowButton as Template.Fragment.ProfileFollowButton
import qualified Honduit.View.Template.Fragment.HomeArticleFeed as Template.Fragment.HomeArticleFeed
import qualified Honduit.View.Template.Fragment.ProfileArticleFeed as Template.Fragment.ProfileArticleFeed
import qualified Honduit.View.Template.Fragment.ArticleMeta as Template.Fragment.ArticleMeta
import qualified Honduit.View.Template.Fragment.ArticlePreview as Template.Fragment.ArticlePreview
import qualified Honduit.Server.Api
import qualified Honduit.Interpreter.View as View
import qualified Honduit.Interpreter.View.Type as View.Type

application ::
  ( MonadIO m
  , Except.MonadError Type.Exception m
  , Capability.Repository.Repository m
  , Capability.Asset.Asset m
  , Capability.Server.Server m
  , Capability.Paginator.Paginator m
  , Capability.ArticleEditorForm.ArticleEditorForm m
  , Capability.CommentCreateForm.CommentCreateForm m
  , Capability.LoginForm.LoginForm m
  , Capability.RegisterForm.RegisterForm m
  , Capability.UserSettingsForm.UserSettingsForm m
  ) =>
  (forall a. m a -> Servant.Handler a) ->
  m Servant.Application
application resolve = do
  publicAssetsFilePath <- Capability.Server.publicAssetsFilePath
  pure $
    Servant.serveWithContextT
      Honduit.Server.Api.proxy
      ( Servant.Server.Experimental.Auth.AuthHandler (resolve . handleAuthOptionalPath) Servant.:.
        Servant.Server.Experimental.Auth.AuthHandler (resolve . handleAuthRequiredPath) Servant.:.
        Servant.Server.Experimental.Auth.AuthHandler (resolve . handleUnauthRequiredPath) Servant.:.
        Servant.EmptyContext
      )
      resolve
      Honduit.Server.Api.RootRouter
        { api = Honduit.Api.RootRouter
          { api = Honduit.Api.ApiRouter
            { users = Honduit.Api.ApiUsersRouter
              { register = handleApiUserRegisterPost
              , login = handleApiUserLoginPost
              }
            , user = Honduit.Api.ApiUserRouter
              { show = handleApiUserGet
              , edit = handleApiUserPut
              }
            , profiles = Honduit.Api.ApiProfilesRouter
              { show = handleApiProfileGet
              , follow = \profileId -> Honduit.Api.ApiProfileFollowerRouter
                { new = handleApiProfileFollowPost profileId
                , delete = handleApiProfileFollowDelete profileId
                } 
              }
            , articles = Honduit.Api.ApiArticlesRouter
              { index = handleApiArticlesGet
              , new = handleApiArticlePost
              , show = handleApiArticleGet
              , edit = handleApiArticlePut
              , delete = handleApiArticleDelete
              , feed = handleApiArticlesFeedGet
              , comments = \slug -> Honduit.Api.ApiArticleCommentsRouter
                { index = handleApiArticleCommentsGet slug
                , new = handleApiArticleCommentPost slug
                , delete = handleApiArticleCommentDelete slug
                }
              , favorite = \slug -> Honduit.Api.ApiArticleFavoriteRouter
                { new = handleApiArticleFavoritePost slug
                , delete = handleApiArticleFavoriteDelete slug
                }
              }
            , tags = Honduit.Api.ApiTagsRouter
              { index = handleApiTagsGet
              }
            }
          }
        , view = Honduit.Server.Api.ViewRouter
          { index = handleViewGetHome
          , editor = Honduit.Server.Api.ViewEditorRouter
            { index = handleViewGetArticleCreateEditor
            , new = handleViewPostArticleCreateEditor
            , show = handleViewGetArticleUpdateEditor
            , edit = handleViewPostArticleUpdateEditor
            }
          , article = Honduit.Server.Api.ViewArticleRouter
            { show = handleViewGetArticle
            , delete = handleViewDeleteArticle
            }
          , settings = Honduit.Server.Api.ViewSettingsRouter
            { index = handleViewGetUserProfileSettings
            , edit = handleViewPostUserProfileSettings
            }
          , profile = Honduit.Server.Api.ViewProfileRouter
            { show = handleViewGetUserProfile
            }
          , login = Honduit.Server.Api.ViewLoginRouter
            { index = handleViewGetLogin
            , login = handleViewPostLogin
            }
          , logout = Honduit.Server.Api.ViewLogoutRouter
            { logout = handleViewGetLogout
            }
          , register = Honduit.Server.Api.ViewRegisterRouter
            { index = handleViewGetRegister
            , register = handleViewPostRegister
            }
          , fragments = Honduit.Server.Api.ViewFragmentsRouter
              { homeArticleFeed = Honduit.Server.Api.ViewFragmentHomeArticleFeedRouter
                { personal = handleViewFragmentsHomeArticleFeedPersonal
                , global = handleViewFragmentsHomeArticleFeedGlobal
                }
              , profileArticleFeed = \profileId -> Honduit.Server.Api.ViewFragmentProfileArticleFeedRouter
                { published = handleViewFragmentsProfileArticleFeedPublished profileId
                , favorited = handleViewFragmentsProfileArticleFeedFavorited profileId
                }
              , profileFollowButton = \profileId -> Honduit.Server.Api.ViewFragmentProfileFollowButtonRouter
                { follow = handleViewFragmentsProfileFollowButtonFollow profileId
                , unfollow = handleViewFragmentsProfileFollowButtonUnfollow profileId
                }
              , articleCommentSection = \slug -> Honduit.Server.Api.ViewFragmentArticleCommentSectionRouter
                { createComment = handleViewFragmentsArticleCommentSectionCreateComment slug
                , deleteComment = handleViewFragmentsArticleCommentSectionDeleteComment slug
                } 
              , articleMeta = \slug -> Honduit.Server.Api.ViewFragmentArticleMetaRouter
                { followAuthor = handleViewFragmentsArticleMetaFollowAuthor slug
                , unfollowAuthor = handleViewFragmentsArticleMetaUnfollowAuthor slug
                , favoriteArticle = handleViewFragmentsArticleMetaFavoriteArticle slug
                , unfavoriteArticle = handleViewFragmentsArticleMetaUnfavoriteArticle slug
                }
              , articlePreview = \slug -> Honduit.Server.Api.ViewFragmentArticlePreviewRouter
                { favoriteArticle = handleViewFragmentsArticlePreviewFavoriteArticle slug
                , unfavoriteArticle = handleViewFragmentsArticlePreviewUnfavoriteArticle slug
                }
              }
          , error = Honduit.Server.Api.ViewErrorRouter
            { notFound = handleViewGet404
            }
          , assets = Servant.Server.StaticFiles.serveDirectoryFileServer publicAssetsFilePath
          }
        }

clientToSession :: (Monad m, Capability.Repository.Repository m) => Honduit.Api.Type.Client -> m Type.Session
clientToSession (Honduit.Api.Type.Unauthorized _) = do
  pure $ Type.Unauthorized Type.UnauthorizedSession
clientToSession (Honduit.Api.Type.Authorized authorized) = do
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  pure $ Type.Authorized Type.AuthorizedSession
    { userProfile = userProfile
    }

handlerFromIO :: IO (Either Type.Exception a) -> Servant.Handler a
handlerFromIO action = do
  result <- liftIO action
  Except.liftEither $ flip first result \exception -> do
    let errBody = fromString $ show $ Aeson.toEncoding exception
    case exception of 
      Type.AuthorizationException ->
        Servant.err302
          { Servant.errBody = errBody
          , Servant.errHeaders =
            [ ("Location", "/login")
            ]
          }
      Type.ResourceNotFoundException ->
        Servant.err302
          { Servant.errBody = errBody
          , Servant.errHeaders =
            [ ("Location", "/error/404")
            ]
          }
      _ ->
        Servant.err400
          { Servant.errBody = errBody
          }

handleAuthOptionalPath :: (Monad m, Capability.Server.Server m) => Network.Wai.Request -> m Honduit.Api.Type.Client
handleAuthOptionalPath request = do
  result <- Maybe.runMaybeT do
    Capability.Server.getPayloadFromAuthRequest request
  case result of
    Nothing -> do
      pure $ Honduit.Api.Type.Unauthorized Honduit.Api.Type.UnauthorizedClient
    Just (user :: Type.User) -> do
      let client = Honduit.Api.Type.AuthorizedClient
            { userId = user.id
            , accessToken = ""
            }
      pure $ Honduit.Api.Type.Authorized client

handleAuthRequiredPath :: (Except.MonadError Type.Exception m, Capability.Server.Server m) => Network.Wai.Request -> m Honduit.Api.Type.AuthorizedClient
handleAuthRequiredPath request = do
  result <- handleAuthOptionalPath request
  case result of
    Honduit.Api.Type.Unauthorized _ -> do
      Except.throwError Type.AuthorizationException
    Honduit.Api.Type.Authorized client -> do
      pure client

handleUnauthRequiredPath :: (Except.MonadError Type.Exception m, Capability.Server.Server m) => Network.Wai.Request -> m Honduit.Api.Type.UnauthorizedClient
handleUnauthRequiredPath request = do
  result <- handleAuthOptionalPath request
  case result of
    Honduit.Api.Type.Unauthorized client -> do
      pure client
    Honduit.Api.Type.Authorized _ -> do
      Except.throwError Type.AuthorizationException

handleApiUserRegisterPost :: (Monad m, Capability.Repository.Repository m) => Honduit.Api.Type.UnauthorizedClient -> Honduit.Api.Type.UserRegisterPostRequestBody -> m Honduit.Api.Type.UserRegisterPostResponseBody
handleApiUserRegisterPost _ body = do
  authUser <- do
    user <- Capability.Repository.createUser $ Type.UserCreateInput
      { email = body.user.email
      , username = body.user.username
      , password = body.user.password
      }
    token <- Capability.Repository.createUserToken $ Type.UserTokenCreateInput
      { user = user
      }
    pure $ Type.AuthUser
      { id = user.id
      , email = user.email
      , token = token
      , username = user.username
      , bio = user.bio
      , image = user.image
      }
  pure $ Honduit.Api.Type.UserRegisterPostResponseBody
    { user = Honduit.Api.Type.User
      { email = authUser.email
      , token = authUser.token
      , username = authUser.username
      , bio = authUser.bio
      , image = authUser.image
      }
    }

handleApiUserLoginPost :: (Monad m, Capability.Repository.Repository m) => Honduit.Api.Type.UnauthorizedClient -> Honduit.Api.Type.UserLoginPostRequestBody -> m Honduit.Api.Type.UserLoginPostResponseBody
handleApiUserLoginPost _ body = do
  authUser <- do
    user <- Capability.Repository.getUserByLogin $ Type.UserLoginPayload
      { email = body.user.email
      , password = body.user.password
      }
    token <- Capability.Repository.createUserToken $ Type.UserTokenCreateInput
      { user = user
      }
    pure $ Type.AuthUser
      { id = user.id
      , email = user.email
      , token = token
      , username = user.username
      , bio = user.bio
      , image = user.image
      }
  pure $ Honduit.Api.Type.UserLoginPostResponseBody
    { user = Honduit.Api.Type.User
      { email = authUser.email
      , token = authUser.token
      , username = authUser.username
      , bio = authUser.bio
      , image = authUser.image
      }
    }


handleApiUserGet :: (Monad m, Capability.Repository.Repository m) => Honduit.Api.Type.AuthorizedClient -> m Honduit.Api.Type.UserGetResponseBody
handleApiUserGet authorized = do
  authUser <- do
    user <- Capability.Repository.getUser $ Type.UserSearchInput
      { userId = pure authorized.userId
      , email = Nothing
      , password = Nothing
      }
    pure $ Type.AuthUser
      { id = user.id
      , email = user.email
      , token = authorized.accessToken
      , username = user.username
      , bio = user.bio
      , image = user.image
      }
  pure $ Honduit.Api.Type.UserGetResponseBody
    { user = Honduit.Api.Type.User
      { email = authUser.email
      , token = authUser.token
      , username = authUser.username
      , bio = authUser.bio
      , image = authUser.image
      }
    }

handleApiUserPut :: (Monad m, Capability.Repository.Repository m) => Honduit.Api.Type.AuthorizedClient -> Honduit.Api.Type.UserPutRequestBody -> m Honduit.Api.Type.UserPutResponseBody
handleApiUserPut authorized body = do
  authUser <- do
    user <- Capability.Repository.updateUser $ Type.UserUpdateInput
      { userId = authorized.userId
      , email = body.user.email
      , username = body.user.username
      , password = body.user.password
      , bio = body.user.bio
      , image = body.user.image
      }
    pure $ Type.AuthUser
      { id = user.id
      , email = user.email
      , token = authorized.accessToken
      , username = user.username
      , bio = user.bio
      , image = user.image
      }
  pure $ Honduit.Api.Type.UserPutResponseBody
    { user = Honduit.Api.Type.User
      { email = authUser.email
      , token = authUser.token
      , username = authUser.username
      , bio = authUser.bio
      , image = authUser.image
      }
    }

handleApiProfileGet :: (Monad m, Capability.Repository.Repository m) => Int -> Honduit.Api.Type.Client -> m Honduit.Api.Type.ProfileGetResponseBody
handleApiProfileGet profileId client = do
  session <- clientToSession client
  profile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userProfile = Session.toUserProfile session
    , profileId = pure profileId
    , userId = Nothing
    }
  pure $ Honduit.Api.Type.ProfileGetResponseBody
    { profile = Honduit.Api.Type.Profile
      { id = profile.id
      , username = profile.username
      , bio = fromMaybe "" profile.bio
      , image = fromMaybe "" profile.image
      , following = profile.following
      }
    }

handleApiProfileFollowPost :: (Monad m, Capability.Repository.Repository m) => Int -> Honduit.Api.Type.AuthorizedClient -> m Honduit.Api.Type.ProfileFollowPostResponseBody
handleApiProfileFollowPost profileId authorized = do
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  profile <- Capability.Repository.createProfileFollower $ Type.ProfileFollowerCreateInput
    { followingId = profileId
    , follower = userProfile
    }
  pure $ Honduit.Api.Type.ProfileFollowPostResponseBody
    { profile = Honduit.Api.Type.Profile
      { id = profile.id
      , username = profile.username
      , bio = fromMaybe "" profile.bio
      , image = fromMaybe "" profile.image
      , following = profile.following
      }
    }

handleApiProfileFollowDelete :: (Monad m, Capability.Repository.Repository m) => Int -> Honduit.Api.Type.AuthorizedClient -> m Honduit.Api.Type.ProfileFollowDeleteResponseBody
handleApiProfileFollowDelete profileId authorized = do
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  profile <- Capability.Repository.deleteProfileFollower $ Type.ProfileFollowerDeleteInput
    { followingId = profileId
    , follower = userProfile
    }
  pure $ Honduit.Api.Type.ProfileFollowDeleteResponseBody
    { profile = Honduit.Api.Type.Profile
      { id = profile.id
      , username = profile.username
      , bio = fromMaybe "" profile.bio
      , image = fromMaybe "" profile.image
      , following = profile.following
      }
    }

handleApiArticlesGet :: (Monad m, Capability.Repository.Repository m) => Honduit.Api.Type.Client -> Maybe Int ->  Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> m Honduit.Api.Type.ArticlesGetResponseBody
handleApiArticlesGet client tagId authorId followerProfileId favoritedProfileId limit offset = do
  let articleFeedFilter = Type.ArticleFeedFilter
        { tagId = tagId
        , authorId = authorId
        , followerProfileId = followerProfileId
        , favoritedProfileId = favoritedProfileId
        , limit = limit
        , offset = offset
        }
  session <- clientToSession client
  articles <- Capability.Repository.getPaginatedArticles $ Type.ArticlesSearchInput
    { userProfile = Session.toUserProfile session
    , tagId = articleFeedFilter.tagId
    , authorId = articleFeedFilter.authorId
    , followerProfileId = articleFeedFilter.followerProfileId
    , favoritedProfileId = articleFeedFilter.favoritedProfileId
    , limit = articleFeedFilter.limit
    , offset = articleFeedFilter.offset
    }
  pure $ Honduit.Api.Type.ArticlesGetResponseBody
    { articles = articles.item <&> \article -> Honduit.Api.Type.Article
      { slug = article.slug
      , title = article.title
      , description = article.description
      , body = article.body
      , tagList = article.tags <&> \tag -> tag.title
      , createdAt = fromString $ show article.createdAt
      , updatedAt = fromString $ show article.updatedAt
      , favorited = article.favorited
      , favoritesCount = article.favoritesCount
      , author = Honduit.Api.Type.Profile
        { id = article.author.id
        , username = article.author.username
        , bio = fromMaybe "" article.author.bio
        , image = fromMaybe "" article.author.image
        , following = article.author.following
        }
      }
    , articlesCount = articles.itemCount
    }

handleApiArticlePost :: (Monad m, Capability.Repository.Repository m) => Honduit.Api.Type.AuthorizedClient -> Honduit.Api.Type.ArticlePostRequestBody -> m Honduit.Api.Type.ArticlePostResponseBody
handleApiArticlePost authorized body = do
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.createArticle $ Type.ArticleCreateInput
    { author = userProfile
    , title = body.title
    , description = body.description
    , body = body.body
    , tags = fromMaybe [] body.tagList
    }
  pure $ Honduit.Api.Type.ArticlePostResponseBody
    { article = Honduit.Api.Type.Article
      { slug = article.slug
      , title = article.title
      , description = article.description
      , body = article.body
      , tagList = article.tags <&> \tag -> tag.title
      , createdAt = fromString $ show article.createdAt
      , updatedAt = fromString $ show article.updatedAt
      , favorited = article.favorited
      , favoritesCount = article.favoritesCount
      , author = Honduit.Api.Type.Profile
        { id = article.author.id
        , username = article.author.username
        , bio = fromMaybe "" article.author.bio
        , image = fromMaybe "" article.author.image
        , following = article.author.following
        }
      }
    }

handleApiArticlesFeedGet :: (Monad m, Capability.Repository.Repository m) => Honduit.Api.Type.Client -> Maybe Int ->  Maybe Int -> m Honduit.Api.Type.ArticlesFeedGetResponseBody
handleApiArticlesFeedGet client limit offset = do
  let articleFeedFilter = Type.ArticleFeedFilter
        { tagId = Nothing
        , authorId = Nothing
        , followerProfileId = Nothing
        , favoritedProfileId = Nothing
        , limit = limit
        , offset = offset
        }
  session <- clientToSession client
  articles <- Capability.Repository.getPaginatedArticles $ Type.ArticlesSearchInput
    { userProfile = Session.toUserProfile session
    , tagId = articleFeedFilter.tagId
    , authorId = articleFeedFilter.authorId
    , followerProfileId = articleFeedFilter.followerProfileId
    , favoritedProfileId = articleFeedFilter.favoritedProfileId
    , limit = articleFeedFilter.limit
    , offset = articleFeedFilter.offset
    }
  pure Honduit.Api.Type.ArticlesFeedGetResponseBody
    { articles = articles.item <&> \article -> Honduit.Api.Type.Article
      { slug = article.slug
      , title = article.title
      , description = article.description
      , body = article.body
      , tagList = article.tags <&> \tag -> tag.title
      , createdAt = fromString $ show article.createdAt
      , updatedAt = fromString $ show article.updatedAt
      , favorited = article.favorited
      , favoritesCount = article.favoritesCount
      , author = Honduit.Api.Type.Profile
        { id = article.author.id
        , username = article.author.username
        , bio = fromMaybe "" article.author.bio
        , image = fromMaybe "" article.author.image
        , following = article.author.following
        }
      }
    , articlesCount = articles.itemCount
    }

handleApiArticleGet :: (Monad m, Capability.Repository.Repository m) => Text -> Honduit.Api.Type.Client -> m Honduit.Api.Type.ArticleGetResponseBody
handleApiArticleGet slug client = do
  session <- clientToSession client
  article <- Capability.Repository.getArticle $ Type.ArticleSearchInput
    { userProfile = Session.toUserProfile session
    , slug = slug 
    }
  pure $ Honduit.Api.Type.ArticleGetResponseBody
    { article = Honduit.Api.Type.Article
      { slug = article.slug
      , title = article.title
      , description = article.description
      , body = article.body
      , tagList = article.tags <&> \tag -> tag.title
      , createdAt = fromString $ show article.createdAt
      , updatedAt = fromString $ show article.updatedAt
      , favorited = article.favorited
      , favoritesCount = article.favoritesCount
      , author = Honduit.Api.Type.Profile
        { id = article.author.id
        , username = article.author.username
        , bio = fromMaybe "" article.author.bio
        , image = fromMaybe "" article.author.image
        , following = article.author.following
        }
      }
    }

handleApiArticlePut :: (Monad m, Capability.Repository.Repository m) => Text -> Honduit.Api.Type.AuthorizedClient -> Honduit.Api.Type.ArticlePutRequestBody -> m Honduit.Api.Type.ArticlePutResponseBody
handleApiArticlePut slug authorized body = do
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.updateArticle $ Type.ArticleUpdateInput
    { slug = slug
    , title = body.article.title
    , description = body.article.description
    , body = body.article.body
    , tags = body.article.tagList
    , author = userProfile
    }
  pure $ Honduit.Api.Type.ArticlePutResponseBody
    { article = Honduit.Api.Type.Article
      { slug = article.slug
      , title = article.title
      , description = article.description
      , body = article.body
      , tagList = article.tags <&> \tag -> tag.title
      , createdAt = fromString $ show article.createdAt
      , updatedAt = fromString $ show article.updatedAt
      , favorited = article.favorited
      , favoritesCount = article.favoritesCount
      , author = Honduit.Api.Type.Profile
        { id = article.author.id
        , username = article.author.username
        , bio = fromMaybe "" article.author.bio
        , image = fromMaybe "" article.author.image
        , following = article.author.following
        }
      }
    }

handleApiArticleDelete :: (Monad m, Capability.Repository.Repository m) => Text -> Honduit.Api.Type.AuthorizedClient -> m Servant.API.NoContent
handleApiArticleDelete slug authorized = do
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  Capability.Repository.deleteArticle $ Type.ArticleDeleteInput
    { slug = slug
    , author = userProfile
    }
  pure Servant.API.NoContent

handleApiArticleCommentsGet :: (Monad m, Capability.Repository.Repository m) => Text -> Honduit.Api.Type.Client -> m Honduit.Api.Type.ArticleCommentsGetResponseBody
handleApiArticleCommentsGet slug client = do
  session <- clientToSession client
  comments <- Capability.Repository.getArticleComments $ Type.ArticleCommentsSearchInput
    { userProfile = Session.toUserProfile session
    , slug = slug
    }
  pure $ Honduit.Api.Type.ArticleCommentsGetResponseBody
    { comments = comments <&> \comment -> Honduit.Api.Type.Comment
      { id = comment.id
      , body = comment.body
      , author = Honduit.Api.Type.Profile
        { id = comment.author.id
        , username = comment.author.username
        , bio = fromMaybe "" comment.author.bio
        , image = fromMaybe "" comment.author.image
        , following = comment.author.following
        }
      , createdAt = fromString $ show comment.createdAt
      , updatedAt = fromString $ show comment.updatedAt
      }
    }

handleApiArticleCommentPost :: (Monad m, Capability.Repository.Repository m) => Text -> Honduit.Api.Type.AuthorizedClient -> Honduit.Api.Type.ArticleCommentPostRequestBody -> m Honduit.Api.Type.ArticleCommentPostResponseBody
handleApiArticleCommentPost slug authorized body = do
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  comment <- Capability.Repository.createArticleComment $ Type.ArticleCommentCreateInput
    { author = userProfile
    , body = body.comment.body
    , slug = slug
    }
  pure $ Honduit.Api.Type.ArticleCommentPostResponseBody
    { comment = Honduit.Api.Type.Comment
      { id = comment.id
      , body = comment.body
      , author = Honduit.Api.Type.Profile
        { id = comment.author.id
        , username = comment.author.username
        , bio = fromMaybe "" comment.author.bio
        , image = fromMaybe "" comment.author.image
        , following = comment.author.following
        }
      , createdAt = fromString $ show comment.createdAt
      , updatedAt = fromString $ show comment.updatedAt
      }
    }

handleApiArticleCommentDelete :: (Monad m, Capability.Repository.Repository m) => Text -> Int -> Honduit.Api.Type.AuthorizedClient -> m Servant.API.NoContent
handleApiArticleCommentDelete _ commentId authorized = do
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  Capability.Repository.deleteComment $ Type.CommentDeleteInput
    { commentId = commentId
    , author = userProfile
    }
  pure Servant.API.NoContent

handleApiArticleFavoritePost :: (Monad m, Capability.Repository.Repository m) => Text -> Honduit.Api.Type.AuthorizedClient -> m Honduit.Api.Type.ArticleFavoritePostResponseBody
handleApiArticleFavoritePost slug authorized = do
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.createArticleFavorite $ Type.ArticleFavoriteCreateInput
    { profile = userProfile
    , slug = slug
    }
  pure $ Honduit.Api.Type.ArticleFavoritePostResponseBody
    { article = Honduit.Api.Type.Article
      { slug = article.slug
      , title = article.title
      , description = article.description
      , body = article.body
      , tagList = article.tags <&> \tag -> tag.title
      , createdAt = fromString $ show article.createdAt
      , updatedAt = fromString $ show article.updatedAt
      , favorited = article.favorited
      , favoritesCount = article.favoritesCount
      , author = Honduit.Api.Type.Profile
        { id = article.author.id
        , username = article.author.username
        , bio = fromMaybe "" article.author.bio
        , image = fromMaybe "" article.author.image
        , following = article.author.following
        }
      }
    }

handleApiArticleFavoriteDelete :: (Monad m, Capability.Repository.Repository m) => Text -> Honduit.Api.Type.AuthorizedClient -> m Honduit.Api.Type.ArticleFavoriteDeleteResponseBody
handleApiArticleFavoriteDelete slug authorized = do
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.deleteArticleFavorite $ Type.ArticleFavoriteDeleteInput
    { profile = userProfile
    , slug = slug
    }
  pure $ Honduit.Api.Type.ArticleFavoriteDeleteResponseBody
    { article = Honduit.Api.Type.Article
      { slug = article.slug
      , title = article.title
      , description = article.description
      , body = article.body
      , tagList = article.tags <&> \tag -> tag.title
      , createdAt = fromString $ show article.createdAt
      , updatedAt = fromString $ show article.updatedAt
      , favorited = article.favorited
      , favoritesCount = article.favoritesCount
      , author = Honduit.Api.Type.Profile
        { id = article.author.id
        , username = article.author.username
        , bio = fromMaybe "" article.author.bio
        , image = fromMaybe "" article.author.image
        , following = article.author.following
        }
      }
    }

handleApiTagsGet :: (Monad m, Capability.Repository.Repository m) => Honduit.Api.Type.Client -> m Honduit.Api.Type.TagsGetResponseBody
handleApiTagsGet _ = do
  tags <- Capability.Repository.getTags $ Type.TagsSearchInput
    { limit = Nothing
    }
  pure $ Honduit.Api.Type.TagsGetResponseBody
    { tags = tags <&> \tag -> tag.title
    }

handleViewGetHome :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Honduit.Api.Type.Client -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> m ByteString.Lazy.ByteString
handleViewGetHome client tagId authorId favorited limit offset = do
  liftIO do
    IO.putStrLn "Handling web home"
  session <- clientToSession client
  let feedFilter = Type.ArticleFeedFilter
        { tagId = tagId
        , authorId = authorId
        , followerProfileId = do
            userProfile <- Session.toUserProfile session
            pure userProfile.id
        , favoritedProfileId = favorited
        , limit = limit
        , offset = offset
        }
  articles <- Capability.Repository.getPaginatedArticles $ Type.ArticlesSearchInput
    { userProfile = Session.toUserProfile session
    , tagId = feedFilter.tagId
    , authorId = feedFilter.authorId
    , followerProfileId = feedFilter.followerProfileId
    , favoritedProfileId = feedFilter.favoritedProfileId
    , limit = feedFilter.limit
    , offset = feedFilter.offset
    }
  tags <- Capability.Repository.getPopularTags
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Page.Home.render $ Template.Page.Home.Props
        { feedFilter = feedFilter
        , articles = articles
        , tags = tags
        }

handleViewGetArticleCreateEditor :: (MonadIO m, Capability.Repository.Repository m, Capability.ArticleEditorForm.ArticleEditorForm m, Capability.Asset.Asset m) => Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewGetArticleCreateEditor authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "Handling web get article create"
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Page.ArticleCreate.render $ Template.Page.ArticleCreate.Props
        { values = Type.ArticleEditorForm
          { title = mempty
          , description = mempty
          , body = mempty
          , tags = mempty
          }
        , errors = Type.ArticleEditorFormValidationRule
          { title = mempty
          , description = mempty
          , body = mempty
          , tags = mempty
          }
        }

handleViewPostArticleCreateEditor :: 
  (MonadIO m, Except.MonadError Type.Exception m, Capability.Repository.Repository m, Capability.Server.Server m, Capability.ArticleEditorForm.ArticleEditorForm m, Capability.Asset.Asset m) =>
  Honduit.Api.Type.AuthorizedClient ->
  Type.ArticleEditorForm ->
  m
    ( Servant.Union
      '[ Servant.API.WithStatus 200 ByteString.Lazy.ByteString
       , Servant.API.WithStatus 303
         ( Servant.API.Headers
           '[ Servant.API.Header "Location" Text
            ]
            Servant.API.NoContent
         )
       ]
    )
handleViewPostArticleCreateEditor authorized form = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "Handling web post article create editor"
  session <- clientToSession client
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  result <- Except.tryError do
    void $ Capability.ArticleEditorForm.validate form
    Capability.Repository.createArticle $ Type.ArticleCreateInput
      { author = userProfile
      , title = form.title
      , description = form.description
      , body = form.body
      , tags = form.tags
      }
  case result of
    Left exception -> do
      let viewContext = View.Type.ViewContext
            { session = session
            }
      html <- View.unlift viewContext do
        Lucid.renderBST do
          Template.Page.ArticleCreate.render $ Template.Page.ArticleCreate.Props
            { values = form
            , errors = case exception of
                Type.ArticleEditorFormValidationException errors -> errors
                _ -> mempty
            }
      Servant.respond $ Servant.API.WithStatus @200 html
    Right article -> do
      Servant.respond $ Servant.API.WithStatus @303
        ( Servant.API.addHeader
            ( Text.intercalate "/"
              [ "/article"
              , article.slug
              ]
            )
            Servant.API.NoContent
          ::
          ( Servant.API.Headers
             '[ Servant.API.Header "Location" Text
              ]
              Servant.API.NoContent
          )
        )

handleViewGetArticleUpdateEditor :: (MonadIO m, Except.MonadError Type.Exception m, Capability.Repository.Repository m, Capability.ArticleEditorForm.ArticleEditorForm m, Capability.Asset.Asset m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewGetArticleUpdateEditor slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "Handling web get article create"
  session <- clientToSession client
  article <- Capability.Repository.getArticle $ Type.ArticleSearchInput
    { userProfile = Session.toUserProfile session
    , slug = slug
    }
  when (article.author.userId /= authorized.userId) do
    Except.throwError Type.ResourceNotFoundException
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Page.ArticleUpdate.render $ Template.Page.ArticleUpdate.Props
        { values = Type.ArticleEditorForm
          { title = article.title
          , description = article.description
          , body = article.body
          , tags = article.tags <&> \tag -> tag.title
          }
        , errors = Type.ArticleEditorFormValidationRule
          { title = mempty
          , description = mempty
          , body = mempty
          , tags = mempty
          }
        }

handleViewPostArticleUpdateEditor :: 
  (MonadIO m, Except.MonadError Type.Exception m, Capability.Repository.Repository m, Capability.Server.Server m, Capability.ArticleEditorForm.ArticleEditorForm m, Capability.Asset.Asset m) =>
  Text ->
  Honduit.Api.Type.AuthorizedClient ->
  Type.ArticleEditorForm ->
  m
    ( Servant.Union
      '[ Servant.API.WithStatus 200 ByteString.Lazy.ByteString
       , Servant.API.WithStatus 303
         ( Servant.API.Headers
           '[ Servant.API.Header "Location" Text
            ]
            Servant.API.NoContent
         )
       ]
    )
handleViewPostArticleUpdateEditor slug authorized form = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "Handling web post article create editor"
  session <- clientToSession client
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  result <- Except.tryError do
    void $ Capability.ArticleEditorForm.validate form
    Capability.Repository.updateArticle $ Type.ArticleUpdateInput
      { slug = slug
      , title = form.title
      , description = form.description
      , body = form.body
      , tags = form.tags
      , author = userProfile
      }
  case result of
    Left exception -> do
      let viewContext = View.Type.ViewContext
            { session = session
            }
      html <- View.unlift viewContext do
        Lucid.renderBST do
          Template.Page.ArticleUpdate.render $ Template.Page.ArticleUpdate.Props
            { values = form
            , errors = case exception of
                Type.ArticleEditorFormValidationException errors -> errors
                _ -> mempty
            }
      Servant.respond $ Servant.API.WithStatus @200 html
    Right article -> do
      Servant.respond $ Servant.API.WithStatus @303
        ( Servant.API.addHeader
            ( Text.intercalate "/"
              [ "/article"
              , article.slug
              ]
            )
            Servant.API.NoContent
          ::
          ( Servant.API.Headers
             '[ Servant.API.Header "Location" Text
              ]
              Servant.API.NoContent
           )
        )

handleViewGetArticle :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m, Capability.CommentCreateForm.CommentCreateForm m) => Text -> Honduit.Api.Type.Client -> m ByteString.Lazy.ByteString
handleViewGetArticle slug client = do
  liftIO do
    IO.putStrLn "Handling web article"
  session <- clientToSession client
  article <- Capability.Repository.getArticle $ Type.ArticleSearchInput
    { userProfile = Session.toUserProfile session
    , slug = slug
    }
  comments <- Capability.Repository.getArticleComments $ Type.ArticleCommentsSearchInput
    { userProfile = Session.toUserProfile session
    , slug = slug
    }
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Page.Article.render $ Template.Page.Article.Props
        { article = article
        , comments = comments
        , commentFormValues = Type.CommentCreateForm
            { body = mempty
            }
        , commentFormErrors = mempty
        }

handleViewDeleteArticle ::
  (MonadIO m, Capability.Repository.Repository m) =>
  Text ->
  Honduit.Api.Type.AuthorizedClient ->
  m
    ( Servant.API.Headers
      '[ Servant.API.Header "HX-Redirect" Text
       ]
      Servant.API.NoContent
    )
handleViewDeleteArticle slug authorized = do
  liftIO do
    IO.putStrLn "handleViewDeleteArticle"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  Capability.Repository.deleteArticle $ Type.ArticleDeleteInput
    { slug = slug
    , author = userProfile
    }
  pure
    ( Servant.API.addHeader (Text.pack "/")
      Servant.API.NoContent
    )

handleViewGetUserProfileSettings :: (MonadIO m, Capability.Repository.Repository m, Capability.UserSettingsForm.UserSettingsForm m, Capability.Asset.Asset m) => Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewGetUserProfileSettings authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "Handling web login"
  userSettings <- Capability.Repository.getUserSettings $ Type.UserSettingsSearchInput
    { userId = authorized.userId
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Page.UserSettings.render $ Template.Page.UserSettings.Props
        { values = Type.UserSettingsForm
          { image = fromMaybe "" userSettings.image
          , username = userSettings.username
          , bio = fromMaybe "" userSettings.bio
          , email = userSettings.email
          , password = userSettings.password
          }
        , errors = mempty
        }

handleViewPostUserProfileSettings ::
  (MonadIO m, Except.MonadError Type.Exception m, Capability.Repository.Repository m, Capability.Server.Server m, Capability.UserSettingsForm.UserSettingsForm m, Capability.Asset.Asset m) =>
  Honduit.Api.Type.AuthorizedClient ->
  Type.UserSettingsForm ->
  m
    ( Servant.Union
      '[ Servant.API.WithStatus 200 ByteString.Lazy.ByteString
       , Servant.API.WithStatus 303
         ( Servant.API.Headers
           '[ Servant.API.Header "Location" Text
            ]
            Servant.API.NoContent
         )
       ]
    )
handleViewPostUserProfileSettings authorized form = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "Handling web post user profile settings"
  session <- clientToSession client
  result <- Except.tryError do
    void $ Capability.UserSettingsForm.validate form
    Capability.Repository.updateUser $ Type.UserUpdateInput
      { userId = authorized.userId
      , email = form.email
      , username = form.username
      , password = do
          guard (form.password /= "")
          pure form.password
      , bio = form.bio
      , image = form.image
      }
  case result of
    Left exception -> do
      let viewContext = View.Type.ViewContext
            { session = session
            }
      liftIO do
        IO.print exception
      html <- View.unlift viewContext do
        Lucid.renderBST do
          Template.Page.UserSettings.render $ Template.Page.UserSettings.Props
            { values = form
            , errors = case exception of
                Type.UserSettingsFormValidationException errors -> errors
                _ -> mempty
            }
      Servant.respond $ Servant.API.WithStatus @200 html
    Right _ -> do
      Servant.respond $ Servant.API.WithStatus @303
        ( Servant.API.addHeader (Text.pack "/") 
          Servant.API.NoContent
          ::
          ( Servant.API.Headers
             '[ Servant.API.Header "Location" Text
              ]
              Servant.API.NoContent
          )
        )

handleViewGetUserProfile :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Int -> Honduit.Api.Type.Client -> Maybe Int -> Maybe Int -> m ByteString.Lazy.ByteString
handleViewGetUserProfile profileId client limit offset = do
  let articleFeedFilter = ArticleFeedFilter.empty
        { Type.authorId = pure profileId
        , Type.limit = limit
        , Type.offset = offset
        } :: Type.ArticleFeedFilter
  liftIO do
    IO.putStrLn "Handling web profile"
  session <- clientToSession client
  articles <- Capability.Repository.getPaginatedArticles $ Type.ArticlesSearchInput
    { userProfile = Session.toUserProfile session
    , tagId = articleFeedFilter.tagId
    , authorId = articleFeedFilter.authorId
    , followerProfileId = articleFeedFilter.followerProfileId
    , favoritedProfileId = articleFeedFilter.favoritedProfileId
    , limit = articleFeedFilter.limit
    , offset = articleFeedFilter.offset
    }
  profile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userProfile = Session.toUserProfile session
    , userId = Nothing
    , profileId = pure profileId
    }
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Page.Profile.render $ Template.Page.Profile.Props
        { profile = profile
        , articles = articles
        , articleFeedFilter = articleFeedFilter
        }

handleViewGetLogin :: (MonadIO m, Capability.Repository.Repository m, Capability.LoginForm.LoginForm m, Capability.Asset.Asset m) => Honduit.Api.Type.UnauthorizedClient -> m ByteString.Lazy.ByteString
handleViewGetLogin unauthorized = do
  let client = Honduit.Api.Type.Unauthorized unauthorized
  liftIO do
    IO.putStrLn "Handling web login"
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Page.Login.render $ Template.Page.Login.Props
        { values = Type.LoginForm
          { email = ""
          , password = ""
          }
        , errors = mempty
        }

handleViewPostLogin :: 
  (MonadIO m, Except.MonadError Type.Exception m, Capability.Repository.Repository m, Capability.Server.Server m, Capability.LoginForm.LoginForm m, Capability.Asset.Asset m) =>
  Honduit.Api.Type.UnauthorizedClient ->
  Type.LoginForm ->
  m
    ( Servant.Union
      '[ Servant.API.WithStatus 200 ByteString.Lazy.ByteString
       , Servant.API.WithStatus 303
         ( Servant.API.Headers
           '[ Servant.API.Header "Location" Text
            , Servant.API.Header "Set-Cookie" Cookie.SetCookie
            ]
            Servant.API.NoContent
         )
       ]
    )
handleViewPostLogin unauthorized form = do
  let client = Honduit.Api.Type.Unauthorized unauthorized
  liftIO do
    IO.putStrLn "Handling web post login"
  session <- clientToSession client
  result <- Except.tryError do
    void $ Capability.LoginForm.validate form
    Capability.Repository.getUserByLogin $ Type.UserLoginPayload
      { email = form.email
      , password = form.password
      }
  case result of
    Left exception -> do
      let viewContext = View.Type.ViewContext
            { session = session
            }
      html <- View.unlift viewContext do
        Lucid.renderBST do
          Template.Page.Login.render $ Template.Page.Login.Props
            { values = form
            , errors = case exception of
                Type.LoginFormValidationException errors -> errors
                _ -> mempty
            }
      Servant.respond $ Servant.API.WithStatus @200 html
    Right user -> do
      cookie <- Capability.Server.getAuthCookieFromPayload user
      Servant.respond $ Servant.API.WithStatus @303
        ( ( Servant.API.addHeader (Text.pack "/") $
            Servant.API.addHeader cookie
            Servant.API.NoContent
          ) ::
          ( Servant.API.Headers
             '[ Servant.API.Header "Location" Text
              , Servant.API.Header "Set-Cookie" Cookie.SetCookie
              ]
              Servant.API.NoContent
          )
        )

handleViewGetLogout :: 
  (MonadIO m, Capability.Server.Server m) =>
  Honduit.Api.Type.AuthorizedClient ->
  m
    ( Servant.Union
       '[ Servant.API.WithStatus 303
         ( Servant.API.Headers
           '[ Servant.API.Header "Location" Text
            , Servant.API.Header "Set-Cookie" Cookie.SetCookie
            ]
            Servant.API.NoContent
         )
       ]
    )
handleViewGetLogout _ = do
  cookie <- Capability.Server.getAuthClearCookie
  Servant.respond $ Servant.API.WithStatus @303
    ( ( Servant.API.addHeader (Text.pack "/") $
        Servant.API.addHeader cookie
        Servant.API.NoContent
      ) ::
      ( Servant.API.Headers
         '[ Servant.API.Header "Location" Text
          , Servant.API.Header "Set-Cookie" Cookie.SetCookie
          ]
          Servant.API.NoContent
      )
    )

handleViewGetRegister :: (MonadIO m, Capability.Repository.Repository m, Capability.RegisterForm.RegisterForm m, Capability.Asset.Asset m) => Honduit.Api.Type.UnauthorizedClient -> m ByteString.Lazy.ByteString
handleViewGetRegister unauthorized = do
  let client = Honduit.Api.Type.Unauthorized unauthorized
  liftIO do
    IO.putStrLn "Handling web register"
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Page.Register.render $ Template.Page.Register.Props
        { values = Type.RegisterForm
          { username = ""
          , email = ""
          , password = ""
          }
        , errors = mempty
        }

handleViewPostRegister :: 
  (MonadIO m, Except.MonadError Type.Exception m, Capability.Repository.Repository m, Capability.Server.Server m, Capability.RegisterForm.RegisterForm m, Capability.Asset.Asset m) =>
  Honduit.Api.Type.UnauthorizedClient ->
  Type.RegisterForm ->
  m
    ( Servant.Union
      '[ Servant.API.WithStatus 200 ByteString.Lazy.ByteString
       , Servant.API.WithStatus 303
         ( Servant.API.Headers
           '[ Servant.API.Header "Location" Text
            , Servant.API.Header "Set-Cookie" Cookie.SetCookie
            ]
            Servant.API.NoContent
         )
       ]
    )
handleViewPostRegister unauthorized form = do
  let client = Honduit.Api.Type.Unauthorized unauthorized
  liftIO do
    IO.putStrLn "Handling web post register"
  session <- clientToSession client
  result <- Except.tryError do
    validated <- Capability.RegisterForm.validate form
    Capability.Repository.createUser $ Type.UserCreateInput
      { email = validated.email
      , username = validated.username
      , password = validated.password
      }
  case result of
    Left exception -> do
      let viewContext = View.Type.ViewContext
            { session = session
            }
      liftIO do
        IO.print exception
      html <- View.unlift viewContext do
        Lucid.renderBST do
          Template.Page.Register.render $ Template.Page.Register.Props
            { values = form
            , errors = case exception of
                Type.RegisterFormValidationException errors -> errors
                _ -> mempty
            }
      Servant.respond $ Servant.API.WithStatus @200 html
    Right user -> do
      cookie <- Capability.Server.getAuthCookieFromPayload user
      Servant.respond $ Servant.API.WithStatus @303
        ( ( Servant.API.addHeader (Text.pack "/") $
            Servant.API.addHeader cookie
            Servant.API.NoContent
          ) ::
          ( Servant.API.Headers
             '[ Servant.API.Header "Location" Text
              , Servant.API.Header "Set-Cookie" Cookie.SetCookie
              ]
              Servant.API.NoContent
          )
        )

handleViewFragmentsHomeArticleFeedPersonal :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Honduit.Api.Type.Client -> Maybe Int -> Maybe Int -> Maybe Int -> m ByteString.Lazy.ByteString
handleViewFragmentsHomeArticleFeedPersonal client tagId limit offset = do
  session <- clientToSession client
  let articleFeedFilter = Type.ArticleFeedFilter 
        { tagId = tagId
        , authorId = Nothing
        , followerProfileId = do
            userProfile <- Session.toUserProfile session
            pure userProfile.id
        , favoritedProfileId = Nothing
        , limit = limit
        , offset = offset
        }
  articles <- Capability.Repository.getPaginatedArticles $ Type.ArticlesSearchInput
    { userProfile = Session.toUserProfile session
    , tagId = articleFeedFilter.tagId
    , authorId = articleFeedFilter.authorId
    , followerProfileId = articleFeedFilter.followerProfileId
    , favoritedProfileId = articleFeedFilter.favoritedProfileId
    , limit = articleFeedFilter.limit
    , offset = articleFeedFilter.offset
    }
  tags <- Capability.Repository.getPopularTags
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.HomeArticleFeed.render $ Template.Fragment.HomeArticleFeed.Props
        { articles = articles
        , articleFeedFilter = articleFeedFilter
        , tags = tags
        , tab = Template.Fragment.HomeArticleFeed.Personal
        , attributes = []
        }

handleViewFragmentsHomeArticleFeedGlobal :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Honduit.Api.Type.Client -> Maybe Int -> Maybe Int -> Maybe Int -> m ByteString.Lazy.ByteString
handleViewFragmentsHomeArticleFeedGlobal client tagId limit offset = do
  let articleFeedFilter = Type.ArticleFeedFilter
        { tagId = tagId
        , authorId = Nothing
        , followerProfileId = Nothing
        , favoritedProfileId = Nothing
        , limit = limit
        , offset = offset
        }
  session <- clientToSession client
  articles <- Capability.Repository.getPaginatedArticles $ Type.ArticlesSearchInput
    { userProfile = Session.toUserProfile session
    , tagId = articleFeedFilter.tagId
    , authorId = articleFeedFilter.authorId
    , followerProfileId = articleFeedFilter.followerProfileId
    , favoritedProfileId = articleFeedFilter.favoritedProfileId
    , limit = articleFeedFilter.limit
    , offset = articleFeedFilter.offset
    }
  tags <- Capability.Repository.getPopularTags
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.HomeArticleFeed.render $ Template.Fragment.HomeArticleFeed.Props
        { articles = articles
        , articleFeedFilter = articleFeedFilter
        , tags = tags
        , tab = Template.Fragment.HomeArticleFeed.Global
        , attributes = []
        }

handleViewFragmentsProfileArticleFeedPublished :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Int -> Honduit.Api.Type.Client -> Maybe Int -> Maybe Int -> Maybe Int -> m ByteString.Lazy.ByteString
handleViewFragmentsProfileArticleFeedPublished profileId client tagId limit offset = do
  let articleFeedFilter = Type.ArticleFeedFilter
        { tagId = tagId
        , authorId = pure profileId
        , followerProfileId = Nothing
        , favoritedProfileId = Nothing
        , limit = limit
        , offset = offset
        }
  session <- clientToSession client
  profile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userProfile = Session.toUserProfile session
    , userId = Nothing
    , profileId = pure profileId
    }
  articles <- Capability.Repository.getPaginatedArticles $ Type.ArticlesSearchInput
    { userProfile = Session.toUserProfile session
    , tagId = articleFeedFilter.tagId
    , authorId = articleFeedFilter.authorId
    , followerProfileId = articleFeedFilter.followerProfileId
    , favoritedProfileId = articleFeedFilter.favoritedProfileId
    , limit = articleFeedFilter.limit
    , offset = articleFeedFilter.offset
    }
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ProfileArticleFeed.render $ Template.Fragment.ProfileArticleFeed.Props
        { profile = profile
        , articles = articles
        , articleFeedFilter = articleFeedFilter
        , tab = Template.Fragment.ProfileArticleFeed.Published
        , attributes = []
        }

handleViewFragmentsProfileArticleFeedFavorited :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m, Capability.Paginator.Paginator m) => Int -> Honduit.Api.Type.Client -> Maybe Int -> Maybe Int -> Maybe Int -> m ByteString.Lazy.ByteString
handleViewFragmentsProfileArticleFeedFavorited profileId client tagId limit offset = do
  let articleFeedFilter = Type.ArticleFeedFilter
        { tagId = tagId
        , authorId = Nothing
        , followerProfileId = Nothing
        , favoritedProfileId = pure profileId
        , limit = limit
        , offset = offset
        }
  session <- clientToSession client
  profile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userProfile = Session.toUserProfile session
    , userId = Nothing
    , profileId = pure profileId
    }
  articles <- Capability.Repository.getPaginatedArticles $ Type.ArticlesSearchInput
    { userProfile = Session.toUserProfile session
    , tagId = articleFeedFilter.tagId
    , authorId = articleFeedFilter.authorId
    , followerProfileId = articleFeedFilter.followerProfileId
    , favoritedProfileId = articleFeedFilter.favoritedProfileId
    , limit = articleFeedFilter.limit
    , offset = articleFeedFilter.offset
    }
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ProfileArticleFeed.render $ Template.Fragment.ProfileArticleFeed.Props
        { profile = profile
        , articles = articles
        , articleFeedFilter = articleFeedFilter
        , tab = Template.Fragment.ProfileArticleFeed.Favorited
        , attributes = []
        }

handleViewFragmentsProfileFollowButtonFollow :: (MonadIO m, Capability.Repository.Repository m) => Int -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsProfileFollowButtonFollow profileId authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsProfileFollowButtonFollow"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  profile <- Capability.Repository.createProfileFollower $ Type.ProfileFollowerCreateInput
    { followingId = profileId
    , follower = userProfile
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ProfileFollowButton.render $ Template.Fragment.ProfileFollowButton.Props
        { profile = profile
        , attributes = []
        }

handleViewFragmentsProfileFollowButtonUnfollow :: (MonadIO m, Capability.Repository.Repository m) => Int -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsProfileFollowButtonUnfollow profileId authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsProfileFollowButtonUnfollow"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  profile <- Capability.Repository.deleteProfileFollower $ Type.ProfileFollowerDeleteInput
    { followingId = profileId
    , follower = userProfile
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ProfileFollowButton.render $ Template.Fragment.ProfileFollowButton.Props
        { profile = profile
        , attributes = []
        }

handleViewFragmentsArticleCommentSectionCreateComment ::
  (MonadIO m, Except.MonadError Type.Exception m, Capability.Repository.Repository m, Capability.Asset.Asset m, Capability.CommentCreateForm.CommentCreateForm m) =>
  Text ->
  Honduit.Api.Type.AuthorizedClient ->
  Type.CommentCreateForm ->
  m ByteString.Lazy.ByteString
handleViewFragmentsArticleCommentSectionCreateComment slug authorized form = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "Handling web fragment article comment section create comment"
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  result <- Except.tryError do
    validated <- Capability.CommentCreateForm.validate form
    Capability.Repository.createArticleComment $ Type.ArticleCommentCreateInput
      { author = userProfile
      , body = validated.body
      , slug = slug
      }
  article <- Capability.Repository.getArticle $ Type.ArticleSearchInput
    { userProfile = Session.toUserProfile session
    , slug = slug
    }
  comments <- Capability.Repository.getArticleComments $ Type.ArticleCommentsSearchInput
    { userProfile = Session.toUserProfile session
    , slug = slug
    }
  case result of
    Left exception -> do
      View.unlift viewContext do
        Lucid.renderBST do
          Template.Fragment.ArticleCommentSection.render $ Template.Fragment.ArticleCommentSection.Props
            { article = article
            , comments = comments
            , commentFormValues = form
            , commentFormErrors = case exception of
                Type.CommentCreateFormValidationException errors -> errors
                _ -> mempty
            , attributes = []
            }
    Right _ -> do
      View.unlift viewContext do
        Lucid.renderBST do
          Template.Fragment.ArticleCommentSection.render $ Template.Fragment.ArticleCommentSection.Props
            { article = article
            , comments = comments
            , commentFormValues = Type.CommentCreateForm
                { body = mempty
                }
            , commentFormErrors = mempty
            , attributes = []
            }

handleViewFragmentsArticleCommentSectionDeleteComment :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m, Capability.CommentCreateForm.CommentCreateForm m) => Text -> Int -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticleCommentSectionDeleteComment slug commentId authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "Handling web fragment article comment section delete comment"
  session <- clientToSession client
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  Capability.Repository.deleteComment $ Type.CommentDeleteInput
    { commentId = commentId
    , author = userProfile
    }
  let viewContext = View.Type.ViewContext
        { session = session
        }
  article <- Capability.Repository.getArticle $ Type.ArticleSearchInput
    { userProfile = pure userProfile
    , slug = slug
    }
  comments <- Capability.Repository.getArticleComments $ Type.ArticleCommentsSearchInput
    { userProfile = pure userProfile
    , slug = slug
    }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticleCommentSection.render $ Template.Fragment.ArticleCommentSection.Props
        { article = article
        , comments = comments
        , commentFormValues = Type.CommentCreateForm
            { body = mempty
            }
        , commentFormErrors = mempty
        , attributes = []
        }

handleViewFragmentsArticleFavoriteCounterFavorite :: (MonadIO m, Capability.Repository.Repository m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticleFavoriteCounterFavorite slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsArticleFavoriteCounterFavorite"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.createArticleFavorite $ Type.ArticleFavoriteCreateInput
    { profile = userProfile
    , slug = slug
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticleFavoriteCounter.render $ Template.Fragment.ArticleFavoriteCounter.Props
        { article = article
        , attributes = []
        }

handleViewFragmentsArticleFavoriteCounterUnfavorite :: (MonadIO m, Capability.Repository.Repository m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticleFavoriteCounterUnfavorite slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsArticleFavoriteCounterUnfavorite"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.deleteArticleFavorite $ Type.ArticleFavoriteDeleteInput
    { profile = userProfile
    , slug = slug
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticleFavoriteCounter.render $ Template.Fragment.ArticleFavoriteCounter.Props
        { article = article
        , attributes = []
        }

handleViewFragmentsArticlePreviewFavoriteCounterFavorite :: (MonadIO m, Capability.Repository.Repository m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticlePreviewFavoriteCounterFavorite slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsArticlePreviewFavoriteCounterFavorite"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.createArticleFavorite $ Type.ArticleFavoriteCreateInput
    { profile = userProfile
    , slug = slug
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticlePreviewFavoriteCounter.render $ Template.Fragment.ArticlePreviewFavoriteCounter.Props
        { article = article
        , attributes = []
        }

handleViewFragmentsArticlePreviewFavoriteCounterUnfavorite :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticlePreviewFavoriteCounterUnfavorite slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsArticlePreviewFavoriteCounterUnfavorite"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.deleteArticleFavorite $ Type.ArticleFavoriteDeleteInput
    { profile = userProfile
    , slug = slug
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticlePreviewFavoriteCounter.render $ Template.Fragment.ArticlePreviewFavoriteCounter.Props
        { article = article
        , attributes = []
        }

handleViewFragmentsArticleMetaFollowAuthor :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticleMetaFollowAuthor slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsArticleMetaUnfollowAuthor"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- do
    article <- Capability.Repository.getArticle $ Type.ArticleSearchInput
      { userProfile = pure userProfile
      , slug = slug
      }
    Capability.Repository.createProfileFollower $ Type.ProfileFollowerCreateInput
      { followingId = article.author.id
      , follower = userProfile
      }
    Capability.Repository.getArticle $ Type.ArticleSearchInput
      { userProfile = pure userProfile
      , slug = slug
      }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticleMeta.render $ Template.Fragment.ArticleMeta.Props
        { article = article
        , attributes = []
        }

handleViewFragmentsArticleMetaUnfollowAuthor :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticleMetaUnfollowAuthor slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsArticleMetaUnfollowAuthor"
  session <- clientToSession client
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- do
    article <- Capability.Repository.getArticle $ Type.ArticleSearchInput
      { userProfile = pure userProfile
      , slug = slug
      }
    Capability.Repository.deleteProfileFollower $ Type.ProfileFollowerDeleteInput
      { followingId = article.author.id
      , follower = userProfile
      }
    Capability.Repository.getArticle $ Type.ArticleSearchInput
      { userProfile = pure userProfile
      , slug = slug
      }
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticleMeta.render $ Template.Fragment.ArticleMeta.Props
        { article = article
        , attributes = []
        }

handleViewFragmentsArticleMetaFavoriteArticle :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticleMetaFavoriteArticle slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsArticleMetaFavoriteArticle"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.createArticleFavorite $ Type.ArticleFavoriteCreateInput
    { profile = userProfile
    , slug = slug
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticleMeta.render $ Template.Fragment.ArticleMeta.Props
        { article = article
        , attributes = []
        }

handleViewFragmentsArticleMetaUnfavoriteArticle :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticleMetaUnfavoriteArticle slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsArticleMetaUnfavoriteArticle"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.deleteArticleFavorite $ Type.ArticleFavoriteDeleteInput
    { profile = userProfile
    , slug = slug
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticleMeta.render $ Template.Fragment.ArticleMeta.Props
        { article = article
        , attributes = []
        }

handleViewFragmentsArticlePreviewFavoriteArticle :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticlePreviewFavoriteArticle slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsArticlePreviewFavoriteArticle"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.createArticleFavorite $ Type.ArticleFavoriteCreateInput
    { profile = userProfile
    , slug = slug
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticlePreview.render $ Template.Fragment.ArticlePreview.Props
        { article = article
        , attributes = []
        }

handleViewFragmentsArticlePreviewUnfavoriteArticle :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m) => Text -> Honduit.Api.Type.AuthorizedClient -> m ByteString.Lazy.ByteString
handleViewFragmentsArticlePreviewUnfavoriteArticle slug authorized = do
  let client = Honduit.Api.Type.Authorized authorized
  liftIO do
    IO.putStrLn "handleViewFragmentsArticlePreviewUnfavoriteArticle"
  userProfile <- Capability.Repository.getProfile $ Type.ProfileSearchInput
    { userId = pure authorized.userId
    , profileId = Nothing
    , userProfile = Nothing
    }
  article <- Capability.Repository.deleteArticleFavorite $ Type.ArticleFavoriteDeleteInput
    { profile = userProfile
    , slug = slug
    }
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Fragment.ArticlePreview.render $ Template.Fragment.ArticlePreview.Props
        { article = article
        , attributes = []
        }

handleViewGet404 :: (MonadIO m, Capability.Repository.Repository m, Capability.Asset.Asset m) => Honduit.Api.Type.Client -> m ByteString.Lazy.ByteString
handleViewGet404 client = do
  liftIO do
    IO.putStrLn "Handling web 404"
  session <- clientToSession client
  let viewContext = View.Type.ViewContext
        { session = session
        }
  View.unlift viewContext do
    Lucid.renderBST do
      Template.Page.NotFound.render


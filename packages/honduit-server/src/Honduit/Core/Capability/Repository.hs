module Honduit.Core.Capability.Repository where

import RIO
import qualified Honduit.Core.Type as Type

-- Split up when the time comes
class Repository m where 
  createUserToken :: Type.UserTokenCreateInput -> m Text 
  createUser :: Type.UserCreateInput -> m Type.User
  updateUser :: Type.UserUpdateInput -> m Type.User
  getUser :: Type.UserSearchInput -> m Type.User
  getUserByLogin :: Type.UserLoginPayload -> m Type.User
  getUserSettings :: Type.UserSettingsSearchInput -> m Type.UserSettings
  createProfileFollower :: Type.ProfileFollowerCreateInput -> m Type.Profile
  deleteProfileFollower :: Type.ProfileFollowerDeleteInput -> m Type.Profile
  getProfile :: Type.ProfileSearchInput -> m Type.Profile
  getArticle :: Type.ArticleSearchInput -> m Type.Article
  getArticles :: Type.ArticlesSearchInput -> m [Type.Article]
  getArticlesCount :: Type.ArticlesSearchInput -> m Int
  getPaginatedArticles :: Type.ArticlesSearchInput -> m (Type.Paginated [Type.Article])
  createArticle :: Type.ArticleCreateInput -> m Type.Article
  updateArticle :: Type.ArticleUpdateInput -> m Type.Article
  deleteArticle :: Type.ArticleDeleteInput -> m ()
  getArticleComments :: Type.ArticleCommentsSearchInput -> m [Type.Comment]
  createArticleComment :: Type.ArticleCommentCreateInput -> m Type.Comment
  deleteComment :: Type.CommentDeleteInput -> m ()
  createArticleFavorite :: Type.ArticleFavoriteCreateInput -> m Type.Article
  deleteArticleFavorite :: Type.ArticleFavoriteDeleteInput -> m Type.Article
  getTags :: Type.TagsSearchInput -> m [Type.Tag]
  getPopularTags :: m [Type.Tag]

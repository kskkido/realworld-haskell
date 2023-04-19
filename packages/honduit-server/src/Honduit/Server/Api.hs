module Honduit.Server.Api where

import RIO
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Web.Cookie as Cookie
import qualified Servant
import qualified Servant.API
import qualified Servant.API.Extension as Servant.API
import qualified Honduit.Core.Type as Type
import qualified Honduit.Api 
import qualified Honduit.Api.Type

type Api = Servant.API.NamedRoutes RootRouter

data RootRouter mode = RootRouter
  { api ::
    mode Servant.API.:- 
    Servant.API.NamedRoutes Honduit.Api.RootRouter
  , view ::
    mode Servant.API.:- 
    Servant.API.NamedRoutes ViewRouter
  }
  deriving (Generic)

data ViewRouter mode = ViewRouter
  { index :: 
    mode Servant.API.:-
    Honduit.Api.Type.AuthOptionalPath Servant.API.:>
    Servant.API.QueryParam "tagId" Int Servant.API.:>
    Servant.API.QueryParam "authorId" Int Servant.API.:>
    Servant.API.QueryParam "favorited" Int Servant.API.:>
    Servant.API.QueryParam "limit" Int Servant.API.:>
    Servant.API.QueryParam "offset" Int Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , editor :: 
    mode Servant.API.:-
    "editor" Servant.API.:>
    Servant.API.NamedRoutes ViewEditorRouter
  , article :: 
    mode Servant.API.:-
    "article" Servant.API.:>
    Servant.API.NamedRoutes ViewArticleRouter
  , settings :: 
    mode Servant.API.:-
    "settings" Servant.API.:>
    Servant.API.NamedRoutes ViewSettingsRouter
  , profile :: 
    mode Servant.API.:-
    "profile" Servant.API.:>
    Servant.API.NamedRoutes ViewProfileRouter
  , login ::
    mode Servant.API.:-
    "login" Servant.API.:>
    Servant.API.NamedRoutes ViewLoginRouter
  , logout ::
    mode Servant.API.:-
    "logout" Servant.API.:>
    Servant.API.NamedRoutes ViewLogoutRouter
  , register ::
    mode Servant.API.:-
    "register" Servant.API.:>
    Servant.API.NamedRoutes ViewRegisterRouter
  , fragments ::
    mode Servant.API.:-
    "fragments" Servant.API.:>
    Servant.API.NamedRoutes ViewFragmentsRouter
  , error ::
    mode Servant.API.:-
    "error" Servant.API.:>
    Servant.API.NamedRoutes ViewErrorRouter
  , assets ::
    mode Servant.API.:-
    Servant.Raw
  }
  deriving (Generic)

data ViewEditorRouter mode = ViewEditorRouter
  { index :: 
    mode Servant.API.:-
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , new ::
    mode Servant.API.:-
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.FormUrlEncoded] Type.ArticleEditorForm Servant.API.:>
    Servant.API.UVerb 'Servant.API.POST '[Servant.API.HtmlContentType]
    '[ Servant.API.WithStatus 200 ByteString.Lazy.ByteString
     , Servant.API.WithStatus 303
       ( Servant.API.Headers
         '[ Servant.API.Header "Location" Text
          ]
         Servant.API.NoContent
       )
     ]
  , show :: 
    mode Servant.API.:-
    Servant.API.Capture "slug" Text Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , edit ::
    mode Servant.API.:-
    Servant.API.Capture "slug" Text Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.FormUrlEncoded] Type.ArticleEditorForm Servant.API.:>
    Servant.API.UVerb 'Servant.API.POST '[Servant.API.HtmlContentType]
    '[ Servant.API.WithStatus 200 ByteString.Lazy.ByteString
     , Servant.API.WithStatus 303
       ( Servant.API.Headers
         '[ Servant.API.Header "Location" Text
          ]
         Servant.API.NoContent
       )
     ]
  }
  deriving (Generic)

data ViewArticleRouter mode = ViewArticleRouter
  { show :: 
    mode Servant.API.:-
    Servant.API.Capture "slug" Text Servant.API.:>
    Honduit.Api.Type.AuthOptionalPath Servant.API.:>                                   
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , delete :: 
    mode Servant.API.:-
    Servant.API.Capture "slug" Text Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>                                   
    Servant.API.Delete '[Servant.API.HtmlContentType]
    ( Servant.API.Headers
      '[ Servant.API.Header "HX-Redirect" Text
       ]
      Servant.API.NoContent
    )
  }
  deriving (Generic)

data ViewSettingsRouter mode = ViewSettingsRouter
  { index :: 
    mode Servant.API.:-
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , edit ::
    mode Servant.API.:-
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.FormUrlEncoded] Type.UserSettingsForm Servant.API.:>
    Servant.API.UVerb 'Servant.API.POST '[Servant.API.HtmlContentType]
    '[ Servant.API.WithStatus 200 ByteString.Lazy.ByteString
     , Servant.API.WithStatus 303
       ( Servant.API.Headers
         '[ Servant.API.Header "Location" Text
          ]
         Servant.API.NoContent
       )
     ]
  }
  deriving (Generic)

data ViewProfileRouter mode = ViewProfileRouter
  { show :: 
    mode Servant.API.:-
    Servant.API.Capture "profileId" Int Servant.API.:>
    Honduit.Api.Type.AuthOptionalPath Servant.API.:>
    Servant.API.QueryParam "limit" Int Servant.API.:>
    Servant.API.QueryParam "offset" Int Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  }
  deriving (Generic)

data ViewLoginRouter mode = ViewLoginRouter
  { index ::
    mode Servant.API.:-
    Honduit.Api.Type.UnauthRequiredPath Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , login :: 
    mode Servant.API.:-
    Honduit.Api.Type.UnauthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.FormUrlEncoded] Type.LoginForm Servant.API.:>
    Servant.API.UVerb 'Servant.API.POST '[Servant.API.HtmlContentType]
    '[ Servant.API.WithStatus 200 ByteString.Lazy.ByteString
     , Servant.API.WithStatus 303
       ( Servant.API.Headers
         '[ Servant.API.Header "Location" Text
          , Servant.API.Header "Set-Cookie" Cookie.SetCookie
          ]
         Servant.API.NoContent
       )
     ]
  }
  deriving (Generic)

data ViewLogoutRouter mode = ViewLogoutRouter
  { logout :: 
    mode Servant.API.:-
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.UVerb 'Servant.API.GET '[Servant.API.HtmlContentType]
     '[ Servant.API.WithStatus 303
       ( Servant.API.Headers
         '[ Servant.API.Header "Location" Text
          , Servant.API.Header "Set-Cookie" Cookie.SetCookie
          ]
         Servant.API.NoContent
       )
     ]
  }
  deriving (Generic)

data ViewRegisterRouter mode = ViewRegisterRouter
  { index ::
    mode Servant.API.:-
    Honduit.Api.Type.UnauthRequiredPath Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , register :: 
    mode Servant.API.:-
    Honduit.Api.Type.UnauthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.FormUrlEncoded] Type.RegisterForm Servant.API.:>
    Servant.API.UVerb 'Servant.API.POST '[Servant.API.HtmlContentType]
    '[ Servant.API.WithStatus 200 ByteString.Lazy.ByteString
     , Servant.API.WithStatus 303
       ( Servant.API.Headers
         '[ Servant.API.Header "Location" Text
          , Servant.API.Header "Set-Cookie" Cookie.SetCookie
          ]
         Servant.API.NoContent
       )
     ]
  }
  deriving (Generic)

data ViewErrorRouter mode = ViewErrorRouter
  { notFound :: 
    mode Servant.API.:-
    "404" Servant.API.:>
    Honduit.Api.Type.AuthOptionalPath Servant.API.:>                                   
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  }
  deriving (Generic)

data ViewFragmentsRouter mode = ViewFragmentsRouter
  { homeArticleFeed ::
    mode Servant.API.:-
    "home-article-feed" Servant.API.:>
    Servant.API.NamedRoutes ViewFragmentHomeArticleFeedRouter
  , profileArticleFeed ::
    mode Servant.API.:-
    "profile-article-feed" Servant.API.:>
    Servant.API.Capture "profileId" Int Servant.API.:>
    Servant.API.NamedRoutes ViewFragmentProfileArticleFeedRouter
  , profileFollowButton ::
    mode Servant.API.:-
    "profile-follow-button" Servant.API.:>
    Servant.API.Capture "profileId" Int Servant.API.:>
    Servant.API.NamedRoutes ViewFragmentProfileFollowButtonRouter
  , articleCommentSection ::
    mode Servant.API.:-
    "article-comment-section" Servant.API.:>
    Servant.API.Capture "slug" Text Servant.API.:>
    Servant.API.NamedRoutes ViewFragmentArticleCommentSectionRouter
  , articleMeta ::
    mode Servant.API.:-
    "article-meta" Servant.API.:>
    Servant.API.Capture "slug" Text Servant.API.:>
    Servant.API.NamedRoutes ViewFragmentArticleMetaRouter
  , articlePreview ::
    mode Servant.API.:-
    "article-preview" Servant.API.:>
    Servant.API.Capture "slug" Text Servant.API.:>
    Servant.API.NamedRoutes ViewFragmentArticlePreviewRouter
  }
  deriving Generic

data ViewFragmentHomeArticleFeedRouter mode = ViewFragmentHomeArticleFeedRouter
  { personal ::
    mode Servant.API.:-
    "personal" Servant.API.:>
    Honduit.Api.Type.AuthOptionalPath Servant.API.:>
    Servant.API.QueryParam "tagId" Int Servant.API.:>
    Servant.API.QueryParam "limit" Int Servant.API.:>
    Servant.API.QueryParam "offset" Int Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , global ::
    mode Servant.API.:-
    "global" Servant.API.:>
    Honduit.Api.Type.AuthOptionalPath Servant.API.:>
    Servant.API.QueryParam "tagId" Int Servant.API.:>
    Servant.API.QueryParam "limit" Int Servant.API.:>
    Servant.API.QueryParam "offset" Int Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  }
  deriving Generic

data ViewFragmentProfileArticleFeedRouter mode = ViewFragmentProfileArticleFeedRouter
  { published ::
    mode Servant.API.:-
    "published" Servant.API.:>
    Honduit.Api.Type.AuthOptionalPath Servant.API.:>
    Servant.API.QueryParam "tagId" Int Servant.API.:>
    Servant.API.QueryParam "limit" Int Servant.API.:>
    Servant.API.QueryParam "offset" Int Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , favorited ::
    mode Servant.API.:-
    "favorited" Servant.API.:>
    Honduit.Api.Type.AuthOptionalPath Servant.API.:>
    Servant.API.QueryParam "tagId" Int Servant.API.:>
    Servant.API.QueryParam "limit" Int Servant.API.:>
    Servant.API.QueryParam "offset" Int Servant.API.:>
    Servant.API.Get '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  }
  deriving Generic

data ViewFragmentProfileFollowButtonRouter mode = ViewFragmentProfileFollowButtonRouter
  { follow ::
    mode Servant.API.:-
    "follow" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Post '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , unfollow ::
    mode Servant.API.:-
    "follow" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  }
  deriving Generic

data ViewFragmentArticleCommentSectionRouter mode = ViewFragmentArticleCommentSectionRouter
  { createComment ::
    mode Servant.API.:-
    "comments" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.ReqBody '[Servant.API.FormUrlEncoded] Type.CommentCreateForm Servant.API.:>
    Servant.API.Post '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , deleteComment ::
    mode Servant.API.:-
    "comments" Servant.API.:>
    Servant.API.Capture "commentId" Int Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  }
  deriving Generic

data ViewFragmentArticleFavoriteCounter mode = ViewFragmentArticleFavoriteCounter
  { favorite ::
    mode Servant.API.:-
    "favorite" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Post '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , unfavorite ::
    mode Servant.API.:-
    "favorite" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  }
  deriving (Generic)

data ViewFragmentArticlePreviewFavoriteCounter mode = ViewFragmentArticlePreviewFavoriteCounter
  { favorite ::
    mode Servant.API.:-
    "favorite" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Post '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , unfavorite ::
    mode Servant.API.:-
    "favorite" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  }
  deriving (Generic)

data ViewFragmentArticleMetaRouter mode = ViewFragmentArticleMetaRouter
  { followAuthor ::
    mode Servant.API.:-
    "follow-author" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Post '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , unfollowAuthor ::
    mode Servant.API.:-
    "follow-author" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , favoriteArticle ::
    mode Servant.API.:-
    "favorite-article" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Post '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , unfavoriteArticle ::
    mode Servant.API.:-
    "favorite-article" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  }
  deriving (Generic)

data ViewFragmentArticlePreviewRouter mode = ViewFragmentArticlePreviewRouter
  { favoriteArticle ::
    mode Servant.API.:-
    "favorite-article" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Post '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  , unfavoriteArticle ::
    mode Servant.API.:-
    "favorite-article" Servant.API.:>
    Honduit.Api.Type.AuthRequiredPath Servant.API.:>
    Servant.API.Delete '[Servant.API.HtmlContentType] ByteString.Lazy.ByteString
  }
  deriving (Generic)

proxy :: Servant.Proxy Api
proxy = Servant.Proxy

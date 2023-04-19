module Honduit.Interpreter.View where

import RIO
import qualified Control.Monad.Reader as Reader
import qualified Honduit.Interpreter.View.Type as Type
import qualified Honduit.Core.Capability.ArticleEditorForm as Capability.ArticleEditorForm
import qualified Honduit.Core.Capability.Asset as Capability.Asset
import qualified Honduit.Core.Capability.Paginator as Capability.Paginator
import qualified Honduit.Core.Capability.CommentCreateForm as Capability.CommentCreateForm
import qualified Honduit.Core.Capability.LoginForm as Capability.LoginForm
import qualified Honduit.Core.Capability.RegisterForm as Capability.RegisterForm
import qualified Honduit.Core.Capability.UserSettingsForm as Capability.UserSettingsForm

newtype View m a = View
  { unwrap :: Reader.ReaderT Type.ViewContext m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadReader Type.ViewContext
    )

instance (Monad m, Capability.Asset.Asset m) => Capability.Asset.Asset (View m) where
  defaultUserAvatarImgSource = lift do
    Capability.Asset.defaultUserAvatarImgSource

instance (Monad m, Capability.Paginator.Paginator m) => Capability.Paginator.Paginator (View m) where
  paginate len lookaround metadata = lift do
    Capability.Paginator.paginate len lookaround metadata

instance (Monad m, Capability.ArticleEditorForm.ArticleEditorForm m) => Capability.ArticleEditorForm.ArticleEditorForm (View m) where
  config = lift do
    Capability.ArticleEditorForm.config
  validate form = lift do
    Capability.ArticleEditorForm.validate form

instance (Monad m, Capability.CommentCreateForm.CommentCreateForm m) => Capability.CommentCreateForm.CommentCreateForm (View m) where
  config = lift do
    Capability.CommentCreateForm.config
  validate form = lift do
    Capability.CommentCreateForm.validate form

instance (Monad m, Capability.LoginForm.LoginForm m) => Capability.LoginForm.LoginForm (View m) where
  config = lift do
    Capability.LoginForm.config
  validate form = lift do
    Capability.LoginForm.validate form

instance (Monad m, Capability.RegisterForm.RegisterForm m) => Capability.RegisterForm.RegisterForm (View m) where
  config = lift do
    Capability.RegisterForm.config
  validate form = lift do
    Capability.RegisterForm.validate form

instance (Monad m, Capability.UserSettingsForm.UserSettingsForm m) => Capability.UserSettingsForm.UserSettingsForm (View m) where
  config = lift do
    Capability.UserSettingsForm.config
  validate form = lift do
    Capability.UserSettingsForm.validate form

unlift :: Monad m => Type.ViewContext -> View m a -> m a
unlift context (View action) = do
  Reader.runReaderT action context

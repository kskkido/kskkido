module Kskkido.Blog.Interpreter.View where

import RIO
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Capability.Date as Capability.Date
import qualified Kskkido.Blog.Interpreter.View.Type as Type

newtype View a = View
  { unwrap :: Reader.ReaderT Type.ViewConfig (Either Type.CoreException) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Type.ViewConfig
    )

instance Except.MonadError Type.CoreException View where
    throwError :: Type.CoreException -> View a
    throwError = View . lift . Left
    catchError :: View a -> (Type.CoreException -> View a) -> View a
    catchError view handler = do
      context <- ask
      let action = run context view
      either handler pure action

instance Capability.Date.Date View where
  getCurrentTime = do
    context <- ask
    pure $ context.now

run :: Type.ViewConfig -> View a -> Either Type.CoreException a
run context (View action) = do
  Reader.runReaderT action context


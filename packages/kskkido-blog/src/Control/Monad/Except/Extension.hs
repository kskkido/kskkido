module Control.Monad.Except.Extension where

import RIO
import qualified Control.Monad.Except as Except

tryError :: Except.MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `Except.catchError` (pure . Left)

modifyError :: Except.MonadError e' m => (e -> e') -> Except.ExceptT e m a -> m a
modifyError f m = Except.runExceptT m >>= either (Except.throwError . f) pure


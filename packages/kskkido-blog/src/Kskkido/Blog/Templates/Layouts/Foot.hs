module Kskkido.Blog.Templates.Layouts.Foot where

import RIO
import qualified Lucid

render :: Monad m => Lucid.HtmlT m ()
render = do
  mempty

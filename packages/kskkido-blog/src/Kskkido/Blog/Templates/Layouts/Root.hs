module Kskkido.Blog.Templates.Layouts.Root where

import RIO
import qualified Lucid
import qualified Data.Has as Has
import qualified Kskkido.Blog.Core.Type as Type

render ::
  ( Has.Has Type.Page r
  , MonadReader r m
  ) => Lucid.HtmlT m () -> Lucid.HtmlT m ()
render children = do
  page :: Type.Page <- asks Has.getter
  Lucid.html_
    [ Lucid.lang_ $ fromString $ show page.locale
    ] do
      children

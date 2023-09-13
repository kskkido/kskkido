module Kskkido.Blog.Templates.Pages.About where

import RIO
import qualified Lucid
import qualified Control.Monad.Except as Except
import qualified Data.Has as Has
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Capability.Date as Capability.Date
import qualified Kskkido.Blog.Templates.Layouts.Root as Root
import qualified Kskkido.Blog.Templates.Layouts.Body as Body
import qualified Kskkido.Blog.Templates.Layouts.Head as Head
import qualified Kskkido.Blog.Templates.Layouts.Foot as Foot
import qualified Kskkido.Blog.Templates.Views.About as About

render ::
  ( Has.Has Type.Page r
  , Has.Has Type.Locale r
  , Has.Has Type.Router r
  , Has.Has Type.SiteMap r
  , Has.Has Type.LocalizedDictionary r
  , Has.Has Type.Profile r
  , Has.Has [Type.Post] r
  , Has.Has [Type.Tag] r
  , Capability.Date.Date m
  , Except.MonadError Type.CoreException m
  , MonadReader r m
  ) => Lucid.HtmlT m ()
render = do
  Root.render do
    Lucid.head_
      Head.render
    Lucid.body_
      [
      ] do
      Body.render do
        About.render
        Foot.render

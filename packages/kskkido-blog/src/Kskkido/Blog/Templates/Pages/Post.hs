module Kskkido.Blog.Templates.Pages.Post where

import RIO
import qualified Lucid
import qualified Control.Monad.Except as Except
import qualified Data.Has as Has
import qualified Kskkido.Blog.Core as Core
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Capability.Date as Capability.Date
import qualified Kskkido.Blog.Templates.Layouts.Root as Root
import qualified Kskkido.Blog.Templates.Layouts.Body as Body
import qualified Kskkido.Blog.Templates.Layouts.Head as Head
import qualified Kskkido.Blog.Templates.Layouts.Foot as Foot
import qualified Kskkido.Blog.Templates.Views.Post as Post

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
  ) => Type.Post -> Lucid.HtmlT m ()
render post = do
  Root.render do
    Lucid.head_ do
      let identifier = Type.Plain "/scripts/article.js"
      metadata <- Core.getSiteItemMetadata identifier
      Head.render
      Lucid.script_
        [ Lucid.src_ metadata.route
        ] do
          fromString ""
    Lucid.body_
      [
      ] do
      Body.render do
        Post.render post
        Foot.render

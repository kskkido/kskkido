module Kskkido.Blog.Templates.Layouts.Head where

import RIO
import qualified Lucid
import qualified Data.Has as Has
import qualified Control.Monad.Except as Except
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Core as Core

render :: 
  ( Has.Has Type.Page r
  , Has.Has Type.SiteMap r
  , Except.MonadError Type.CoreException m
  , MonadReader r m
  ) => Lucid.HtmlT m ()
render = do
  page :: Type.Page <- asks Has.getter
  Lucid.meta_
    [ Lucid.charset_ "utf-8"
    ]
  Lucid.meta_
    [ Lucid.name_ "viewport"
    , Lucid.content_ "width=device-width, initial-scale=1"
    ]
  Lucid.link_
    [ Lucid.rel_ "stylesheet"
    , Lucid.href_ "https://unpkg.com/modern-css-reset/dist/reset.min.css"
    ]
  Lucid.link_
    [ Lucid.rel_ "stylesheet"
    , Lucid.href_ $ fromString "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
    ]
  do 
    let identifier = Type.Plain "/scripts/bootstrap.js"
    metadata <- Core.getSiteItemMetadata identifier
    Lucid.script_
      [ Lucid.src_ metadata.route
      ] do
        fromString ""
  Lucid.script_
    [ Lucid.src_ "https://kit.fontawesome.com/56e0eb8f60.js"
    ] do
      fromString ""
  do 
    let identifier = Type.Plain "/scripts/pageSearch.js"
    metadata <- Core.getSiteItemMetadata identifier
    Lucid.script_
      [ Lucid.src_ metadata.route
      ] do
        fromString ""
  do 
    let identifier = Type.Plain "/styles/main.css"
    metadata <- Core.getSiteItemMetadata identifier
    Lucid.link_
      [ Lucid.rel_ "stylesheet"
      , Lucid.href_ metadata.route
      ] 
  do 
    let identifier = Type.Plain "/styles/syntax.css"
    metadata <- Core.getSiteItemMetadata identifier
    Lucid.link_
      [ Lucid.rel_ "stylesheet"
      , Lucid.href_ metadata.route
      ]
  Lucid.title_
    [
    ] do
      Lucid.toHtml page.title

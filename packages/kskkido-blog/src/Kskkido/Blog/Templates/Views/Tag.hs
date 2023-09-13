module Kskkido.Blog.Templates.Views.Tag where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad.Except as Except
import qualified Data.Has as Has
import qualified Lucid
import qualified Kskkido.Blog.Core as Core
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Templates.Components.PostPreview as PostPreview

render :: 
  ( Has.Has Type.Page r
  , Has.Has Type.Locale r
  , Has.Has Type.Router r
  , Has.Has Type.SiteMap r
  , Has.Has Type.LocalizedDictionary r
  , Has.Has [Type.Post] r
  , Except.MonadError Type.CoreException m
  , MonadReader r m
  ) => Type.Tag -> Lucid.HtmlT m ()
render tag = do
  router :: Type.Router <- asks Has.getter
  page :: Type.Page <- asks Has.getter
  posts <- Core.postsByTag tag
  Lucid.main_ [] do
    Lucid.div_
      [ Lucid.classes_
        [ "flex"
        , "items-center"
        , "text-sm"
        , "mb-4"
        , "text-gray-400"
        , "text-secondary"
        , "fill-secondary"
        , "w-full"
        ]
      ] do
      do 
        identifier <- Core.localizeKey router.home
        metadata <- Core.getSiteItemMetadata identifier
        Lucid.a_
          [ Lucid.href_ metadata.route
          , Lucid.classes_
            [ "text-link"
            ]
          ] do
            text <- Core.localize "navigation.home"
            Lucid.toHtml text
      Lucid.span_
        [ Lucid.classes_
          [ "mx-3"
          ]
        ] do
          Lucid.toHtml ("/" :: Text)
      do 
        identifier <- Core.localizeKey router.tags
        metadata <- Core.getSiteItemMetadata identifier
        Lucid.a_
          [ Lucid.href_ metadata.route
          , Lucid.classes_
            [ "text-link"
            ]
          ] do
            text <- Core.localize "navigation.tags"
            Lucid.toHtml text
      Lucid.span_
        [ Lucid.classes_
          [ "mx-3"
          ]
        ] do
          Lucid.toHtml ("/" :: Text)
      do 
        Lucid.span_
          [ Lucid.classes_
            [ "text-link"
            , "active"
            ]
          ] do
            Lucid.toHtml page.title
    Lucid.h1_
      [ Lucid.classes_
        [ "text-4xl"
        , "text-gray-800"
        , "mb-8"
        ]
      ] do
      fromString "#"
      Lucid.toHtml page.title
      Lucid.span_
        [ Lucid.classes_
          [ "ml-1"
          , "text-sm"
          , "font-normal"
          , "align-top"
          ]
        ] do
        Lucid.toHtml . Text.pack . show $ length posts
    Lucid.ul_
      [ Lucid.classes_
        [ "test-sm"
        , "w-full"
        , "mb-6"
        ]
      ] do
      for_ posts $ \post -> do
        identifier <- Core.localizeKey $ router.post post.metadata.slug
        metadata <- Core.getSiteItemMetadata identifier
        Lucid.li_
          [ Lucid.classes_
            [ "mb-8"
            , "pb-8"
            , "border-b"
            , "border-gray-200"
            ]
          ] do
          PostPreview.render $ PostPreview.Props
            { post = post
            , metadata = metadata
            }

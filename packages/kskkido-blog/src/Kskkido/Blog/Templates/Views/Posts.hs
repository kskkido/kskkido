module Kskkido.Blog.Templates.Views.Posts where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Except as Except
import qualified Data.Has as Has
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Time.Calendar.Extension as Time.Calendar
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
  ) => Lucid.HtmlT m ()
render = do
  router :: Type.Router <- asks Has.getter
  page :: Type.Page <- asks Has.getter
  posts :: [Type.Post] <- asks Has.getter
  let postsByYear = List.NonEmpty.groupWith
        (\post -> Time.Calendar.toYear $ Time.Clock.utctDay post.metadata.createdAt)
        posts
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
      Lucid.toHtml page.title
      Lucid.span_
        [ Lucid.classes_
          [ "ml-1"
          , "text-sm"
          , "align-top"
          ]
        ] do
        Lucid.toHtml . Text.pack . show $ length posts
    for_ postsByYear $ \posts -> do
      let head = List.NonEmpty.head posts
      Lucid.h2_
        [ Lucid.classes_
          [ "text-lg"
          , "mb-6"
          ]
        ] do
        let year = Time.Calendar.toYear $ Time.Clock.utctDay head.metadata.createdAt
        Lucid.toHtml . Text.pack $ show year
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
              [ "pb-8"
              , "mb-8"
              , "border-b"
              , "border-gray-200"
              ]
            ] do
            PostPreview.render $ PostPreview.Props
              { post = post
              , metadata = metadata
              }

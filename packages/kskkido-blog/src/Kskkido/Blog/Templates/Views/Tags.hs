module Kskkido.Blog.Templates.Views.Tags where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Except as Except
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Has as Has
import qualified Kskkido.Blog.Core as Core
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Templates.Components.TagPreview as TagPreview

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
  tags <- Core.tags
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
          , "font-normal"
          , "align-top"
          ]
        ] do
        Lucid.toHtml . Text.pack . show $ length tags
    Lucid.ul_
      [ Lucid.classes_
        [ "test-sm"
        , "w-full"
        ]
      ] do
      for_ tags $ \tag -> do
        let label = CaseInsensitive.original tag
        identifier <- Core.localizeKey $ router.tag label
        metadata <- Core.getSiteItemMetadata identifier
        Lucid.li_
          [ Lucid.classes_
            [ "pb-8"
            , "mb-8"
            , "border-b"
            , "border-gray-200"
            ]
          ] do
          TagPreview.render $ TagPreview.Props
            { tag = tag
            , metadata = metadata
            }

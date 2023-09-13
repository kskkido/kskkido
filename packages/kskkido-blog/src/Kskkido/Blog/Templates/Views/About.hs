module Kskkido.Blog.Templates.Views.About where

import RIO
import qualified Lucid
import qualified Control.Monad.Except as Except
import qualified Data.Has as Has
import qualified Kskkido.Blog.Core as Core
import qualified Kskkido.Blog.Core.Type as Type

render :: 
  ( Has.Has Type.Page r
  , Has.Has Type.Locale r
  , Has.Has Type.Router r
  , Has.Has Type.SiteMap r
  , Has.Has Type.LocalizedDictionary r
  , Has.Has Type.Profile r
  , Except.MonadError Type.CoreException m
  , MonadReader r m
  ) => Lucid.HtmlT m ()
render = do
  page :: Type.Page <- asks Has.getter
  router :: Type.Router <- asks Has.getter
  profile :: Type.Profile <- asks Has.getter
  Lucid.main_
    [ Lucid.classes_ []
    ] do
    Lucid.section_
      [ Lucid.classes_
        [ "pb-8"
        , "border-black"
        , "border-b"
        ]
      ] do
      Lucid.div_
        [ Lucid.classes_
          [ "flex"
          , "items-center"
          , "text-xs"
          , "sm:text-sm"
          , "mb-8"
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
              [ "hover:underline"
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
              [ "text-gray-400"
              ]
            ] do
              Lucid.toHtml page.title
      Lucid.h1_
        [ Lucid.classes_
          [ "text-2xl"
          , "mb-8"
          ]
        ] do
          Lucid.toHtml page.title
      Lucid.p_
        [ Lucid.classes_
          [ "text-base"
          ]
        ] do
        text <- Core.localize "page.home.description"
        Lucid.toHtml text
    Lucid.section_
      [ Lucid.classes_
        [ "py-8"
        , "border-black"
        , "border-b"
        ]
      ] do
      Lucid.h2_
        [ Lucid.classes_
          [ "text-lg"
          , "mb-8"
          ]
        ] do
        text <- Core.localize "socials"
        Lucid.toHtml text
      Lucid.ul_
        [ Lucid.classes_
          [ "test-sm"
          , "w-full"
          , "mb-8"
          ]
        ] do
        for_ profile.socials $ \social -> do
          Lucid.li_
            [ Lucid.classes_
              [ "mb-4"
              ]
            ] do
            Lucid.a_
              [ Lucid.href_ social.url
              ] do
              case social.variant of 
                Type.ProfileSocialVariantGithub -> do
                  Lucid.span_
                    [
                    ] do
                    Lucid.toHtml "Github"
                Type.ProfileSocialVariantLinkedin -> 
                  Lucid.span_
                    [
                    ] do
                    Lucid.toHtml "Linkedin"
                Type.ProfileSocialVariantPersonal -> 
                  Lucid.span_
                    [
                    ] do
                    Lucid.toHtml "Personal"
    Lucid.section_
      [ Lucid.classes_
        [ "py-8"
        , "border-black"
        , "border-b"
        ]
      ] do
      Lucid.h2_
        [ Lucid.classes_
          [ "text-lg"
          , "mb-8"
          ]
        ] do
        text <- Core.localize "resume"
        Lucid.toHtml text
      Lucid.div_
        [ Lucid.classes_
          [ "test-sm"
          , "w-full"
          , "mb-8"
          ]
        ] do
        Lucid.ul_
          [
          ] do
          Lucid.li_
            [
            ] do
            let identifier = Type.Plain "/resume-en.pdf"
            metadata <- Core.getSiteItemMetadata identifier
            Lucid.a_
              [ Lucid.href_ metadata.route
              ] do
              Lucid.toHtml "en"

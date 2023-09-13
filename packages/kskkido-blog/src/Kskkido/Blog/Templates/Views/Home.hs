module Kskkido.Blog.Templates.Views.Home where

import RIO
import qualified Lucid
import qualified Control.Monad.Except as Except
import qualified Data.Has as Has
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
  , Has.Has Type.Profile r
  , Except.MonadError Type.CoreException m
  , MonadReader r m
  ) => Lucid.HtmlT m ()
render = do
  router :: Type.Router <- asks Has.getter
  page :: Type.Page <- asks Has.getter
  posts :: [Type.Post] <- asks Has.getter
  profile :: Type.Profile <- asks Has.getter
  Lucid.main_
    [ Lucid.classes_
      [ "sm:grid"
      , "md:grid-cols-[min-content_1fr]"
      , "sm:grid-rows-[min-content_1fr]"
      , "md:gap-28"
      , "sm:gap-8"
      ]
    ] do
    Lucid.section_
      [ Lucid.classes_
        [ "md:w-[12.5rem]"
        , "sm:w-full"
        , "sm:flex"
        , "md:flex-col"
        , "sm:flex-row"
        , "hidden"
        ]
      ] do
          Lucid.div_
            [ Lucid.classes_
              [ "flex"
              , "align-center"
              , "justify-end"
              , "md:mb-8"
              , "md:mr-0"
              , "mr-8"
              ]
            ] do
            let identifier = Type.Plain "/images/kido.jpeg"
            metadata <- Core.getSiteItemMetadata identifier
            Lucid.img_
              [ Lucid.classes_
                [ "lazy"
                , "rounded"
                , "object-cover"
                , "md:w-[12.5rem]"
                , "md:h-[12.5rem]"
                , "w-28"
                , "h-28"
                , "border"
                , "border-gray-100"
                , "grayscale-[80%]"
                ]
              , Lucid.alt_ "Me"
              , Lucid.data_ "src" metadata.route
              ]
          Lucid.div_
            [ Lucid.classes_
              [ "flex"
              , "flex-col"
              , "flex-grow"
              , "md:self-start"
              , "self-center"
              ]
            ] do
            Lucid.div_
              [ Lucid.classes_
                [ "mb-6"
                ]
              ] do
              Lucid.span_
                [ Lucid.classes_
                  [ "text-base"
                  , "text-gray-800"
                  , "block"
                  ]
                ] do
                  text <- Core.localize "name"
                  Lucid.toHtml text
              Lucid.span_
                [ Lucid.classes_
                  [ "text-sm"
                  , "text-gray-700"
                  , "block"
                  ]
                ] do
                  text <- Core.localize "title"
                  Lucid.toHtml text
            Lucid.div_
              [ Lucid.classes_
                [ "flex"
                , "flex-col"
                , "items-start"
                , "text-gray-500"
                , "text-sm"
                ]
              ] do
              Lucid.ul_
                [ Lucid.classes_
                  [ "flex"
                  , "md:flex-col"
                  , "flex-row"
                  , "w-full"
                  ]
                ] do
                for_ profile.socials $ \social -> do
                  Lucid.li_
                    [ Lucid.classes_
                      [ "mb-0.5"
                      , "md:mr-0"
                      , "mr-2"
                      ]
                    ] do
                    case social.variant of 
                      Type.ProfileSocialVariantGithub -> do
                        Lucid.a_
                          [ Lucid.href_ social.url
                          , Lucid.target_ "blank"
                          , Lucid.classes_
                            [ "text-link"
                            ]
                          ] do
                          Lucid.span_
                            [
                            ] do
                            label <- Core.localize "github"
                            Lucid.toHtml label
                      Type.ProfileSocialVariantLinkedin -> 
                        Lucid.a_
                          [ Lucid.href_ social.url
                          , Lucid.target_ "blank"
                          , Lucid.classes_
                            [ "text-link"
                            ]
                          ] do
                          Lucid.span_
                            [
                            ] do
                            label <- Core.localize "linkedin"
                            Lucid.toHtml label
                      Type.ProfileSocialVariantPersonal -> 
                        Lucid.a_
                          [ Lucid.href_ ("mailto:" <> social.url)
                          , Lucid.classes_
                            [ "text-link"
                            ]
                          ] do
                          Lucid.span_
                            [
                            ] do
                            Lucid.toHtml social.url
              Lucid.ul_
                [ Lucid.classes_
                  [ "flex"
                  , "md:flex-col"
                  , "flex-row"
                  , "w-full"
                  ]
                ] do
                Lucid.li_
                  [ Lucid.classes_
                    [ "mb-0.5"
                    , "md:mr-0"
                    , "mr-2"
                    ]
                  ] do
                  let identifier = Type.Plain "/resume-en.pdf"
                  metadata <- Core.getSiteItemMetadata identifier
                  label <- Core.localize "resume.en"
                  Lucid.a_
                    [ Lucid.href_ metadata.route
                    , Lucid.classes_
                      [ "text-link"
                      ]
                    ] do
                    Lucid.toHtml label
                Lucid.li_
                  [ Lucid.classes_
                    [ "mb-0.5"
                    , "md:mr-0"
                    , "mr-2"
                    ]
                  ] do
                  let identifier = Type.Plain "/resume-en.pdf"
                  metadata <- Core.getSiteItemMetadata identifier
                  label <- Core.localize "resume.ja"
                  Lucid.a_
                    [ Lucid.href_ metadata.route
                    , Lucid.classes_
                      [ "text-link"
                      ]
                    ] do
                    Lucid.toHtml label
    Lucid.section_
      [ Lucid.classes_
        [
        ]
      ] do
      Lucid.div_
        [ Lucid.classes_
          [ "mb-12"
          , "border-gray-200"
          , "border-b"
          ]
        ] do
        Lucid.h1_
          [ Lucid.classes_
            [ "text-4xl"
            , "text-gray-800"
            , "font-medium"
            , "mb-4"
            ]
          ] do
            Lucid.toHtml page.title
        Lucid.p_
          [ Lucid.classes_
            [ "text-base"
            , "mb-12"
            ]
          ] do
          text <- Core.localize "page.home.description"
          Lucid.toHtml text
        Lucid.div_
          [ Lucid.classes_
            [ "sm:hidden"
            , "flex"
            , "flex-col"
            , "items-start"
            , "mb-6"
            , "text-gray-500"
            , "text-sm"
            ]
          ] do
          Lucid.ul_
            [ Lucid.classes_
              [ "flex"
              , "flex-row"
              , "w-full"
              ]
            ] do
            for_ profile.socials $ \social -> do
              Lucid.li_
                [ Lucid.classes_
                  [ "mb-0.5"
                  , "mr-2"
                  ]
                ] do
                case social.variant of 
                  Type.ProfileSocialVariantGithub -> do
                    Lucid.a_
                      [ Lucid.href_ social.url
                      , Lucid.classes_
                        [ "text-link"
                        ]
                      ] do
                      Lucid.span_
                        [
                        ] do
                        label <- Core.localize "github"
                        Lucid.toHtml label
                  Type.ProfileSocialVariantLinkedin -> 
                    Lucid.a_
                      [ Lucid.href_ social.url
                      , Lucid.classes_
                        [ "text-link"
                        ]
                      ] do
                      Lucid.span_
                        [
                        ] do
                        label <- Core.localize "linkedin"
                        Lucid.toHtml label
                  Type.ProfileSocialVariantPersonal -> 
                    Lucid.a_
                      [ Lucid.href_ ("mailto:" <> social.url)
                      , Lucid.classes_
                        [ "text-link"
                        ]
                      ] do
                      Lucid.span_
                        [
                        ] do
                        Lucid.toHtml social.url
          Lucid.ul_
            [ Lucid.classes_
              [ "flex"
              , "flex-row"
              , "w-full"
              ]
            ] do
            Lucid.li_
              [ Lucid.classes_
                [ "mb-0.5"
                , "mr-2"
                ]
              ] do
              let identifier = Type.Plain "/resume-en.pdf"
              metadata <- Core.getSiteItemMetadata identifier
              label <- Core.localize "resume.en"
              Lucid.a_
                [ Lucid.href_ metadata.route
                , Lucid.classes_
                  [ "text-link"
                  ]
                ] do
                Lucid.toHtml label
            Lucid.li_
              [ Lucid.classes_
                [ "mb-0.5"
                , "mr-2"
                ]
              ] do
              let identifier = Type.Plain "/resume-en.pdf"
              metadata <- Core.getSiteItemMetadata identifier
              label <- Core.localize "resume.ja"
              Lucid.a_
                [ Lucid.href_ metadata.route
                , Lucid.classes_
                  [ "text-link"
                  ]
                ] do
                Lucid.toHtml label
      Lucid.div_
        [ Lucid.classes_
          [ "pb-12"
          , "mb-12"
          ]
        ] do
        Lucid.div_
          [ Lucid.classes_
            [ "mb-10"
            , "flex"
            , "flex-row"
            , "justify-between"
            ]
          ] do
          Lucid.h2_
            [ Lucid.classes_
              [ "text-base"
              , "text-gray-700"
              ]
            ] do
            text <- Core.localize "recent-posts"
            Lucid.toHtml text
          do
            identifier <- Core.localizeKey router.posts
            metadata <- Core.getSiteItemMetadata identifier
            text <- Core.localize "view-all"
            Lucid.a_
              [ Lucid.href_ metadata.route
              , Lucid.classes_
                [ "flex"
                , "items-center"
                , "text-sm"
                , "text-gray-500"
                , "text-link"
                ]
              ] do
              Lucid.span_
                [ Lucid.classes_
                  [ "pr-2"
                  ]
                ] do
                Lucid.toHtml text
              Lucid.i_
                [ Lucid.classes_
                  [ "fa-solid"
                  , "fa-arrow-right"
                  ]
                ] do
                mempty
        Lucid.ul_
          [ Lucid.classes_
            [ "test-sm"
            , "w-full"
            , "mb-8"
            ]
          ] do
          for_ (take 5 posts) $ \post -> do
            identifier <- Core.localizeKey $ router.post post.metadata.slug
            metadata <- Core.getSiteItemMetadata identifier
            Lucid.li_
              [ Lucid.classes_
                [ "pb-6"
                , "mb-6"
                , "border-b"
                , "border-gray-200"
                ]
              ] do
              PostPreview.render $ PostPreview.Props
                { post = post
                , metadata = metadata
                }


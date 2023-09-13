module Kskkido.Blog.Templates.Views.Post where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.List.Extension as List
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Has as Has
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Time.Calendar.Extension as Time.Calendar
import qualified Kskkido.Blog.Core as Core
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Templates.Components.TableOfContents as TableOfContents

render :: 
  ( Has.Has Type.Page r
  , Has.Has Type.Locale r
  , Has.Has Type.Router r
  , Has.Has Type.SiteMap r
  , Has.Has Type.LocalizedDictionary r
  , Has.Has [Type.Post] r
  , Except.MonadError Type.CoreException m
  , MonadReader r m
  ) => Type.Post -> Lucid.HtmlT m ()
render post = do
  router :: Type.Router <- asks Has.getter
  page :: Type.Page <- asks Has.getter
  posts :: [Type.Post] <- asks Has.getter
  Lucid.main_
    [ Lucid.classes_
      [ "flex"
      , "flex-col"
      , "flex-grow"
      ]
    ] do
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
        identifier <- Core.localizeKey router.posts
        metadata <- Core.getSiteItemMetadata identifier
        Lucid.a_
          [ Lucid.href_ metadata.route
          , Lucid.classes_
            [ "text-link"
            ]
          ] do
            text <- Core.localize "navigation.posts"
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
    Lucid.div_
      [ Lucid.classes_
        [ "border-b"
        , "border-gray-200"
        , "pb-12"
        , "mb-14"
        ]
      ] do
      Lucid.p_
        [ Lucid.classes_
          [ "text-base"
          , "text-gray-600"
          ]
        ] do
        Lucid.toHtml post.metadata.description
      Lucid.div_
        [ Lucid.classes_
          [ "block"
          , "text-sm"
          , "text-gray-500"
          , "mt-2"
          ]
        ] do
        Lucid.span_
          [ Lucid.classes_
            [ "block"
            , "text-sm"
            , "mb-1"
            ]
          ] do
          let date = Time.Calendar.toText $ Time.Clock.utctDay post.metadata.createdAt
          Lucid.toHtml date
        void $ Maybe.runMaybeT do
          guard (page.locale == Type.EN)
          lift do
            Lucid.span_
              [ Lucid.classes_
                [ "block"
                , "text-sm"
                , "mb-1"
                ]
              ] do
              Lucid.toHtml $ Text.intercalate " "
                [ Text.pack $ show post.metadata.wordCount
                , fromString "words"
                ]
        Lucid.ul_
          [ Lucid.classes_
            [ "text-sm"
            ]
          ] do
          for_ post.metadata.tags $ \tag -> do
            let label = CaseInsensitive.original tag
            identifier <- Core.localizeKey $ router.tag label
            metadata <- Core.getSiteItemMetadata identifier
            Lucid.li_
              [ Lucid.classes_
                [ "inline-block"
                , "mr-2"
                ]
              ] do
              Lucid.a_
                [ Lucid.href_ metadata.route
                , Lucid.classes_
                  [ "hover:underline"
                  ]
                ] do
                Lucid.toHtml $ Text.intercalate ""
                  [ fromString "#"
                  , label
                  ]
    Lucid.articleContainer_
      [ Lucid.classes_
        [ "flex-grow"
        , "md:grid"
        , "md:grid-cols-[min-content_1fr]"
        , "gap-28"
        ]
      , Lucid.data_
          "article-element-query"
          "#post-article"
      , Lucid.data_
          "table-of-contents-element-query"
          "#table-of-contents"
      ] do
      Lucid.section_
        [ Lucid.classes_
          [ "hidden"
          , "md:block"
          , "self-start"
          , "flex-1"
          , "w-56"
          , "sticky"
          , "top-14"
          ]
        ] do
        Lucid.div_
          [
          ] do
          TableOfContents.render $ TableOfContents.Props
            { tableOfContents = post.metadata.tableOfContents
            , attributes =
                [ Lucid.classes_
                  [ "text-sm"
                  , "table-of-contents"
                  ]
                , Lucid.id_ "table-of-contents"
                ]
            }
      Lucid.section_
        [ Lucid.classes_
          [ "flex"
          , "flex-col"
          , "overflow-hidden"
          ]
        ] do
        Lucid.article_
          [ Lucid.classes_
            [ "flex-grow"
            , "markdown"
            , "border-b"
            , "border-gray-200"
            ]
          , Lucid.id_ "post-article"
          ] do
          Lucid.toHtmlRaw post.body
        Lucid.div_
          [ Lucid.classes_
            [ "py-8"
            , "grid"
            , "grid-cols-2"
            ]
          ] do
          Lucid.div_
            [ Lucid.classes_
              [ "col-start-1"
              ]
            ] do
            void $ Maybe.runMaybeT do
              previous <- Maybe.MaybeT $ pure do
                List.after post posts
              let key = router.post previous.metadata.slug
              identifier <- Core.localizeKey key
              metadata <- Core.getSiteItemMetadata identifier
              lift do
                Lucid.div_
                  [
                  ] do
                  Lucid.span_
                    [ Lucid.classes_
                      [ "block"
                      , "mb-4"
                      , "text-sm"
                      , "text-gray-400"
                      ]
                    ] do
                    text <- Core.localize "article.previous"
                    Lucid.toHtml text
                  Lucid.a_
                    [ Lucid.classes_
                      [ "text-link"
                      , "hover:underline"
                      ]
                    , Lucid.href_ metadata.route
                    ] do
                    Lucid.toHtml previous.metadata.title
          Lucid.div_
            [ Lucid.classes_
              [ "grid"
              , "justify-items-end"
              , "col-end-3"
              ]
            ] do
            void $ Maybe.runMaybeT do
              next <- Maybe.MaybeT $ pure do
                List.before post posts
              let key = router.post next.metadata.slug
              identifier <- Core.localizeKey key
              metadata <- Core.getSiteItemMetadata identifier
              lift do
                Lucid.div_
                  [ Lucid.classes_
                    [ "text-right"
                    ]
                  ] do
                  Lucid.span_
                    [ Lucid.classes_
                      [ "block"
                      , "mb-4"
                      , "text-sm"
                      , "text-gray-400"
                      ]
                    ] do
                    text <- Core.localize "article.next"
                    Lucid.toHtml text
                  Lucid.a_
                    [ Lucid.classes_
                      [ "text-link"
                      , "hover:underline"
                      ]
                    , Lucid.href_ metadata.route
                    ] do
                    Lucid.toHtml next.metadata.title

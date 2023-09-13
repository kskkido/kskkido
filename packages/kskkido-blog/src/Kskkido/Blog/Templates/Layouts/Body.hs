module Kskkido.Blog.Templates.Layouts.Body where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Has as Has
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Time.Calendar as Time.Calendar
import qualified Lucid
import qualified Kskkido.Blog.Core as Core
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Capability.Date as Capability.Date
import qualified Kskkido.Blog.Templates.Components.PageSearch as PageSearch

render ::
  ( Has.Has Type.Page r
  , Has.Has Type.Locale r
  , Has.Has Type.LocalizedDictionary r
  , Has.Has Type.Router r
  , Has.Has Type.SiteMap r
  , Capability.Date.Date m
  , Except.MonadError Type.CoreException m
  , MonadReader r m
  ) => Lucid.HtmlT m () -> Lucid.HtmlT m ()
render html = do
  router :: Type.Router <- asks Has.getter
  page :: Type.Page <- asks Has.getter
  now <- lift Capability.Date.getCurrentTime
  Lucid.div_
    [ Lucid.classes_
      [ "flex"
      , "flex-col"
      , "min-h-screen"
      , "max-w-5xl"
      , "w-11/12"
      , "m-auto"
      , "md:pt-4"
      , "text-gray-600"
      ]
    ] do
    Lucid.header_
      [ Lucid.classes_
        [ "text-xs"
        , "text-gray-400"
        , "h-16"
        , "flex"
        , "flex-wrap"
        , "items-center"
        , "justify-between"
        , "w-full"
        ]
      ] do
      Lucid.ul_
        [ Lucid.classes_
          [ "flex"
          , "justify-items-start"
          ]
        ] do
        Lucid.li_
          [ Lucid.classes_
            [ "md:mr-14"
            , "mr-6"
            ]
          ] do
          identifier <- Core.localizeKey router.home
          metadata <- Core.getSiteItemMetadata identifier
          label <- Core.localize "navigation.home"
          Lucid.a_
            [ Lucid.href_ metadata.route
            , Lucid.classes_
              [ "text-link"
              , fold do
                  guard (metadata.route == page.route)
                  ["active"]
              ]
            ] do
            Lucid.toHtml label
        Lucid.li_
          [ Lucid.classes_
            [ "md:mr-14"
            , "mr-6"
            ]
          ] do
          identifier <- Core.localizeKey router.posts
          metadata <- Core.getSiteItemMetadata identifier
          label <- Core.localize "navigation.posts"
          Lucid.a_
            [ Lucid.href_ metadata.route
            , Lucid.classes_
              [ "text-link"
              , fold do
                  guard (metadata.route `Text.isPrefixOf` page.route)
                  ["active"]
              ]
            ] do
            Lucid.toHtml label
        Lucid.li_
          [ Lucid.classes_
            [ "md:mr-14"
            , "mr-6"
            ]
          ] do
          identifier <- Core.localizeKey router.tags
          metadata <- Core.getSiteItemMetadata identifier
          label <- Core.localize "navigation.tags"
          Lucid.a_
            [ Lucid.href_ metadata.route
            , Lucid.classes_
              [ "text-link"
              , fold do
                  guard (metadata.route `Text.isPrefixOf` page.route)
                  ["active"]
              ]
            ] do
            Lucid.toHtml label
      Lucid.div_
        [ Lucid.classes_
          [ "flex"
          , "justify-items-end"
          ]
        ] do
        do
          locales <- Core.localesByIdentifier page.identifier
          let currentLocale = page.locale
          Lucid.select_
            [ Lucid.classes_
              [ "md:mr-14"
              , "mr-6"
              , "bg-inherit"
              ]
            , Lucid.onchange_
                "location.href=this.value"
            ] do
            for_ locales $ \locale -> do
              Maybe.runMaybeT do
                local (Has.modifier $ const locale) do
                  identifier <- Core.localizeIdentifier page.identifier
                  metadata <- Core.lookupSiteItemMetadata identifier
                  text <- Core.localize $ Text.pack $ show locale
                  lift do
                    Lucid.option_
                      ( fold
                        [ do
                            guard (locale == currentLocale) 
                            pure $ Lucid.selected_ $ fromString "" 
                        , [ Lucid.value_ metadata.route
                          ]
                        ]
                      ) do
                        Lucid.toHtml text
        Lucid.div_
          [ Lucid.classes_
            [ 
            ]
          ] do
          PageSearch.render
            [
            ]
    Lucid.div_
      [ Lucid.classes_
        [ "flex"
        , "flex-col"
        , "flex-grow"
        , "sm:mt-16"
        , "sm:mb-12"
        , "my-6"
        ]
      ] do
      html
    Lucid.footer_
      [ Lucid.classes_
        [ "text-xs"
        , "text-gray-400"
        , "h-16"
        , "flex"
        , "flex-wrap"
        , "items-center"
        , "justify-between"
        , "w-full"
        ]
      ] do
      Lucid.span_
        [ Lucid.classes_
          [
          ]
        ] do
        let (year, _, _) = Time.Calendar.toGregorian $ Time.Clock.utctDay now
        Lucid.toHtml $ Text.intercalate " | "
          [ Text.intercalate " "
            [ "Copyright"
            , "©"
            , Text.pack $ show year
            ]
          , "All rights reserved."
          ]
      Lucid.a_
        [ Lucid.href_ "https://github.com/kskkido/kskkido"
        , Lucid.target_ "blank"
        , Lucid.classes_
          [ "text-link"
          ]
        ] do
        fromString "Built with Haskell and tears"
    Lucid.script_
      [ Lucid.src_ "https://cdn.jsdelivr.net/npm/vanilla-lazyload@17.8.5/dist/lazyload.min.js"
      , Lucid.async_ "true"
      ] do
        fromString ""

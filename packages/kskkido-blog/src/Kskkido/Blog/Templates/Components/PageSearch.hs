module Kskkido.Blog.Templates.Components.PageSearch where

import RIO
import qualified Control.Monad.Except as Except
import qualified Data.Has as Has
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Lucid.Svg
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Core as Core

render :: 
  ( Has.Has Type.Locale r
  , Has.Has Type.LocalizedDictionary r
  , Except.MonadError Type.CoreException m
  , MonadReader r m
  ) => [Lucid.Attribute] -> Lucid.HtmlT m ()
render attributes = do
  Lucid.pageSearch_
    ( [ Lucid.classes_
        [ "ms-auto"
        ]
      , Lucid.data_ "element-id" "search"
      ] `Lucid.concatAttributes` attributes
    ) do
    Lucid.button_
      [ Lucid.data_ "type" "show-modal"
      ] do
      Lucid.Svg.svg_
        [ Lucid.classes_
          [ "h-5"
          , "w-5"
          ]
        , Lucid.Svg.width_ "16"
        , Lucid.Svg.height_ "16"
        , Lucid.Svg.viewBox_ "0 0 24 24"
        , Lucid.Svg.fill_ "none"
        , Lucid.Svg.stroke_ "currentColor"
        , Lucid.Svg.stroke_linecap_ "round"
        , Lucid.Svg.stroke_linejoin_ "round"
        , Lucid.Svg.stroke_width_ "1.5"
        ] do
        Lucid.Svg.path_
          [ Lucid.Svg.stroke_ "none"
          , Lucid.Svg.d_ "M0 0h24v24H0z"
          ]
        Lucid.Svg.path_
          [ Lucid.Svg.d_ "M3 10a7 7 0 1 0 14 0 7 7 0 1 0-14 0M21 21l-6-6"
          ]
    Lucid.dialog_
      [ Lucid.classes_
        [ "h-full"
        , "max-h-full"
        , "w-full"
        , "max-w-full"
        , "border"
        , "border-zinc-400"
        , "bg-bgColor"
        , "shadow"
        , "backdrop:backdrop-blur"
        , "sm:mx-auto"
        , "sm:mb-auto"
        , "sm:mt-16"
        , "sm:h-max"
        , "sm:max-h-[calc(100%-8rem)]"
        , "sm:min-h-[15rem]"
        , "sm:w-5/6"
        , "sm:max-w-[48rem]"
        , "sm:rounded-md"
        ]
      ] do
      Lucid.div_
        [ Lucid.classes_
          [ "flex"
          , "flex-col"
          , "gap-4"
          , "p-6"
          , "pt-12"
          , "sm:pt-6"
          ]
        , Lucid.data_ "type" "dialog-frame"
        ] do
        Lucid.button_
          [ Lucid.classes_
            [ "ms-auto"
            , "cursor-pointer"
            , "rounded-md"
            , "p-2"
            , "dark:bg-zinc-700"
            ]
          , Lucid.data_ "type" "hide-modal"
          ] do
          label <- Core.localize "page-search.close"
          Lucid.toHtml label
        Lucid.div_
          [ Lucid.classes_
            [ "search-container"
            ]
          ] do
          Lucid.div_
            [ Lucid.id_ "search"
            ] do
            mempty


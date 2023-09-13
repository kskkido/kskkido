module Kskkido.Blog.Templates.Components.TagPreview where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Data.Has as Has
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Core as Core

data Props = Props
  { tag :: Type.Tag
  , metadata :: Type.SiteItemMetadata
  }

render :: (Has.Has [Type.Post] r, MonadReader r m) => Props -> Lucid.HtmlT m ()
render props = do
  let label = CaseInsensitive.original props.tag
  posts <- Core.postsByTag props.tag
  Lucid.div_
    [ Lucid.classes_
      [ "w-full"
      ]
    ] do
    Lucid.div_
      [ Lucid.classes_
        [ "flex"
        , "sm:flex-row"
        , "sm:justify-between"
        , "sm:items-center"
        , "flex-col"
        ]
      ] do
      Lucid.a_
        [ Lucid.href_ props.metadata.route
        , Lucid.classes_
          [ "block"
          , "text-lg"
          , "text-gray-800"
          , "col-start-1"
          , "hover:underline"
          ]
        ] do
        fromString "#"
        Lucid.toHtml label
      Lucid.span_
        [ Lucid.classes_
          [ "text-sm"
          , "text-gray-500"
          , "col-start-2"
          , "justify-self-end"
          ]
        ] do
        Lucid.toHtml $ Text.intercalate " "
          [ Text.pack . show $ length posts
          , fromString "posts"
          ]

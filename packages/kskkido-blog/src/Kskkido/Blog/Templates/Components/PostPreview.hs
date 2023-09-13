module Kskkido.Blog.Templates.Components.PostPreview where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Except as Except
import qualified Data.Has as Has
import qualified Data.Foldable as Foldable
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Time.Calendar.Extension as Time.Calendar
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Core as Core

data Props = Props
  { post :: Type.Post
  , metadata :: Type.SiteItemMetadata
  }

render :: 
  ( Has.Has Type.Locale r
  , Has.Has Type.Router r
  , Has.Has Type.SiteMap r
  , Except.MonadError Type.CoreException m
  , MonadReader r m
  ) => Props -> Lucid.HtmlT m ()
render props = do
  locale :: Type.Locale <- asks Has.getter
  router :: Type.Router <- asks Has.getter
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
        , "mb-2"
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
        Lucid.toHtml props.post.metadata.title
      Lucid.span_
        [ Lucid.classes_
          [ "text-sm"
          , "text-gray-500"
          , "col-start-2"
          , "justify-self-end"
          ]
        ] do
        let date = Time.Calendar.toText $ Time.Clock.utctDay props.post.metadata.createdAt
        Lucid.toHtml $ Text.intercalate " | " $ catMaybes
          [ pure date
          , do
              guard (locale == Type.EN)
              pure $ Text.intercalate " "
                [ Text.pack $ show props.post.metadata.wordCount
                , fromString "words"
                ]
          ]
    Lucid.ul_
      [ Lucid.classes_
        [ "col-span-full"
        , "text-sm"
        , "text-gray-500"
        , "mb-4"
        ]
      ] do
      flip Foldable.foldMap props.post.metadata.tags $ \tag -> do
        let label = CaseInsensitive.original tag
            key = router.tag label
        identifier <- Core.localizeKey key
        metadata <- Core.getSiteItemMetadata identifier
        Lucid.li_
          [ Lucid.classes_
            [ "inline"
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
    Lucid.p_
      [ Lucid.classes_
        [ "text-sm"
        , "text-gray-500"
        , "col-span-full"
        ]
      ] do
      Lucid.toHtml props.post.metadata.description

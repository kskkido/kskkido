module Kskkido.Blog.Templates.Components.TableOfContents where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Extension as Lucid
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Tree as Tree
import qualified Kskkido.Blog.Core.Type as Type

data Props = Props
  { tableOfContents :: Tree.Forest Type.TableOfContentsHeader
  , attributes :: [Lucid.Attribute]
  }

render :: Monad m => Props -> Lucid.HtmlT m ()
render props = do
  Lucid.ul_
    ( [ Lucid.classes_
        [ "table-of-contents"
        ]
      ] `Lucid.concatAttributes`
      props.attributes
    ) do
    for_ props.tableOfContents $ \(Tree.Node header subtree) -> do
      Lucid.li_
        [
        ] do
        Lucid.div_
          [ Lucid.classes_
            [ "mb-2"
            ]
          , Lucid.data_
              "target"
              header.identifier
          ] do
          Lucid.span_
            [ Lucid.classes_
              [ "mr-2"
              ]
            ] do
            Lucid.toHtml $ Text.pack "-"
          Lucid.a_
            [ Lucid.classes_
              [ "hover:underline"
              ]
            , Lucid.href_ $ Text.pack "#" <> header.identifier
            ] do
            Lucid.toHtml $ header.title
        void $ Maybe.runMaybeT do
          guard (not $ null subtree)
          lift do
            iterate subtree

iterate :: (Monad m) => Tree.Forest Type.TableOfContentsHeader -> Lucid.HtmlT m ()
iterate forest = do
  Lucid.ul_
    [ Lucid.classes_
      [ "pl-3"
      ]
    ] do
    for_ forest $ \(Tree.Node header subtree) -> do
      Lucid.li_
        [ Lucid.classes_
          [ "mb-2"
          ]
        ] do
        Lucid.div_
          [ Lucid.classes_
            [ "mb-2"
            ]
          , Lucid.data_
              "target"
              header.identifier
          ] do
          Lucid.span_
            [ Lucid.classes_
              [ "mr-2"
              ]
            ] do
            Lucid.toHtml $ Text.pack "-"
          Lucid.a_
            [ Lucid.classes_
              [ "hover:underline"
              ]
            , Lucid.href_ $ Text.pack "#" <> header.identifier
            ] do
            Lucid.toHtml $ header.title
        void $ Maybe.runMaybeT do
          guard (not $ null subtree)
          lift do
            iterate subtree

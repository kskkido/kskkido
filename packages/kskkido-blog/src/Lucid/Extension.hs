module Lucid.Extension where

import RIO hiding (concat)
import qualified RIO.Text as Text
import qualified Lucid
import qualified Lucid.Base
import qualified Control.Monad
import qualified Data.Maybe as Maybe
import qualified Data.Maybe.Extension as Maybe

attributeToClass :: Lucid.Attribute -> Maybe.Maybe Text.Text
attributeToClass (Lucid.Base.Attribute key val) = do
  Control.Monad.guard (key == "class")
  pure val

concatAttributes :: [Lucid.Attribute] -> [Lucid.Attribute] -> [Lucid.Attribute]
concatAttributes xs ys = concatClasses (xs <> ys)

concatClasses :: [Lucid.Attribute] -> [Lucid.Attribute]
concatClasses xs =
  let (cs,rs) = Maybe.partition attributeToClass xs
   in rs <> [Lucid.classes_ $ cs >>= Text.words]

pageSearch_ :: Lucid.Term arg result => arg -> result
pageSearch_ = Lucid.term "page-search"

articleContainer_ :: Lucid.Term arg result => arg -> result
articleContainer_ = Lucid.term "article-container"

dialog_ :: Lucid.Term arg result => arg -> result
dialog_ = Lucid.term "dialog"

nomodule_ :: Text -> Lucid.Attribute
nomodule_ = Lucid.Base.makeAttribute "nomodule"

ionIcon_ :: Lucid.Term arg result => arg -> result
ionIcon_ = Lucid.term "ion-icon"


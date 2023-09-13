module Kskkido.Blog.Interpreter.Generator.Type where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Has as Has
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Tree as Tree
import qualified Data.Validation as Validation
import qualified Text.Pandoc as Pandoc
import qualified Kskkido.Blog.Core.Type as Type

data GeneratorConfig = GeneratorConfig
  { postsFilePath :: String
  , assetsFilePath :: String
  , resumeFilePath :: String
  , profileFilePath :: String
  , localizedDictionaryFilePath :: String
  , pandocReaderOptions :: Pandoc.ReaderOptions
  , pandocWriterOptions :: Pandoc.WriterOptions
  }

instance Has.Has Pandoc.ReaderOptions GeneratorConfig where
  getter context = context.pandocReaderOptions
  modifier fn context = context { pandocReaderOptions = fn context.pandocReaderOptions }
instance Has.Has Pandoc.WriterOptions GeneratorConfig where
  getter context = context.pandocWriterOptions
  modifier fn context = context { pandocWriterOptions = fn context.pandocWriterOptions }


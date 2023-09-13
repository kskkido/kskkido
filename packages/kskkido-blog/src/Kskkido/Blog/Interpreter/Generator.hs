module Kskkido.Blog.Interpreter.Generator where

import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified RIO.Map as Map
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Maybe as Maybe
import qualified Data.Has as Has
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Data.Yaml as Yaml
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Highlighting as Pandoc.Highlighting
import qualified System.IO as IO
import qualified System.FilePath as FilePath
import qualified System.Directory.Extension as Directory
import qualified Kskkido.Blog.Capability.Date as Capability.Date
import qualified Kskkido.Blog.Capability.Repository as Capability.Repository
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Core as Core
import qualified Kskkido.Blog.Interpreter.Generator.Type as Type
import qualified Kskkido.Blog.Generator.Type as Type

newtype Generator a = Generator
  { unwrap :: Reader.ReaderT Type.GeneratorConfig IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Type.GeneratorConfig
    )

instance Except.MonadError Type.CoreException Generator where
    throwError :: Type.CoreException -> Generator a
    throwError = liftIO . throwIO
    catchError :: Generator a -> (Type.CoreException -> Generator a) -> Generator a
    catchError app handler = Generator $ ReaderT $ \config -> do
      let ioAction = runReaderT (app.unwrap) config
      ioAction `catch` \exception -> runReaderT ((handler exception).unwrap) config

instance Capability.Date.Date Generator where
  getCurrentTime = getCurrentTime

instance Capability.Repository.Repository Generator where
  getAssets = getAssets
  getHighlightStyleAssets = getHighlightStyleAssets
  getProfile = getProfile
  getPosts = getPosts
  getLocalizedDictionary = getLocalizedDictionary

toIO :: Type.GeneratorConfig -> Generator a -> IO (Either Type.CoreException a)
toIO config (Generator action) = do
  try $ Reader.runReaderT action config

getSiteConfig :: (Has.Has Type.GeneratorConfig r, Has.Has Pandoc.ReaderOptions r, Has.Has Pandoc.WriterOptions r, MonadIO m, MonadReader r m) => m Type.SiteConfig
getSiteConfig = do
  time <- getCurrentTime
  posts <- getPosts
  profile <- getProfile
  assets <- fold <$> sequence
    [ getHighlightStyleAssets
    , getAssets
    ]
  localizedDictionary <- getLocalizedDictionary
  pure $ Type.SiteConfig
    { time = time
    , defaultLocale = Type.EN
    , locales = [Type.EN, Type.JA]
    , router = Type.Router
        { home = "/"
        , about = "/about"
        , posts = "/posts"
        , post = \slug -> Text.pack $ "/posts" FilePath.</> Text.unpack slug
        , tags = "/tags"
        , tag = \label -> Text.pack $ "/tags" FilePath.</> Text.unpack label
        }
    , posts = posts
    , assets = assets
    , profile = profile
    , localizedDictionary = localizedDictionary
    }

getCurrentTime :: MonadIO m => m Time.Clock.UTCTime
getCurrentTime = liftIO do
  Time.Clock.getCurrentTime

getAssets :: (Has.Has Type.GeneratorConfig r, MonadIO m, MonadReader r m) => m [Type.Asset]
getAssets = do
  config :: Type.GeneratorConfig <- asks Has.getter
  filePaths <- liftIO do
    zip
      <$> fold 
        [ Directory.listFilePaths config.assetsFilePath
        , Directory.listFilePaths config.resumeFilePath
        ]
      <*> fold 
        [ Directory.listRelativeFilePaths config.assetsFilePath
        , Directory.listRelativeFilePaths config.resumeFilePath
        ]
  foldM
    ( \assets (absolute, relative) -> do
        let path = "/" <> dropWhile (== '/') relative
        file <- liftIO do
          ByteString.Lazy.readFile absolute
        pure $ assets <> [Type.Asset (Text.pack path) file]
    )
    []
    filePaths

getHighlightStyleAssets :: (Has.Has Type.GeneratorConfig r, MonadIO m, MonadReader r m) => m [Type.Asset]
getHighlightStyleAssets = do
  config :: Type.GeneratorConfig <- asks Has.getter
  pure $ fromMaybe mempty do
    style <- Pandoc.writerHighlightStyle config.pandocWriterOptions
    pure
      [ Type.Asset 
          { filePath = "/styles/syntax.css"
          , file = fromString $ Pandoc.Highlighting.styleToCss style
          }
      ]

getProfile :: (Has.Has Type.GeneratorConfig r, MonadIO m, MonadReader r m) => m Type.Profile
getProfile = do
  config :: Type.GeneratorConfig <- asks Has.getter
  result <- Maybe.runMaybeT do
    decodeDataFile config.profileFilePath
  liftIO do
    case result of 
      Nothing -> fail "Missing profile"
      Just profile -> pure profile

decodeDataFile :: MonadIO m => Aeson.FromJSON a => FilePath -> Maybe.MaybeT m a
decodeDataFile filePath = do
  fileType <- Maybe.MaybeT . pure $ dataFileTypeFromFilePath filePath
  case fileType of 
    Type.DataFileTypeJson -> do
      result <- liftIO do 
        Aeson.decodeFileStrict filePath
      Maybe.MaybeT $ pure result
    Type.DataFileTypeYaml -> do
      result <- either (const Nothing) pure <$> liftIO do 
        Yaml.decodeFileEither filePath
      Maybe.MaybeT $ pure result

dataFileTypeFromFilePath :: FilePath -> Maybe Type.DataFileType
dataFileTypeFromFilePath filePath = do
  let extension = FilePath.takeExtension filePath
  case extension of
    ".yml" -> pure Type.DataFileTypeYaml
    ".yaml" -> pure Type.DataFileTypeYaml
    ".json" -> pure Type.DataFileTypeJson
    _ -> Nothing

getPosts :: (Has.Has Type.GeneratorConfig r, Has.Has Pandoc.ReaderOptions r, Has.Has Pandoc.WriterOptions r, MonadIO m, MonadReader r m) => m [Type.Post]
getPosts = do
  config :: Type.GeneratorConfig <- asks Has.getter
  filePaths <- liftIO do
    filePaths <- Directory.listFilePaths config.postsFilePath
    pure $ flip filter filePaths $ \filePath -> isJust do
      let extension = FilePath.takeExtension filePath
      guard $ extension == ".md"
  posts <- foldM
    ( \posts filePath -> do
        markdown <- liftIO do
          body <- ByteString.readFile filePath
          pure $ Text.Encoding.decodeUtf8 body
        fromMaybe posts <$> Maybe.runMaybeT do
          post <- postFromMarkdown markdown
          pure $ posts <> [post]
    )
    []
    filePaths
  pure $ List.sortBy
    (\x y -> y.metadata.createdAt `compare` x.metadata.createdAt
    )
    posts

postFromMarkdown :: (Has.Has Pandoc.ReaderOptions r, Has.Has Pandoc.WriterOptions r, MonadReader r m) => Text -> Maybe.MaybeT m Type.Post
postFromMarkdown markdown = do
  readerOptions :: Pandoc.ReaderOptions <- asks Has.getter
  writerOptions :: Pandoc.WriterOptions <- asks Has.getter
  Maybe.exceptToMaybeT do
    Except.ExceptT $ pure $ Pandoc.runPure do
      pandoc <- Core.configurePandoc <$> Pandoc.readMarkdown readerOptions markdown
      html <- Pandoc.writeHtml5String writerOptions pandoc
      metadata <- postMetadataFromPandoc pandoc
      pure $ Type.Post
        { metadata = metadata
        , body = html
        }

postMetadataFromPandoc :: Pandoc.PandocMonad m => Pandoc.Pandoc -> m Type.PostMetadata
postMetadataFromPandoc pandoc@(Pandoc.Pandoc meta _) = do
  title <- maybe (Except.throwError $ Pandoc.PandocSomeError "Missing title") pure $ do
    metaValue <- Pandoc.lookupMeta "title" meta
    let inlines = Core.inlineStringFromMetaValue metaValue
        value = Core.inlineStringToString inlines
    guard (not $ Text.null value)
    pure value
  slug <- maybe (Except.throwError $ Pandoc.PandocSomeError "Missing slug") pure $ do
    metaValue <- Pandoc.lookupMeta "slug" meta
    let inlines = Core.inlineStringFromMetaValue metaValue
        value = Core.inlineStringToString inlines
    guard (not $ Text.null value)
    pure $ Text.pack $ Text.unpack value <&> \case
      ' ' -> '_'
      x -> x
  locale <- maybe (Except.throwError $ Pandoc.PandocSomeError "Missing locale") pure $ do
    metaValue <- Pandoc.lookupMeta "locale" meta
    let inlines = Core.inlineStringFromMetaValue metaValue
        value = Core.inlineStringToString inlines
    Core.localeFromText value
  description <- maybe (Except.throwError $ Pandoc.PandocSomeError "Missing description") pure $ do
    metaValue <- Pandoc.lookupMeta "description" meta
    let inlines = Core.inlineStringFromMetaValue metaValue
        value = Core.inlineStringToString inlines
    guard (not $ Text.null value)
    pure value
  createdAt <- maybe (Except.throwError $ Pandoc.PandocSomeError "Missing created_at") pure $ do
    metaValue <- Pandoc.lookupMeta "created_at" meta
    let inlines = Core.inlineStringFromMetaValue metaValue
        value = Core.inlineStringToString inlines
    ISO8601.iso8601ParseM $ Text.unpack value
  tags <- maybe (pure []) pure $ do
    metaValue <- Pandoc.lookupMeta "tags" meta
    let multiline = Core.inlineStringToString <$> Core.multilineStringFromMetaValue metaValue
    pure $ CaseInsensitive.mk <$> (multiline >>= Text.split (== ' '))
  wordCount <- maybe (pure 0) pure $ do
    pure $ Core.wordCountFromPandoc pandoc
  tableOfContents <- maybe (Except.throwError $ Pandoc.PandocSomeError "Missing table of contents") pure $ do
    Core.tableOfContentsFromPandoc pandoc
  pure $ Type.PostMetadata
    { title = title
    , slug = slug
    , locale = locale
    , description = description
    , tags = tags
    , wordCount = wordCount
    , createdAt = createdAt
    , tableOfContents = tableOfContents
    }

getLocalizedDictionary :: (Has.Has Type.GeneratorConfig r, MonadIO m, MonadReader r m) => m Type.LocalizedDictionary
getLocalizedDictionary = do
  config :: Type.GeneratorConfig <- asks Has.getter
  liftIO do
    fold <$> localizedDictionaryFromFile config.localizedDictionaryFilePath

localizedDictionaryFromFile :: IO.FilePath -> IO.IO (Aeson.Types.Result Type.LocalizedDictionary)
localizedDictionaryFromFile path = do
  mv <- fmap (Aeson.Types.parse localizedDictionaryFromJson) . Aeson.decode <$> ByteString.Lazy.Char8.readFile path
  maybe (fail "Invalid file") pure mv

localizedDictionaryFromJson :: Aeson.Value -> Aeson.Types.Parser Type.LocalizedDictionary
localizedDictionaryFromJson = Aeson.withObject "localizedDictionary" $ \x -> Type.LocalizedDictionary
  <$> Aeson.Types.explicitParseField dictionaryFromJson x "en"
  <*> Aeson.Types.explicitParseField dictionaryFromJson x "ja"

dictionaryFromFile :: IO.FilePath -> IO.IO (Aeson.Types.Result Type.Dictionary)
dictionaryFromFile path = do
  mv <- fmap (Aeson.Types.parse dictionaryFromJson) . Aeson.decode <$> ByteString.Lazy.Char8.readFile path
  maybe (fail "Invalid file") pure mv

dictionaryFromJson :: Aeson.Value -> Aeson.Types.Parser Type.Dictionary
dictionaryFromJson = Aeson.withObject "dictionary" $ \x -> Maybe.maybe (fail "Invalid source") pure $ do
  txs <- for (Aeson.KeyMap.toList x) $ \(key, val) -> do
    rx <- fold $ Aeson.fromJSON val
    pure (Aeson.Key.toText key, rx)
  pure $ Map.fromList txs


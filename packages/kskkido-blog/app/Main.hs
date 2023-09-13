module Main where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad.Extra as Extra
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.Directory as Directory
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Highlighting as Pandoc.Highlighting
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Interpreter.Generator.Type as Type
import qualified Kskkido.Blog.Interpreter.Generator as Interpreter.Generator
import qualified Kskkido.Blog.Generator as Generator

data Env = Env
  { assetsFilePath :: String
  , resumeFilePath :: String
  , postsFilePath :: String
  , pagesFilePath :: String
  , profileFilePath :: String
  , translationsFilePath :: String
  }

data BuildMetadata = BuildMetadata
  { artifacts :: [ArtifactMetadata]
  }
  deriving (Generic, Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

data ArtifactMetadata = ArtifactMetadata
  { filePath :: Text
  , route :: Text
  }
  deriving (Generic, Eq, Show, Aeson.ToJSON, Aeson.FromJSON)

main :: IO ()
main = do
  IO.putStrLn "Starting"
  result <- Except.runExceptT do
    env <- envFromSystem
    liftIO do
      config <- configFromEnv env
      result <- Interpreter.Generator.toIO config do
        siteConfig <- Interpreter.Generator.getSiteConfig
        flip Reader.runReaderT siteConfig do
          Generator.main
      case result of
        Left exception -> fail $ displayException exception 
        Right artifacts -> do
          Extra.whenM (Directory.doesDirectoryExist env.pagesFilePath) do
            Directory.removeDirectoryRecursive env.pagesFilePath
          Directory.createDirectoryIfMissing True env.pagesFilePath
          do
            for_ artifacts $ \artifact -> do
              let filePath = env.pagesFilePath FilePath.</> dropWhile (== '/') (Text.unpack artifact.filePath)
                  directory = FilePath.takeDirectory filePath
              Directory.createDirectoryIfMissing True directory
              ByteString.Lazy.writeFile filePath artifact.file
              IO.putStrLn filePath
          do
            let filePath = env.pagesFilePath FilePath.</> dropWhile (== '/') "_metadata.json"
                metadata = builtMetadataFromArtifacts artifacts
            ByteString.Lazy.writeFile filePath (Aeson.encode metadata)
  case result of
    Left exception -> do
      IO.putStrLn "Failure"
      IO.putStrLn exception
    _ -> do
      IO.putStrLn "Success"

configFromEnv :: Env -> IO.IO Type.GeneratorConfig
configFromEnv env = do
  pure $ Type.GeneratorConfig
    { assetsFilePath = env.assetsFilePath
    , resumeFilePath = env.resumeFilePath
    , postsFilePath = env.postsFilePath
    , profileFilePath = env.profileFilePath
    , localizedDictionaryFilePath = env.translationsFilePath
    , pandocReaderOptions = Pandoc.def
      { Pandoc.readerExtensions =
          Pandoc.enableExtension Pandoc.Ext_hard_line_breaks $
          Pandoc.enableExtension Pandoc.Ext_smart $
          Pandoc.enableExtension Pandoc.Ext_yaml_metadata_block
          Pandoc.pandocExtensions
      }
    , pandocWriterOptions = Pandoc.def
      { Pandoc.writerHighlightStyle = pure Pandoc.Highlighting.pygments
      , Pandoc.writerWrapText = Pandoc.WrapPreserve
      , Pandoc.writerExtensions =
          Pandoc.enableExtension Pandoc.Ext_smart
          Pandoc.pandocExtensions
      }
    }

envFromSystem :: Except.ExceptT String IO.IO Env
envFromSystem = do
  cwd <- liftIO Directory.getCurrentDirectory
  assetsFilePath <- do
    value <- lift $ Maybe.runMaybeT $ do
      Maybe.MaybeT (Environment.lookupEnv "ASSETS_FILE_PATH")
    maybe (Except.throwE "Invalid ASSETS_FILE_PATH") pure value
  resumeFilePath <- do
    value <- lift $ Maybe.runMaybeT $ do
      Maybe.MaybeT (Environment.lookupEnv "RESUME_FILE_PATH")
    maybe (Except.throwE "Invalid ASSETS_FILE_PATH") pure value
  postsFilePath <- do
    value <- lift $ Maybe.runMaybeT $ do
      Maybe.MaybeT (Environment.lookupEnv "POSTS_FILE_PATH")
    maybe (Except.throwE "Invalid POSTS_FILE_PATH") pure value
  pagesFilePath <- do
    value <- lift $ Maybe.runMaybeT $ do
      Maybe.MaybeT (Environment.lookupEnv "PAGES_FILE_PATH")
    maybe (Except.throwE "Invalid PAGES_FILE_PATH") pure value
  profileFilePath <- do
    value <- lift $ Maybe.runMaybeT $ do
      Maybe.MaybeT (Environment.lookupEnv "PROFILE_FILE_PATH")
    maybe (Except.throwE "Invalid PROFILE_FILE_PATH") pure value
  buildMetadataFilePath <- do
    value <- lift $ Maybe.runMaybeT $ do
      Maybe.MaybeT (Environment.lookupEnv "BUILD_METADATA_FILE_PATH")
    maybe (Except.throwE "Invalid BUILD_METADATA_FILE_PATH") pure value
  translationsFilePath <- do
    value <- lift $ Maybe.runMaybeT $ do
      Maybe.MaybeT (Environment.lookupEnv "TRANSLATIONS_FILE_PATH")
    maybe (Except.throwE "Invalid TRANSLATIONS_FILE_PATH") pure value
  pure $
    Env
      { assetsFilePath =
          cwd FilePath.</>
          assetsFilePath
      , resumeFilePath =
          cwd FilePath.</>
          resumeFilePath
      , postsFilePath =
          cwd FilePath.</>
          postsFilePath
      , pagesFilePath =
          cwd FilePath.</>
          pagesFilePath
      , profileFilePath =
          cwd FilePath.</>
          profileFilePath
      , translationsFilePath =
          cwd FilePath.</>
          translationsFilePath
      }

builtMetadataFromArtifacts :: [Type.Artifact] -> BuildMetadata
builtMetadataFromArtifacts artifacts =
  BuildMetadata
    { artifacts = artifacts <&> \artifact -> ArtifactMetadata
        { filePath = artifact.filePath
        , route = artifact.route
        }
    }

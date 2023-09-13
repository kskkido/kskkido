module Main where

import RIO
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.Directory as Directory
import qualified Network.Wai.Handler.Warp as Wai
import qualified Kskkido.BlogServer
import qualified Kskkido.BlogServer.Type

data Env = Env
  { serverPort :: Int
  , postsFilePath :: String
  , pagesFilePath :: String
  , assetsFilePath :: String
  }

main :: IO ()
main = do
  IO.putStrLn "Starting"
  result <- Except.runExceptT do
    env <- getEnv
    liftIO do
      config <- getServerConfigFromEnv env
      server <- flip Reader.runReaderT config do
        Kskkido.BlogServer.application
      IO.putStrLn $ "Running on: " <> show env.serverPort
      Wai.run env.serverPort server
  either IO.putStrLn (const $ pure ()) result


getEnv :: Except.ExceptT String IO.IO Env
getEnv = do
  cwd <- liftIO do
    Directory.getCurrentDirectory
  postsFilePath <- do
    value <- lift $ Maybe.runMaybeT $ do
      Maybe.MaybeT (Environment.lookupEnv "POSTS_FILE_PATH")
    maybe (Except.throwE "Invalid POSTS_FILE_PATH") pure value
  pagesFilePath <- do
    value <- lift $ Maybe.runMaybeT $ do
      Maybe.MaybeT (Environment.lookupEnv "PAGES_FILE_PATH")
    maybe (Except.throwE "Invalid PAGES_FILE_PATH") pure value
  assetsFilePath <- do
    value <- lift $ Maybe.runMaybeT $ do
      Maybe.MaybeT (Environment.lookupEnv "ASSETS_FILE_PATH")
    maybe (Except.throwE "Invalid ASSETS_FILE_PATH") pure value
  pure $
    Env
      { serverPort = 3030
      , postsFilePath =
          cwd FilePath.</>
          postsFilePath
      , pagesFilePath =
          cwd FilePath.</>
          pagesFilePath
      , assetsFilePath =
          cwd FilePath.</>
          assetsFilePath
      }

getServerConfigFromEnv :: Env -> IO.IO Kskkido.BlogServer.Type.ServerConfig
getServerConfigFromEnv env = do
  pure $ Kskkido.BlogServer.Type.ServerConfig
    { pagesFilePath = env.pagesFilePath
    , assetsFilePath = env.assetsFilePath
    }

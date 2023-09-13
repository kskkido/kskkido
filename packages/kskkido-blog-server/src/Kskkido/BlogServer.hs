module Kskkido.BlogServer where

import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified Control.Monad.Reader as Reader
import qualified Servant
import qualified Servant.Server.StaticFiles
import qualified Network.Wai
import qualified System.FilePath as FilePath
import qualified Kskkido.BlogServer.Api as Api
import qualified Kskkido.BlogServer.Type as Type

application :: Monad m => Type.ServerAction m Servant.Application
application = do
  context <- Reader.ask
  pure $ middleware $ Servant.serveWithContextT
    Api.proxy
    Servant.EmptyContext
    ( `Reader.runReaderT` context )
    ( Servant.Server.StaticFiles.serveDirectoryFileServer context.pagesFilePath )

middleware :: Network.Wai.Application -> Network.Wai.Application
middleware app request respond = do
  let pathInfo = fromMaybe request.pathInfo do
        last <- Text.unpack <$> List.lastMaybe request.pathInfo
        init <- List.initMaybe request.pathInfo
        guard (null $ FilePath.takeExtension last)
        pure $ init ++ [fromString $ last FilePath.<.> "html"]
  app (request { Network.Wai.pathInfo = pathInfo }) respond


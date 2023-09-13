module Kskkido.BlogServer.Type where

import RIO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Network.HTTP.Media as Media
import qualified Servant
import qualified Servant.API

data ServerConfig = ServerConfig
  { assetsFilePath :: String
  , pagesFilePath :: String
  }

type ServerAction = Reader.ReaderT ServerConfig

type ServerHandler = ServerAction Servant.Handler

data HtmlContentType = HtmlContentType

instance Servant.API.Accept HtmlContentType where
  contentType _ = "text" Media.// "html" Media./: ("charset", "utf-8")
instance Servant.API.MimeRender HtmlContentType ByteString.Lazy.ByteString where
  mimeRender _ bs = bs


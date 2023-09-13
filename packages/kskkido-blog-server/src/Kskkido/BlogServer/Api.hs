module Kskkido.BlogServer.Api where

import qualified Servant
import qualified Servant.API

type Api =
  Servant.Raw

proxy :: Servant.Proxy Api
proxy = Servant.Proxy


module Kskkido.Blog.Generator.Type where

import RIO
import qualified Lucid
import qualified Data.Has as Has
import qualified Data.Time.Clock as Time.Clock
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Interpreter.View as View

data SiteConfig = SiteConfig
  { time :: Time.Clock.UTCTime
  , defaultLocale :: Type.Locale
  , locales :: [Type.Locale]
  , posts :: [Type.Post]
  , assets :: [Type.Asset]
  , profile :: Type.Profile
  , router :: Type.Router
  , localizedDictionary :: Type.LocalizedDictionary
  }

instance Has.Has Type.LocalizedDictionary SiteConfig where
  getter context = context.localizedDictionary
  modifier fn context = context { localizedDictionary = fn context.localizedDictionary }
instance Has.Has [Type.Post] SiteConfig where
  getter context = context.posts
  modifier fn context = context { posts = fn context.posts }

type BuildMap = Map Type.SiteItemIdentifier BuildItem

data BuildItem = BuildItem
  { identifier :: Type.SiteItemIdentifier
  , route :: Text
  , filePath :: Text
  , content :: BuildItemContent
  }

data BuildItemContent =
  PageContent
    { title :: Text
    , locale :: Type.Locale
    , posts :: [Type.Post]
    , tags :: [Type.Tag]
    , profile :: Type.Profile
    , template :: Lucid.HtmlT View.View ()
    }
  | StaticContent
    { file :: ByteString.Lazy.ByteString
    }

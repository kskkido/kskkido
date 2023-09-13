module Kskkido.Blog.Interpreter.View.Type where

import RIO
import qualified Data.Has as Has
import qualified Data.Time.Clock as Time.Clock
import qualified Kskkido.Blog.Core.Type as Type

data ViewConfig = ViewConfig
  { identifier :: Type.SiteItemIdentifier
  , now :: Time.Clock.UTCTime
  , title :: Text
  , route :: Text
  , locale :: Type.Locale
  , posts :: [Type.Post]
  , tags :: [Type.Tag]
  , profile :: Type.Profile
  , router :: Type.Router
  , siteMap :: Type.SiteMap
  , localizedDictionary :: Type.LocalizedDictionary
  }

instance Has.Has Type.Page ViewConfig where
  getter context = Type.Page
    { identifier = context.identifier
    , title = context.title
    , route = context.route
    , locale = context.locale
    }
  modifier fn context = context
    { identifier = next.identifier
    , title = next.title
    , route = next.route
    , locale = next.locale
    }
    where next = fn $ Has.getter context
instance Has.Has Type.Profile ViewConfig where
  getter context = context.profile
  modifier fn context = context { profile = fn context.profile }
instance Has.Has [Type.Post] ViewConfig where
  getter context = context.posts
  modifier fn context = context { posts = fn context.posts }
instance Has.Has [Type.Tag] ViewConfig where
  getter context = context.tags
  modifier fn context = context { tags = fn context.tags }
instance Has.Has Type.Locale ViewConfig where
  getter context = context.locale
  modifier fn context = context { locale = fn context.locale }
instance Has.Has Type.LocalizedDictionary ViewConfig where
  getter context = context.localizedDictionary
  modifier fn context = context { localizedDictionary = fn context.localizedDictionary }
instance Has.Has Type.Router ViewConfig where
  getter context = context.router
  modifier fn context = context { router = fn context.router }
instance Has.Has Type.SiteMap ViewConfig where
  getter context = context.siteMap
  modifier fn context = context { siteMap = fn context.siteMap }


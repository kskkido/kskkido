{-# LANGUAGE DeriveAnyClass #-}

module Kskkido.Blog.Core.Type where

import RIO
import qualified RIO.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Has as Has
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Tree as Tree
import qualified Data.Validation as Validation

data CoreException =
    MissingLocalization Text
  | MissingSiteItem Text
  deriving (Generic, Eq, Show)

instance Exception CoreException

data Artifact = Artifact
  { filePath :: Text
  , file :: ByteString.Lazy.ByteString
  , route :: Text
  }

type SiteMap = Map SiteItemIdentifier SiteItemMetadata

data SiteItemIdentifier =
    Plain Text
  | Localized Text Locale
  deriving (Generic, Eq, Show, Ord)

data SiteItemMetadata = SiteItemMetadata
  { identifier :: SiteItemIdentifier
  , route :: Text
  , filePath :: Text
  }

data Asset = Asset
  { filePath :: Text
  , file :: ByteString.Lazy.ByteString
  }

data Locale =
    EN
  | JA
  deriving (Generic, Eq, Ord)

instance Show Locale where
  show EN = "en"
  show JA = "ja"

data PageContext = PageContext
  { identifier :: SiteItemIdentifier
  , now :: Time.Clock.UTCTime
  , title :: Text
  , route :: Text
  , locale :: Locale
  , posts :: [Post]
  , tags :: [Tag]
  , profile :: Profile
  , router :: Router
  , siteMap :: SiteMap
  , localizedDictionary :: LocalizedDictionary
  }

instance Has.Has [Post] PageContext where
  getter context = context.posts
  modifier fn context = context { posts = fn context.posts }
instance Has.Has Locale PageContext where
  getter context = context.locale
  modifier fn context = context { locale = fn context.locale }
instance Has.Has LocalizedDictionary PageContext where
  getter context = context.localizedDictionary
  modifier fn context = context { localizedDictionary = fn context.localizedDictionary }
instance Has.Has Router PageContext where
  getter context = context.router
  modifier fn context = context { router = fn context.router }
instance Has.Has SiteMap PageContext where
  getter context = context.siteMap
  modifier fn context = context { siteMap = fn context.siteMap }

data Page = Page
  { identifier :: SiteItemIdentifier
  , title :: Text
  , route :: Text
  , locale :: Locale
  }

data Router = Router
  { home :: Text
  , about :: Text
  , post :: Text -> Text
  , posts :: Text
  , tag :: Text -> Text
  , tags :: Text
  }

type Tag = CaseInsensitive.CI Text

data Post = Post
  { metadata :: PostMetadata
  , body :: Text
  }
  deriving (Generic, Eq, Show)

data PostMetadata = PostMetadata
  { title :: Text
  , slug :: Text
  , locale :: Locale
  , tags :: [Tag]
  , description :: Text
  , wordCount :: Int
  , createdAt :: Time.Clock.UTCTime
  , tableOfContents :: TableOfContents
  }
  deriving (Generic, Eq, Show)

data LocalizedDictionary = LocalizedDictionary
  { en :: Dictionary
  , ja :: Dictionary
  }

instance Semigroup LocalizedDictionary where
  mx <> my = LocalizedDictionary
    { en = mx.en <> my.en
    , ja = mx.ja <> my.ja
    }
instance Monoid LocalizedDictionary where
  mempty = LocalizedDictionary mempty mempty

type Dictionary = Map Text Text

type TableOfContents = Tree.Forest TableOfContentsHeader

data TableOfContentsHeader = TableOfContentsHeader
  { identifier :: Text
  , title      :: Text
  , level      :: Int
  }
  deriving (Generic, Eq, Show)

data Profile = Profile
  { resume :: ProfileResume
  , socials :: [ProfileSocial]
  }
  deriving (Generic, Eq, Show, Aeson.FromJSON)

data ProfileResume = ProfileResumse
  { url :: Text
  }
  deriving (Generic, Eq, Show, Aeson.FromJSON)

data ProfileSocial = ProfileSocial
  { variant :: ProfileSocialVariant
  , url :: Text
  }
  deriving (Generic, Eq, Show, Aeson.FromJSON)

data ProfileSocialVariant = 
    ProfileSocialVariantGithub
  | ProfileSocialVariantLinkedin
  | ProfileSocialVariantPersonal
  deriving (Generic, Eq, Show)

instance Aeson.FromJSON ProfileSocialVariant where
  parseJSON (Aeson.String text) = either (const mempty) pure do
    Validation.toEither $
      Validation.validate
        ( "Unsupported social variant" :: String )
        ( \cs -> asum
          [ do
              guard (cs == "github")
              pure ProfileSocialVariantGithub
          , do
              guard (cs == "linkedin")
              pure ProfileSocialVariantLinkedin
          , do
              guard (cs == "personal")
              pure ProfileSocialVariantPersonal
          ]
        )
        ( Text.strip text )
  parseJSON _ = mempty

data DataFileType =
    DataFileTypeJson
  | DataFileTypeYaml
  deriving (Generic, Eq, Show)

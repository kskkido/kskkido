module Kskkido.Blog.Core where

import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified RIO.Map as Map
import qualified System.FilePath as FilePath
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Has as Has
import qualified Data.Foldable as Foldable
import qualified Data.Tree as Tree
import qualified Data.Validation as Validation
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc.Walk
import qualified Kskkido.Blog.Core.Type as Type

localize :: (Has.Has Type.Locale r, Has.Has Type.LocalizedDictionary r, Except.MonadError Type.CoreException m, MonadReader r m) => Text -> m Text
localize key = do
  result <- Maybe.runMaybeT do
    lookupLocalization key
  case result of 
    Nothing -> Except.throwError $ Type.MissingLocalization key
    Just metadata -> pure metadata

lookupLocalization :: (Has.Has Type.Locale r, Has.Has Type.LocalizedDictionary r, MonadReader r m) => Text -> Maybe.MaybeT m Text
lookupLocalization key = do
  locale :: Type.Locale <- asks Has.getter
  localizedDictionary :: Type.LocalizedDictionary <- asks Has.getter
  let dictionary = dictionaryFromLocale locale localizedDictionary
  Maybe.MaybeT $ pure do
    Map.lookup key dictionary

localizePath :: (Has.Has Type.Locale r, MonadReader r m) => Text -> m Text
localizePath path = do
  locale :: Type.Locale <- asks Has.getter
  pure $ Text.pack $ "/" <> show locale FilePath.</> dropWhile (== '/') (Text.unpack path)

localizedPosts :: (Has.Has Type.Locale r, Has.Has [Type.Post] r, MonadReader r m) => m [Type.Post]
localizedPosts = do
  locale :: Type.Locale <- asks Has.getter
  posts :: [Type.Post] <- asks Has.getter
  pure $ filter (\post -> post.metadata.locale == locale) posts

tags :: (Has.Has [Type.Post] r, MonadReader r m) => m [Type.Tag]
tags = do
  posts :: [Type.Post] <- asks Has.getter
  pure $ tagsFromPosts posts

tagsFromPosts :: [Type.Post] -> [Type.Tag]
tagsFromPosts posts = List.nub do
  post <- posts
  post.metadata.tags

localesByIdentifier :: (Has.Has Type.SiteMap r, MonadReader r m) => Type.SiteItemIdentifier -> m [Type.Locale]
localesByIdentifier identifier = do
  siteMap :: Type.SiteMap <- asks Has.getter
  pure $ foldMap go (Map.toList siteMap)
  where go (Type.Plain _, _) = mempty
        go (Type.Localized key locale, _) = guard (key == keyFromIdentifier identifier) >> pure locale

keyFromIdentifier :: Type.SiteItemIdentifier -> Text
keyFromIdentifier (Type.Plain key) = key
keyFromIdentifier (Type.Localized key _) = key

getSiteItemMetadata :: (Has.Has Type.SiteMap r, Except.MonadError Type.CoreException m, MonadReader r m) => Type.SiteItemIdentifier -> m Type.SiteItemMetadata
getSiteItemMetadata identifier = do
  result <- Maybe.runMaybeT do
    lookupSiteItemMetadata identifier
  case result of 
    Nothing -> Except.throwError $ Type.MissingSiteItem $ keyFromIdentifier identifier
    Just metadata -> pure metadata

lookupSiteItemMetadata :: (Has.Has Type.SiteMap r, MonadReader r m) => Type.SiteItemIdentifier -> Maybe.MaybeT m Type.SiteItemMetadata
lookupSiteItemMetadata identifier = do
  siteMap :: Type.SiteMap <- asks Has.getter
  Maybe.MaybeT $ pure do
    Map.lookup identifier siteMap

localizeKey :: (Has.Has Type.Locale r, MonadReader r m) => Text -> m Type.SiteItemIdentifier
localizeKey key = do
  locale :: Type.Locale <- asks Has.getter
  pure $ Type.Localized key locale

localizeIdentifier :: (Has.Has Type.Locale r, MonadReader r m) => Type.SiteItemIdentifier -> m Type.SiteItemIdentifier
localizeIdentifier (Type.Plain key) = do
  locale :: Type.Locale <- asks Has.getter
  pure $ Type.Localized key locale
localizeIdentifier (Type.Localized key _) = do
  locale :: Type.Locale <- asks Has.getter
  pure $ Type.Localized key locale

postsByTag :: (Has.Has [Type.Post] r, MonadReader r m) => Type.Tag -> m [Type.Post]
postsByTag tag = do
  posts :: [Type.Post] <- asks Has.getter
  pure $ filter (\p -> tag `elem` p.metadata.tags) posts

configurePandoc :: Pandoc.Pandoc -> Pandoc.Pandoc
configurePandoc = Pandoc.Walk.walk configureBlock

configureBlock :: Pandoc.Block -> Pandoc.Block
configureBlock (Pandoc.Header level (identifier, classes, hash) inlines) = Pandoc.Header
  level
  (identifier, classes, hash ++
    [ (Text.pack "data-type", Text.pack "heading")
    ]
  )
  ( inlines ++
  [ Pandoc.Link
    ( Text.empty
    , []
    , [(Text.pack "data-type", Text.pack "anchor")
      ]
    )
    []
    (Text.pack "#" <> identifier, identifier)
  ]
  )
configureBlock block = block

wordCountFromPandoc :: Pandoc.Pandoc -> Int
wordCountFromPandoc (Pandoc.Pandoc _ blocks) =
  length $ foldMap (Pandoc.Walk.query xs) blocks
  where xs (Pandoc.Str s) = Text.words s
        xs _ = []

tableOfContentsFromPandoc :: Pandoc.Pandoc -> Maybe Type.TableOfContents
tableOfContentsFromPandoc (Pandoc.Pandoc _ blocks) = do
  let headers = tableOfContentsHeaderFromBlocks blocks
  tableOfContentsFromHeaders headers

tableOfContentsFromHeaders :: [Type.TableOfContentsHeader] -> Maybe Type.TableOfContents
tableOfContentsFromHeaders headers = mapM step headersByLevel
  where
      step [] = Nothing
      step (x:xs) = Tree.Node x <$> tableOfContentsFromHeaders xs
      headersByLevel = List.groupBy (\x y -> x.level < y.level) headers

tableOfContentsHeaderFromBlock :: Pandoc.Block -> Maybe Type.TableOfContentsHeader
tableOfContentsHeaderFromBlock (Pandoc.Header level (identifier, _, _) inlines) = do
  either (const Nothing) pure $ do
    Pandoc.runPure do
      let document = Pandoc.Pandoc Pandoc.nullMeta [Pandoc.Plain inlines]
      title <- Pandoc.writePlain Pandoc.def document
      pure $ Type.TableOfContentsHeader
        { identifier = identifier
        , title = title
        , level = level
        }
tableOfContentsHeaderFromBlock _ = Nothing
tableOfContentsHeaderFromBlocks :: [Pandoc.Block] -> [Type.TableOfContentsHeader]
tableOfContentsHeaderFromBlocks = mapMaybe tableOfContentsHeaderFromBlock

headerBlocksFromPandoc :: Pandoc.Pandoc -> [Pandoc.Block]
headerBlocksFromPandoc = Pandoc.Walk.query header
  where header x@(Pandoc.Header {}) = [x]
        header _ = []

localeFromText :: Text -> Maybe Type.Locale
localeFromText text = either (const Nothing) pure do
  Validation.toEither $
    Validation.validate
      ( "Unsupported locale string" :: String )
      ( \cs -> Foldable.asum
        [ do
            guard (cs == CaseInsensitive.mk "en")
            pure Type.EN
        , do
            guard (cs == CaseInsensitive.mk "ja")
            pure Type.JA
        ]
      )
      ( CaseInsensitive.mk $ Text.strip text )

inlineStringToString :: [Pandoc.Inline] -> Text
inlineStringToString = foldMap \case
  Pandoc.Str x -> x
  Pandoc.Space -> " "
  _ -> ""

inlineStringFromMetaValue :: Pandoc.MetaValue -> [Pandoc.Inline]
inlineStringFromMetaValue value = case value of
  Pandoc.MetaString s -> [Pandoc.Str s]
  Pandoc.MetaInlines ils -> ils
  Pandoc.MetaBlocks [Pandoc.Plain ils] -> ils
  Pandoc.MetaBlocks [Pandoc.Para ils] -> ils
  _ -> []

multilineStringFromMetaValue :: Pandoc.MetaValue -> [[Pandoc.Inline]]
multilineStringFromMetaValue value = case value of
  Pandoc.MetaString s -> [[Pandoc.Str s]]
  Pandoc.MetaInlines ils -> [ils]
  Pandoc.MetaList ms -> [ils | Pandoc.MetaInlines ils <- ms] ++
                        [ils | Pandoc.MetaBlocks [Pandoc.Plain ils] <- ms] ++
                        [ils | Pandoc.MetaBlocks [Pandoc.Para ils]  <- ms] ++
                        [[Pandoc.Str x] | Pandoc.MetaString x <- ms]
  _ -> []

dictionaryFromLocale :: Type.Locale -> Type.LocalizedDictionary -> Type.Dictionary
dictionaryFromLocale Type.EN localizedDictionary = localizedDictionary.en
dictionaryFromLocale Type.JA localizedDictionary = localizedDictionary.ja


module Kskkido.Blog.Generator where

import RIO
import qualified RIO.Map as Map
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Has as Has
import qualified Lucid
import qualified Kskkido.Blog.Templates.Pages.About as Templates.Pages.About
import qualified Kskkido.Blog.Templates.Pages.Home as Templates.Pages.Home
import qualified Kskkido.Blog.Templates.Pages.Post as Templates.Pages.Post
import qualified Kskkido.Blog.Templates.Pages.Posts as Templates.Pages.Posts
import qualified Kskkido.Blog.Templates.Pages.Tag as Templates.Pages.Tag
import qualified Kskkido.Blog.Templates.Pages.Tags as Templates.Pages.Tags
import qualified Kskkido.Blog.Core as Core
import qualified Kskkido.Blog.Core.Type as Type
import qualified Kskkido.Blog.Interpreter.View.Type as Type
import qualified Kskkido.Blog.Interpreter.View as View
import qualified Kskkido.Blog.Generator.Type as Type

main :: Except.MonadError Type.CoreException m => Reader.ReaderT Type.SiteConfig m [Type.Artifact]
main = do
  siteConfig <- Reader.ask
  buildMap <- fold <$> sequence
    [ do
        Reader.withReaderT (siteConfig.defaultLocale ,) do
          locale :: Type.Locale <- asks Has.getter
          posts <- Core.localizedPosts
          let tags = Core.tagsFromPosts posts
          Map.fromList . fold <$> sequence
            [ pure <$> do
                let route = siteConfig.router.home 
                    identifier = Type.Plain route
                title <- fromMaybe "Home" <$> Maybe.runMaybeT do
                  Core.lookupLocalization "page.home.title"
                let item = Type.BuildItem
                      { identifier = identifier
                      , route = route
                      , filePath = "/index.html"
                      , content = Type.PageContent
                        { title = title
                        , locale = locale
                        , posts = posts
                        , tags = tags
                        , profile = siteConfig.profile
                        , template = Templates.Pages.Home.render
                        }
                      }
                pure
                  ( item.identifier
                  , item
                  )
            ]
    , fold <$> for siteConfig.assets \asset -> do
        Map.fromList . fold <$> sequence
          [ pure <$> do
              let identifier = Type.Plain asset.filePath
                  item = Type.BuildItem
                    { identifier = identifier
                    , route = asset.filePath
                    , filePath = asset.filePath
                    , content = Type.StaticContent
                        { file = asset.file
                        }
                    }
              pure
                ( identifier
                , item
                )
          ]
    , fold <$> for siteConfig.locales \locale -> do
        Reader.withReaderT (locale ,) do
          posts <- Core.localizedPosts
          let tags = Core.tagsFromPosts posts
          Map.fromList . fold <$> sequence
            [ pure <$> do
                let route = siteConfig.router.home 
                    identifier = Type.Localized route locale
                title <- fromMaybe "Home" <$> Maybe.runMaybeT do
                  Core.lookupLocalization "page.home.title"
                localizedRoute <- Core.localizePath route
                let item = Type.BuildItem
                      { identifier = identifier
                      , route = localizedRoute
                      , filePath = localizedRoute <> ".html"
                      , content = Type.PageContent
                        { title = title
                        , locale = locale
                        , posts = posts
                        , tags = tags
                        , profile = siteConfig.profile
                        , template = Templates.Pages.Home.render
                        }
                      }
                pure
                  ( item.identifier
                  , item
                  )
            , pure <$> do
                let route = siteConfig.router.about 
                    identifier = Type.Localized route locale
                title <- fromMaybe "About" <$> Maybe.runMaybeT do
                  Core.lookupLocalization "page.about.title"
                localizedRoute <- Core.localizePath route
                let item = Type.BuildItem
                      { identifier = identifier
                      , route = localizedRoute
                      , filePath = localizedRoute <> ".html"
                      , content = Type.PageContent
                        { title = title
                        , locale = locale
                        , posts = posts
                        , tags = tags
                        , profile = siteConfig.profile
                        , template = Templates.Pages.About.render
                        }
                      }
                pure
                  ( item.identifier
                  , item
                  )
            , pure <$> do
                let route = siteConfig.router.posts 
                    identifier = Type.Localized route locale
                title <- fromMaybe "Posts" <$> Maybe.runMaybeT do
                  Core.lookupLocalization "page.posts.title"
                localizedRoute <- Core.localizePath route
                let item = Type.BuildItem
                      { identifier = identifier
                      , route = localizedRoute
                      , filePath = localizedRoute <> ".html"
                      , content = Type.PageContent
                        { title = title
                        , locale = locale
                        , posts = posts
                        , tags = tags
                        , profile = siteConfig.profile
                        , template = Templates.Pages.Posts.render
                        }
                      }
                pure
                  ( item.identifier
                  , item
                  )
            , pure <$> do
                let route = siteConfig.router.tags 
                    identifier = Type.Localized route locale
                title <- fromMaybe "Tags" <$> Maybe.runMaybeT do
                  Core.lookupLocalization "page.tags.title"
                localizedRoute <- Core.localizePath route
                let item = Type.BuildItem
                      { identifier = identifier
                      , route = localizedRoute
                      , filePath = localizedRoute <> ".html"
                      , content = Type.PageContent
                        { title = title
                        , locale = locale
                        , posts = posts
                        , tags = tags
                        , profile = siteConfig.profile
                        , template = Templates.Pages.Tags.render
                        }
                      }
                pure
                  ( item.identifier
                  , item
                  )
            , for posts \post -> do
                let route = siteConfig.router.post post.metadata.slug
                    identifier = Type.Localized route locale
                localizedRoute <- Core.localizePath route
                let item = Type.BuildItem
                      { identifier = identifier
                      , route = localizedRoute
                      , filePath = localizedRoute <> ".html"
                      , content = Type.PageContent
                        { title = post.metadata.title
                        , locale = locale
                        , posts = posts
                        , tags = tags
                        , profile = siteConfig.profile
                        , template = Templates.Pages.Post.render post
                        }
                      }
                pure
                  ( item.identifier
                  , item
                  )
            , for tags \tag -> do
                let label = CaseInsensitive.original tag
                    route = siteConfig.router.tag label
                    identifier = Type.Localized route locale
                localizedRoute <- Core.localizePath route
                let item = Type.BuildItem
                      { identifier = identifier
                      , route = localizedRoute
                      , filePath = localizedRoute <> ".html"
                      , content = Type.PageContent
                        { title = label
                        , locale = locale
                        , posts = posts
                        , tags = tags
                        , profile = siteConfig.profile
                        , template = Templates.Pages.Tag.render tag
                        }
                      }
                pure
                  ( item.identifier
                  , item
                  )
            ]
    ]
  let siteMap = buildMap <&> \item -> Type.SiteItemMetadata
        { identifier = item.identifier
        , route = item.route
        , filePath = item.filePath
        }
  fold <$> sequence
    [ for (Map.toList buildMap) \(_, item) -> do
        Reader.mapReaderT Except.liftEither do
          case item.content of 
            Type.StaticContent{..} -> do
              pure $ Type.Artifact
                { filePath = item.filePath
                , file = file
                , route = item.route
                }
            Type.PageContent{..} -> do
              let config = Type.ViewConfig
                    { siteMap = siteMap
                    , identifier = item.identifier
                    , posts = posts
                    , tags = tags
                    , profile = profile
                    , locale = locale
                    , title = title
                    , route = item.route
                    , now = siteConfig.time
                    , router = siteConfig.router
                    , localizedDictionary = siteConfig.localizedDictionary
                    }
              html <- lift do
                View.run config do
                  Lucid.renderBST template
              pure $ Type.Artifact
                { filePath = item.filePath
                , file = html
                , route = item.route
                }
    ]


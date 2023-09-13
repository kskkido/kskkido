---
title: Structuring a Haskell Application
slug: structuring_a_haskell_application
locale: en
created_at: 2023-12-09T00:00:00.001Z
description: How I structured the Haskell application for generating this blog
tags:
- haskell
- blog
- three-layer-cake
- architecture
---
<section>
## Context
When I wrote the Haskell code for generating this blog, I tried to separate implementation detail from the business logic in a way that follows the [Three Layer Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html). I like working with this concept because it keeps the IO layer at the edge, which makes my code easier to test (I did not write tests for this project of course). I've seen enough projects in other languages that abuse test mocks.

There are plenty of materials online that explain the Three Layer Cake, but I thought the interet could use more examples of how the concept is applied to a real Haskell application. In this post I will walk through how I applied the Three Layer Cake to structure the [static site generator for this blog](/en/posts/build_a_blog_with_haskell). 

</section>

<section>
### Layer 1 - Implementation detail
In the source code I treat the implementation detail as a collection of Haskell modules under the <em>Interpreter</em> namespace. These modules are made up of IO code and other operations that I thought could be separated from the business logic. In Three Layer Cake, this would be the layer 1 code.

```haskell
module Kskkido.Blog.Interpreter.Generator where
-- ...

newtype Generator a = Generator
  { unwrap :: Reader.ReaderT Type.GeneratorConfig IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Type.GeneratorConfig
    )
-- ...

getSiteConfig :: (Has.Has Type.GeneratorConfig r, Has.Has Pandoc.ReaderOptions r, Has.Has Pandoc.WriterOptions r, MonadIO m, MonadReader r m) => m Type.SiteConfig
getSiteConfig = do
  time <- getCurrentTime
  posts <- getPosts
  profile <- getProfile
-- ...

module Kskkido.Blog.Interpreter.View where
-- ...

newtype View a = View
  { unwrap :: Reader.ReaderT Type.ViewConfig (Either Type.CoreException) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Type.ViewConfig
    )

instance Except.MonadError Type.CoreException View where
    throwError :: Type.CoreException -> View a
    throwError = View . lift . Left
    catchError :: View a -> (Type.CoreException -> View a) -> View a
    catchError view handler = do
      context <- ask
      let action = run context view
      either handler pure action

instance Capability.Date.Date View where
  getCurrentTime = do
    context <- ask
    pure $ context.now

```

</section>
<section>
### Layer 3 - Business logic
The modules that are responsible for the business logic are made up of pure functions that perform the core work of the application. In the context of this application, the core work is to generate html files that make up the contents of the blog using [lucid](https://hackage.haskell.org/package/lucid). This collection of modules make up layer 3 in Three Layer Cake.

```haskell
module Kskkido.Blog.Templates.Pages.Home where
-- ...

render ::
  ( Has.Has Type.Page r
  , Has.Has Type.Locale r
  , Has.Has Type.Router r
  , Has.Has Type.SiteMap r
  , Has.Has Type.LocalizedDictionary r
  , Has.Has Type.Profile r
  , Has.Has [Type.Post] r
  , Has.Has [Type.Tag] r
  , Capability.Date.Date m
  , Except.MonadError Type.CoreException m
  , MonadReader r m
  ) => Lucid.HtmlT m ()
render = do
  Root.render do
    Lucid.head_
      Head.render
    Lucid.body_
      [ Lucid.classes_
        [ 
        ]
      ] do
      Body.render do
        Home.render
        Foot.render
-- ...

```
</section>
<section>
### Layer 2 - Bridging the two together
The code for the business logic (layer 3) does not directly communicate with the implementation details (layer 1). If it needs to call some function, it will do so through typeclasses, which, in the codebase, I call <em>Capability</em>. If it needs to reference some value in a Reader context, I will use [data-has](https://hackage.haskell.org/package/data-has-0.4.0.0/docs/Data-Has.html) to make sure the business logic does not depend on a concrete implementation of the context. This abstraction of functionality and configuration makes the layer 2 code of the Three Layer Cake. You can think of them as interfaces that we use for dependency injection in OOP. The implementation code bundles its functions into concrete instances of the capability typeclasses (I like to think of the implementation code as an interpretation of the typeclasses and hence the <em>Interpreter</em> namespace) and create a Reader context that defines all the necessary data-has instances. For capabiltiies, I ended up using the [mtl library](https://hackage.haskell.org/package/mtl) out of familiarity, but I'm keen on trying out a library like [polysemy](https://hackage.haskell.org/package/polysemy-1.9.1.3) if time allows.

```haskell
module Kskkido.Blog.Capability.Date where

import qualified Data.Time.Clock as Time.Clock

class Date m where
  getCurrentTime :: m Time.Clock.UTCTime
-- ...

module Kskkido.Blog.Interpreter.View where
-- ...

newtype View a = View
  { unwrap :: Reader.ReaderT Type.ViewConfig (Either Type.CoreException) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Type.ViewConfig
    )

-- ...
instance Capability.Date.Date View where
  getCurrentTime = do
    context <- ask
    pure $ context.now

-- ...

module Kskkido.Blog.Interpreter.View.Type where
-- ...

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
-- ...

```
</section>
<section>
### Running the layers
The final piece of this implementation is to run the three layers together. I have two sets of layers in the codebase; one at the entrypoint of the whole application (app/Main.hs) and another within the application where I generate each page of the website. At the entrypoint of the application I'm calling the business logic (layer 3) for generating the website with a concrete implementation (layer 1) of a Reader context (layer 2). Likewise, I'm calling the business logic code for building a page (layer 3) with concrete implementations (layer 1) of its dependencies (layer 2). 

```haskell
module Main where
-- ...

main :: IO ()
main = do
  IO.putStrLn "Starting"
  result <- Except.runExceptT do
    env <- envFromSystem
    liftIO do
      config <- configFromEnv env
      result <- Interpreter.Generator.toIO config do -- Layer 1
        siteConfig <- Interpreter.Generator.getSiteConfig
        flip Reader.runReaderT siteConfig do
          Generator.main -- Layer 3
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
-- ...

module Kskkido.Blog.Generator where
-- ...

main :: Except.MonadError Type.CoreException m => Reader.ReaderT Type.SiteConfig m [Type.Artifact]
main = do
  siteConfig <- Reader.ask
  buildMap <- fold <$> sequence
    [ do
        Reader.withReaderT (siteConfig.defaultLocale ,) do
          locale :: Type.Locale <- asks Has.getter
  -- ...
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
                View.run config do -- Layer 1 code
                  Lucid.renderBST template -- Layer 3 code
              pure $ Type.Artifact
                { filePath = item.filePath
                , file = html
                , route = item.route
                }
    ]
```
</section>

<section>
## Conclusion
I hope this post gave you some clarity on how to apply the Three Layer cake in a real Haskell application. Please feel free to reference the source code of the application on [GitHub](https://github.com/kskkido/kskkido). I use the same concept to structure an implementation of [realworld.io](https://www.realworld.how/) in Haskell, which is also available on [Github](https://github.com/kskkido/realworld-haskell).

</section>


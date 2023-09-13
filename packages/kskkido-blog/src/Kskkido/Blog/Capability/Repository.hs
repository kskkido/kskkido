module Kskkido.Blog.Capability.Repository where

import qualified Kskkido.Blog.Core.Type as Type

class Repository m where
  getAssets :: m [Type.Asset]
  getHighlightStyleAssets :: m [Type.Asset]
  getProfile :: m Type.Profile
  getPosts :: m [Type.Post]
  getLocalizedDictionary :: m Type.LocalizedDictionary

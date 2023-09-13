module Lucid.Extension.Alpine where

import RIO
import qualified RIO.Text as Text
import Lucid.Base (Attribute, makeAttribute)

xData_ :: Text -> Attribute
xData_ = makeAttribute "x-data"

xBind_ ::
  Text ->
  Text ->
  Attribute
xBind_ attr = makeAttribute ("x-bind:" <> attr)

xOn_ ::
  Text ->
  Text ->
  Attribute
xOn_ event = makeAttribute ("x-on:" <> event)

xText_ :: Text -> Attribute
xText_ = makeAttribute "x-text"

xHtml_ :: Text -> Attribute
xHtml_ = makeAttribute "x-html"

xModel_ ::
  [Text] ->
  Text ->
  Attribute
xModel_ mods = case mods of
  [] -> makeAttribute "x-model"
  _ -> makeAttribute ("x-model." <> Text.intercalate "." mods)

xShow_ :: Text -> Attribute
xShow_ = makeAttribute "x-show"

xTransition_ ::
  Maybe Text ->
  [Text] ->
  Text ->
  Attribute
xTransition_ Nothing [] _ = makeAttribute "x-transition" mempty -- No directive or modifiers
xTransition_ (Just dir) [] attrVal = makeAttribute ("x-transition:" <> dir) attrVal -- Directive with custom transition classes
xTransition_ Nothing mods _ = makeAttribute ("x-transition." <> Text.intercalate "." mods) mempty -- No directive, but with modifiers
xTransition_ (Just dir) mods _ = makeAttribute ("x-transition:" <> dir <> "." <> Text.intercalate "." mods) mempty -- Directive with modifiers

xFor_ :: Text -> Attribute
xFor_ = makeAttribute "x-for"

xForKey_ :: Text -> Attribute
xForKey_ = makeAttribute ":key"

xIf_ :: Text -> Attribute
xIf_ = makeAttribute "x-if"

xInit_ :: Text -> Attribute
xInit_ = makeAttribute "x-init"

xEffect_ :: Text -> Attribute
xEffect_ = makeAttribute "x-effect"

xRef_ :: Text -> Attribute
xRef_ = makeAttribute "x-ref"

xCloak_ :: Attribute
xCloak_ = makeAttribute "x-cloak" mempty

xIgnore_ :: Attribute
xIgnore_ = makeAttribute "x-ignore" mempty

xIntersect_ ::
  Maybe Text ->
  [Text] ->
  Text -> 
  Attribute
xIntersect_ Nothing [] = makeAttribute "x-intersect"
xIntersect_ Nothing modifiers = makeAttribute ("x-intersect" <> "." <> Text.intercalate "." modifiers)
xIntersect_ (Just attr) [] = makeAttribute ("x-intersect" <> ":" <> attr)
xIntersect_ (Just attr) modifiers = makeAttribute ("x-intersect" <> ":" <> attr <> "." <> Text.intercalate "." modifiers)


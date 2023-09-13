module Lucid.Extension.Htmx where

import RIO
import qualified Lucid
import qualified Lucid.Base

hxBoost_ :: Text -> Lucid.Attribute
hxBoost_ = Lucid.Base.makeAttribute "hx-boost"

hxConfirm_ :: Text -> Lucid.Attribute
hxConfirm_ = Lucid.Base.makeAttribute "hx-confirm"

hxDelete_ :: Text -> Lucid.Attribute
hxDelete_ = Lucid.Base.makeAttribute "hx-delete"

hxDisable_ :: Lucid.Attribute
hxDisable_ = Lucid.Base.makeAttribute "hx-disable" mempty

hxEncoding_ :: Text -> Lucid.Attribute
hxEncoding_ = Lucid.Base.makeAttribute "hx-encoding"

hxExt_ :: Text -> Lucid.Attribute
hxExt_ = Lucid.Base.makeAttribute "hx-ext"

hxGet_ :: Text -> Lucid.Attribute
hxGet_ = Lucid.Base.makeAttribute "hx-get"

hxHeaders_ :: Text -> Lucid.Attribute
hxHeaders_ = Lucid.Base.makeAttribute "hx-headers"

hxHistoryElt_ :: Lucid.Attribute
hxHistoryElt_ = Lucid.Base.makeAttribute "hx-history-elt" mempty

hxInclude_ :: Text -> Lucid.Attribute
hxInclude_ = Lucid.Base.makeAttribute "hx-include"

hxIndicator_ :: Text -> Lucid.Attribute
hxIndicator_ = Lucid.Base.makeAttribute "hx-indicator"

hxParams_ :: Text -> Lucid.Attribute
hxParams_ = Lucid.Base.makeAttribute "hx-params"

hxPatch_ :: Text -> Lucid.Attribute
hxPatch_ = Lucid.Base.makeAttribute "hx-patch"

hxPost_ :: Text -> Lucid.Attribute
hxPost_ = Lucid.Base.makeAttribute "hx-post"

hxPreserve_ :: Text -> Lucid.Attribute
hxPreserve_ = Lucid.Base.makeAttribute "hx-preserve"

hxPrompt_ :: Text -> Lucid.Attribute
hxPrompt_ = Lucid.Base.makeAttribute "hx-prompt"

hxPushUrl_ :: Text -> Lucid.Attribute
hxPushUrl_ = Lucid.Base.makeAttribute "hx-push-url"

hxPut_ :: Text -> Lucid.Attribute
hxPut_ = Lucid.Base.makeAttribute "hx-put"

hxRequest_ :: Text -> Lucid.Attribute
hxRequest_ = Lucid.Base.makeAttribute "hx-request"

hxSelect_ :: Text -> Lucid.Attribute
hxSelect_ = Lucid.Base.makeAttribute "hx-select"

hxSse_ :: Text -> Lucid.Attribute
hxSse_ = Lucid.Base.makeAttribute "hx-sse"

hxSwapOob_ :: Text -> Lucid.Attribute
hxSwapOob_ = Lucid.Base.makeAttribute "hx-swap-oob"

hxSwap_ :: Text -> Lucid.Attribute
hxSwap_ = Lucid.Base.makeAttribute "hx-swap"

hxTarget_ :: Text -> Lucid.Attribute
hxTarget_ = Lucid.Base.makeAttribute "hx-target"

hxTrigger_ :: Text -> Lucid.Attribute
hxTrigger_ = Lucid.Base.makeAttribute "hx-trigger"

hxVals_ :: Text -> Lucid.Attribute
hxVals_ = Lucid.Base.makeAttribute "hx-vals"

hxWs_ :: Text -> Lucid.Attribute
hxWs_ = Lucid.Base.makeAttribute "hx-ws"



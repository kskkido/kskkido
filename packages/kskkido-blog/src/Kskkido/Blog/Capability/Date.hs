module Kskkido.Blog.Capability.Date where

import qualified Data.Time.Clock as Time.Clock

class Date m where
  getCurrentTime :: m Time.Clock.UTCTime

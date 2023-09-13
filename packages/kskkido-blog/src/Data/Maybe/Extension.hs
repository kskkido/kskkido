module Data.Maybe.Extension where

import RIO
import qualified Data.Maybe as Maybe

partition :: (a -> Maybe b) -> [a] -> ([b], [a])
partition fn = foldr step ([],[])
  where step x (ls,rs) = Maybe.maybe (ls,x:rs) (\y -> (y:ls,rs)) $ fn x


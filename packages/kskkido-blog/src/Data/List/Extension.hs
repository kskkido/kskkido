module Data.List.Extension where

import RIO
import qualified RIO.List as List

prependAll :: a -> [a] -> [a]
prependAll delim xs = do
  x <- xs
  [delim, x]

lookaround :: [a] -> [(Maybe a, a, Maybe a)]
lookaround xs =
  let pxs = Nothing : (pure <$> xs)
      nxs = (Just <$> fromMaybe [] (List.tailMaybe xs)) <> [Nothing]
      axs = zip pxs nxs
   in zipWith (\curr around -> (fst around, curr, snd around)) xs axs

before :: forall a. Eq a => a -> [a] -> Maybe a
before target xs = asum $ lookaround xs <&> \(x,y,_) -> do
  guard (target == y)
  x

after :: forall a. Eq a => a -> [a] -> Maybe a
after target xs = asum $ lookaround xs <&> \(_,y,z) -> do
  guard (target == y)
  z


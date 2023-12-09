module Domain.Util.List where

import Data.List
import Domain.Prelude

unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  fmap (swap . fmap reverse) . uncons . reverse

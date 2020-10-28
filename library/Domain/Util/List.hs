module Domain.Util.List
where

import Domain.Prelude
import Data.List


unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  fmap (swap . fmap reverse) . uncons . reverse

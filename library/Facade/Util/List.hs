module Facade.Util.List
where

import Facade.Prelude
import Data.List


unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
  fmap (swap . fmap reverse) . uncons . reverse

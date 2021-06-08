module Domain.Text
where

import Domain.Prelude
import Data.Text
import qualified Data.Char as Char


mapFirstChar fn =
  foldMap (\ (a, b) -> cons (fn a) b) .
  uncons

ucFirst =
  mapFirstChar Char.toUpper

lcFirst =
  mapFirstChar Char.toLower

module Domain.Text where

import qualified Data.Char as Char
import Data.Text
import Domain.Prelude

mapFirstChar :: (Char -> Char) -> Text -> Text
mapFirstChar fn =
  foldMap (\(a, b) -> cons (fn a) b)
    . uncons

ucFirst :: Text -> Text
ucFirst =
  mapFirstChar Char.toUpper

lcFirst :: Text -> Text
lcFirst =
  mapFirstChar Char.toLower

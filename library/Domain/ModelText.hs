module Domain.ModelText
where

import Domain.Prelude
import Domain.Model
import qualified Data.Text as Text
import qualified Data.Char as Char


recordField underscore a b =
  bool mempty "_" underscore <>
  mapFirstChar Char.toLower a <> mapFirstChar Char.toUpper b

sumConstructor a b =
  mapFirstChar Char.toUpper b <> a

mapFirstChar fn =
  foldMap (\ (a, b) -> Text.cons (fn a) b) .
  Text.uncons

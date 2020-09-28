module Domain.ModelText
where

import Domain.Prelude
import Domain.Model
import qualified Data.Text as Text
import qualified Data.Char as Char


typeRef =
  \ case
    LocalTypeRef a -> a
    GlobalTypeRef a b ->
      if null a
        then b
        else Text.intercalate "." a <> "." <> b

recordField a b =
  mapFirstChar Char.toLower a <> mapFirstChar Char.toUpper b

sumConstructor a b =
  mapFirstChar Char.toUpper b <> a

mapFirstChar fn =
  foldMap (\ (a, b) -> Text.cons (fn a) b) .
  Text.uncons

module Domain.Attoparsec.CategoryCentricDoc
where

import Domain.Prelude hiding (takeWhile)
import Domain.Models.CategoryCentricDoc
import Data.Attoparsec.Text
import Domain.Attoparsec.General


typeRefOnly =
  complete typeRef

typeRef =
  fmap TypeRef $ sepBy1 ucName (char '.')

typeOnly =
  complete type_ <|> complete typeListType

type_ =
  do
    a <- nonAppType
    cont a
  where
    cont a =
      asum [
        do
          skipSpace1
          b <- nonAppType
          cont (AppType a b)
        ,
        pure a
        ]

nonAppType =
  inSquareBracketsType <|> inParensType <|> refType

refType =
  RefType <$> typeRef

inParensType =
  inParens typeListType

inSquareBracketsType =
  InSquareBracketsType <$> inSquareBrackets type_

typeListType =
  InParensType <$> commaSeparated type_

module Domain.Attoparsec
where

import Domain.Prelude hiding (takeWhile)
import Domain.V1DocModel
import Data.Attoparsec.Text
import qualified Data.Text as Text


complete parser =
  skipSpace *> parser <* skipSpace <* endOfInput

typeRef =
  fmap TypeRef $ sepBy1 ucName (char '.')

typeOnly =
  complete type_ <|> complete typeListType

type_ =
  appType <|> nonAppType

appType =
  do
    a <- nonAppType
    skipSpace1
    b <- type_
    return (AppType a b)

nonAppType =
  inSquareBracketsType <|> inParensType <|> refType

refType =
  RefType <$> typeRef

inParensType =
  do
    char '('
    skipSpace
    a <- typeListType
    skipSpace
    char ')'
    return a

typeListType =
  InParensType <$> sepBy type_ (skipSpace *> char ',' <* skipSpace)

inSquareBracketsType =
  do
    char '['
    skipSpace
    a <- type_
    skipSpace
    char ']'
    return (InSquareBracketsType a)

skipSpace1 =
  space *> skipSpace

name firstCharPred =
  do
    a <- satisfy firstCharPred
    b <- takeWhile (\ a -> isAlphaNum a || a == '\'' || a == '_')
    return (Text.cons a b)

ucName =
  name isUpper

lcName =
  name (\ a -> isLower a || a == '_')

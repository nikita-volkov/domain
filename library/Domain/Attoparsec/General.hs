module Domain.Attoparsec.General where

import Data.Attoparsec.Text
import qualified Data.Text as Text
import Domain.Prelude hiding (takeWhile)

only :: Parser a -> Parser a
only parser =
  skipSpace *> parser <* skipSpace <* endOfInput

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser =
  sepBy parser comma

comma :: Parser Char
comma =
  skipSpace *> char ',' <* skipSpace

inParens :: Parser b -> Parser b
inParens parser =
  do
    char '('
    skipSpace
    a <- parser
    skipSpace
    char ')'
    return a

inSquareBrackets :: Parser b -> Parser b
inSquareBrackets parser =
  do
    char '['
    skipSpace
    a <- parser
    skipSpace
    char ']'
    return a

skipSpace1 :: Parser ()
skipSpace1 =
  space *> skipSpace

name :: (Char -> Bool) -> Parser Text
name firstCharPred =
  do
    a <- satisfy firstCharPred
    b <- takeWhile (\a -> isAlphaNum a || a == '\'' || a == '_')
    return (Text.cons a b)

ucName :: Parser Text
ucName =
  name isUpper

lcName :: Parser Text
lcName =
  name (\a -> isLower a || a == '_')

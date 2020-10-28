module Domain.Attoparsec.General
where

import Domain.Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import qualified Data.Text as Text


only parser =
  skipSpace *> parser <* skipSpace <* endOfInput

commaSeparated parser =
  sepBy parser comma

comma =
  skipSpace *> char ',' <* skipSpace

inParens parser =
  do
    char '('
    skipSpace
    a <- parser
    skipSpace
    char ')'
    return a

inSquareBrackets parser =
  do
    char '['
    skipSpace
    a <- parser
    skipSpace
    char ']'
    return a

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

module Domain.Attoparsec.TypeString
where

import Domain.Prelude hiding (takeWhile)
import Domain.Models.TypeString
import Data.Attoparsec.Text hiding (sepBy1)
import Domain.Attoparsec.General
import Control.Applicative.Combinators.NonEmpty


only =
  complete

commaSeq =
  commaSeparated appSeq

appSeq =
  sepBy1 unit skipSpace1

unit =
  asum [
    InSquareBracketsUnit <$> inSquareBrackets appSeq
    ,
    InParensUnit <$> inParens commaSeq
    ,
    RefUnit <$> typeRef
    ]

typeRef =
  sepBy1 ucName (char '.')

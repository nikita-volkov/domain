module Domain.Attoparsec.TypeString where

import Control.Applicative.Combinators.NonEmpty
import Data.Attoparsec.Text hiding (sepBy1)
import Domain.Attoparsec.General
import Domain.Models.TypeString
import Domain.Prelude hiding (takeWhile)

commaSeq :: Parser [NonEmpty Unit]
commaSeq =
  commaSeparated appSeq

appSeq :: Parser (NonEmpty Unit)
appSeq =
  sepBy1 unit skipSpace1

unit :: Parser Unit
unit =
  asum
    [ InSquareBracketsUnit <$> inSquareBrackets appSeq,
      InParensUnit <$> inParens commaSeq,
      RefUnit <$> typeRef
    ]

typeRef :: Parser (NonEmpty Text)
typeRef =
  sepBy1 ucName (char '.')

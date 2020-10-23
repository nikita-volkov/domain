module Domain.Attoparsec.TypeString
where

import Domain.Prelude hiding (takeWhile)
import Domain.Models.TypeString
import Data.Attoparsec.Text
import Domain.Attoparsec.General


typeString =
  commaSeparated (sepBy typeStringUnit skipSpace1)

typeStringUnit =
  asum [
    InSquareBracketsTypeStringUnit <$> inSquareBrackets typeString
    ,
    InParensTypeStringUnit <$> inParens typeString
    ,
    RefTypeStringUnit <$> typeRef
    ]

typeRef =
  sepBy1 ucName (char '.')

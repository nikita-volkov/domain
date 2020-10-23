module Domain.Models.TypeString
where

import Domain.Prelude


{-| Type application sequences nested in comma-sequences. -}
type TypeString =
  [[TypeStringUnit]]

data TypeStringUnit =
  InSquareBracketsTypeStringUnit TypeString |
  InParensTypeStringUnit TypeString |
  RefTypeStringUnit [Text]
  deriving (Show)

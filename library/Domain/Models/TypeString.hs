module Domain.Models.TypeString where

import Domain.Prelude

type CommaSeq =
  [AppSeq]

type AppSeq =
  NonEmpty Unit

data Unit
  = InSquareBracketsUnit AppSeq
  | InParensUnit CommaSeq
  | RefUnit (NonEmpty Text)
  deriving (Show)

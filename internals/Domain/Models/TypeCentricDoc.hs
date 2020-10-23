module Domain.Models.TypeCentricDoc
where

import Domain.Prelude hiding (Product, Sum, Enum)
import qualified Domain.Models.TypeString as TypeString


type Doc =
  [(Text, Structure)]

data Structure =
  ProductStructure [(Text, TypeString.AppSeq)] |
  SumStructure [(Text, SumTypeExpression)] |
  EnumStructure [Text] |
  WrapperStructure TypeString.AppSeq |
  AliasStructure TypeString.AppSeq
  deriving (Show)

data SumTypeExpression =
  SequenceSumTypeExpression [TypeString.AppSeq] |
  StringSumTypeExpression TypeString.CommaSeq
  deriving (Show)

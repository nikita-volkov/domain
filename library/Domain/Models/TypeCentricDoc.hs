module Domain.Models.TypeCentricDoc
where

import Domain.Prelude hiding (Product, Sum, Enum)
import qualified Domain.Models.TypeString as TypeString


type Doc =
  [(Text, Structure)]

data Structure =
  ProductStructure [(Text, NestedTypeExpression)] |
  SumStructure [(Text, [NestedTypeExpression])] |
  EnumStructure [Text]
  deriving (Show)

data NestedTypeExpression =
  AppSeqNestedTypeExpression TypeString.AppSeq |
  StructureNestedTypeExpression Structure
  deriving (Show)

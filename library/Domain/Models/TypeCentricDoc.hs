module Domain.Models.TypeCentricDoc where

import qualified Domain.Models.TypeString as TypeString
import Domain.Prelude hiding (Enum, Product, Sum)

type Doc =
  [(Text, Structure)]

data Structure
  = ProductStructure [(Text, NestedTypeExpression)]
  | SumStructure [(Text, [NestedTypeExpression])]
  | EnumStructure [Text]
  deriving (Show)

data NestedTypeExpression
  = AppSeqNestedTypeExpression TypeString.AppSeq
  | StructureNestedTypeExpression Structure
  deriving (Show)

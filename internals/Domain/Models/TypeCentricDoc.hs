module Domain.Models.TypeCentricDoc
where

import Domain.Prelude hiding (Product, Sum, Enum)
import Domain.Models.TypeString


type Doc =
  [(Text, Structure)]

data Structure =
  ProductStructure [(Text, TypeExpression)] |
  SumStructure [(Text, TypeExpression)] |
  EnumStructure [Text] |
  WrapperStructure TypeExpression |
  AliasStructure TypeExpression
  deriving (Show)

data TypeExpression =
  SequenceTypeExpression [TypeString] |
  StringTypeExpression TypeString
  deriving (Show)

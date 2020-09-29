module Domain.V1DocModel
where

import Domain.Prelude hiding (Product, Sum, Enum)


data Doc =
  Doc 
    (ByTypeName ImportDef)
    (ByTypeName AliasDef)
    (ByTypeName WrapperDef)
    (ByTypeName EnumDef)
    (ByTypeName ProductDef)
    (ByTypeName SumDef)
  deriving (Show)

-- *
-------------------------

newtype ByTypeName a =
  ByTypeName [(Text, a)]
  deriving (Show)

newtype ImportDef =
  ImportDef TypeRef
  deriving (Show)

newtype AliasDef =
  AliasDef (Maybe Type)
  deriving (Show)

newtype WrapperDef =
  WrapperDef (Maybe Type)
  deriving (Show)

newtype EnumDef =
  EnumDef [Text]
  deriving (Show)

newtype ProductDef =
  ProductDef TypeByFieldName
  deriving (Show)

newtype SumDef =
  SumDef TypeByFieldName
  deriving (Show)

-- *
-------------------------

newtype TypeRef =
  TypeRef [Text]
  deriving (Show)

newtype TypeByFieldName =
  TypeByFieldName [(Text, Maybe Type)]
  deriving (Show)

data Type =
  SequenceType [Maybe Type] |
  InSquareBracketsType Type |
  InParensType [Type] |
  AppType Type Type |
  RefType TypeRef
  deriving (Show)

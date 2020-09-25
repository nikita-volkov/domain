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

-- *
-------------------------

newtype ByTypeName a =
  ByTypeName [(Text, a)]

newtype ImportDef =
  ImportDef TypeRef

newtype AliasDef =
  AliasDef (Maybe Type)

newtype WrapperDef =
  WrapperDef (Maybe Type)

newtype EnumDef =
  EnumDef [Text]

newtype ProductDef =
  ProductDef TypeByFieldName

newtype SumDef =
  SumDef TypeByFieldName

-- *
-------------------------

newtype TypeRef =
  TypeRef [Text]

newtype TypeByFieldName =
  TypeByFieldName [(Text, Maybe Type)]

data Type =
  InSquareBracketsType Type |
  InParensType [Type] |
  AppType Type Type |
  RefType TypeRef

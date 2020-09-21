module Facade.V1DocModel
where

import Facade.Prelude hiding (Product, Sum, Enum)


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
  ImportDef QualifiedTypeRef

newtype AliasDef =
  AliasDef Type

newtype WrapperDef =
  WrapperDef Type

newtype EnumDef =
  EnumDef [Text]

newtype ProductDef =
  ProductDef TypeByFieldName

newtype SumDef =
  SumDef TypeByFieldName

-- *
-------------------------

newtype QualifiedTypeRef =
  QualifiedTypeRef [Text]

data TypeRef =
  QualifiedTypeRefTypeRef QualifiedTypeRef |
  UnqualifiedTypeRef Text

newtype TypeByFieldName =
  TypeByFieldName [(Text, Type)]

data Type =
  TupleType [Type] |
  AppType Type Type |
  RefType TypeRef

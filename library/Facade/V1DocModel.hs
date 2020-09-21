module Facade.V1DocModel
where

import Facade.Prelude


data Doc =
  Doc Imports Aliases Wrappers Enums Products Sums

-- *
-------------------------

newtype Imports =
  Imports (ByTypeName QualifiedTypeRef)

newtype Aliases =
  Aliases (ByTypeName Type)

newtype Wrappers =
  Wrappers (ByTypeName WrapperDef)

newtype Enums =
  Enums (ByTypeName EnumDef)

newtype Products =
  Products (ByTypeName ProductDef)

newtype Sums =
  Sums (ByTypeName SumDef)

-- *
-------------------------

newtype ByTypeName a =
  ByTypeName [(Text, a)]

newtype QualifiedTypeRef =
  QualifiedTypeRef [Text]

data TypeRef =
  QualifiedTypeRefTypeRef QualifiedTypeRef |
  UnqualifiedTypeRef Text

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

newtype TypeByFieldName =
  TypeByFieldName [(Text, Type)]

data Type =
  TupleType [Type] |
  AppType Type Type |
  RefType TypeRef

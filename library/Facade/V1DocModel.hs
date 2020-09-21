module Facade.V1DocModel
where

import Facade.Prelude


data Doc =
  Doc Imports Aliases Wrappers Enums Products Sums

-- *
-------------------------

newtype Imports =
  Imports (ByTypeName QualifiedType)

newtype Aliases =
  Aliases (ByTypeName TypeExp)

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
  ByTypeName (HashMap Text a)

newtype QualifiedType =
  QualifiedType (Vector Text)

data TypeRef =
  QualifiedTypeRef QualifiedType |
  UnqualifiedTypeRef Text

newtype WrapperDef =
  WrapperDef TypeExp

newtype EnumDef =
  EnumDef (Vector Text)

newtype ProductDef =
  ProductDef TypeExpByFieldName

newtype SumDef =
  SumDef TypeExpByFieldName

-- *
-------------------------

newtype TypeExpByFieldName =
  TypeExpByFieldName (HashMap Text TypeExp)

data TypeExp



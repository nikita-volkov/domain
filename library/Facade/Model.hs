{-|
High level model.
-}
module Facade.Model
where

import Facade.Prelude


data Dec =
  TypeDec Text TypeDef

data TypeDef =
  AliasTypeDef Type |
  WrapperTypeDef Type |
  EnumTypeDef [Text] |
  CompositeTypeDef Composition [(Text, Type)] 

data Composition =
  ProductComposition | SumComposition

newtype TypeRef =
  TypeRef [Text]

data Type =
  ListType |
  TupleType Int |
  RefType TypeRef |
  AppType Type Type

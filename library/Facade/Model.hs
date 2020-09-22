{-|
High level model.
-}
module Facade.Model
where

import Facade.Prelude


data Dec =
  TypeDec Text TypeDef
  deriving (Generic, Show, Eq, Ord)

data TypeDef =
  AliasTypeDef Type |
  WrapperTypeDef Type |
  EnumTypeDef [Text] |
  CompositeTypeDef Composition [(Text, Type)] 
  deriving (Generic, Show, Eq, Ord)

data Composition =
  ProductComposition | SumComposition
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)

data TypeRef =
  LocalTypeRef Text |
  GlobalTypeRef [Text] Text
  deriving (Generic, Show, Eq, Ord)

data Type =
  ListType |
  TupleType Int |
  RefType TypeRef |
  AppType Type Type
  deriving (Generic, Show, Eq, Ord)

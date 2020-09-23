{-|
High level model.
-}
module Domain.Model
where

import Domain.Prelude


{-|
Declaration of a type.
-}
data TypeDec =
  TypeDec Text TypeDef
  deriving (Generic, Show, Eq, Ord)

{-|
Definition of a type.
-}
data TypeDef =
  AliasTypeDef Type |
  WrapperTypeDef Type |
  EnumTypeDef [Text] |
  CompositeTypeDef Composition [(Text, Type)] 
  deriving (Generic, Show, Eq, Ord)

{-|
Type of composition.
-}
data Composition =
  ProductComposition | SumComposition
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)

{-|
Reference to a type.
-}
data TypeRef =
  LocalTypeRef Text |
  GlobalTypeRef [Text] Text
  deriving (Generic, Show, Eq, Ord)

{-|
Type.
-}
data Type =
  ListType |
  TupleType Int |
  RefType TypeRef |
  AppType Type Type
  deriving (Generic, Show, Eq, Ord)

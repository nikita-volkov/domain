{-|
High level model.
-}
module Domain.Model
where

import Domain.Prelude
import Language.Haskell.TH.Syntax (Lift(..))


{-|
Declaration of a type.
-}
data TypeDec =
  TypeDec Text TypeDef
  deriving (Generic, Show, Eq, Ord, Lift)

{-|
Definition of a type.
-}
data TypeDef =
  AliasTypeDef Type |
  WrapperTypeDef Type |
  EnumTypeDef [Text] |
  SumTypeDef [(Text, [Type])] |
  ProductTypeDef [(Text, Type)] 
  deriving (Generic, Show, Eq, Ord, Lift)

{-|
Reference to a type.
-}
data TypeRef =
  LocalTypeRef Text |
  GlobalTypeRef [Text] Text
  deriving (Generic, Show, Eq, Ord, Lift)

{-|
Type.
-}
data Type =
  ListType |
  TupleType Int |
  RefType TypeRef |
  AppType Type Type
  deriving (Generic, Show, Eq, Ord, Lift)

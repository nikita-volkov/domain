module Domain.ModelV2
where

import Domain.Prelude
import Language.Haskell.TH.Syntax (Lift(..))


{-|
Declaration of a type.
-}
data TypeDec =
  {-|
  Name of the type and its definition.
  -}
  TypeDec Text TypeDef
  deriving (Generic, Show, Eq, Ord, Lift)

{-|
Definition of a type.
-}
data TypeDef =
  {-|
  Alias.
  Think of it as a @type@ declartion.
  -}
  AliasTypeDef Type |
  {-|
  Wrapper type.
  Think of it as a @newtype@ declartion.
  -}
  WrapperTypeDef Type |
  {-|
  Enumeration.
  Think of it as an ADT, which only has constructors.
  -}
  EnumTypeDef [Text] |
  {-|
  Sum.
  A list of pairs of names of its members
  (which will be mapped to constructors) and
  types which will populate the according constructors.
  -}
  SumTypeDef [(Text, [Type])] |
  {-|
  Product.
  Think of it as a record.
  Carries a list of associations of field names with types.
  -}
  ProductTypeDef [(Text, Type)] 
  deriving (Generic, Show, Eq, Ord, Lift)

{-|
Type.
-}
data Type =
  TupleType [Type] |
  AppType (NonEmpty Type) |
  ListType Type |
  RefType Text
  deriving (Generic, Show, Eq, Ord, Lift)

module Domain.InstanceDecs
where

import Domain.Prelude
import Domain.Model
import qualified Domain.InstanceDec as InstanceDec
import qualified Language.Haskell.TH as TH (Dec, Name)


hasField :: TypeDec -> [TH.Dec]
hasField (TypeDec typeName typeDef) =
  case typeDef of
    EnumTypeDef variants ->
      variants &
      fmap (InstanceDec.enumHasField typeName)
    ProductTypeDef members ->
      zipWith zipper (enumFrom 0) members
      where
        numMembers =
          length members
        zipper offset (fieldName, fieldType) =
          InstanceDec.productHasField typeName fieldName fieldType numMembers offset
    SumTypeDef variants ->
      fmap mapper variants
      where
        mapper (variantName, memberTypes) =
          InstanceDec.sumHasField typeName variantName memberTypes
    WrapperTypeDef t ->
      pure $ InstanceDec.productHasField typeName "value" t 1 0
    AliasTypeDef _ ->
      empty

accessorIsLabel :: TypeDec -> [TH.Dec]
accessorIsLabel (TypeDec typeName typeDef) =
  case typeDef of
    AliasTypeDef _ -> []
    WrapperTypeDef wrappedType ->
      pure $
      InstanceDec.productAccessorIsLabel typeName "value" wrappedType 1 0
    EnumTypeDef variants ->
      variants &
      fmap (InstanceDec.enumAccessorIsLabel typeName)
    ProductTypeDef members ->
      zipWith zipper (enumFrom 0) members
      where
        numMembers =
          length members
        zipper offset (fieldName, fieldType) =
          InstanceDec.productAccessorIsLabel typeName fieldName fieldType numMembers offset
    SumTypeDef variants ->
      variants &
      fmap (\ (variantName, memberTypes) ->
        InstanceDec.sumAccessorIsLabel typeName variantName memberTypes
        )

constructorIsLabel :: TypeDec -> [TH.Dec]
constructorIsLabel (TypeDec typeName typeDef) =
  case typeDef of
    AliasTypeDef _ -> []
    WrapperTypeDef wrappedType ->
      pure $
      InstanceDec.wrapperConstructorIsLabel typeName wrappedType
    EnumTypeDef variants ->
      variants &
      fmap (InstanceDec.enumConstructorIsLabel typeName)
    ProductTypeDef members ->
      []
    SumTypeDef variants ->
      variants &
      foldMap (variantConstructorIsLabel typeName)

variantConstructorIsLabel :: Text -> (Text, [Type]) -> [TH.Dec]
variantConstructorIsLabel typeName (variantName, memberTypes) =
  mconcat [
    pure $
    InstanceDec.curriedSumConstructorIsLabel typeName variantName memberTypes
    ,
    case memberTypes of
      [] ->
        []
      [_] ->
        []
      _ ->
        pure $
        InstanceDec.uncurriedSumConstructorIsLabel typeName variantName memberTypes
    ]


-- * Deriving
-------------------------

byNonAliasName :: (Text -> TH.Dec) -> TypeDec -> [TH.Dec]
byNonAliasName cont (TypeDec a b) =
  case b of
    AliasTypeDef _ ->
      []
    _ ->
      [cont a]

byEnumName :: (Text -> TH.Dec) -> TypeDec -> [TH.Dec]
byEnumName cont (TypeDec a b) =
  case b of
    EnumTypeDef _ ->
      [cont a]
    _ ->
      []

enum =
  byEnumName (InstanceDec.deriving_ ''Enum)

bounded =
  byEnumName (InstanceDec.deriving_ ''Bounded)

show =
  byNonAliasName (InstanceDec.deriving_ ''Show)

eq =
  byNonAliasName (InstanceDec.deriving_ ''Eq)

ord =
  byNonAliasName (InstanceDec.deriving_ ''Ord)

generic =
  byNonAliasName (InstanceDec.deriving_ ''Generic)

data_ =
  byNonAliasName (InstanceDec.deriving_ ''Data)

typeable =
  byNonAliasName (InstanceDec.deriving_ ''Typeable)

hashable =
  byNonAliasName (InstanceDec.empty ''Hashable)

lift =
  byNonAliasName (InstanceDec.deriving_ ''Lift)

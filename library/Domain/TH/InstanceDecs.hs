module Domain.TH.InstanceDecs
where

import Domain.Prelude
import DomainCore.Model
import qualified Domain.TH.InstanceDec as InstanceDec
import qualified Language.Haskell.TH as TH (Dec, Name)


hasField :: TypeDec -> [TH.Dec]
hasField (TypeDec typeName typeDef) =
  case typeDef of
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

accessorIsLabel :: TypeDec -> [TH.Dec]
accessorIsLabel (TypeDec typeName typeDef) =
  case typeDef of
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
    ProductTypeDef members ->
      []
    SumTypeDef variants ->
      variants &
      fmap (\ (variantName, memberTypes) ->
        InstanceDec.curriedSumConstructorIsLabel typeName variantName memberTypes)

variantConstructorIsLabel :: Text -> (Text, [Type]) -> [TH.Dec]
variantConstructorIsLabel typeName (variantName, memberTypes) =
  let
    curried =
      InstanceDec.curriedSumConstructorIsLabel typeName variantName memberTypes
    uncurried =
      InstanceDec.uncurriedSumConstructorIsLabel typeName variantName memberTypes
    in case memberTypes of
      [] ->
        [curried]
      [_] ->
        [curried]
      _ ->
        [curried, uncurried]

mapperIsLabel :: TypeDec -> [TH.Dec]
mapperIsLabel (TypeDec typeName typeDef) =
  case typeDef of
    ProductTypeDef members ->
      zipWith zipper (enumFrom 0) members
      where
        numMembers =
          length members
        zipper offset (fieldName, fieldType) =
          InstanceDec.productMapperIsLabel typeName fieldName fieldType numMembers offset
    SumTypeDef variants ->
      do
        (variantName, memberTypes) <- variants
        if null memberTypes
          then empty
          else pure (InstanceDec.sumMapperIsLabel typeName variantName memberTypes)


-- * Deriving
-------------------------

byNonAliasName :: (Text -> TH.Dec) -> TypeDec -> [TH.Dec]
byNonAliasName cont (TypeDec a b) =
  [cont a]

byEnumName :: (Text -> TH.Dec) -> TypeDec -> [TH.Dec]
byEnumName cont (TypeDec name def) =
  case def of
    SumTypeDef variants | all (null . snd) variants ->
      [cont name]
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

{-|
This module introduces a new development style.

The 'Domain.Deriver.TH' module should be gradually migrated here.
-}
module Domain.InstanceDecs
where

import Domain.Prelude
import Domain.Model
import qualified Domain.InstanceDecTemplate as InstanceDecTemplate
import qualified Language.Haskell.TH as TH


hasField :: TypeDec -> [TH.Dec]
hasField (TypeDec typeName typeDef) =
  case typeDef of
    EnumTypeDef variants ->
      variants &
      fmap (InstanceDecTemplate.enumHasField typeName)
    ProductTypeDef members ->
      zipWith zipper (enumFrom 0) members
      where
        numMembers =
          length members
        zipper offset (fieldName, fieldType) =
          InstanceDecTemplate.productHasField typeName fieldName fieldType numMembers offset
    SumTypeDef variants ->
      fmap mapper variants
      where
        mapper (variantName, memberTypes) =
          InstanceDecTemplate.sumHasField typeName variantName memberTypes
    WrapperTypeDef t ->
      pure $ InstanceDecTemplate.productHasField typeName "value" t 1 0
    AliasTypeDef _ ->
      empty

module Facade.Deriver.TH
where

import Facade.Prelude
import Language.Haskell.TH
import qualified Facade.Model as Mo
import qualified Data.Text as Text


enumInstanceDecs =
  enumTypeDecDerivingInstanceDecs ''Enum

boundedInstanceDecs =
  enumTypeDecDerivingInstanceDecs ''Bounded

showInstanceDecs =
  nonAliasTypeDecDerivingInstanceDecs ''Show

eqInstanceDecs =
  nonAliasTypeDecDerivingInstanceDecs ''Eq

ordInstanceDecs =
  nonAliasTypeDecDerivingInstanceDecs ''Ord

genericInstanceDecs =
  nonAliasTypeDecDerivingInstanceDecs ''Generic

dataInstanceDecs =
  nonAliasTypeDecDerivingInstanceDecs ''Data

typeableInstanceDecs =
  nonAliasTypeDecDerivingInstanceDecs ''Typeable


nonAliasTypeDecDerivingInstanceDecs className =
  \ case
    Mo.TypeDec a b ->
      case b of
        Mo.AliasTypeDef _ ->
          []
        _ ->
          [derivingInstanceDec className a]

enumTypeDecDerivingInstanceDecs className =
  \ case
    Mo.TypeDec a b ->
      case b of
        Mo.EnumTypeDef c ->
          [derivingInstanceDec className a]
        _ ->
          []


derivingInstanceDec className typeNameText =
  StandaloneDerivD Nothing [] headType
  where
    headType =
      AppT (ConT className) (ConT (textName typeNameText))

textName =
  mkName . Text.unpack

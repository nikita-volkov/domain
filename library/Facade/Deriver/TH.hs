module Facade.Deriver.TH
where

import Facade.Prelude
import Language.Haskell.TH
import qualified Facade.Model as Mo
import qualified Data.Text as Text
import qualified Facade.TH as TH


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
      AppT (ConT className) (ConT (TH.textName typeNameText))


constructorIsLabelInstanceDecs =
  \ case
    Mo.TypeDec a b ->
      case b of
        Mo.AliasTypeDef _ -> []
        Mo.WrapperTypeDef c ->
          [wrapperConstructorIsLabelInstanceDec a c]
        Mo.EnumTypeDef c ->
          fmap (enumConstructorIsLabelInstanceDec a) c
        Mo.CompositeTypeDef c d ->
          case c of
            Mo.ProductComposition -> []
            Mo.SumComposition ->
              fmap (uncurry (sumConstructorIsLabelInstanceDec a)) d

wrapperConstructorIsLabelInstanceDec typeName type_ =
  InstanceD Nothing [] headType bodyDecs
  where
    headType =
      listAppT (ConT ''IsLabel) [labelType, repType]
      where
        labelType =
          LitT (StrTyLit "value")
        repType =
          listAppT ArrowT [payloadType, sumType]
          where
            payloadType =
              TH.typeType type_
            sumType =
              ConT (TH.textName typeName)
    bodyDecs =
      [fromLabelDec]
      where
        fromLabelDec =
          FunD 'fromLabel [Clause [] body []]
          where
            body =
              NormalB (ConE (TH.textName typeName))

enumConstructorIsLabelInstanceDec typeName label =
  InstanceD Nothing [] headType bodyDecs
  where
    headType =
      AppT (AppT (ConT ''IsLabel) (LitT (StrTyLit (toList label))))
        (ConT (TH.textName typeName))
    bodyDecs =
      [fromLabelDec]
      where
        fromLabelDec =
          FunD 'fromLabel [Clause [] body []]
          where
            body =
              NormalB (ConE (TH.sumConstructorName typeName label))

sumConstructorIsLabelInstanceDec typeName label type_ =
  InstanceD Nothing [] headType bodyDecs
  where
    headType =
      listAppT (ConT ''IsLabel) [labelType, repType]
      where
        labelType =
          LitT (StrTyLit (toList label))
        repType =
          case type_ of
            Mo.TupleType 0 ->
              sumType
            _ ->
              listAppT ArrowT [payloadType, sumType]
          where
            payloadType =
              TH.typeType type_
            sumType =
              ConT (TH.textName typeName)
    bodyDecs =
      [fromLabelDec]
      where
        fromLabelDec =
          FunD 'fromLabel [Clause [] body []]
          where
            body =
              NormalB (ConE (TH.sumConstructorName typeName label))


listAppT base args =
  foldl' AppT base args

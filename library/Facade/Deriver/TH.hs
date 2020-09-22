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

hashableInstanceDecs =
  nonAliasTypeDecEmptyInstanceDecs ''Hashable


nonAliasTypeDecEmptyInstanceDecs className =
  \ case
    Mo.TypeDec a b ->
      case b of
        Mo.AliasTypeDef _ ->
          []
        _ ->
          [emptyInstanceDec className a]

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

emptyInstanceDec className typeNameText =
  InstanceD Nothing [] headType []
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


accessorIsLabelInstanceDecs =
  \ case
    Mo.TypeDec a b ->
      case b of
        Mo.AliasTypeDef _ -> []
        Mo.WrapperTypeDef c ->
          [wrapperAccessorIsLabelInstanceDec a c]
        Mo.EnumTypeDef c ->
          fmap (enumAccessorIsLabelInstanceDec a) c
        Mo.CompositeTypeDef c d ->
          case c of
            Mo.ProductComposition ->
              fmap (uncurry (productAccessorIsLabelInstanceDec a)) d
            Mo.SumComposition ->
              fmap (uncurry (sumAccessorIsLabelInstanceDec a)) d

wrapperAccessorIsLabelInstanceDec typeName type_ =
  productAccessorIsLabelInstanceDec typeName "value" type_

enumAccessorIsLabelInstanceDec typeName label =
  InstanceD Nothing [] headType [fromLabelDec]
  where
    conName =
      TH.sumConstructorName typeName label
    headType =
      listAppT (ConT ''IsLabel) [labelType, repType]
      where
        labelType =
          LitT (StrTyLit (toList label))
        repType =
          listAppT ArrowT [sumType, resultType]
          where
            sumType =
              ConT (TH.textName typeName)
            resultType =
              ConT ''Bool
    fromLabelDec =
      FunD 'fromLabel [Clause [] (NormalB exp) []]
      where
        exp =
          LamCaseE [positiveMatch, negativeMatch]
          where
            positiveMatch =
              Match pat (NormalB exp) []
              where
                pat =
                  ConP conName []
                exp =
                  ConE 'True
            negativeMatch =
              Match WildP (NormalB exp) []
              where
                exp =
                  ConE 'False

sumAccessorIsLabelInstanceDec typeName label type_ =
  case type_ of
    Mo.TupleType 0 ->
      enumAccessorIsLabelInstanceDec typeName label
    _ ->
      InstanceD Nothing [] headType bodyDecs
      where
        conName =
          TH.sumConstructorName typeName label
        headType =
          listAppT (ConT ''IsLabel) [labelType, repType]
          where
            labelType =
              LitT (StrTyLit (toList label))
            repType =
              listAppT ArrowT [sumType, resultType]
              where
                resultType =
                  AppT (ConT ''Maybe) (TH.typeType type_)
                sumType =
                  ConT (TH.textName typeName)
        bodyDecs =
          [fromLabelDec]
          where
            fromLabelDec =
              FunD 'fromLabel [Clause [] (NormalB exp) []]
              where
                exp =
                  LamCaseE [positiveMatch, negativeMatch]
                  where
                    positiveMatch =
                      Match pat (NormalB exp) []
                      where
                        pat =
                          ConP conName [VarP (mkName "a")]
                        exp =
                          AppE (ConE 'Just) (VarE (mkName "a"))
                    negativeMatch =
                      Match WildP (NormalB exp) []
                      where
                        exp =
                          ConE 'Nothing

productAccessorIsLabelInstanceDec typeName field type_ =
  InstanceD Nothing [] headType [fromLabelDec]
  where
    headType =
      listAppT (ConT ''IsLabel) [labelType, repType]
      where
        labelType =
          LitT (StrTyLit (toList field))
        repType =
          listAppT ArrowT [compositeType, resultType]
          where
            compositeType =
              ConT (TH.textName typeName)
            resultType =
              TH.typeType type_
    fromLabelDec =
      FunD 'fromLabel [Clause [] (NormalB exp) []]
      where
        exp =
          VarE (TH.recordFieldName typeName field)

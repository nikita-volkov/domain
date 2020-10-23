module Domain.YamlUnscrambler.TypeCentricDoc
where

import Domain.Prelude
import Domain.Models.TypeCentricDoc
import YamlUnscrambler
import qualified Domain.Attoparsec.TypeString as TypeStringAttoparsec
import qualified Control.Foldl as Fold


doc =
  value onScalar (Just onMapping) Nothing
  where
    onScalar =
      [nullScalar []]
    onMapping =
      foldMapping (,) Fold.list textString structure

structure =
  value [] (Just onMapping) Nothing
  where
    onMapping =
      byKeyMapping (CaseSensitive True) $
        atByKey "product" (ProductStructure <$> byFieldName appTypeString) <|>
        atByKey "sum" (SumStructure <$> byFieldName sumTypeExpression) <|>
        atByKey "enum" (EnumStructure <$> enumVariants) <|>
        atByKey "wrapper" (WrapperStructure <$> appTypeString) <|>
        atByKey "alias" (AliasStructure <$> appTypeString)

byFieldName onElement =
  value onScalar (Just onMapping) Nothing
  where
    onScalar =
      [nullScalar []]
    onMapping =
      foldMapping (,) Fold.list textString onElement

appTypeString =
  value [
    stringScalar $ attoparsedString "Type signature" $
    TypeStringAttoparsec.only TypeStringAttoparsec.appSeq
    ] Nothing Nothing

sumTypeExpression =
  value onScalar Nothing (Just onSequence)
  where
    onScalar =
      [
        nullScalar (SequenceSumTypeExpression [])
        ,
        fmap StringSumTypeExpression $
        stringScalar $ attoparsedString "Type signature" $
        TypeStringAttoparsec.only TypeStringAttoparsec.commaSeq
        ]
    onSequence =
      SequenceSumTypeExpression <$> foldSequence Fold.list appTypeString

enumVariants =
  sequenceValue (foldSequence Fold.list variant)
  where
    variant =
      scalarsValue [stringScalar textString]

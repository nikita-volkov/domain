module Domain.YamlUnscrambler.TypeCentricDoc
where

import Domain.Prelude
import Domain.Models.TypeCentricDoc
import YamlUnscrambler
import qualified Domain.Attoparsec.TypeString as TypeStringAttoparsec
import qualified Control.Foldl as Fold


typeExpression =
  value onScalar Nothing (Just onSequence)
  where
    onScalar =
      typeStringOnScalar &
        fmap (fmap StringTypeExpression)
    onSequence =
      SequenceTypeExpression <$> foldSequence Fold.list typeString
    typeStringOnScalar =
      [
        nullScalar []
        ,
        stringScalar $ attoparsedString "Type signature" $ TypeStringAttoparsec.typeStringOnly
        ]
    typeString =
      value typeStringOnScalar Nothing Nothing

structure =
  value [] (Just onMapping) Nothing
  where
    onMapping =
      byKeyMapping (CaseSensitive True) $
        atByKey "product" (ProductStructure <$> typeByFieldName) <|>
        atByKey "sum" (SumStructure <$> typeByFieldName) <|>
        atByKey "enum" (EnumStructure <$> enumVariants) <|>
        atByKey "wrapper" (WrapperStructure <$> typeExpression) <|>
        atByKey "alias" (AliasStructure <$> typeExpression)

typeByFieldName =
  value onScalar (Just onMapping) Nothing
  where
    onScalar =
      [nullScalar []]
    onMapping =
      foldMapping (,) Fold.list textString typeExpression

enumVariants =
  sequenceValue (foldSequence Fold.list variant)
  where
    variant =
      scalarsValue [stringScalar textString]

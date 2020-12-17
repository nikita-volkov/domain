module Domain.YamlUnscrambler.TypeCentricDoc
where

import Domain.Prelude
import Domain.Models.TypeCentricDoc
import YamlUnscrambler
import qualified Domain.Attoparsec.TypeString as TypeStringAttoparsec
import qualified Domain.Attoparsec.General as GeneralAttoparsec
import qualified Control.Foldl as Fold
import qualified Data.Text as Text


doc =
  value onScalar (Just onMapping) Nothing
  where
    onScalar =
      [nullScalar []]
    onMapping =
      foldMapping (,) Fold.list typeNameString structure
      where
        typeNameString =
          formattedString "type name" $ \ input ->
            case Text.uncons input of
              Just (h, t) ->
                if isUpper h
                  then
                    if Text.all (\ a -> isAlphaNum a || a == '\'' || a == '_') t
                      then
                        Right input
                      else
                        Left "Contains invalid chars"
                  else
                    Left "First char is not upper-case"
              Nothing ->
                Left "Empty string"

structure =
  value [] (Just structureMapping) Nothing

byFieldName onElement =
  value onScalar (Just onMapping) Nothing
  where
    onScalar =
      [nullScalar []]
    onMapping =
      foldMapping (,) Fold.list textString onElement

sumTypeExpression =
  value onScalar (Just onMapping) (Just onSequence)
  where
    onScalar =
      [
        nullScalar []
        ,
        fmap (fmap AppSeqNestedTypeExpression) $
        stringScalar $ attoparsedString "Type signature" $
        GeneralAttoparsec.only TypeStringAttoparsec.commaSeq
        ]
    onMapping =
      pure . StructureNestedTypeExpression <$> structureMapping
    onSequence =
      foldSequence Fold.list nestedTypeExpression

nestedTypeExpression =
  value [onScalar] (Just onMapping) Nothing
  where
    onScalar =
      AppSeqNestedTypeExpression <$> appTypeStringScalar
    onMapping =
      StructureNestedTypeExpression <$> structureMapping

enumVariants =
  sequenceValue (foldSequence Fold.list variant)
  where
    variant =
      scalarsValue [stringScalar textString]


-- * Scalar
-------------------------

appTypeStringScalar =
  stringScalar $ attoparsedString "Type signature" $
  GeneralAttoparsec.only TypeStringAttoparsec.appSeq

-- * Mapping
-------------------------

structureMapping =
  byKeyMapping (CaseSensitive True) $
    atByKey "product" (ProductStructure <$> byFieldName nestedTypeExpression) <|>
    atByKey "sum" (SumStructure <$> byFieldName sumTypeExpression) <|>
    atByKey "enum" (EnumStructure <$> enumVariants)

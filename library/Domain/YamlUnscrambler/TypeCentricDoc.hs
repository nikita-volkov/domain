module Domain.YamlUnscrambler.TypeCentricDoc where

import qualified Control.Foldl as Fold
import qualified Data.Text as Text
import qualified Domain.Attoparsec.General as GeneralAttoparsec
import qualified Domain.Attoparsec.TypeString as TypeStringAttoparsec
import Domain.Models.TypeCentricDoc
import qualified Domain.Models.TypeString as TypeStringModel
import Domain.Prelude
import YamlUnscrambler

doc :: Value [(Text, Structure)]
doc =
  value onScalar (Just onMapping) Nothing
  where
    onScalar =
      [nullScalar []]
    onMapping =
      foldMapping (,) Fold.list typeNameString structure
      where
        typeNameString =
          formattedString "type name" $ \input ->
            case Text.uncons input of
              Just (h, t) ->
                if isUpper h
                  then
                    if Text.all (\a -> isAlphaNum a || a == '\'' || a == '_') t
                      then Right input
                      else Left "Contains invalid chars"
                  else Left "First char is not upper-case"
              Nothing ->
                Left "Empty string"

structure :: Value Structure
structure =
  value [] (Just structureMapping) Nothing

byFieldName :: Value val -> Value [(Text, val)]
byFieldName onElement =
  value onScalar (Just onMapping) Nothing
  where
    onScalar =
      [nullScalar []]
    onMapping =
      foldMapping (,) Fold.list textString onElement

sumTypeExpression :: Value [NestedTypeExpression]
sumTypeExpression =
  value onScalar (Just onMapping) (Just onSequence)
  where
    onScalar =
      [ nullScalar [],
        fmap (fmap AppSeqNestedTypeExpression)
          $ stringScalar
          $ attoparsedString "Type signature"
          $ GeneralAttoparsec.only TypeStringAttoparsec.commaSeq
      ]
    onMapping =
      pure . StructureNestedTypeExpression <$> structureMapping
    onSequence =
      foldSequence Fold.list nestedTypeExpression

nestedTypeExpression :: Value NestedTypeExpression
nestedTypeExpression =
  value [onScalar] (Just onMapping) Nothing
  where
    onScalar =
      AppSeqNestedTypeExpression <$> appTypeStringScalar
    onMapping =
      StructureNestedTypeExpression <$> structureMapping

enumVariants :: Value [Text]
enumVariants =
  sequenceValue (foldSequence Fold.list variant)
  where
    variant =
      scalarsValue [stringScalar textString]

-- * Scalar

-------------------------

appTypeStringScalar :: Scalar (NonEmpty TypeStringModel.Unit)
appTypeStringScalar =
  stringScalar
    $ attoparsedString "Type signature"
    $ GeneralAttoparsec.only TypeStringAttoparsec.appSeq

-- * Mapping

-------------------------

structureMapping :: Mapping Structure
structureMapping =
  byKeyMapping (CaseSensitive True)
    $ atByKey "product" (ProductStructure <$> byFieldName nestedTypeExpression)
    <|> atByKey "sum" (SumStructure <$> byFieldName sumTypeExpression)
    <|> atByKey "enum" (EnumStructure <$> enumVariants)

module Domain.YamlUnscrambler.CategoryCentricDoc
(
  parseText,
  parseByteString,
  doc,
)
where

import Domain.Prelude
import Domain.Models.CategoryCentricDoc
import YamlUnscrambler
import qualified Domain.Attoparsec.CategoryCentricDoc as Attoparsec
import qualified Control.Foldl as Fold


possibleByTypeNameAtByKey :: Text -> Value a -> ByKey Text (ByTypeName a)
possibleByTypeNameAtByKey key body =
  atByKey key (value [nullScalar absent] (Just (byTypeNameMapping body)) Nothing) <|>
  pure absent
  where
    absent =
      ByTypeName mempty

byTypeNameMapping :: Value a -> Mapping (ByTypeName a)
byTypeNameMapping =
  fmap ByTypeName .
  foldMapping (,) Fold.list textString

typeRef :: Value TypeRef
typeRef =
  scalarsValue [
    stringScalar $ attoparsedString "Type reference" $
    Attoparsec.typeRefOnly
    ]

type_ :: Value (Maybe Type)
type_ =
  value
    [
      nullScalar Nothing
      ,
      fmap Just $ stringScalar $ attoparsedString "Type signature" $
      Attoparsec.typeOnly
      ]
    Nothing
    (Just (Just . SequenceType <$> foldSequence Fold.list type_))

wrapperDef :: Value WrapperDef
wrapperDef =
  WrapperDef <$> type_

productDef :: Value ProductDef
productDef =
  ProductDef <$> typeByFieldName

sumDef :: Value SumDef
sumDef =
  SumDef <$> typeByFieldName

typeByFieldName :: Value TypeByFieldName
typeByFieldName =
  fmap TypeByFieldName $
  value [nullScalar mempty] (Just mapping) Nothing
  where
    mapping =
      foldMapping (,) Fold.list textString type_

doc :: Value Doc
doc =
  mappingValue $ byKeyMapping (CaseSensitive False) $
    Doc <$>
      possibleByTypeNameAtByKey "imports" importDef <*>
      possibleByTypeNameAtByKey "aliases" aliasDef <*>
      possibleByTypeNameAtByKey "wrappers" wrapperDef <*>
      possibleByTypeNameAtByKey "enums" enumDef <*>
      possibleByTypeNameAtByKey "products" productDef <*>
      possibleByTypeNameAtByKey "sums" sumDef

aliasDef =
  AliasDef <$> type_

importDef =
  ImportDef <$> typeRef

enumDef =
  EnumDef <$> sequenceValue (foldSequence Fold.list (scalarsValue [stringScalar textString]))

module Domain.YamlUnscrambler
where

import Domain.Prelude
import Domain.V1DocModel
import YamlUnscrambler
import qualified Domain.Attoparsec as Attoparsec
import qualified Control.Foldl as Fold


sequenceValue :: Sequence a -> Value a
sequenceValue sequence =
  value [] Nothing (Just sequence)

mappingValue :: Mapping a -> Value a
mappingValue mapping =
  value [] (Just mapping) Nothing

scalarsValue :: [Scalar a] -> Value a
scalarsValue scalars =
  value scalars Nothing Nothing

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
    Attoparsec.complete Attoparsec.typeRef
    ]

type_ :: Value (Maybe Type)
type_ =
  value
    [
      nullScalar Nothing
      ,
      fmap Just $ stringScalar $ attoparsedString "Type signature" $
      Attoparsec.complete Attoparsec.typeOnly
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
  mappingValue $
  foldMapping (,) Fold.list textString type_

doc :: Value Doc
doc =
  mappingValue $ byKeyMapping False $
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

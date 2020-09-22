module Domain.AesonValueParser
where

import Domain.Prelude hiding (null)
import Domain.V1DocModel
import AesonValueParser
import Domain.Util.AesonValueParser
import qualified Domain.Attoparsec as Attoparsec


doc =
  object $
    Doc <$>
      possibleByTypeNameField "imports" importDef <*>
      possibleByTypeNameField "aliases" aliasDef <*>
      possibleByTypeNameField "wrappers" wrapperDef <*>
      possibleByTypeNameField "enums" enumDef <*>
      possibleByTypeNameField "products" productDef <*>
      possibleByTypeNameField "sums" sumDef

possibleByTypeNameField name =
  possibleFieldWithDefault name (ByTypeName mempty) . byTypeName

byTypeName =
  fmap (ByTypeName . toList) .
  object . fieldMap (attoparsedText (Attoparsec.complete Attoparsec.ucName))

importDef =
  ImportDef <$> typeRef

aliasDef =
  AliasDef <$> type_

wrapperDef =
  WrapperDef <$> type_

enumDef =
  EnumDef . toList <$>
  array (elementVector (string (attoparsedText (Attoparsec.complete Attoparsec.lcName))))

productDef =
  ProductDef <$> typeByFieldName

sumDef =
  SumDef <$> typeByFieldName

typeRef =
  string (attoparsedText (Attoparsec.complete Attoparsec.typeRef))

type_ =
  string (attoparsedText (Attoparsec.complete Attoparsec.typeOnly)) <|>
  null $> InParensType []

typeByFieldName =
  TypeByFieldName . toList <$>
  object (fieldMap (attoparsedText (Attoparsec.complete Attoparsec.lcName)) type_)
    <|>
  null $> TypeByFieldName []

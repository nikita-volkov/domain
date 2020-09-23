module Domain.Util.Yaml
where

import Domain.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.Text.Encoding as Text
import qualified AesonValueParser as Avp


parseByteStringAst :: ByteString -> Either Text Aeson.Value
parseByteStringAst input =
  first (fromString . Yaml.prettyPrintParseException) (Yaml.decodeEither' input)

parseByteString :: ByteString -> Avp.Value a -> Either Text a
parseByteString input parser =
  parseByteStringAst input >>= Avp.runWithTextError parser

parseText :: Text -> Avp.Value a -> Either Text a
parseText =
  parseByteString . Text.encodeUtf8

parseString :: String -> Avp.Value a -> Either Text a
parseString =
  parseText . fromString

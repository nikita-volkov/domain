module Facade.Util.Yaml
where

import Facade.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml


parseByteString :: ByteString -> Either Text Aeson.Value
parseByteString input = left (fromString . Yaml.prettyPrintParseException) (Yaml.decodeEither' input)

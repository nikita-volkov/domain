module Facade
where

import Facade.Prelude
import Facade.Model
import qualified Facade.Util.Yaml as Yaml
import qualified Facade.AesonValueParser as AesonValueParser
import qualified Facade.Components.Resolver as Resolver
import qualified Facade.Components.TypeResolutionMapBuilder as TypeResolutionMapBuilder
import qualified Data.ByteString as ByteString


loadFile :: FilePath -> IO (Either Text [Dec])
loadFile path =
  ByteString.readFile path &
  fmap parse

parse :: ByteString -> Either Text [Dec]
parse input =
  do
    doc <- Yaml.parseByteString input AesonValueParser.doc
    typeResolutionMap <- TypeResolutionMapBuilder.run (TypeResolutionMapBuilder.addDoc doc)
    Resolver.run (Resolver.doc doc) typeResolutionMap

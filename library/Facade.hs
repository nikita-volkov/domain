module Facade
where

import Facade.Prelude
import Facade.Model
import qualified Facade.Util.Yaml as Yaml
import qualified Facade.AesonValueParser as AesonValueParser
import qualified Facade.Components.Resolver as Resolver
import qualified Facade.Components.TypeResolutionMapBuilder as TypeResolutionMapBuilder
import qualified Data.ByteString as ByteString
import qualified Facade.TH as TH
import qualified Language.Haskell.TH.Syntax as TH


parse :: ByteString -> Either Text [Dec]
parse input =
  do
    doc <- Yaml.parseByteString input AesonValueParser.doc
    typeResolutionMap <- TypeResolutionMapBuilder.run (TypeResolutionMapBuilder.addDoc doc)
    Resolver.run (Resolver.doc doc) typeResolutionMap

parseFile :: FilePath -> IO (Either Text [Dec])
parseFile path =
  ByteString.readFile path &
  fmap parse

load :: FilePath -> TH.Q [TH.Dec]
load path =
  do
    TH.addDependentFile path
    parseRes <- liftIO (parseFile path)
    case parseRes of
      Left err -> fail (toList err)
      Right decs -> return (fmap TH.typeDec decs)

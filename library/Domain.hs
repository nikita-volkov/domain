module Domain
(
  load,
  loadStd,
)
where

import Domain.Prelude
import Domain.Model
import qualified Domain.Util.Yaml as Yaml
import qualified Domain.AesonValueParser as AesonValueParser
import qualified Domain.Deriver as Deriver
import qualified Domain.Components.Resolver as Resolver
import qualified Domain.Components.TypeResolutionMapBuilder as TypeResolutionMapBuilder
import qualified Data.ByteString as ByteString
import qualified Domain.TH as TH
import qualified Language.Haskell.TH.Syntax as TH


parse :: ByteString -> Either Text [Dec]
parse input =
  do
    doc <- Yaml.parseByteString input AesonValueParser.doc
    Resolver.doc doc

parseFile :: FilePath -> IO (Either Text [Dec])
parseFile path =
  ByteString.readFile path &
  fmap parse

load :: FilePath -> Deriver.Deriver -> TH.Q [TH.Dec]
load path (Deriver.Deriver derive) =
  do
    TH.addDependentFile path
    parseRes <- liftIO (parseFile path)
    case parseRes of
      Left err -> fail (toList err)
      Right decs -> do
        instanceDecs <- fmap concat (traverse derive decs)
        return (fmap TH.typeDec decs <> instanceDecs)

loadStd :: FilePath -> TH.Q [TH.Dec]
loadStd path =
  load path Deriver.std

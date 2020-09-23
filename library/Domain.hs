module Domain
(
  load,
  loadDerivingAll,
)
where

import Domain.Prelude hiding (liftEither)
import Domain.Model
import qualified Domain.Util.Yaml as Yaml
import qualified Domain.AesonValueParser as AesonValueParser
import qualified Domain.Deriver as Deriver
import qualified Domain.Components.Resolver as Resolver
import qualified Domain.Components.TypeResolutionMapBuilder as TypeResolutionMapBuilder
import qualified Data.ByteString as ByteString
import qualified Domain.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH


parse :: ByteString -> Either Text [TypeDec]
parse input =
  do
    doc <- Yaml.parseByteString input AesonValueParser.doc
    Resolver.doc doc

parseFile :: FilePath -> IO (Either Text [TypeDec])
parseFile path =
  catchIOError act handle
  where
    act =
      ByteString.readFile path & fmap parse
    handle =
      return . Left . showAsText

{-|
Load a YAML domain spec file while explicitly defining the instance deriver.
Will generate the according type definitions and instances.

Call this function on the top-level (where you declare your module members).
-}
load :: FilePath -> Deriver.Deriver -> TH.Q [TH.Dec]
load path deriver =
  loadSpec path >>= declare deriver

{-|
Load a YAML domain spec file using the 'Deriver.all' instance deriver.
-}
loadDerivingAll :: FilePath -> TH.Q [TH.Dec]
loadDerivingAll path =
  load path Deriver.all

loadSpec :: FilePath -> TH.Q [TypeDec]
loadSpec path =
  do
    TH.addDependentFile path
    parseRes <- liftIO (parseFile path)
    liftEither parseRes

declare :: Deriver.Deriver -> [TypeDec] -> TH.Q [TH.Dec]
declare (Deriver.Deriver derive) spec =
  do
    instanceDecs <- fmap concat (traverse derive spec)
    return (fmap TH.typeDec spec <> instanceDecs)

parseString :: String -> TH.Q [TypeDec]
parseString input =
  liftEither $ do
    doc <- Yaml.parseString input AesonValueParser.doc
    Resolver.doc doc

liftEither :: Either Text a -> TH.Q a
liftEither =
  \ case
    Left err -> fail (toList err)
    Right a -> return a 

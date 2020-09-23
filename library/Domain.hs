module Domain
(
  -- * Loading from external files
  load,
  loadDerivingAll,
  -- * Inlining
  declare,
  declareDerivingAll,
  spec,
)
where

import Domain.Prelude hiding (liftEither, readFile, lift)
import Domain.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Domain.Model as Model
import qualified Domain.Util.Yaml as Yaml
import qualified Domain.AesonValueParser as AesonValueParser
import qualified Domain.Deriver as Deriver
import qualified Domain.Components.Resolver as Resolver
import qualified Domain.Components.TypeResolutionMapBuilder as TypeResolutionMapBuilder
import qualified Data.ByteString as ByteString


{-|
Load a YAML domain spec file while explicitly defining the instance deriver.
Will generate the according type definitions and instances.

Call this function on the top-level (where you declare your module members).
-}
load :: Deriver.Deriver -> FilePath -> Q [Dec]
load deriver path =
  loadSpec path >>= declare deriver

{-|
Load a YAML domain spec file using the 'Deriver.all' instance deriver.
-}
loadDerivingAll :: FilePath -> Q [Dec]
loadDerivingAll =
  load Deriver.all

{-|
Declare datatypes from a spec tree.

Use this in combination with the 'spec' quasi-quoter.
-}
declare :: Deriver.Deriver -> [Model.TypeDec] -> Q [Dec]
declare (Deriver.Deriver derive) spec =
  do
    instanceDecs <- fmap concat (traverse derive spec)
    return (fmap typeDec spec <> instanceDecs)

{-|
Declare datatypes from a spec tree using the 'Deriver.all' instance deriver
-}
declareDerivingAll :: [Model.TypeDec] -> Q [Dec]
declareDerivingAll =
  declare Deriver.all

{-|
Quasi-quoter, which parses a YAML spec into @['Model.TypeDec']@.

Use 'declare' to generate the code from it.
-}
spec :: QuasiQuoter
spec =
  QuasiQuoter exp pat type_ dec
  where
    unsupported =
      const (fail "Quotation in this context is not supported")
    exp =
      lift <=< parseString
    pat =
      unsupported
    type_ =
      unsupported
    dec =
      unsupported


-- * Helpers
-------------------------

loadSpec :: FilePath -> Q [Model.TypeDec]
loadSpec path =
  readFile path >>= parseByteString

readFile :: FilePath -> Q ByteString
readFile path =
  do
    addDependentFile path
    readRes <- liftIO (tryIOError (ByteString.readFile path))
    liftEither (first showAsText readRes)

parseString :: String -> Q [Model.TypeDec]
parseString input =
  liftEither $ do
    doc <- Yaml.parseString input AesonValueParser.doc
    Resolver.doc doc

parseByteString :: ByteString -> Q [Model.TypeDec]
parseByteString input =
  liftEither $ do
    doc <- Yaml.parseByteString input AesonValueParser.doc
    Resolver.doc doc

liftEither :: Either Text a -> Q a
liftEither =
  \ case
    Left err -> fail (toList err)
    Right a -> return a 

module Domain
(
  -- * Loading from external files
  load,
  loadDerivingAll,
  -- * Inlining
  declare,
  spec,
)
where

import Domain.Prelude hiding (liftEither, readFile)
import qualified Domain.Model as Model
import qualified Domain.Util.Yaml as Yaml
import qualified Domain.AesonValueParser as AesonValueParser
import qualified Domain.Deriver as Deriver
import qualified Domain.Components.Resolver as Resolver
import qualified Domain.Components.TypeResolutionMapBuilder as TypeResolutionMapBuilder
import qualified Data.ByteString as ByteString
import qualified Domain.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH


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

{-|
Declare datatypes from a spec tree.

Use this in combination with the 'spec' quasi-quoter.
-}
declare :: Deriver.Deriver -> [Model.TypeDec] -> TH.Q [TH.Dec]
declare (Deriver.Deriver derive) spec =
  do
    instanceDecs <- fmap concat (traverse derive spec)
    return (fmap TH.typeDec spec <> instanceDecs)

{-|
Quasi-quoter, which parses a YAML spec into @['TypeDec']@.

Use 'declare' to generate the code from it.
-}
spec :: TH.QuasiQuoter
spec =
  TH.QuasiQuoter exp pat type_ dec
  where
    unsupported =
      const (fail "Quotation in this context is not supported")
    exp =
      TH.lift <=< parseString
    pat =
      unsupported
    type_ =
      unsupported
    dec =
      unsupported


-- * Helpers
-------------------------

loadSpec :: FilePath -> TH.Q [Model.TypeDec]
loadSpec path =
  readFile path >>= parseByteString

readFile :: FilePath -> TH.Q ByteString
readFile path =
  do
    TH.addDependentFile path
    readRes <- liftIO (tryIOError (ByteString.readFile path))
    liftEither (first showAsText readRes)

parseString :: String -> TH.Q [Model.TypeDec]
parseString input =
  liftEither $ do
    doc <- Yaml.parseString input AesonValueParser.doc
    Resolver.doc doc

parseByteString :: ByteString -> TH.Q [Model.TypeDec]
parseByteString input =
  liftEither $ do
    doc <- Yaml.parseByteString input AesonValueParser.doc
    Resolver.doc doc

liftEither :: Either Text a -> TH.Q a
liftEither =
  \ case
    Left err -> fail (toList err)
    Right a -> return a 

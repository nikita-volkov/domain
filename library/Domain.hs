module Domain
(
  -- * Loading from external files
  load,
  loadStd,
  -- * Inlining
  declare,
  declareStd,
  spec,
)
where

import Domain.Prelude hiding (liftEither, readFile, lift)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Domain.Model as Model
import qualified Domain.ModelTH as ModelTH
import qualified Domain.YamlUnscrambler as YamlUnscrambler
import qualified Domain.Deriver as Deriver
import qualified Domain.Components.Resolver as Resolver
import qualified Domain.Components.TypeResolutionMapBuilder as TypeResolutionMapBuilder
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text
import qualified YamlUnscrambler


{-|
Load a YAML domain spec file while explicitly defining the instance deriver.
Will generate the according type definitions and instances.

Call this function on the top-level (where you declare your module members).
-}
load ::
  {-|
  Field naming.
  When nothing, no fields will be generated.
  Otherwise the wrapped boolean specifies,
  whether to prefix the names with underscore.
  -}
  Maybe Bool ->
  {-|
  How to derive instances.
  -}
  Deriver.Deriver ->
  FilePath -> Q [Dec]
load fieldNaming deriver path =
  loadSpec path >>= declare fieldNaming deriver

{-|
Load a YAML domain spec file using the 'Deriver.all' instance deriver
and generating no field accessors.
-}
loadStd :: FilePath -> Q [Dec]
loadStd =
  load Nothing Deriver.all

{-|
Declare datatypes from a spec tree.

Use this in combination with the 'spec' quasi-quoter.
-}
declare ::
  {-|
  Field naming.
  When nothing, no fields will be generated.
  Otherwise the wrapped boolean specifies,
  whether to prefix the names with underscore.
  -}
  Maybe Bool ->
  {-|
  How to derive instances.
  -}
  Deriver.Deriver ->
  [Model.TypeDec] -> Q [Dec]
declare fieldNaming (Deriver.Deriver derive) spec =
  do
    instanceDecs <- fmap concat (traverse derive spec)
    return (fmap (ModelTH.typeDec fieldNaming) spec <> instanceDecs)

{-|
Declare datatypes from a spec tree using the 'Deriver.all' instance deriver
and generating no field accessors.
-}
declareStd :: [Model.TypeDec] -> Q [Dec]
declareStd =
  declare Nothing Deriver.all

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
parseString =
  parseText . fromString

parseText :: Text -> Q [Model.TypeDec]
parseText =
  parseByteString . Text.encodeUtf8

parseByteString :: ByteString -> Q [Model.TypeDec]
parseByteString input =
  liftEither $ do
    doc <- YamlUnscrambler.parseByteString YamlUnscrambler.doc input
    Resolver.doc doc

liftEither :: Either Text a -> Q a
liftEither =
  \ case
    Left err -> fail (toList err)
    Right a -> return a 

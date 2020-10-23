module Domain
(
  -- * Loading from external files
  load,
  -- * Inlining
  declare,
  schema,
)
where

import Domain.Prelude hiding (liftEither, readFile, lift)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Domain.Model as Model
import qualified Domain.ModelTH as ModelTH
import qualified Domain.YamlUnscrambler.CategoryCentricDoc as YamlUnscrambler
import qualified Domain.Deriver as Deriver
import qualified Domain.Components.Resolver as Resolver
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text
import qualified YamlUnscrambler


{-|
Load a YAML domain schema file while explicitly defining the instance deriver.
Will generate the according type definitions and instances.

Call this function on the top-level (where you declare your module members).
-}
load ::
  {-|
  Field naming.
  When nothing, no fields will be generated.
  Otherwise the first wrapped boolean specifies,
  whether to prefix the names with underscore,
  and the second - whether to prefix with the type name.
  Please notice that when you choose not to prefix with the type name
  you need to have the @DuplicateRecords@ extension enabled.
  -}
  Maybe (Bool, Bool) ->
  {-|
  How to derive instances.
  -}
  Deriver.Deriver ->
  FilePath -> Q [Dec]
load fieldNaming deriver path =
  loadSchema path >>= declare fieldNaming deriver

{-|
Declare datatypes from a schema tree.

Use this in combination with the 'schema' quasi-quoter.
-}
declare ::
  {-|
  Field naming.
  When nothing, no fields will be generated.
  Otherwise the first wrapped boolean specifies,
  whether to prefix the names with underscore,
  and the second - whether to prefix with the type name.
  Please notice that when you choose not to prefix with the type name
  you need to have the @DuplicateRecords@ extension enabled.
  -}
  Maybe (Bool, Bool) ->
  {-|
  How to derive instances.
  -}
  Deriver.Deriver ->
  [Model.TypeDec] -> Q [Dec]
declare fieldNaming (Deriver.Deriver derive) schema =
  do
    instanceDecs <- fmap (nub . concat) (traverse derive schema)
    return (fmap (ModelTH.typeDec fieldNaming) schema <> instanceDecs)

{-|
Quasi-quoter, which parses a YAML schema into @['Model.TypeDec']@.

Use 'declare' to generate the code from it.
-}
schema :: QuasiQuoter
schema =
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

loadSchema :: FilePath -> Q [Model.TypeDec]
loadSchema path =
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

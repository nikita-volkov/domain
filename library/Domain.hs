module Domain
(
  -- * Declaration
  declare,
  -- * Schema
  Schema,
  -- ** Schema construction
  schema,
  loadSchema,
)
where

import Domain.Prelude hiding (liftEither, readFile, lift)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Domain.Model as Model
import qualified Domain.ModelTH as ModelTH
import qualified Domain.YamlUnscrambler.CategoryCentricDoc as CategoryCentricYaml
import qualified Domain.YamlUnscrambler.TypeCentricDoc as TypeCentricYaml
import qualified Domain.Resolvers.CategoryCentricDoc as CategoryCentricResolver
import qualified Domain.Resolvers.TypeCentricDoc as TypeCentricResolver
import qualified Domain.Deriver as Deriver
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text
import qualified YamlUnscrambler


{-|
Declare datatypes and typeclass instances
from a schema definition according to the provided settings.

Use this function in combination with the 'schema' quasi-quoter or
the 'loadSchema' function.
Refer to their documentation for examples.

Call it on the top-level (where you declare your module members).
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
  Which instances to derive and how.
  -}
  Deriver.Deriver ->
  {-|
  Schema definition.
  -}
  Schema ->
  {-|
  Template Haskell action splicing the generated code on declaration level.
  -}
  Q [Dec]
declare fieldNaming (Deriver.Deriver derive) (Schema schema) =
  do
    instanceDecs <- fmap (nub . concat) (traverse derive schema)
    return (fmap (ModelTH.typeDec fieldNaming) schema <> instanceDecs)


-- * Schema
-------------------------

{-|
Parsed and validated schema.

You can only produce it using the 'schema' quasi-quoter or
the 'loadSchema' function.
-}
newtype Schema =
  Schema [Model.TypeDec]
  deriving (Lift)

{-|
Quasi-quoter, which parses a YAML schema into a 'Schema' expression.

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

{-|
Load and parse a YAML file into a schema definition.

Use 'declare' to generate the code from it.
-}
loadSchema :: FilePath -> Q Schema
loadSchema path =
  readFile path >>= parseByteString


-- * Helpers
-------------------------

readFile :: FilePath -> Q ByteString
readFile path =
  do
    addDependentFile path
    readRes <- liftIO (tryIOError (ByteString.readFile path))
    liftEither (first showAsText readRes)

parseString :: String -> Q Schema
parseString =
  parseText . fromString

parseText :: Text -> Q Schema
parseText =
  parseByteString . Text.encodeUtf8

parseByteString :: ByteString -> Q Schema
parseByteString input =
  liftEither $ do
    doc <- YamlUnscrambler.parseByteString TypeCentricYaml.doc input
    decs <- TypeCentricResolver.eliminateDoc doc
    return (Schema decs)

liftEither :: Either Text a -> Q a
liftEither =
  \ case
    Left err -> fail (toList err)
    Right a -> return a 

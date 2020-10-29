module Domain
(
  -- * Declaration
  declare,
  -- * Schema
  Schema,
  schema,
  loadSchema,
  -- * Deriver
  Deriver.Deriver,
  stdDeriver,
  -- ** Common
  enumDeriver,
  boundedDeriver,
  showDeriver,
  eqDeriver,
  ordDeriver,
  genericDeriver,
  dataDeriver,
  typeableDeriver,
  hashableDeriver,
  liftDeriver,
  -- ** HasField
  hasFieldDeriver,
  -- ** IsLabel
  constructorIsLabelDeriver,
  accessorIsLabelDeriver,
  mapperIsLabelDeriver,
  -- * Clarifications
  -- ** Type Equality Constraint #type-equality-constraint#
  -- |
  -- You may have noticed that some instances (in particular of 'IsLabel')
  -- have some unusual tilde (@~@) constraint:
  -- 
  -- @
  -- instance a ~ TransportProtocol => IsLabel "protocol" (NetworkAddress -> a)
  -- @
  -- 
  -- This constraint states that types are equal.
  -- You might be wondering why do that instead of just
  -- 
  -- @
  -- instance IsLabel "protocol" (NetworkAddress -> TransportProtocol)
  -- @
  -- 
  -- The reason is that it helps the compiler pick up this instance having
  -- only the non-variable parts of the type signature,
  -- since type equality is verified after the instance match.
  -- This provides for better type inference and better error messages.
  -- 
  -- In case of our example we're ensuring that the compiler will pick
  -- up the instance for any function parameterised by @NetworkAddress@.
)
where

import Domain.Prelude hiding (liftEither, readFile, lift)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text
import qualified Domain.Resolvers.TypeCentricDoc as TypeCentricResolver
import qualified Domain.TH.TypeDec as TypeDec
import qualified Domain.TH.InstanceDecs as InstanceDecs
import qualified Domain.YamlUnscrambler.TypeCentricDoc as TypeCentricYaml
import qualified DomainCore.Deriver as Deriver
import qualified DomainCore.Model as Model
import qualified YamlUnscrambler


{-|
Declare datatypes and typeclass instances
from a schema definition according to the provided settings.

Use this function in combination with the 'schema' quasi-quoter or
the 'loadSchema' function.
__For examples__ refer to their documentation.

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
    return (fmap (TypeDec.typeDec fieldNaming) schema <> instanceDecs)


-- * Schema
-------------------------

{-|
Parsed and validated schema.

You can only produce it using the 'schema' quasi-quoter or
the 'loadSchema' function
and generate the code from it using 'declare'.
-}
newtype Schema =
  Schema [Model.TypeDec]
  deriving (Lift)

{-|
Quasi-quoter, which parses a YAML schema into a 'Schema' expression.

Use 'declare' to generate the code from it.

==== __Example__

@
{\-# LANGUAGE
  QuasiQuotes, TemplateHaskell,
  StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, DeriveLift,
  FlexibleInstances, MultiParamTypeClasses,
  DataKinds, TypeFamilies
  #-\}
module Model where

import Data.Text (Text)
import Data.Word (Word16, Word32, Word64)
import Domain

'declare'
  (Just (False, True))
  'stdDeriver'
  ['schema'|

    Host:
      sum:
        ip: Ip
        name: Text

    Ip:
      sum:
        v4: Word32
        v6: Word128

    Word128:
      product:
        part1: Word64
        part2: Word64

    |]
@

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

==== __Example__

@
{\-# LANGUAGE
  QuasiQuotes, TemplateHaskell,
  StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, DeriveLift,
  FlexibleInstances, MultiParamTypeClasses,
  DataKinds, TypeFamilies
  #-\}
module Model where

import Data.Text (Text)
import Data.Word (Word16, Word32, Word64)
import Domain

'declare'
  (Just (True, False))
  (mconcat [
    'deriveBase',
    'deriveIsLabel',
    'hashableDeriver',
    'hasFieldDeriver'
    ])
  =<< 'loadSchema' "domain.yaml"
@
-}
loadSchema ::
  {-|
  Path to the schema file relative to the root of the project.
  -}
  FilePath ->
  {-|
  Template Haskell action producing a valid schema.
  -}
  Q Schema
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


-- * Deriver
-------------------------

{-|
Combination of all derivers exported by this module.
-}
stdDeriver =
  mconcat [
    enumDeriver,
    boundedDeriver,
    showDeriver,
    eqDeriver,
    ordDeriver,
    genericDeriver,
    dataDeriver,
    typeableDeriver,
    hashableDeriver,
    liftDeriver,
    hasFieldDeriver,
    constructorIsLabelDeriver,
    mapperIsLabelDeriver,
    accessorIsLabelDeriver
    ]

{-|
Derives 'Enum' for enums or sums having no members in all variants.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
enumDeriver =
  Deriver.effectless InstanceDecs.enum

{-|
Derives 'Bounded' for enums.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
boundedDeriver =
  Deriver.effectless InstanceDecs.bounded

{-|
Derives 'Show'.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
showDeriver =
  Deriver.effectless InstanceDecs.show

{-|
Derives 'Eq'.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
eqDeriver =
  Deriver.effectless InstanceDecs.eq

{-|
Derives 'Ord'.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
ordDeriver =
  Deriver.effectless InstanceDecs.ord

{-|
Derives 'Generic'.

Requires to have the @StandaloneDeriving@ and @DeriveGeneric@ compiler extensions enabled.
-}
genericDeriver =
  Deriver.effectless InstanceDecs.generic

{-|
Derives 'Data'.

Requires to have the @StandaloneDeriving@ and @DeriveDataTypeable@ compiler extensions enabled.
-}
dataDeriver =
  Deriver.effectless InstanceDecs.data_

{-|
Derives 'Typeable'.

Requires to have the @StandaloneDeriving@ and @DeriveDataTypeable@ compiler extensions enabled.
-}
typeableDeriver =
  Deriver.effectless InstanceDecs.typeable

{-|
Generates 'Generic'-based instances of 'Hashable'.
-}
hashableDeriver =
  Deriver.effectless InstanceDecs.hashable

{-|
Derives 'Lift'.

Requires to have the @StandaloneDeriving@ and @DeriveLift@ compiler extensions enabled.
-}
liftDeriver =
  Deriver.effectless InstanceDecs.lift

-- ** HasField
-------------------------

{-|
Derives 'HasField' with unprefixed field names.

For each field of a product generates instances mapping to their values.

For each constructor of a sum maps to a 'Maybe' tuple of members of that constructor,
unless there\'s no members, in which case it maps to 'Bool'.

For each variant of an enum maps to 'Bool' signaling whether the value equals to it.

/Please notice that if you choose to generate unprefixed record field accessors, it will conflict with this deriver, since it\'s gonna generate duplicate instances./
-}
hasFieldDeriver =
  Deriver.effectless InstanceDecs.hasField


-- * IsLabel
-------------------------

{-|
Generates instances of 'IsLabel' for wrappers, enums and sums,
providing mappings from labels to constructors.

==== __Sum Example__

Having the following schema:

@
Host:
  sum:
    ip: Ip
    name: Text
@

The following instances will be generated:

@
instance a ~ Ip => IsLabel "ip" (a -> Host) where
  fromLabel = IpHost

instance a ~ Text => IsLabel "name" (a -> Host) where
  fromLabel = NameHost
@

In case you\'re wondering what this tilde (@~@) constraint business is about,
refer to the [Type Equality Constraint](#type-equality-constraint) section.

==== __Enum Example__

Having the following schema:

@
TransportProtocol:
  enum:
    - tcp
    - udp
@

The following instances will be generated:

@
instance IsLabel "tcp" TransportProtocol where
  fromLabel = TcpTransportProtocol

instance IsLabel "udp" TransportProtocol where
  fromLabel = UdpTransportProtocol
@

In case you\'re wondering what this tilde (@~@) constraint business is about,
refer to the [Type Equality Constraint](#type-equality-constraint) section.
-}
constructorIsLabelDeriver =
  Deriver.effectless InstanceDecs.constructorIsLabel

{-|
Generates instances of 'IsLabel' for enums, sums and products,
providing accessors to their components.

==== __Product Example__

Having the following schema:

@
NetworkAddress:
  product:
    protocol: TransportProtocol
    host: Host
    port: Word16
@

The following instances will be generated:

@
instance a ~ TransportProtocol => IsLabel "protocol" (NetworkAddress -> a) where
  fromLabel (NetworkAddress a _ _) = a

instance a ~ Host => IsLabel "host" (NetworkAddress -> a) where
  fromLabel (NetworkAddress _ b _) = b

instance a ~ Word16 => IsLabel "port" (NetworkAddress -> a) where
  fromLabel (NetworkAddress _ _ c) = c
@

In case you\'re wondering what this tilde (@~@) constraint business is about,
refer to the [Type Equality Constraint](#type-equality-constraint) section.

==== __Sum Example__

Having the following schema:

@
Host:
  sum:
    ip: Ip
    name: Text
@

The following instances will be generated:

@
instance a ~ Maybe Ip => IsLabel "ip" (Host -> a) where
  fromLabel (IpHost a) = Just a
  fromLabel _ = Nothing

instance a ~ Maybe Text => IsLabel "name" (Host -> a) where
  fromLabel (NameHost a) = Just a
  fromLabel _ = Nothing
@

In case you\'re wondering what this tilde (@~@) constraint business is about,
refer to the [Type Equality Constraint](#type-equality-constraint) section.

==== __Enum Example__

Having the following schema:

@
TransportProtocol:
  enum:
    - tcp
    - udp
@

The following instances will be generated:

@
instance a ~ Bool => IsLabel "tcp" (TransportProtocol -> a) where
  fromLabel TcpTransportProtocol = True
  fromLabel _ = False

instance a ~ Bool => IsLabel "udp" (TransportProtocol -> a) where
  fromLabel UdpTransportProtocol = True
  fromLabel _ = False
@

In case you\'re wondering what this tilde (@~@) constraint business is about,
refer to the [Type Equality Constraint](#type-equality-constraint) section.
-}
accessorIsLabelDeriver =
  Deriver.effectless InstanceDecs.accessorIsLabel

{-|
Generates instances of 'IsLabel' for sums and products,
providing mappers over their components.

==== __Product Example__

Having the following schema:

@
NetworkAddress:
  product:
    protocol: TransportProtocol
    host: Host
    port: Word16
@

The following instances will be generated:

@
instance
  mapper ~ (TransportProtocol -> TransportProtocol) =>
  IsLabel "protocol" (mapper -> NetworkAddress -> NetworkAddress)
  where
    fromLabel mapper (NetworkAddress a b c) =
      NetworkAddress (mapper a) b c

instance
  mapper ~ (Host -> Host) =>
  IsLabel "host" (mapper -> NetworkAddress -> NetworkAddress)
  where
    fromLabel mapper (NetworkAddress a b c) = 
      NetworkAddress a (mapper b) c

instance
  mapper ~ (Word16 -> Word16) =>
  IsLabel "port" (mapper -> NetworkAddress -> NetworkAddress)
  where
    fromLabel mapper (NetworkAddress a b c) =
      NetworkAddress a b (mapper c)
@

In case you\'re wondering what this tilde (@~@) constraint business is about,
refer to the [Type Equality Constraint](#type-equality-constraint) section.

==== __Sum Example__

Having the following schema:

@
Host:
  sum:
    ip: Ip
    name: Text
@

The following instances will be generated:

@
instance
  mapper ~ (Ip -> Ip) =>
  IsLabel "ip" (mapper -> Host -> Host)
  where
    fromLabel fn (IpHost a) = IpHost (fn a)
    fromLabel _ a = a

instance
  mapper ~ (Text -> Text) =>
  IsLabel "name" (mapper -> Host -> Host)
  where
    fromLabel fn (NameHost a) = NameHost (fn a)
    fromLabel _ a = a
@

In case you\'re wondering what this tilde (@~@) constraint business is about,
refer to the [Type Equality Constraint](#type-equality-constraint) section.
-}
mapperIsLabelDeriver =
  Deriver.effectless InstanceDecs.mapperIsLabel

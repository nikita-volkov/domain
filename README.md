# About

Codegen solving multiple problems of the standard Haskell approach to defining models.

# Problems

Declaring a data model in Haskell can get cumbersome. While it is much better than in most popular languages it still suffers from boilerplate, it distracts the developer by forcing to solve unrelated problems in the same place and the code is not terribly readable. On top of that we have the notorious records problem and other naming issues to solve causing inconsistent naming conventions or absense thereof, or workarounds causing more distraction and noise. Declaring instances for typeclasses other than the basic ones requires multiple approaches depending on the class, becoming a non-trivial task for non-experts and producing even more boilerplate.

# Features

In its approach to those problems this projects offers the following features:

- Let the domain model definition be focused. Think about data and nothing else.
- Let it be readable and comfortably editable, avoiding syntactic noise.
- Separate its declaration from the problems of declaration of instances, accessor functions, optics and etc.
- Have the records problem solved.
- Have the problem of conflicting constructor names solved.
- Avoid boilerplate while doing all the above.
- Avoid complications of the build process while doing all the above.

# Solution

This project introduces a clear boundary between the data model declaration and the rest of the code base.
It introduces a low-noise YAML format designed specifically for the problem of defining types and relations between them and that only. We call it Domain Schema.

Schemas can be loaded at compile time and transformed into Haskell declarations using Template Haskell. Since it's just Template Haskell, no extra build software is needed for you to use this library. It is a simple Haskell package.

Schema gets analysed allowing to generate all kinds of instances automatically using a set of prepackaged derivers. An API is provided for creation of custom derivers of uncovered typeclasses.

# Case in point

We'll show you how this whole thing works on an example of a model of a service address.

## Schema

```yaml
# Service can be either located on the network or
# by a socket file.
#
# Choice between two or more types can be encoded using
# "sum" type composition, which you may also know as
# "union" or "variant". That's what we use here.
ServiceAddress:
  sum:
    network: NetworkAddress
    local: FilePath

# Network address is a combination of transport protocol,
# host and port. All those three things at once.
#
# "product" type composition lets us encode that.
# You may also know it as "record" or "tuple".
NetworkAddress:
  product:
    protocol: TransportProtocol
    host: Host
    port: Word16

# Transport protocol is either TCP or UDP.
# We encode that using enumeration.
TransportProtocol:
  enum:
    - tcp
    - udp

# Host can be adressed by either an IP or its name,
# so "sum" again.
Host:
  sum:
    ip: Ip
    name: Text

# IP can be either of version 4 or version 6.
# We encode it as a sum over words of the accordingly required
# amount of bits.
Ip:
  sum:
    v4: Word32
    v6: Word128

# Since the standard lib lacks a definition
# of a 128-bit word, we define a custom one
# as a product of two 64-bit words.
Word128:
  product:
    part1: Word64
    part2: Word64
```

As you can see in the specification above we're not concerned with typeclass instances or problems of name disambiguation. We're only concerned with data and relations that it has. This is what we meant by focus. It makes the experience of designing a model way smoother and the maintenance easier.

Those three methods of defining types (product, sum, enum) are all that you need to define a model of any complexity. If you understand them, there's nothing new to learn.

### Codegen

Now, having that schema defined in a file at path `schemas/model.yaml`,
we can load it in a Haskell module as follows:

```haskell
{-# LANGUAGE
  TemplateHaskell,
  StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, DeriveLift,
  FlexibleInstances, MultiParamTypeClasses,
  DataKinds, TypeFamilies
  #-}
module Model where

import Data.Text (Text)
import Data.Word (Word16, Word32, Word64)
import Domain

declare (Just (False, True)) mempty
  =<< loadSchema "schemas/model.yaml"
```

And that will cause the compiler to generate the following declarations:

```haskell
data ServiceAddress =
  NetworkServiceAddress !NetworkAddress |
  LocalServiceAddress !FilePath

data NetworkAddress =
  NetworkAddress {
    networkAddressProtocol :: !TransportProtocol,
    networkAddressHost :: !Host,
    networkAddressPort :: !Word16
  }

data TransportProtocol =
  TcpTransportProtocol |
  UdpTransportProtocol

data Host =
  IpHost !Ip |
  NameHost !Text

data Ip =
  V4Ip !Word32 |
  V6Ip !Word128

data Word128 =
  Word128 {
    word128Part1 :: !Word64,
    word128Part2 :: !Word64
  }
```

As you can see in the generated code the field names from the schema get translated to record fields or constructors depending on the type composition method.

In this example the record fields are prefixed with type names for disambiguation, but by modifying the options passed to the `declare` function it is possible to remove the type name prefix or prepend with underscore, you can also avoid generating record fields altogether (to keep the value-level namespace clean).

The constructor names are also disambiguated by appending the type name to the label from schema. Thus we are introducing a consistent naming convention, while avoiding the boilerplate in the declaration of the model.

### Instances

If we introduce the following change to our code:

```diff
-declare (Just (False, True)) mempty
+declare (Just (False, True)) stdDeriver
```

We'll get a ton of instances generated including the obvious `Show`, `Eq` and even `Hashable` for all the declared types. We'll also get some useful ones, which you wouldn't derive otherwise.

<details>
  <summary><strong>Listing of generated instances</strong> (it's big)</summary>

```haskell
deriving instance Show ServiceAddress
deriving instance Eq ServiceAddress
deriving instance Ord ServiceAddress
deriving instance GHC.Generics.Generic ServiceAddress
deriving instance Data.Data.Data ServiceAddress
deriving instance base-4.14.1.0:Data.Typeable.Internal.Typeable ServiceAddress
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable ServiceAddress
deriving instance template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.Lift ServiceAddress
instance GHC.Records.HasField "network" ServiceAddress (Maybe NetworkAddress) where
  GHC.Records.getField (NetworkServiceAddress a) = Just a
  GHC.Records.getField _ = Nothing
instance GHC.Records.HasField "local" ServiceAddress (Maybe FilePath) where
  GHC.Records.getField (LocalServiceAddress a) = Just a
  GHC.Records.getField _ = Nothing
instance (a ~ NetworkAddress) =>
         GHC.OverloadedLabels.IsLabel "network" (a -> ServiceAddress) where
  GHC.OverloadedLabels.fromLabel = NetworkServiceAddress
instance (a ~ FilePath) =>
         GHC.OverloadedLabels.IsLabel "local" (a -> ServiceAddress) where
  GHC.OverloadedLabels.fromLabel = LocalServiceAddress
instance (mapper ~ (NetworkAddress -> NetworkAddress)) =>
         GHC.OverloadedLabels.IsLabel "network" (mapper
                                                 -> ServiceAddress -> ServiceAddress) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  NetworkServiceAddress a -> NetworkServiceAddress (fn a)
                  a -> a
instance (mapper ~ (FilePath -> FilePath)) =>
         GHC.OverloadedLabels.IsLabel "local" (mapper
                                               -> ServiceAddress -> ServiceAddress) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  LocalServiceAddress a -> LocalServiceAddress (fn a)
                  a -> a
instance (a ~ Maybe NetworkAddress) =>
         GHC.OverloadedLabels.IsLabel "network" (ServiceAddress -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             NetworkServiceAddress a -> Just a
             _ -> Nothing
instance (a ~ Maybe FilePath) =>
         GHC.OverloadedLabels.IsLabel "local" (ServiceAddress -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             LocalServiceAddress a -> Just a
             _ -> Nothing
deriving instance Show NetworkAddress
deriving instance Eq NetworkAddress
deriving instance Ord NetworkAddress
deriving instance GHC.Generics.Generic NetworkAddress
deriving instance Data.Data.Data NetworkAddress
deriving instance base-4.14.1.0:Data.Typeable.Internal.Typeable NetworkAddress
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable NetworkAddress
deriving instance template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.Lift NetworkAddress
instance GHC.Records.HasField "protocol" NetworkAddress TransportProtocol where
  GHC.Records.getField (NetworkAddress a _ _) = a
instance GHC.Records.HasField "host" NetworkAddress Host where
  GHC.Records.getField (NetworkAddress _ a _) = a
instance GHC.Records.HasField "port" NetworkAddress Word16 where
  GHC.Records.getField (NetworkAddress _ _ a) = a
instance (mapper ~ (TransportProtocol -> TransportProtocol)) =>
         GHC.OverloadedLabels.IsLabel "protocol" (mapper
                                                  -> NetworkAddress -> NetworkAddress) where
  GHC.OverloadedLabels.fromLabel
    = \ fn (NetworkAddress a b c) -> ((NetworkAddress (fn a)) b) c
instance (mapper ~ (Host -> Host)) =>
         GHC.OverloadedLabels.IsLabel "host" (mapper
                                              -> NetworkAddress -> NetworkAddress) where
  GHC.OverloadedLabels.fromLabel
    = \ fn (NetworkAddress a b c) -> ((NetworkAddress a) (fn b)) c
instance (mapper ~ (Word16 -> Word16)) =>
         GHC.OverloadedLabels.IsLabel "port" (mapper
                                              -> NetworkAddress -> NetworkAddress) where
  GHC.OverloadedLabels.fromLabel
    = \ fn (NetworkAddress a b c) -> ((NetworkAddress a) b) (fn c)
instance (a ~ TransportProtocol) =>
         GHC.OverloadedLabels.IsLabel "protocol" (NetworkAddress -> a) where
  GHC.OverloadedLabels.fromLabel = \ (NetworkAddress a _ _) -> a
instance (a ~ Host) =>
         GHC.OverloadedLabels.IsLabel "host" (NetworkAddress -> a) where
  GHC.OverloadedLabels.fromLabel = \ (NetworkAddress _ b _) -> b
instance (a ~ Word16) =>
         GHC.OverloadedLabels.IsLabel "port" (NetworkAddress -> a) where
  GHC.OverloadedLabels.fromLabel = \ (NetworkAddress _ _ c) -> c
deriving instance Enum TransportProtocol
deriving instance Bounded TransportProtocol
deriving instance Show TransportProtocol
deriving instance Eq TransportProtocol
deriving instance Ord TransportProtocol
deriving instance GHC.Generics.Generic TransportProtocol
deriving instance Data.Data.Data TransportProtocol
deriving instance base-4.14.1.0:Data.Typeable.Internal.Typeable TransportProtocol
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable TransportProtocol
deriving instance template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.Lift TransportProtocol
instance GHC.Records.HasField "tcp" TransportProtocol Bool where
  GHC.Records.getField TcpTransportProtocol = True
  GHC.Records.getField _ = False
instance GHC.Records.HasField "udp" TransportProtocol Bool where
  GHC.Records.getField UdpTransportProtocol = True
  GHC.Records.getField _ = False
instance GHC.OverloadedLabels.IsLabel "tcp" TransportProtocol where
  GHC.OverloadedLabels.fromLabel = TcpTransportProtocol
instance GHC.OverloadedLabels.IsLabel "udp" TransportProtocol where
  GHC.OverloadedLabels.fromLabel = UdpTransportProtocol
instance (a ~ Bool) =>
         GHC.OverloadedLabels.IsLabel "tcp" (TransportProtocol -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             TcpTransportProtocol -> True
             _ -> False
instance (a ~ Bool) =>
         GHC.OverloadedLabels.IsLabel "udp" (TransportProtocol -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             UdpTransportProtocol -> True
             _ -> False
deriving instance Show Host
deriving instance Eq Host
deriving instance Ord Host
deriving instance GHC.Generics.Generic Host
deriving instance Data.Data.Data Host
deriving instance base-4.14.1.0:Data.Typeable.Internal.Typeable Host
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable Host
deriving instance template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.Lift Host
instance GHC.Records.HasField "ip" Host (Maybe Ip) where
  GHC.Records.getField (IpHost a) = Just a
  GHC.Records.getField _ = Nothing
instance GHC.Records.HasField "name" Host (Maybe Text) where
  GHC.Records.getField (NameHost a) = Just a
  GHC.Records.getField _ = Nothing
instance (a ~ Ip) =>
         GHC.OverloadedLabels.IsLabel "ip" (a -> Host) where
  GHC.OverloadedLabels.fromLabel = IpHost
instance (a ~ Text) =>
         GHC.OverloadedLabels.IsLabel "name" (a -> Host) where
  GHC.OverloadedLabels.fromLabel = NameHost
instance (mapper ~ (Ip -> Ip)) =>
         GHC.OverloadedLabels.IsLabel "ip" (mapper -> Host -> Host) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  IpHost a -> IpHost (fn a)
                  a -> a
instance (mapper ~ (Text -> Text)) =>
         GHC.OverloadedLabels.IsLabel "name" (mapper -> Host -> Host) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  NameHost a -> NameHost (fn a)
                  a -> a
instance (a ~ Maybe Ip) =>
         GHC.OverloadedLabels.IsLabel "ip" (Host -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             IpHost a -> Just a
             _ -> Nothing
instance (a ~ Maybe Text) =>
         GHC.OverloadedLabels.IsLabel "name" (Host -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             NameHost a -> Just a
             _ -> Nothing
deriving instance Show Ip
deriving instance Eq Ip
deriving instance Ord Ip
deriving instance GHC.Generics.Generic Ip
deriving instance Data.Data.Data Ip
deriving instance base-4.14.1.0:Data.Typeable.Internal.Typeable Ip
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable Ip
deriving instance template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.Lift Ip
instance GHC.Records.HasField "v4" Ip (Maybe Word32) where
  GHC.Records.getField (V4Ip a) = Just a
  GHC.Records.getField _ = Nothing
instance GHC.Records.HasField "v6" Ip (Maybe Word128) where
  GHC.Records.getField (V6Ip a) = Just a
  GHC.Records.getField _ = Nothing
instance (a ~ Word32) =>
         GHC.OverloadedLabels.IsLabel "v4" (a -> Ip) where
  GHC.OverloadedLabels.fromLabel = V4Ip
instance (a ~ Word128) =>
         GHC.OverloadedLabels.IsLabel "v6" (a -> Ip) where
  GHC.OverloadedLabels.fromLabel = V6Ip
instance (mapper ~ (Word32 -> Word32)) =>
         GHC.OverloadedLabels.IsLabel "v4" (mapper -> Ip -> Ip) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  V4Ip a -> V4Ip (fn a)
                  a -> a
instance (mapper ~ (Word128 -> Word128)) =>
         GHC.OverloadedLabels.IsLabel "v6" (mapper -> Ip -> Ip) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  V6Ip a -> V6Ip (fn a)
                  a -> a
instance (a ~ Maybe Word32) =>
         GHC.OverloadedLabels.IsLabel "v4" (Ip -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             V4Ip a -> Just a
             _ -> Nothing
instance (a ~ Maybe Word128) =>
         GHC.OverloadedLabels.IsLabel "v6" (Ip -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             V6Ip a -> Just a
             _ -> Nothing
deriving instance Show Word128
deriving instance Eq Word128
deriving instance Ord Word128
deriving instance GHC.Generics.Generic Word128
deriving instance Data.Data.Data Word128
deriving instance base-4.14.1.0:Data.Typeable.Internal.Typeable Word128
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable Word128
deriving instance template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.Lift Word128
instance GHC.Records.HasField "part1" Word128 Word64 where
  GHC.Records.getField (Word128 a _) = a
instance GHC.Records.HasField "part2" Word128 Word64 where
  GHC.Records.getField (Word128 _ a) = a
instance (mapper ~ (Word64 -> Word64)) =>
         GHC.OverloadedLabels.IsLabel "part1" (mapper
                                               -> Word128 -> Word128) where
  GHC.OverloadedLabels.fromLabel
    = \ fn (Word128 a b) -> (Word128 (fn a)) b
instance (mapper ~ (Word64 -> Word64)) =>
         GHC.OverloadedLabels.IsLabel "part2" (mapper
                                               -> Word128 -> Word128) where
  GHC.OverloadedLabels.fromLabel
    = \ fn (Word128 a b) -> (Word128 a) (fn b)
instance (a ~ Word64) =>
         GHC.OverloadedLabels.IsLabel "part1" (Word128 -> a) where
  GHC.OverloadedLabels.fromLabel = \ (Word128 a _) -> a
instance (a ~ Word64) =>
         GHC.OverloadedLabels.IsLabel "part2" (Word128 -> a) where
  GHC.OverloadedLabels.fromLabel = \ (Word128 _ b) -> b
```
</details>
<p/>


### Labels

Among the generated instances you'll find instances for the `IsLabel` class. It is a class powering Haskell's `OverloadedLabels` extension. The instances we define for it let us reduce the boilerplate in the way we address our model. Here's how.

#### We can access the members of records:

```haskell
getNetworkAddressPort :: NetworkAddress -> Word16
getNetworkAddressPort = #port
```

Yep. Finally. Address your fields without crazy prefixes or dealing with disambiguation otherwise.

_Labels will be unprefixed regardless of what you choose to do about record fields. You can also name them whatever you like. Literally, even `type` and `data` make up valid labels, and unless you choose to generate unprefixed record fields, you can freely use them._

#### We get accessors to the members of sums as well:

```haskell
getHostIp :: Host -> Maybe Ip
getHostIp = #ip
```

Yep. Sum types can have accessors if you look at them from a certain perspective.

#### Accessors to enums - why not?

```haskell
isTransportProtocolTcp :: TransportProtocol -> Bool
isTransportProtocolTcp = #tcp
```

#### We get shortcuts to enums:

```haskell
tcpTransportProtocol :: TransportProtocol
tcpTransportProtocol = #tcp
```

#### We can instantiate sums:

```haskell
ipHost :: Ip -> Host
ipHost = #ip
```

#### We can map over both record fields and sum variants:

```haskell
mapNetworkAddressHost :: (Host -> Host) -> NetworkAddress -> NetworkAddress
mapNetworkAddressHost = #host
```

```haskell
mapHostIp :: (Ip -> Ip) -> Host -> Host
mapHostIp = #ip
```

There's a few things worth noticing here. Unfortunately the type inferencer will be unable to automatically detect the type of the mapping lambda parameter, so it needs to have an unambiguous type. This means that often times you'll have to provide an explicit type for it. But there's a solution.

There is a ["domain-optics"](https://github.com/nikita-volkov/domain-optics) library which provides an integration with the ["optics"](https://github.com/well-typed/optics) library. By including the derivers from it in the parameters to the `declare` macro, you'll be able to map as follows without type inference issues:

```haskell
mapNetworkAddressHost :: (Host -> Host) -> NetworkAddress -> NetworkAddress
mapNetworkAddressHost = over #host
```

You can read more about the "optics" library integration in [the Optics section](#optics).

#### If we can map, then we can also set:

```haskell
setNetworkAddressHost :: Host -> NetworkAddress -> NetworkAddress
setNetworkAddressHost host = #host (const host)
```

## Optics

Extensional ["domain-optics"](https://github.com/nikita-volkov/domain-optics) library provides integration with ["optics"](https://github.com/well-typed/optics). By using the derivers from it we can get optics using labels as well.

Coming back to our example here's all we'll have to do to enable our model with optics:

```haskell
{-# LANGUAGE
  TemplateHaskell,
  StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, DeriveLift,
  FlexibleInstances, MultiParamTypeClasses,
  DataKinds, TypeFamilies,
  UndecidableInstances
  #-}
module Model where

import Data.Text (Text)
import Data.Word (Word16, Word32, Word64)
import Domain
import DomainOptics

declare (Just (False, True)) (stdDeriver <> labelOpticDeriver)
  =<< loadSchema "schemas/model.yaml"
```

Here are some of the optics that will become available to us:

```haskell
networkAddressHostOptic :: Lens' NetworkAddress Host
networkAddressHostOptic = #host
```

```haskell
hostIpOptic :: Prism' Host Ip
hostIpOptic = #ip
```

```haskell
tcpTransportProtocolOptic :: Prism' TransportProtocol ()
tcpTransportProtocolOptic = #tcp
```

_As you may have noticed, we avoid the "underscore-uppercase" naming convention for prisms. With labels there's no longer any need for it._

We recommend using "optics" instead of direct `IsLabel` instances, because functions like `view`, `over`, `set`, `review` make your intent clearer to the reader in many cases and in some cases provide better type inference.

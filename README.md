# About

Domain model codegen solving multiple problems of the standard Haskell approach.

# Problems

Data model declaration in Haskell can get cumbersome.
While it is much better than in most popular languages
it still suffers from boilerplate,
it distracts the developer by forcing to solve unrelated problems in the same place and
the code is not terribly readable.
On top of that we have the notorious records problem and
other naming issues to solve causing inconsistent naming conventions, absense thereof or workarounds causing more distraction and noise.
Declaring instances for typeclasses other than the basic ones
can require multiple approaches depending on the class,
becoming a non-trivial task for non-experts and producing even more boilerplate.

# Goals

This project has the following goals:

- Let the domain model definition be focused on domain model only.
- Let it be readable and comfortly editable, avoiding syntactic noise.
- Separate its declaration from the problems of declaration of instances,
accessor functions, optics and etc.
- Have the notorious records problem solved.
- Similarly have the less notorious problem of conflicting constructor names of sum-types solved.
- Avoid boilerplate while doing all the above.
- Avoid complications of the build process while doing all the above.

# Solution

This project introduces a clear boundary between the data model declaration and the rest of the code base.
It introduces a low-noise YAML format designed specifically for the problem of defining types and relations between them and that only. We call it Domain Schema.

Schemas can be loaded at compile time and transformed into Haskell declarations using Template Haskell. Since it's just Template Haskell, no extra build software is needed for you to use this library. It is a simple Haskell package.

Schema gets analysed allowing to generate all kinds of instances automatically using a set of prepackaged derivers. An API is provided for creation of custom derivers of uncovered typeclasses.

# How it works by example

We'll show you how this whole thing works on an example of a schema for a model of a service address.

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

_For more details about "product", "sum", "enumeration" and
other methods of defining types refer to the [Schema spec](docs/Schema.md)._

Now, having that schema defined in a file at path `models/domain.yaml`,
we can load it in a Haskell module as follows:

```haskell
{-# LANGUAGE
  QuasiQuotes, TemplateHaskell,
  StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, DeriveLift,
  FlexibleInstances, MultiParamTypeClasses,
  DataKinds, TypeFamilies
  #-}
module Model where

import Data.Text (Text)
import Data.Word (Word16, Word32, Word64)
import Domain

declare (Just (False, True)) mempty
  =<< loadSchema "models/domain.yaml"
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
  TcpTransportProtocol | UdpTransportProtocol

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

If we introduce the following change to our code:

```diff
-declare (Just (False, True)) mempty
+declare (Just (False, True)) deriveBase
```

We'll also get a ton of instances generated including
very useful ones, which you couldn't otherwise derive.

<details>
  <summary>Listing of generated instances (we've collapsed it intentionally).</summary>

```haskell
deriving instance Show ServiceAddress
deriving instance Eq ServiceAddress
deriving instance Ord ServiceAddress
deriving instance GHC.Generics.Generic ServiceAddress
deriving instance Data.Data.Data ServiceAddress
deriving instance base-4.13.0.0:Data.Typeable.Internal.Typeable ServiceAddress
instance a ~ NetworkAddress =>
         GHC.OverloadedLabels.IsLabel "network" (a -> ServiceAddress) where
  GHC.OverloadedLabels.fromLabel = NetworkServiceAddress
instance a ~ FilePath =>
         GHC.OverloadedLabels.IsLabel "local" (a -> ServiceAddress) where
  GHC.OverloadedLabels.fromLabel = LocalServiceAddress
instance mapper ~ (NetworkAddress -> NetworkAddress) =>
         GHC.OverloadedLabels.IsLabel "network" (mapper
                                                 -> ServiceAddress -> ServiceAddress) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  NetworkServiceAddress a
                    -> (\ (a) -> NetworkServiceAddress a) (fn a)
                  a -> a
instance mapper ~ (FilePath -> FilePath) =>
         GHC.OverloadedLabels.IsLabel "local" (mapper
                                               -> ServiceAddress -> ServiceAddress) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  LocalServiceAddress a -> (\ (a) -> LocalServiceAddress a) (fn a)
                  a -> a
instance a ~ Maybe NetworkAddress =>
         GHC.OverloadedLabels.IsLabel "network" (ServiceAddress -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             NetworkServiceAddress a -> Just (a)
             _ -> Nothing
instance a ~ Maybe FilePath =>
         GHC.OverloadedLabels.IsLabel "local" (ServiceAddress -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             LocalServiceAddress a -> Just (a)
             _ -> Nothing
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable ServiceAddress
deriving instance template-haskell-2.15.0.0:Language.Haskell.TH.Syntax.Lift ServiceAddress
instance GHC.Records.HasField "network" ServiceAddress (Maybe NetworkAddress) where
  GHC.Records.getField (NetworkServiceAddress a) = Just (a)
  GHC.Records.getField _ = Nothing
instance GHC.Records.HasField "local" ServiceAddress (Maybe FilePath) where
  GHC.Records.getField (LocalServiceAddress a) = Just (a)
  GHC.Records.getField _ = Nothing
deriving instance Show NetworkAddress
deriving instance Eq NetworkAddress
deriving instance Ord NetworkAddress
deriving instance GHC.Generics.Generic NetworkAddress
deriving instance Data.Data.Data NetworkAddress
deriving instance base-4.13.0.0:Data.Typeable.Internal.Typeable NetworkAddress
instance mapper ~ (TransportProtocol -> TransportProtocol) =>
         GHC.OverloadedLabels.IsLabel "protocol" (mapper
                                                  -> NetworkAddress -> NetworkAddress) where
  GHC.OverloadedLabels.fromLabel
    = \ fn (NetworkAddress a b c) -> ((NetworkAddress (fn a)) b) c
instance mapper ~ (Host -> Host) =>
         GHC.OverloadedLabels.IsLabel "host" (mapper
                                              -> NetworkAddress -> NetworkAddress) where
  GHC.OverloadedLabels.fromLabel
    = \ fn (NetworkAddress a b c) -> ((NetworkAddress a) (fn b)) c
instance mapper ~ (Word16 -> Word16) =>
         GHC.OverloadedLabels.IsLabel "port" (mapper
                                              -> NetworkAddress -> NetworkAddress) where
  GHC.OverloadedLabels.fromLabel
    = \ fn (NetworkAddress a b c) -> ((NetworkAddress a) b) (fn c)
instance a ~ TransportProtocol =>
         GHC.OverloadedLabels.IsLabel "protocol" (NetworkAddress -> a) where
  GHC.OverloadedLabels.fromLabel = \ (NetworkAddress a _ _) -> a
instance a ~ Host =>
         GHC.OverloadedLabels.IsLabel "host" (NetworkAddress -> a) where
  GHC.OverloadedLabels.fromLabel = \ (NetworkAddress _ b _) -> b
instance a ~ Word16 =>
         GHC.OverloadedLabels.IsLabel "port" (NetworkAddress -> a) where
  GHC.OverloadedLabels.fromLabel = \ (NetworkAddress _ _ c) -> c
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable NetworkAddress
deriving instance template-haskell-2.15.0.0:Language.Haskell.TH.Syntax.Lift NetworkAddress
instance GHC.Records.HasField "protocol" NetworkAddress TransportProtocol where
  GHC.Records.getField (NetworkAddress a _ _) = a
instance GHC.Records.HasField "host" NetworkAddress Host where
  GHC.Records.getField (NetworkAddress _ a _) = a
instance GHC.Records.HasField "port" NetworkAddress Word16 where
  GHC.Records.getField (NetworkAddress _ _ a) = a
deriving instance Enum TransportProtocol
deriving instance Bounded TransportProtocol
deriving instance Show TransportProtocol
deriving instance Eq TransportProtocol
deriving instance Ord TransportProtocol
deriving instance GHC.Generics.Generic TransportProtocol
deriving instance Data.Data.Data TransportProtocol
deriving instance base-4.13.0.0:Data.Typeable.Internal.Typeable TransportProtocol
instance GHC.OverloadedLabels.IsLabel "tcp" TransportProtocol where
  GHC.OverloadedLabels.fromLabel = TcpTransportProtocol
instance GHC.OverloadedLabels.IsLabel "udp" TransportProtocol where
  GHC.OverloadedLabels.fromLabel = UdpTransportProtocol
instance a ~ Bool =>
         GHC.OverloadedLabels.IsLabel "tcp" (TransportProtocol -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             TcpTransportProtocol -> True
             _ -> False
instance a ~ Bool =>
         GHC.OverloadedLabels.IsLabel "udp" (TransportProtocol -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             UdpTransportProtocol -> True
             _ -> False
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable TransportProtocol
deriving instance template-haskell-2.15.0.0:Language.Haskell.TH.Syntax.Lift TransportProtocol
instance GHC.Records.HasField "tcp" TransportProtocol Bool where
  GHC.Records.getField TcpTransportProtocol = True
  GHC.Records.getField _ = False
instance GHC.Records.HasField "udp" TransportProtocol Bool where
  GHC.Records.getField UdpTransportProtocol = True
  GHC.Records.getField _ = False
deriving instance Show Host
deriving instance Eq Host
deriving instance Ord Host
deriving instance GHC.Generics.Generic Host
deriving instance Data.Data.Data Host
deriving instance base-4.13.0.0:Data.Typeable.Internal.Typeable Host
instance a ~ Ip =>
         GHC.OverloadedLabels.IsLabel "ip" (a -> Host) where
  GHC.OverloadedLabels.fromLabel = IpHost
instance a ~ Text =>
         GHC.OverloadedLabels.IsLabel "name" (a -> Host) where
  GHC.OverloadedLabels.fromLabel = NameHost
instance mapper ~ (Ip -> Ip) =>
         GHC.OverloadedLabels.IsLabel "ip" (mapper -> Host -> Host) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  IpHost a -> (\ (a) -> IpHost a) (fn a)
                  a -> a
instance mapper ~ (Text -> Text) =>
         GHC.OverloadedLabels.IsLabel "name" (mapper -> Host -> Host) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  NameHost a -> (\ (a) -> NameHost a) (fn a)
                  a -> a
instance a ~ Maybe Ip =>
         GHC.OverloadedLabels.IsLabel "ip" (Host -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             IpHost a -> Just (a)
             _ -> Nothing
instance a ~ Maybe Text =>
         GHC.OverloadedLabels.IsLabel "name" (Host -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             NameHost a -> Just (a)
             _ -> Nothing
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable Host
deriving instance template-haskell-2.15.0.0:Language.Haskell.TH.Syntax.Lift Host
instance GHC.Records.HasField "ip" Host (Maybe Ip) where
  GHC.Records.getField (IpHost a) = Just (a)
  GHC.Records.getField _ = Nothing
instance GHC.Records.HasField "name" Host (Maybe Text) where
  GHC.Records.getField (NameHost a) = Just (a)
  GHC.Records.getField _ = Nothing
deriving instance Show Ip
deriving instance Eq Ip
deriving instance Ord Ip
deriving instance GHC.Generics.Generic Ip
deriving instance Data.Data.Data Ip
deriving instance base-4.13.0.0:Data.Typeable.Internal.Typeable Ip
instance a ~ Word32 =>
         GHC.OverloadedLabels.IsLabel "v4" (a -> Ip) where
  GHC.OverloadedLabels.fromLabel = V4Ip
instance a ~ Word128 =>
         GHC.OverloadedLabels.IsLabel "v6" (a -> Ip) where
  GHC.OverloadedLabels.fromLabel = V6Ip
instance mapper ~ (Word32 -> Word32) =>
         GHC.OverloadedLabels.IsLabel "v4" (mapper -> Ip -> Ip) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  V4Ip a -> (\ (a) -> V4Ip a) (fn a)
                  a -> a
instance mapper ~ (Word128 -> Word128) =>
         GHC.OverloadedLabels.IsLabel "v6" (mapper -> Ip -> Ip) where
  GHC.OverloadedLabels.fromLabel
    = \ fn
        -> \ a
             -> case a of
                  V6Ip a -> (\ (a) -> V6Ip a) (fn a)
                  a -> a
instance a ~ Maybe Word32 =>
         GHC.OverloadedLabels.IsLabel "v4" (Ip -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             V4Ip a -> Just (a)
             _ -> Nothing
instance a ~ Maybe Word128 =>
         GHC.OverloadedLabels.IsLabel "v6" (Ip -> a) where
  GHC.OverloadedLabels.fromLabel
    = \ a
        -> case a of
             V6Ip a -> Just (a)
             _ -> Nothing
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable Ip
deriving instance template-haskell-2.15.0.0:Language.Haskell.TH.Syntax.Lift Ip
instance GHC.Records.HasField "v4" Ip (Maybe Word32) where
  GHC.Records.getField (V4Ip a) = Just (a)
  GHC.Records.getField _ = Nothing
instance GHC.Records.HasField "v6" Ip (Maybe Word128) where
  GHC.Records.getField (V6Ip a) = Just (a)
  GHC.Records.getField _ = Nothing
deriving instance Show Word128
deriving instance Eq Word128
deriving instance Ord Word128
deriving instance GHC.Generics.Generic Word128
deriving instance Data.Data.Data Word128
deriving instance base-4.13.0.0:Data.Typeable.Internal.Typeable Word128
instance mapper ~ (Word64 -> Word64) =>
         GHC.OverloadedLabels.IsLabel "part1" (mapper
                                               -> Word128 -> Word128) where
  GHC.OverloadedLabels.fromLabel
    = \ fn (Word128 a b) -> (Word128 (fn a)) b
instance mapper ~ (Word64 -> Word64) =>
         GHC.OverloadedLabels.IsLabel "part2" (mapper
                                               -> Word128 -> Word128) where
  GHC.OverloadedLabels.fromLabel
    = \ fn (Word128 a b) -> (Word128 a) (fn b)
instance a ~ Word64 =>
         GHC.OverloadedLabels.IsLabel "part1" (Word128 -> a) where
  GHC.OverloadedLabels.fromLabel = \ (Word128 a _) -> a
instance a ~ Word64 =>
         GHC.OverloadedLabels.IsLabel "part2" (Word128 -> a) where
  GHC.OverloadedLabels.fromLabel = \ (Word128 _ b) -> b
instance hashable-1.3.0.0:Data.Hashable.Class.Hashable Word128
deriving instance template-haskell-2.15.0.0:Language.Haskell.TH.Syntax.Lift Word128
instance GHC.Records.HasField "part1" Word128 Word64 where
  GHC.Records.getField (Word128 a _) = a
instance GHC.Records.HasField "part2" Word128 Word64 where
  GHC.Records.getField (Word128 _ a) = a
```
</details>


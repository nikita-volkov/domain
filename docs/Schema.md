# Schema syntax reference

Schema definition is a YAML document listing declarations of your domain types.
There is 5 types of type declarations: [Product](#product), [Sum](#sum), [Enum](#enum), [Wrapper](#wrapper), [Alias](#alias).

## Product

Defines a compounded type comprised of other types using [Product type composition](https://en.wikipedia.org/wiki/Product_type), associating a unique textual label with each member type. You may know it as "record".

Here's an example of a product type declaration in schema:

```yaml
NetworkAddress:
  product:
    protocol: TransportProtocol
    host: Host
    port: Word16
```

Depending on the settings you provide one of the following Haskell type declarations can be generated from it:

```haskell
data NetworkAddress =
  NetworkAddress !TransportProtocol !Host !Word16
```

```haskell
data NetworkAddress =
  NetworkAddress {
    networkAddressProtocol :: !TransportProtocol,
    networkAddressHost :: !Host,
    networkAddressPort :: !Word16
  }
```

```haskell
data NetworkAddress =
  NetworkAddress {
    _protocol :: !TransportProtocol,
    _host :: !Host,
    _port :: !Word16
  }
```

```haskell
data NetworkAddress =
  NetworkAddress {
    protocol :: !TransportProtocol,
    host :: !Host,
    port :: !Word16
  }
```

### Accessing fields

Regardless of the way you choose to generate the data declaration,
neat mechanisms of accessing members can be provided
using the automatically generated `IsLabel` instances or
instances of `LabelOptic` (using the "domain-optics" package).

E.g., here's how you can be accessing the members of the example data-type:

```haskell
getNetworkAddressPort :: NetworkAddress -> Word16
getNetworkAddressPort = #port
```

```haskell
mapNetworkAddressHost :: (Host -> Host) -> NetworkAddress -> NetworkAddress
mapNetworkAddressHost = over #host -- Using "domain-optics" and "optics"
```

For more details on instance generation refer to the [Instance Derivation](#instance-derivation) section.

## Sum

Defines a compounded type comprised of other types using [Sum type composition](https://en.wikipedia.org/wiki/Tagged_union), associating a unique textual label with each member type. You may know it as tagged union, variant.

Here's an example of a schema declaration of a sum type:

```yaml
Host:
  sum:
    ip: Ip
    name: Text
```

The following Haskell code will be generated from it:

```haskell
data Host =
  IpHost !Ip |
  NameHost !Text
```

As you can see the constructor names are intentionally made to be unambiguous.
You may already be thinking "But the code is gonna get so verbose".
It's not. Thanks to the automatically generatable `IsLabel` and `LabelOptic` instances.

E.g., here's how you'll be able to access the variants of the data-type:

```haskell
getHostIp :: Host -> Maybe Ip
getHostIp = #ip
```

```haskell
ipHost :: Ip -> Host
ipHost = #ip
```

```haskell
mapHostIp :: (Ip -> Ip) -> Host -> Host
mapHostIp = over #ip -- Using "domain-optics" and "optics"
```

### Multi-member sums

It is possible to provide multiple members of a sum variant using a comma-separated list or YAML sequence. You can provide zero members as well. E.g.,

```yaml
Error:
  sum:
    channel:
      - ChannelId
      - Text
    connectionLost:
```

This will generate the following declaration:

```haskell
data Error =
  ChannelError !ChannelId !Text |
  ConnectionLostError
```

Depending on the number of variant members the generated accessors will point to tuples or booleans:

```haskell
getErrorChannel :: Error -> Maybe (ChannelId, Text)
getErrorChannel = #channel

getErrorConnectionLost :: Error -> Bool
getErrorConnectionLost = #connectionLost
```

## Enum

Type which can have one value out of a specific set of options.

Here's an example of a schema declaration of an enum type:

```yaml
TransportProtocol:
  enum:
    - tcp
    - udp
```

This will generate the following Haskell data type:

```haskell
data TransportProtocol =
  TcpTransportProtocol |
  UdpTransportProtocol
```

`IsLabel` helpers:

```haskell
tcpTransportProtocol :: TransportProtocol
tcpTransportProtocol = #tcp

getTransportProtocolTcp :: TransportProtocol -> Bool
getTransportProtocolTcp = #tcp
```

## Wrapper

Type which provides a new identity to the existing type. Maps to Haskell `newtype` declaration.

Here's an example of a schema declaration:

```yaml
HostName:
  wrapper: Text
```

It'll generate one of the following Haskell type declaration depending on your settings:

```haskell
newtype HostName = HostName Text
```

```haskell
newtype HostName = HostName { hostNameValue :: Text }
```

```haskell
newtype HostName = HostName { _value :: Text }
```

```haskell
newtype HostName = HostName { value :: Text }
```

The accessors will be the same as for [Product](#product).

## Alias

Alias to an existing type.

Schema declaration example:

```yaml
Port:
  alias: Word16
```

On the Haskell end it'll generate the following:

```haskell
type Port = Word16
```

# Instance derivation

The library provides a mechanism to automatically derive all kinds of typeclass instances.

## Extending

The instance derivation mechanism is extensible.
You can define your own derivers.

# Special cases

## List data-type

Since square brackets get interpreted as YAML array, you have to explicitly state that the value is a YAML string. To achieve that prefix the value with the vertical line (`|`) character.



# Niceties

## Reserved names are available

You can use those previously banned field names like "data", "type", "class".


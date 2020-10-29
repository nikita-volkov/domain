module Domain.Docs
(
  -- * Schema Syntax Reference #schema-syntax-reference#
  {-|
  Schema definition is a YAML document listing declarations of your domain
  types. The listing is represented as a dictionary from type names to their
  definitions. There is 3 types of definitions: <#product Product>,
  <#sum Sum>, <#enum Enum>.
  -}
  -- ** Product #product#
  {-|
  Defines a type comprised of other types using
  <https://en.wikipedia.org/wiki/Product_type Product type composition>,
  associating a unique textual label with each member. You may know it as
  \"record\".

  Here\'s an example of a product type declaration in schema:

  > NetworkAddress:
  >   product:
  >     protocol: TransportProtocol
  >     host: Host
  >     port: Word16

  Depending on the settings you provide one of the following Haskell type
  declarations can be generated from it:

  > data NetworkAddress =
  >   NetworkAddress !TransportProtocol !Host !Word16

  > data NetworkAddress =
  >   NetworkAddress {
  >     networkAddressProtocol :: !TransportProtocol,
  >     networkAddressHost :: !Host,
  >     networkAddressPort :: !Word16
  >   }

  > data NetworkAddress =
  >   NetworkAddress {
  >     _protocol :: !TransportProtocol,
  >     _host :: !Host,
  >     _port :: !Word16
  >   }

  > data NetworkAddress =
  >   NetworkAddress {
  >     protocol :: !TransportProtocol,
  >     host :: !Host,
  >     port :: !Word16
  >   }
  -}
  -- *** Accessing fields #accessing-product-fields#
  {-|

  Regardless of the way you choose to generate the data declaration, neat
  mechanisms of accessing members can be provided using the automatically
  generated @IsLabel@ instances or instances of @LabelOptic@ (using the
  \"domain-optics\" package).

  E.g., here\'s how you can be accessing the members of the example
  data-type:

  > getNetworkAddressPort :: NetworkAddress -> Word16
  > getNetworkAddressPort = #port

  > mapNetworkAddressHost :: (Host -> Host) -> NetworkAddress -> NetworkAddress
  > mapNetworkAddressHost = over #host -- Using "domain-optics" and "optics"

  -}
  -- ** Sum #sum#
  {-|

  Defines a type comprised of other types using
  <https://en.wikipedia.org/wiki/Tagged_union Sum type composition>,
  associating a unique textual label with each member. You may know it as
  tagged union or variant.

  Here\'s an example of a schema declaration of a sum type:

  > Host:
  >   sum:
  >     ip: Ip
  >     name: Text

  The following Haskell code will be generated from it:

  > data Host =
  >   IpHost !Ip |
  >   NameHost !Text

  As you can see the constructor names are intentionally made to be
  unambiguous. You may already be thinking \"But the code is gonna get so
  verbose\". It\'s not. Thanks to the automatically generatable @IsLabel@
  and @LabelOptic@ instances.

  E.g., here\'s how you\'ll be able to access the variants of the
  data-type:

  > getHostIp :: Host -> Maybe Ip
  > getHostIp = #ip

  > ipHost :: Ip -> Host
  > ipHost = #ip

  > mapHostIp :: (Ip -> Ip) -> Host -> Host
  > mapHostIp = over #ip -- Using "domain-optics" and "optics"
  
  -}
  -- *** Multi-member sums #multi-member-sums#
  {-|

  It is possible to provide multiple members of a sum variant using a
  comma-separated list or YAML sequence. You can provide zero members as
  well. E.g.,

  > Error:
  >   sum:
  >     channel:
  >       - ChannelId
  >       - Text
  >     connectionLost:

  This will generate the following declaration:

  > data Error =
  >   ChannelError !ChannelId !Text |
  >   ConnectionLostError

  Depending on the number of variant members the generated accessors will
  point to tuples or booleans:

  > getErrorChannel :: Error -> Maybe (ChannelId, Text)
  > getErrorChannel = #channel
  >
  > getErrorConnectionLost :: Error -> Bool
  > getErrorConnectionLost = #connectionLost
  -}
  -- ** Enum #enum#
  {-|
  Type which can have one value out of a specific set of options.

  Here\'s an example of a schema declaration of an enum type:

  > TransportProtocol:
  >   enum:
  >     - tcp
  >     - udp

  This will generate the following Haskell data type:

  > data TransportProtocol =
  >   TcpTransportProtocol |
  >   UdpTransportProtocol

  The following 'IsLabel' helpers will be available for it:

  > tcpTransportProtocol :: TransportProtocol
  > tcpTransportProtocol = #tcp
  >
  > getTransportProtocolTcp :: TransportProtocol -> Bool
  > getTransportProtocolTcp = #tcp

  -}
  -- ** Notes #notes#
  -- *** List Data-type #list-data-type#
  {-|
  Since square brackets get interpreted in YAML as array literal, you have to
  explicitly state that the value is a string literal. To achieve that prefix
  the value with the vertical line character (@|@). E.g.,

  > Artist:
  >   product:
  >     name: Text
  >     genres: | [Genre]
  -}
  -- *** Reserved Names
  {-|
  You can use the otherwise banned field names like \"data\", \"type\",
  \"class\".
  -}
)
where

import Domain.Prelude hiding (liftEither, readFile, lift)
import Domain

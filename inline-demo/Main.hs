module Main
where

import Prelude
import Domain
import qualified Domain.Deriver as Deriver


main =
  return ()

declare (Just (False, True)) Deriver.all [schema|

  sums:
    ProcessAddress:
      internet: InternetAddress
      local: DomainSocketPath
    Host:
      ip: Ip
      name: Text
    Ip:
      v4: Word32
      v6: Word128

  products:
    DomainSocketPath:
      path: FilePath
    InternetAddress:
      protocol: TransportProtocol
      host: Host
      port: Word16
    Word128:
      part1: Word64
      part2: Word64

  enums:
    TransportProtocol:
      - tcp
      - udp

  |]

{-|
Shows how you can construct sum-types and enum-types using labels.

We need to specify the type for the #name constructor member,
because otherwise the compiler interprets it as String.
-}
processAddress :: ProcessAddress
processAddress =
  #internet (InternetAddress #tcp (#name ("local" :: Text)) 1234)

{-|
Shows how you can map. Unfortunately that requires a lot of manual typing.
-}
updatedProcessAddress :: ProcessAddress
updatedProcessAddress =
  #internet (#port (succ @Word16) :: InternetAddress -> InternetAddress) processAddress

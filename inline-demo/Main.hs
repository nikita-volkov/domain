{-# LANGUAGE
  QuasiQuotes, TemplateHaskell,
  StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, DeriveLift,
  FlexibleInstances, MultiParamTypeClasses,
  DataKinds, TypeFamilies,
  OverloadedStrings, OverloadedLabels, TypeApplications
  #-}
module Main where

import Data.Text (Text)
import Data.Word (Word16, Word32, Word64)
import Domain


main =
  return ()

declare (Just (False, True)) deriveAll [schema|

  ProcessAddress:
    sum:
      internet: InternetAddress
      local: DomainSocketPath

  InternetAddress:
    product:
      protocol: TransportProtocol
      host: Host
      port: Word16

  DomainSocketPath:
    product:
      path: FilePath

  TransportProtocol:
    enum:
      - tcp
      - udp

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

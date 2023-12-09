{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-equality-requires-operators #-}

module Main where

import Data.Text (Text)
import Data.Word (Word16, Word32, Word64)
import Domain

main :: IO ()
main =
  return ()

declare
  (Just (False, True))
  stdDeriver
  [schema|

  ServiceAddress:
    sum:
      network: NetworkAddress
      local: FilePath

  NetworkAddress:
    product:
      protocol: TransportProtocol
      host: Host
      port: Word16

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

-- |
-- Shows how you can construct sum-types and enum-types using labels.
--
-- We need to specify the type for the #name constructor member,
-- because otherwise the compiler interprets it as String.
serviceAddress :: ServiceAddress
serviceAddress =
  #network (NetworkAddress #tcp (#name ("local" :: Text)) 1234)

-- |
-- Shows how you can map. Unfortunately that requires a lot of manual typing.
updatedServiceAddress :: ServiceAddress
updatedServiceAddress =
  #network (#port (succ @Word16) :: NetworkAddress -> NetworkAddress) serviceAddress

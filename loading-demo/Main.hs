{-# LANGUAGE DuplicateRecordFields #-}
module Main
where

import Prelude
import Domain
import qualified Domain.Deriver as Deriver


main =
  return ()

load (Just (True, False)) Deriver.hasField "samples/1.yaml"

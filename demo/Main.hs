module Main
where

import Prelude
import Domain
import qualified Domain.Deriver as Deriver


main =
  return ()

load "samples/1.yaml" Deriver.base

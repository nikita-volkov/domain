module Main
where

import Prelude
import Domain
import Data.Hashable.Time ()
import qualified Domain.Deriver as Deriver


main =
  return ()

load "samples/1.yaml" Deriver.all

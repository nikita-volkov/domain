module Main
where

import Prelude
import Facade
import Data.Hashable.Time ()
import qualified Facade.Deriver as Deriver


main =
  return ()

load "samples/1.yaml" Deriver.all

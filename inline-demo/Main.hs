module Main
where

import Prelude
import Domain
import qualified Domain.Deriver as Deriver


main =
  return ()

declare (Just (True, True)) Deriver.base [schema|
  sums:
    A:
      a:
        - Int
        - Bool
      b: Char, Double
  |]

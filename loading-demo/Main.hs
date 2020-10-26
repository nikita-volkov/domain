{-# LANGUAGE DuplicateRecordFields #-}
module Main
where

import Prelude
import Domain
import qualified Domain.Deriver as Deriver


main =
  return ()

declare
  (Just (True, False))
  (mconcat [
    Deriver.base,
    Deriver.isLabel,
    Deriver.hashable,
    Deriver.hasField
    ])
  =<< loadSchema "samples/1.yaml"

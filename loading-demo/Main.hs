{-# LANGUAGE
  QuasiQuotes, TemplateHaskell,
  StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, DeriveLift,
  FlexibleInstances, MultiParamTypeClasses,
  DataKinds, TypeFamilies
  #-}
module Main where

import Data.Text (Text)
import Data.Word (Word16, Word32, Word64)
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

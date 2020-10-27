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
  (Just (False, True))
  (Deriver.all)
  =<< loadSchema "samples/1.yaml"

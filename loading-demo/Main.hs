{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-equality-requires-operators #-}

module Main where

import Data.Text (Text)
import Data.Word (Word16, Word32, Word64)
import Domain

main :: IO ()
main =
  return ()

declare (Just (True, False)) stdDeriver
  =<< loadSchema "samples/1.yaml"

module Main where

import Prelude hiding (assert)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Domain.YamlUnscrambler.CategoryCentricDoc as YamlUnscrambler
import qualified Domain.Model as Model
import qualified Test.QuickCheck as QuickCheck
import qualified Control.Foldl as Fold
import qualified NeatInterpolation as NeatInterpolation
import qualified Data.Text as Text


main =
  defaultMain $ 
  testGroup "All tests" [
    testCase "Should fail when wrong member of sum-type is supplied" $ let
      res =
        YamlUnscrambler.parseText YamlUnscrambler.doc [NeatInterpolation.text|
            sums:
              A:
                a:
                  c: Int
                b: Char, Double
          |]
      in case res of
        Right res ->
          assertFailure (show res)
        Left err ->
          return ()
    ]

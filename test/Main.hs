module Main where

import Prelude hiding (assert)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Domain
import qualified DomainCore.Model as Model
import qualified Test.QuickCheck as QuickCheck
import qualified Data.Text as Text
import qualified Util.TH as TH


main =
  defaultMain $ 
  testGroup "All tests" [
    testCase "Should fail when wrong member of sum-type is supplied" $ let
      res :: Maybe [Model.TypeDec]
      res =
        [TH.maybeDecsQQ|
          A:
            sum:
              a:
                c: Int
              b: Char, Double
          |]
      in case res of
        Just res ->
          assertFailure (show res)
        Nothing ->
          return ()
    ]

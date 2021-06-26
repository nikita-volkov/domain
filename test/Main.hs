module Main where

import Prelude hiding (assert)
import Language.Haskell.TH.Instances ()
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
import qualified Util.TH.LeafTypes as THLeafTypes
import qualified Language.Haskell.TH.Syntax as TH


main :: IO ()
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
    ,
    testCase "Nested structures shouldn't contain any unit-tuple types" $ let
      decs :: [TH.Dec]
      decs =
        $(TH.lift
            =<< Domain.declare Nothing mempty [Domain.schema|
                  A:
                    product:
                      a: Maybe (Maybe Int)
                  |])
      leafTypes =
        foldMap THLeafTypes.fromDec decs
      in case elemIndex (TH.TupleT 1) leafTypes of
        Just _ ->
          assertFailure (show decs)
        Nothing ->
          return ()
    ]

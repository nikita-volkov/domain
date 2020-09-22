{-|
Abstraction which allows to define automatic derivation of any class.
-}
module Facade.Deriver
where

import Facade.Prelude
import Facade.Model
import qualified Language.Haskell.TH as TH
import qualified Facade.Deriver.TH as TH
import qualified Data.Text as Text
import qualified Data.Char as Char


newtype Deriver =
  Deriver (Dec -> [TH.Dec])
  deriving (Semigroup, Monoid)

enum =
  Deriver TH.enumInstanceDecs

bounded =
  Deriver TH.boundedInstanceDecs

show =
  Deriver TH.showInstanceDecs

eq =
  Deriver TH.eqInstanceDecs

ord =
  Deriver TH.ordInstanceDecs

generic =
  Deriver TH.genericInstanceDecs

data_ =
  Deriver TH.dataInstanceDecs

typeable =
  Deriver TH.typeableInstanceDecs

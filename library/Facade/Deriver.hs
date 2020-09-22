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
  Deriver (Dec -> TH.Q [TH.Dec])
  deriving (Semigroup, Monoid)
    via ((->) Dec (Ap TH.Q [TH.Dec]))

effectless f =
  Deriver (pure . f)

-- *
-------------------------

enum =
  effectless TH.enumInstanceDecs

bounded =
  effectless TH.boundedInstanceDecs

show =
  effectless TH.showInstanceDecs

eq =
  effectless TH.eqInstanceDecs

ord =
  effectless TH.ordInstanceDecs

generic =
  effectless TH.genericInstanceDecs

data_ =
  effectless TH.dataInstanceDecs

typeable =
  effectless TH.typeableInstanceDecs

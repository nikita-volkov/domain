{-|
Abstraction which allows to define automatic derivation of any class.
-}
module Facade.Deriver
(
  -- * Deriver definitions
  Deriver(..),
  std,
  -- ** Specific
  enum,
  bounded,
  show,
  eq,
  ord,
  generic,
  data_,
  typeable,
  -- * Declarations model
  module Facade.Model,
)
where

import Facade.Prelude hiding (show, ord)
import Facade.Model
import qualified Language.Haskell.TH as TH
import qualified Facade.Deriver.TH as TH


newtype Deriver =
  Deriver (Dec -> TH.Q [TH.Dec])
  deriving (Semigroup, Monoid)
    via ((->) Dec (Ap TH.Q [TH.Dec]))

effectless f =
  Deriver (pure . f)

-- *
-------------------------

{-|
Combination of all standard derivers exported by this module.
-}
std =
  mconcat [
    enum,
    bounded,
    show,
    eq,
    ord,
    generic,
    data_,
    typeable
    ]

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

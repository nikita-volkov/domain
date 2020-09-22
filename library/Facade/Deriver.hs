{-|
Abstraction which allows to define automatic derivation of any class.
-}
module Facade.Deriver
(
  -- * Deriver definitions
  Deriver(..),
  std,
  -- ** Standard specific
  enum,
  bounded,
  show,
  eq,
  ord,
  generic,
  data_,
  typeable,
  -- ** IsLabel
  constructorIsLabel,
  -- * Spec model
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


-- * Std
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


-- * IsLabel
-------------------------

{-|
Generates instances of 'IsLabel' for enums and sums,
providing mappings from labels to constructors.

E.g., for the following spec:

>sums:
>  ApiError:
>    unauthorized:
>    rejected: Maybe Text

It'll generate the following instances:

>instance IsLabel "unauthorized" ApiError where
>  fromLabel = UnauthorizedApiError
>
>instance IsLabel "rejected" (Maybe Text -> ApiError) where
>  fromLabel = RejectedApiError

Allowing you to construct the value by simply addressing the label:

>unauthorizedApiError :: ApiError
>unauthorizedApiError = #unauthorized
>
>rejectedApiError :: Maybe Text -> ApiError
>rejectedApiError reason = #rejected reason

To make use of that ensure to have the @OverloadedLabels@ compiler extension enabled.
-}
constructorIsLabel =
  effectless TH.constructorIsLabelInstanceDecs

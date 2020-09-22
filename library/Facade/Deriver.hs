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
  isLabel,
  -- *** Specific
  constructorIsLabel,
  accessorIsLabel,
  -- * Spec model
  module Facade.Model,
)
where

import Facade.Prelude hiding (show, ord)
import Facade.Model
import qualified Language.Haskell.TH as TH
import qualified Facade.Deriver.TH as TH


{-|
Abstraction which allows to define automatic derivation of any class.

It is implemented as a function from the type declaration in this package\'s own AST
to a list of Template Haskell declarations in its quotation monad.

Its Monoid instance allows you to combine derivers.
-}
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
Generates instances of 'IsLabel' for wrappers, enums and sums,
providing mappings from labels to constructors.

=== __Example__

For the following spec:

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

{-|
Generates instances of 'IsLabel' for wrappers, enums, sums and products,
providing mappings from labels to component accessors.

=== __Product example__

The following spec:

>products:
>  Config:
>    host: Text
>    port: Int

Will generate the following instances:

>instance IsLabel "host" (Config -> Text) where
>  fromLabel = configHost
>
>instance IsLabel "port" (Config -> Int) where
>  fromLabel = configPort

Which you can use to access individual fields as follows:

>getConfigHost :: Config -> Text
>getConfigHost = #host

To make use of that ensure to have the @OverloadedLabels@ compiler extension enabled.
-}
accessorIsLabel =
  effectless TH.accessorIsLabelInstanceDecs

{-|
Combination of 'constructorIsLabel' and 'accessorIsLabel'.
-}
isLabel =
  constructorIsLabel <>
  accessorIsLabel

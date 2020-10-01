{-|
Toolkit for construction and definition of instance derivers for Domain specs.
-}
module Domain.Deriver
(
  -- * Deriver definitions
  Deriver(..),
  all,
  -- ** Base
  base,
  -- *** Specific
  enum,
  bounded,
  show,
  eq,
  ord,
  generic,
  data_,
  typeable,
  -- ** Common
  hashable,
  lift,
  -- ** HasField
  hasField,
  -- ** IsLabel
  -- |
  -- Custom instances of 'IsLabel'.
  isLabel,
  -- *** Specific
  constructorIsLabel,
  accessorIsLabel,
  -- * Schema model
  module Domain.Model,
)
where

import Domain.Prelude hiding (show, ord, all, lift)
import Domain.Model
import qualified Language.Haskell.TH as TH
import qualified Domain.Deriver.TH as TH
import qualified Domain.InstanceDecs as InstanceDecs


{-|
Abstraction which allows to define automatic derivation of any class.

It is implemented as a function from the type declaration in this package\'s own AST
to a list of Template Haskell declarations in its quotation monad.

Its Monoid instance allows you to combine derivers.
-}
newtype Deriver =
  Deriver (TypeDec -> TH.Q [TH.Dec])
  deriving (Semigroup, Monoid)
    via ((->) TypeDec (Ap TH.Q [TH.Dec]))

effectless f =
  Deriver (pure . f)

{-|
Combination of all derivers exported by this module.
-}
all =
  mconcat [
    base,
    isLabel,
    hashable,
    lift,
    hasField
    ]


-- * Base
-------------------------

{-|
Combination of all derivers for classes from the \"base\" package.
-}
base =
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

{-|
Derives 'Enum' for types from the \"enum\" section of spec.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
enum =
  effectless TH.enumInstanceDecs

{-|
Derives 'Bounded' for types from the \"enum\" section of spec.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
bounded =
  effectless TH.boundedInstanceDecs

{-|
Derives 'Show'.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
show =
  effectless TH.showInstanceDecs

{-|
Derives 'Eq'.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
eq =
  effectless TH.eqInstanceDecs

{-|
Derives 'Ord'.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
ord =
  effectless TH.ordInstanceDecs

{-|
Derives 'Generic'.

Requires to have the @StandaloneDeriving@ and @DeriveGeneric@ compiler extensions enabled.
-}
generic =
  effectless TH.genericInstanceDecs

{-|
Derives 'Data'.

Requires to have the @StandaloneDeriving@ and @DeriveDataTypeable@ compiler extensions enabled.
-}
data_ =
  effectless TH.dataInstanceDecs

{-|
Derives 'Typeable'.

Requires to have the @StandaloneDeriving@ and @DeriveDataTypeable@ compiler extensions enabled.
-}
typeable =
  effectless TH.typeableInstanceDecs

{-|
Generates 'Generic'-based instances of 'Hashable'.
-}
hashable =
  effectless TH.hashableInstanceDecs

{-|
Derives 'Lift'.

Requires to have the @StandaloneDeriving@ and @DeriveLift@ compiler extensions enabled.
-}
lift =
  effectless TH.liftInstanceDecs


-- * HasField
-------------------------

{-|
Derives 'HasField' with unprefixed field names.

For each field of product generates instances mapping to their values.

For each constructor of a sum maps to a 'Maybe' tuple of members of that constructor.

For each variant of an enum maps to 'Bool' signaling whether the value equals to it.

For wrapper maps the symbol \"value\" to the contents of the wrapper.

_Please notice that if you choose to generate unprefixed record field accessors,
it will conflict with this deriver, since it\'s gonna generate duplicate instances._
-}
hasField =
  effectless InstanceDecs.hasField


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

{-|
Toolkit for construction and definition of instance derivers for Domain specs.
-}
module Domain.Deriver
(
  -- * Deriver definitions
  Deriver,
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
  module DomainCore.Model,
)
where

import Domain.Prelude hiding (show, ord, all, lift)
import DomainCore.Model
import DomainCore.Deriver
import qualified Language.Haskell.TH as TH (Q, Dec)
import qualified Domain.TH.InstanceDecs as InstanceDecs


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
  effectless InstanceDecs.enum

{-|
Derives 'Bounded' for types from the \"enum\" section of spec.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
bounded =
  effectless InstanceDecs.bounded

{-|
Derives 'Show'.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
show =
  effectless InstanceDecs.show

{-|
Derives 'Eq'.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
eq =
  effectless InstanceDecs.eq

{-|
Derives 'Ord'.

Requires to have the @StandaloneDeriving@ compiler extension enabled.
-}
ord =
  effectless InstanceDecs.ord

{-|
Derives 'Generic'.

Requires to have the @StandaloneDeriving@ and @DeriveGeneric@ compiler extensions enabled.
-}
generic =
  effectless InstanceDecs.generic

{-|
Derives 'Data'.

Requires to have the @StandaloneDeriving@ and @DeriveDataTypeable@ compiler extensions enabled.
-}
data_ =
  effectless InstanceDecs.data_

{-|
Derives 'Typeable'.

Requires to have the @StandaloneDeriving@ and @DeriveDataTypeable@ compiler extensions enabled.
-}
typeable =
  effectless InstanceDecs.typeable

{-|
Generates 'Generic'-based instances of 'Hashable'.
-}
hashable =
  effectless InstanceDecs.hashable

{-|
Derives 'Lift'.

Requires to have the @StandaloneDeriving@ and @DeriveLift@ compiler extensions enabled.
-}
lift =
  effectless InstanceDecs.lift


-- * HasField
-------------------------

{-|
Derives 'HasField' with unprefixed field names.

For each field of product generates instances mapping to their values.

For each constructor of a sum maps to a 'Maybe' tuple of members of that constructor.

For each variant of an enum maps to 'Bool' signaling whether the value equals to it.

For wrapper maps the symbol \"value\" to the contents of the wrapper.

/Please notice that if you choose to generate unprefixed record field accessors, it will conflict with this deriver, since it\'s gonna generate duplicate instances./
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
  effectless InstanceDecs.constructorIsLabel

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

>instance a ~ Text => IsLabel "host" (Config -> a) where
>  fromLabel = \ (Config a _) -> a
>instance a ~ Word16 => IsLabel "port" (Config -> a) where
>  fromLabel = \ (Config _ b) -> b

Which you can use to access individual fields as follows:

>getConfigHost :: Config -> Text
>getConfigHost = #host

To make use of that ensure to have the @OverloadedLabels@ compiler extension enabled.
-}
accessorIsLabel =
  effectless InstanceDecs.accessorIsLabel

mapperIsLabel =
  effectless InstanceDecs.mapperIsLabel

{-|
Combination of 'constructorIsLabel', 'mapperIsLabel' and 'accessorIsLabel'.
-}
isLabel =
  constructorIsLabel <>
  mapperIsLabel <>
  accessorIsLabel

module Domain.Settings
where

import Domain.Prelude


data FieldNaming =
  FieldNaming
    Bool
    {-^ Prefix with underscore. -}

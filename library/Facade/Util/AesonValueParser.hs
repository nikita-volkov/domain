module Facade.Util.AesonValueParser
where

import Facade.Prelude
import AesonValueParser


possibleField :: Text -> Value a -> Object (Maybe a)
possibleField name parser =
  field name (nullable parser) <|> pure Nothing

{-|
Same as 'possibleField', but generalized to monoidal values.
-}
monoidalPossibleField :: Monoid a => Text -> Value a -> Object a
monoidalPossibleField name parser =
  fmap fold (possibleField name parser)

joinedPossibleField :: Text -> Value (Maybe a) -> Object (Maybe a)
joinedPossibleField name parser =
  fmap join (possibleField name parser)

possibleFieldWithDefault :: Text -> a -> Value a -> Object a
possibleFieldWithDefault name def parser =
  possibleField name parser &
  fmap (fromMaybe def)

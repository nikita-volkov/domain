module Facade.Components.Resolver
where

import Facade.Prelude hiding (lookup)
import Facade.Model
import qualified Facade.V1DocModel as Doc
import qualified Facade.Util.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


type Env =
  HashMap Text TypeRef

type Err =
  Text

type Eff =
  ExceptT Err ((->) Env)

resolve :: Text -> Eff TypeRef
resolve =
  error "TODO"

type_ :: Doc.Type -> Eff Type
type_ =
  \ case
    Doc.RefType a ->
      RefType <$> typeRef a

typeRef :: Doc.TypeRef -> Eff TypeRef
typeRef (Doc.TypeRef a) =
  case List.unsnoc a of
    Just (b, c) ->
      case b of
        [] ->
          resolve c
        _ ->
          return (GlobalTypeRef b c)
    Nothing ->
      throwE "Broken type ref"
